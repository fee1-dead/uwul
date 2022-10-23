#![deny(rust_2018_idioms)]
#![feature(exit_status_error)]

use std::path::Path;
use std::process::Command;

use inkwell::builder::Builder;
use inkwell::context::Context as LLCxt;
use inkwell::module::Module;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
};
use inkwell::types::{BasicType, BasicTypeEnum, FunctionType};
use inkwell::values::{BasicValueEnum, FunctionValue, PointerValue};
use inkwell::{AddressSpace, IntPredicate, OptimizationLevel};
use terryc_base::ast::{BinOpKind, TyKind, UnOpKind};
use terryc_base::data::FxHashMap;
use terryc_base::errors::ErrorReported;
use terryc_base::hir::{Literal, Resolution};
use terryc_base::mir::{self, Function, Local, Operand, Rvalue, Statement, Terminator};
use terryc_base::sym::{self, Symbol};
use terryc_base::{Context, FileId, Id, Providers, TyList};

fn codegen(cx: &dyn Context, id: FileId) -> Result<(), ErrorReported> {
    let llcx = LLCxt::create();
    let mut codegen = LlvmCodegen::new(cx, &llcx, cx.mir(id)?);
    codegen.gen();
    codegen.module.print_to_stderr();
    codegen
        .module
        .verify()
        .unwrap_or_else(|x| println!("{x:?}"));
    Target::initialize_native(&InitializationConfig::default()).unwrap();
    let triple = TargetMachine::get_default_triple();
    let target = Target::from_triple(&triple).unwrap();
    let machine = target
        .create_target_machine(
            &triple,
            "x86-64",
            "",
            OptimizationLevel::Default,
            RelocMode::PIC,
            CodeModel::Default,
        )
        .unwrap();
    machine
        .write_to_file(&codegen.module, FileType::Object, Path::new("/tmp/a"))
        .unwrap();
    let mut cmd = Command::new("cc")
        .arg("-fPIE")
        .arg("-o")
        .arg("out")
        .arg("/tmp/a")
        .spawn()
        .unwrap();
    cmd.wait().unwrap().exit_ok().unwrap();
    Ok(())
}

pub struct LlvmCodegen<'a, 'cx> {
    pub cx: &'cx dyn Context,
    pub llcx: &'a LLCxt,
    pub mir: mir::MirTree,
    pub builder: Builder<'a>,
    pub module: Module<'a>,
    pub fun: Option<FunctionValue<'a>>,
    pub locals: FxHashMap<Local, PointerValue<'a>>,
    pub genned_functions: FxHashMap<Id, FunctionValue<'a>>,
    pub builtins: FxHashMap<(Symbol, TyList), FunctionValue<'a>>,
    pub c_printf: Option<FunctionValue<'a>>,
}

macro_rules! cached {
    ($(pub fn $name:ident cached in $cache:ident via $raw:ident ($key:expr) (&mut self, $($arg: ident: $argty: ty),* $(,)?) -> $ret:ty;)+) => {
        $(pub fn $name(&mut self, $($arg: $argty),*) -> $ret {
            if let Some(result) = self.$cache.get(&$key) {
                *result
            } else {
                let result = self.$raw($($arg),*);
                self.$cache.insert($key, result);
                result
            }
        })+
    };
}

impl<'a, 'cx> LlvmCodegen<'a, 'cx> {
    pub fn new(cx: &'cx dyn Context, llcx: &'a LLCxt, mir: mir::MirTree) -> Self {
        let module = llcx.create_module("main");
        let builder = llcx.create_builder();
        Self {
            cx,
            llcx,
            mir,
            builder,
            module,
            fun: None,
            locals: Default::default(),
            genned_functions: Default::default(),
            builtins: Default::default(),
            c_printf: None,
        }
    }

    pub fn basic_ty(&mut self, ty: TyKind) -> BasicTypeEnum<'a> {
        match ty {
            TyKind::Bool => self.llcx.bool_type().into(),
            TyKind::F32 => self.llcx.f32_type().into(),
            TyKind::I32 => self.llcx.i32_type().into(),
            TyKind::Unit => unreachable!(),
            TyKind::String => self.llcx.i8_type().ptr_type(AddressSpace::Generic).into(),
            // x => todo!("{x:?}"),
        }
    }

    pub fn func_ty(&mut self, f: &Function) -> FunctionType<'a> {
        let args: Vec<_> = f
            .args
            .iter()
            .copied()
            .map(|x| self.basic_ty(x).into())
            .collect();
        if f.ret == TyKind::Unit {
            self.llcx.void_type().fn_type(&args, false)
        } else {
            self.basic_ty(f.ret).fn_type(&args, false)
        }
    }
    pub fn literal(&mut self, c: &Literal) -> BasicValueEnum<'a> {
        match c {
            Literal::Bool(b) => self.llcx.bool_type().const_int(*b as u64, false).into(),
            Literal::Int(i) => self.llcx.i32_type().const_int(*i as u64, false).into(),
            Literal::String(s) => self
                .builder
                .build_global_string_ptr(s.get_str(), "global")
                .as_pointer_value()
                .into(),
            x => todo!("{x:?}"),
        }
    }
    pub fn operand(&mut self, op: &Operand) -> BasicValueEnum<'a> {
        match op {
            Operand::Const(c) => self.literal(c),
            Operand::Copy(local) => self.local(*local),
        }
    }
    pub fn binop(
        &mut self,
        binop: BinOpKind,
        a: BasicValueEnum<'a>,
        b: BasicValueEnum<'a>,
    ) -> BasicValueEnum<'a> {
        macro_rules! gen_match {
            (
                $($binop: ident => {
                    $($value: ident => [$name: ident$(, $($args:tt)+)?])*
                })*
            ) => {
                match binop {
                    $(BinOpKind::$binop => match (a, b) {
                        $((BasicValueEnum::$value(a), BasicValueEnum::$value(b)) => self.builder.$name($($($args)*,)? a, b, "").into(),)*
                        x => todo!("{x:?}"),
                    },)*
                    x => todo!("{x:?}"),
                }
            };
        }
        gen_match! {
            Add => {
                IntValue => [build_int_add]
                FloatValue => [build_float_add]
            }
            Sub => {
                IntValue => [build_int_sub]
            }
            Mul => {
                IntValue => [build_int_mul]
            }
            Div => {
                IntValue => [build_int_signed_div]
            }
            Mod => {
                IntValue => [build_int_signed_rem]
            }
            Equal => {
                IntValue => [build_int_compare, IntPredicate::EQ]
            }
            Greater => {
                IntValue => [build_int_compare, IntPredicate::SGT]
            }
        }
    }
    pub fn rvalue(&mut self, rv: &Rvalue) -> BasicValueEnum<'a> {
        match rv {
            Rvalue::Use(op) => self.operand(op),
            Rvalue::BinaryOp(kind, a, b) => {
                let a = self.operand(a);
                let b = self.operand(b);
                self.binop(*kind, a, b)
            }
            Rvalue::UnaryOp(UnOpKind::Minus, a) => match self.operand(a) {
                BasicValueEnum::IntValue(x) => self.builder.build_int_neg(x, "").into(),
                _ => todo!(),
            },
            x => todo!("{x:?}"),
        }
    }

    fn c_printf(&mut self) -> FunctionValue<'a> {
        *self.c_printf.get_or_insert_with(|| {
            self.module.add_function(
                "printf",
                self.llcx.void_type().fn_type(
                    &[self.llcx.i8_type().ptr_type(AddressSpace::Generic).into()],
                    true,
                ),
                None,
            )
        })
    }
    fn get_builtin_raw(&mut self, sym: Symbol, types: TyList) -> FunctionValue<'a> {
        match (sym, &*types) {
            (sym::println, &[ty]) => {
                let input = self.basic_ty(ty);
                let func = self.module.add_function(
                    "println",
                    self.llcx.void_type().fn_type(&[input.into()], false),
                    None,
                );
                let bb = self.llcx.append_basic_block(func, "entry");
                let builder = self.llcx.create_builder();
                builder.position_at_end(bb);
                let fmt_global = format!("fmt_{ty:?}");

                let fmt_global = if let Some(g) = self.module.get_global(&fmt_global) {
                    g
                } else {
                    let fmt_value = match ty {
                        TyKind::I32 => "%d\n",
                        TyKind::F32 => "%f\n",
                        TyKind::String => "%s\n",
                        TyKind::Unit => "()\n",
                        TyKind::Bool => "%s\n",
                    };
                    builder.build_global_string_ptr(fmt_value, &fmt_global)
                };

                let p = func.get_first_param().unwrap();
                let val = if ty == TyKind::Bool {
                    let ty = self.llcx.i8_type().ptr_type(AddressSpace::Generic);
                    let s = ty.const_array(&[
                        self.builder
                            .build_global_string_ptr("false", "false_value")
                            .as_pointer_value(),
                        self.builder
                            .build_global_string_ptr("true", "true_value")
                            .as_pointer_value(),
                    ]);
                    let global = self.module.add_global(ty.array_type(2), None, "bool");
                    global.set_initializer(&s);
                    unsafe {
                        builder.build_in_bounds_gep(
                            global.as_pointer_value(),
                            &[p.into_int_value()],
                            "",
                        )
                    }
                    .into()
                } else {
                    p
                };

                let printf = self.c_printf();
                builder.build_call(
                    printf,
                    &[fmt_global.as_pointer_value().into(), val.into()],
                    "",
                );
                builder.build_return(None);

                func
            }
            _ => todo!(),
        }
    }
    cached! {
        pub fn get_builtin cached in builtins via get_builtin_raw((sym, types)) (&mut self, sym: Symbol, types: TyList) -> FunctionValue<'a>;
    }
    pub fn get_fn(&mut self, resolution: Resolution, types: TyList) -> FunctionValue<'a> {
        match resolution {
            Resolution::Builtin(s) => self.get_builtin(s, types),
            Resolution::Fn(id) => self.gen_function(id, &self.mir.functions.clone()[&id]),
            Resolution::Local(_) => unreachable!(),
        }
    }
    fn local(&mut self, l: Local) -> BasicValueEnum<'a> {
        if l.index() < self.fun.unwrap().count_params() as usize {
            self.fun.unwrap().get_nth_param(l.index() as u32).unwrap()
        } else {
            self.builder.build_load(self.locals[&l], "")
        }
    }
    fn gen_function(&mut self, id: Id, f: &Function) -> FunctionValue<'a> {
        if let Some(val) = self.genned_functions.get(&id) {
            return *val;
        }
        let name = if f.name == sym::main {
            "__entrypoint_actual"
        } else {
            f.name.get_str()
        };
        let func_ty = self.func_ty(f);
        let fun = self.module.add_function(name, func_ty, None);
        self.genned_functions.insert(id, fun);
        self.fun = Some(fun);
        let bb = self.llcx.append_basic_block(fun, "entry");

        self.builder.position_at_end(bb);

        let locals: FxHashMap<_, _> = f
            .body
            .locals
            .iter_enumerated()
            .skip(func_ty.count_param_types() as usize)
            .filter(|(_, data)| data.ty != TyKind::Unit)
            .map(|(local, data)| {
                let ty = self.basic_ty(data.ty);
                let ptr = self.builder.build_alloca(ty, &format!("{local:?}"));
                (local, ptr)
            })
            .collect();
        self.locals = locals;

        let basic_blocks: Vec<_> = f
            .body
            .blocks
            .iter_enumerated()
            .map(|(bb, _)| self.llcx.append_basic_block(fun, &format!("{bb:?}")))
            .collect();

        self.builder
            .build_unconditional_branch(basic_blocks.iter().copied().next().unwrap());

        for (i, bb) in f.body.blocks.iter_enumerated() {
            self.builder.position_at_end(basic_blocks[i.index()]);
            for stmt in &bb.statements {
                match stmt {
                    Statement::Assign(to, from) => {
                        let place = self.locals[to];
                        let rv = self.rvalue(from);
                        self.builder.build_store(place, rv);
                    }
                }
            }
            match &bb.terminator {
                Terminator::Goto(bb) => {
                    self.builder
                        .build_unconditional_branch(basic_blocks[bb.index()]);
                }
                Terminator::Call {
                    callee,
                    args,
                    destination: (destination_value, destination_bb),
                    types,
                } => {
                    let func = self.get_fn(*callee, *types);
                    let args: Vec<_> = args.iter().map(|x| self.rvalue(x).into()).collect();
                    let ret = self.builder.build_call(func, &args, "");
                    if f.body.locals[*destination_value].ty != TyKind::Unit {
                        self.builder.build_store(
                            self.locals[destination_value],
                            ret.try_as_basic_value().expect_left("not void"),
                        );
                    }
                    self.builder
                        .build_unconditional_branch(basic_blocks[destination_bb.index()]);
                }
                Terminator::Return(local) => {
                    if f.body.locals[*local].ty == TyKind::Unit {
                        self.builder.build_return(None);
                    } else {
                        let val = self.local(*local);
                        self.builder.build_return(Some(&val));
                    }
                }
                Terminator::SwitchInt(x, cases) => {
                    let rv = self.rvalue(x).into_int_value();
                    let ty = rv.get_type();
                    self.builder.build_switch(
                        rv,
                        basic_blocks[cases.else_().index()],
                        &cases
                            .iter()
                            .map(|(x, bb)| {
                                (ty.const_int(x as u64, false), basic_blocks[bb.index()])
                            })
                            .collect::<Vec<_>>(),
                    );
                }
                Terminator::ReplacedAfterConstruction => unreachable!(),
            }
        }

        fun
    }

    pub fn gen(&mut self) {
        for (id, fun) in &*self.mir.functions.clone() {
            self.gen_function(*id, fun);
        }
        let main = self.module.add_function(
            "main",
            self.llcx.i32_type().fn_type(
                &[
                    self.llcx.i32_type().into(),
                    self.llcx
                        .i8_type()
                        .ptr_type(AddressSpace::Generic)
                        .ptr_type(AddressSpace::Generic)
                        .into(),
                ],
                false,
            ),
            None,
        );
        self.builder
            .position_at_end(self.llcx.append_basic_block(main, "start"));
        self.builder.build_call(
            self.module.get_function("__entrypoint_actual").unwrap(),
            &[],
            "call_main",
        );
        self.builder
            .build_return(Some(&self.llcx.i32_type().const_int(0, false)));
    }
}

pub fn provide(providers: &mut Providers) {
    *providers = Providers {
        codegen,
        ..*providers
    }
}
