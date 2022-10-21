#![deny(rust_2018_idioms)]

use inkwell::builder::Builder;
use inkwell::context::{Context as LLCxt};
use inkwell::module::Module;
use inkwell::types::{BasicType, BasicTypeEnum, FunctionType};
use inkwell::values::{BasicValueEnum, FunctionValue, PointerValue};
use terryc_base::ast::{BinOpKind, TyKind};
use terryc_base::data::FxHashMap;
use terryc_base::errors::ErrorReported;
use terryc_base::hir::{Literal, Resolution};
use terryc_base::mir::{
    self, Function, Operand, Rvalue, Statement, Terminator,
};
use terryc_base::sym::Symbol;
use terryc_base::{Context, FileId, Id, Providers, TyList};

fn codegen(cx: &dyn Context, id: FileId) -> Result<(), ErrorReported> {
    let llcx = LLCxt::create();
    LlvmCodegen::new(cx, &llcx, cx.mir(id)?).gen();

    Ok(())
}

pub struct LlvmCodegen<'a, 'cx> {
    pub cx: &'cx dyn Context,
    pub llcx: &'a LLCxt,
    pub mir: mir::MirTree,
    pub builder: Builder<'a>,
    pub module: Module<'a>,
    pub locals: Vec<PointerValue<'a>>,
    pub genned_functions: FxHashMap<Id, FunctionValue<'a>>,
    pub builtins: FxHashMap<(Symbol, TyList), FunctionValue<'a>>,
}

macro_rules! cached {
    (pub fn $name:ident cached in $cache:ident via $raw:ident ($key:expr) (&mut self, $($arg: ident: $argty: ty),* $(,)?) -> $ret:ty;) => {
        pub fn $name(&mut self, $($arg: $argty),*) -> $ret {
            if let Some(result) = self.$cache.get(&$key) {
                *result
            } else {
                let result = self.$raw($($arg),*);
                self.$cache.insert($key, result);
                result
            }
        }
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
            locals: Vec::new(),
            genned_functions: Default::default(),
            builtins: Default::default(),
        }
    }

    pub fn basic_ty(&mut self, ty: TyKind) -> BasicTypeEnum<'a> {
        match ty {
            TyKind::Bool => self.llcx.bool_type().into(),
            TyKind::F32 => self.llcx.f32_type().into(),
            TyKind::I32 => self.llcx.i32_type().into(),
            _ => todo!(),
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
            _ => todo!(),
        }
    }
    pub fn operand(&mut self, op: &Operand) -> BasicValueEnum<'a> {
        match op {
            Operand::Const(c) => self.literal(c),
            Operand::Copy(local) => self.locals[local.index()].into(),
        }
    }
    pub fn rvalue(&mut self, rv: &Rvalue) -> BasicValueEnum<'a> {
        match rv {
            Rvalue::BinaryOp(BinOpKind::Add, a, b) => {
                let a = self.operand(a);
                let b = self.operand(b);
                match (a, b) {
                    (BasicValueEnum::IntValue(int_a), BasicValueEnum::IntValue(int_b)) => {
                        self.builder.build_int_add(int_a, int_b, "").into()
                    }
                    _ => todo!(),
                }
            }
            _ => todo!(),
        }
    }
    fn get_builtin_raw(&mut self, _sym: Symbol, _types: TyList) -> FunctionValue<'a> {
        todo!()
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
    fn gen_function_raw(&mut self, _id: Id, f: &Function) -> FunctionValue<'a> {
        let func_ty = self.func_ty(f);
        let fun = self.module.add_function(f.name.get_str(), func_ty, None);
        let bb = self.llcx.append_basic_block(fun, "entry");

        self.builder.position_at_end(bb);

        let locals: Vec<_> = f
            .body
            .locals
            .iter_enumerated()
            .map(|(local, data)| {
                let ty = self.basic_ty(data.ty);
                self.builder.build_alloca(ty, &format!("{local:?}"))
            })
            .collect();
        self.locals = locals;

        let basic_blocks: Vec<_> = f
            .body
            .blocks
            .iter_enumerated()
            .map(|(bb, _)| self.llcx.append_basic_block(fun, &format!("{bb:?}")))
            .collect();

        for (i, bb) in f.body.blocks.iter_enumerated() {
            let llbb = self.llcx.append_basic_block(fun, &format!("{i:?}"));
            self.builder.position_at_end(llbb);
            for stmt in &bb.statements {
                match stmt {
                    Statement::Assign(to, from) => {
                        let place = self.locals[to.index()];
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
                            self.locals[destination_value.index()],
                            ret.try_as_basic_value().expect_left("not void"),
                        );
                    }
                    self.builder
                        .build_unconditional_branch(basic_blocks[destination_bb.index()]);
                }
                Terminator::Return(local) => {
                    self.builder
                        .build_return(if f.body.locals[*local].ty == TyKind::Unit {
                            None
                        } else {
                            Some(&self.locals[local.index()])
                        });
                }
                Terminator::SwitchInt(x, cases) => {
                    let rv = self.rvalue(x).into_int_value();
                    self.builder.build_switch(
                        rv,
                        basic_blocks[cases.else_().index()],
                        &cases
                            .iter()
                            .map(|(x, bb)| {
                                (
                                    self.llcx.i32_type().const_int(x as u64, false),
                                    basic_blocks[bb.index()],
                                )
                            })
                            .collect::<Vec<_>>(),
                    );
                }
                Terminator::ReplacedAfterConstruction => unreachable!(),
            }
        }

        fun
    }
    cached! {
        pub fn gen_function cached in genned_functions via gen_function_raw(id) (&mut self, id: Id, f: &Function) -> FunctionValue<'a>;
    }
    pub fn gen(&mut self) {
        for (id, fun) in &*self.mir.functions.clone() {
            self.gen_function(*id, fun);
        }
    }
}

pub fn provide(providers: &mut Providers) {
    *providers = Providers {
        codegen,
        ..*providers
    }
}
