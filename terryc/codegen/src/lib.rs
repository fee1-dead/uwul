#![deny(rust_2018_idioms)]
use std::borrow::Cow;
use std::collections::hash_map::Entry;
use std::io::Cursor;
use std::mem::take;
use std::rc::Rc;

use coffer::flags::{ClassFlags, MethodFlags};
use coffer::prelude::{
    Code, Constant, GetOrPut, Instruction, Label, LoadOrStore, LocalType, MemberRef, MemberType,
    Method, MethodAttribute, Type,
};
use coffer::version::JavaVersion;
use coffer::{Class, ReadWrite};
use inkwell::builder::Builder;
use inkwell::context::{Context as LLCxt, ContextRef};
use inkwell::module::Module;
use inkwell::types::{BasicType, BasicTypeEnum, FunctionType};
use inkwell::values::{BasicValueEnum, FunctionValue, PointerValue};
use terryc_base::ast::{BinOpKind, TyKind, UnOpKind};
use terryc_base::data::FxHashMap;
use terryc_base::errors::ErrorReported;
use terryc_base::hir::{Func, Literal, Resolution};
use terryc_base::mir::{
    self, Body, Function, MirTree, Operand, Rvalue, Statement, Targets, Terminator,
};
use terryc_base::sym::Symbol;
use terryc_base::{sym, Context, FileId, Id, Providers, TyList};

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
    fn get_builtin_raw(&mut self, sym: Symbol, types: TyList) -> FunctionValue<'a> {
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
    fn gen_function_raw(&mut self, id: Id, f: &Function) -> FunctionValue<'a> {
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

pub struct ClassWriter {
    class: Class,
    mir: Rc<FxHashMap<Id, Function>>,
}

pub struct BodyMaker<'a> {
    c: &'a mut ClassWriter,
    f: &'a Function,
    code: Code,
    locals: FxHashMap<mir::Local, usize>,
}

impl BodyMaker<'_> {
    pub fn gen(&mut self) {
        self.code.max_locals = 5; // FIXME automatically infer locals size
        self.code.max_stack = 5;
        let mut locali = 0usize;
        for (l, ld) in self.f.body.locals.iter_enumerated() {
            if ld.ty == TyKind::Unit {
                continue;
            }
            self.locals.insert(l, locali);
            locali += 1;
        }
        let body = self.f.body.clone();
        for (bb, data) in body.blocks.iter_enumerated() {
            self.add(Instruction::Label(Label(bb.index() as u32)));
            for stmt in &data.statements {
                match stmt {
                    Statement::Assign(l, rvalue) => {
                        if let Rvalue::Use(Operand::Copy(l)) = rvalue {
                            if self.f.body.locals[*l].ty == TyKind::Unit {
                                continue;
                            }
                        }
                        let ty = self.local_type_of(rvalue);
                        self.pushrv(rvalue);
                        self.add(Instruction::store(ty, self.lc(*l).try_into().unwrap()))
                    }
                }
            }
            match &data.terminator {
                mir::Terminator::Return(l) => match self.f.body.locals[*l].ty {
                    TyKind::I32 => {
                        self.add(Instruction::load(
                            LocalType::Int,
                            self.lc(*l).try_into().unwrap(),
                        ));
                        self.add(Instruction::Return(Some(LocalType::Int)))
                    }
                    TyKind::Unit => self.add(Instruction::Return(None)),
                    _ => todo!(),
                },
                mir::Terminator::Goto(bbnew) => {
                    if *bbnew == bb + 1 {
                    } else {
                        todo!()
                    }
                }
                mir::Terminator::SwitchInt(rv, Targets { values, targets }) => {
                    match (&**values, &**targets, rv) {
                        // N.B. We are inverting the binary operation as true will fall through and false will jump.
                        ([1], [iftrue, iffalse], Rvalue::BinaryOp(BinOpKind::Equal, a, b))
                            if *iftrue == bb + 1 =>
                        {
                            self.pushop(a);
                            self.pushop(b);
                            self.add(Instruction::if_icmpne(Label(iffalse.index() as u32)));
                        }
                        ([1], [iftrue, iffalse], Rvalue::BinaryOp(BinOpKind::Greater, a, b))
                            if *iftrue == bb + 1 =>
                        {
                            self.pushop(a);
                            self.pushop(b);
                            self.add(Instruction::if_icmple(Label(iffalse.index() as u32)));
                        }
                        _ => todo!(),
                    }
                }

                mir::Terminator::Call {
                    callee: Resolution::Fn(id),
                    args,
                    destination: (local, destination),
                    types: _,
                } => {
                    if *destination == bb + 1 {
                        // TODO this is super rudimentary as we do not generate other classes
                        for arg in args {
                            self.pushrv(arg);
                        }
                        let func = &self.c.mir[id];
                        let desc = Self::descriptor(&func.args, func.ret);
                        let name = func.name.get_str();
                        let ret = func.ret;
                        self.add(Instruction::invokestatic(MemberRef {
                            owner: "Main".into(),
                            name: name.to_owned().into(),
                            descriptor: desc,
                            itfs: false,
                        }));
                        if ret != TyKind::Unit {
                            self.add(Instruction::store(
                                Self::lower_to_local_type(ret),
                                self.lc(*local).try_into().unwrap(),
                            ))
                        }
                    } else {
                        todo!()
                    }
                }
                mir::Terminator::Call {
                    callee: Resolution::Builtin(sym::println),
                    args,
                    destination,
                    types: _,
                } => {
                    if let [arg] = &args[..] {
                        if destination.1 == bb + 1 {
                            let t = self.type_ofr(arg);

                            let member = MemberRef {
                                owner: "java/io/PrintStream".into(),
                                name: "println".into(),
                                descriptor: Type::method([t], None),
                                itfs: false,
                            };

                            let out = MemberRef {
                                owner: "java/lang/System".into(),
                                name: "out".into(),
                                descriptor: Type::Ref("java/io/PrintStream".into()),
                                itfs: false,
                            };

                            self.add(Instruction::getstatic(out));
                            self.pushrv(arg);
                            self.add(Instruction::invokevirtual(member));
                        } else {
                            todo!()
                        }
                    } else {
                        todo!()
                    }
                }
                mir::Terminator::ReplacedAfterConstruction => todo!(),
                x => todo!("{x:?}"),
            }
        }
        let descriptor = if self.f.name.get_str() == "main" {
            Type::method([Type::array(1, Type::string())], None)
        } else {
            Self::descriptor(&self.f.args, self.f.ret)
        };
        self.c.class.methods.push(Method {
            access: MethodFlags::ACC_FINAL | MethodFlags::ACC_PUBLIC | MethodFlags::ACC_STATIC,
            name: self.f.name.get_str().to_owned().into(),
            descriptor,
            attributes: vec![MethodAttribute::Code(take(&mut self.code))],
        })
    }
    fn add(&mut self, instruction: Instruction) {
        self.code.code.push(instruction);
    }
    fn descriptor(args: &[TyKind], ret: TyKind) -> Type {
        let mut parameters = args.iter().map(|x| Self::lower_to_type(*x)).collect();
        let ret = Self::lower_to_return_type(ret);
        Type::Method { parameters, ret }
    }
    fn lower_to_return_type(ty: TyKind) -> Option<Box<Type>> {
        match ty {
            TyKind::Unit => None,
            x => Some(Box::new(Self::lower_to_type(x))),
        }
    }
    fn lower_to_type(ty: TyKind) -> Type {
        match ty {
            TyKind::I32 => Type::Int,
            TyKind::String => Type::Ref(Cow::Borrowed("java/lang/String")),
            t => todo!("{t:?}"),
        }
    }
    fn lower_to_local_type(ty: TyKind) -> LocalType {
        match ty {
            TyKind::I32 => LocalType::Int,
            TyKind::String => LocalType::Reference,
            TyKind::Unit => LocalType::Reference,
            t => todo!("{t:?}"),
        }
    }
    fn type_ofo(&mut self, op: &Operand) -> Type {
        match op {
            Operand::Copy(l) => Self::lower_to_type(self.f.body.locals[*l].ty),
            Operand::Const(Literal::String(_)) => Type::string(),
            Operand::Const(Literal::Int(_)) => Type::Int,
            Operand::Const(_) => todo!(),
        }
    }
    fn type_ofr(&mut self, rv: &Rvalue) -> Type {
        match rv {
            Rvalue::BinaryOp(_, o, _) => self.type_ofo(o),
            Rvalue::Use(op) => self.type_ofo(op),
            _ => todo!(),
        }
    }
    fn local_type_ofo(&mut self, o: &Operand) -> LocalType {
        match o {
            Operand::Const(Literal::Int(_)) => LocalType::Int,
            Operand::Const(Literal::String(_)) => LocalType::Reference,
            Operand::Copy(l) => Self::lower_to_local_type(self.f.body.locals[*l].ty),
            a => todo!("{a:?}"),
        }
    }
    fn local_type_of(&mut self, rv: &Rvalue) -> LocalType {
        match rv {
            Rvalue::UnaryOp(_, o) => self.local_type_ofo(o),
            Rvalue::BinaryOp(_, o, _) => self.local_type_ofo(o),
            Rvalue::Use(op) => self.local_type_ofo(op),
            a => todo!("{a:?}"),
        }
    }
    fn pushrv(&mut self, rv: &Rvalue) {
        match rv {
            Rvalue::Use(op) => self.pushop(op),
            Rvalue::BinaryOp(bop, o1, o2) => {
                self.pushop(o1);
                self.pushop(o2);
                let insn = match (bop, self.type_ofo(o1)) {
                    (BinOpKind::Add, Type::Int) => Instruction::iadd(),
                    (BinOpKind::Mod, Type::Int) => Instruction::irem(),
                    (BinOpKind::Mul, Type::Int) => Instruction::imul(),
                    (BinOpKind::Sub, Type::Int) => Instruction::isub(),
                    x => todo!("{x:?}"),
                };
                self.add(insn);
            }
            Rvalue::UnaryOp(uop, o) => {
                self.pushop(o);
                let insn = match (uop, self.type_ofo(o)) {
                    (UnOpKind::Minus, Type::Int) => Instruction::ineg(),
                    _ => todo!(),
                };
                self.add(insn);
            }
            _ => todo!(),
        }
    }
    fn pushop(&mut self, op: &Operand) {
        self.add(match op {
            Operand::Const(Literal::String(s)) => {
                Instruction::Push(Constant::String(s.get_str().to_owned().into()).into())
            }
            Operand::Const(Literal::Int(i)) => {
                Instruction::Push(Constant::I32((*i).try_into().unwrap()).into())
            }
            Operand::Copy(l) => Instruction::load(
                Self::lower_to_local_type(self.f.body.locals[*l].ty),
                self.lc(*l).try_into().unwrap(),
            ),
            _ => todo!(),
        })
    }
    fn lc(&self, l: mir::Local) -> usize {
        self.locals[&l]
    }
}

impl ClassWriter {
    fn new(mir: MirTree) -> Self {
        Self {
            class: Class {
                version: JavaVersion::J8,
                access: ClassFlags::ACC_FINAL | ClassFlags::ACC_PUBLIC,
                name: Cow::Borrowed("Main"),
                super_name: Cow::Borrowed("java/lang/Object").into(),
                interfaces: vec![],
                fields: vec![],
                methods: vec![],
                attributes: vec![],
            },
            mir: mir.functions,
        }
    }

    fn gen(&mut self) {
        for f in self.mir.clone().values() {
            let mut b = BodyMaker {
                c: self,
                f,
                code: Code::default(),
                locals: FxHashMap::default(),
            };
            b.gen()
        }
    }
}

// use inkwell::context::Context as LLContext;

fn codegen_llvm(cx: &dyn Context) {
    // let llcx = LLContext::create();
}
pub fn provide(providers: &mut Providers) {
    *providers = Providers {
        codegen,
        ..*providers
    }
}
