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
use terryc_base::ast::{BinOpKind, TyKind, UnOpKind};
use terryc_base::data::FxHashMap;
use terryc_base::errors::ErrorReported;
use terryc_base::hir::{Func, Literal, Resolution};
use terryc_base::mir::{self, Body, Function, MirTree, Operand, Rvalue, Statement, Targets};
use terryc_base::{sym, Context, FileId, Id, Providers};

fn codegen(cx: &dyn Context, id: FileId) -> Result<Rc<[u8]>, ErrorReported> {
    let mut writer = ClassWriter::new(cx.mir(id)?);
    writer.gen();
    let mut buf = Cursor::new(vec![]);
    writer.class.write_to(&mut buf).unwrap();
    Ok(buf.into_inner().into())
}

pub struct ClassWriter {
    class: Class,
    mir: Rc<[Function]>,
    funcs: FxHashMap<Id, Func>,
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
                } => {
                    if *destination == bb + 1 {
                        // TODO this is super rudimentary as we do not generate other classes
                        for arg in args {
                            self.pushrv(arg);
                        }
                        let func = &self.c.funcs[id];
                        let desc = Self::descriptor(&func.args, func.ret);
                        let name = func.name.symbol.get_str();
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
            funcs: mir.funcs,
        }
    }

    fn gen(&mut self) {
        for f in &*self.mir.clone() {
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
