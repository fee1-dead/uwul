use std::borrow::Cow;
use std::collections::hash_map::Entry;
use std::io::Cursor;
use std::mem::take;
use std::rc::Rc;

use coffer::prelude::{MemberRef, Type, Code, Instruction, GetOrPut, MemberType, Method, MethodAttribute, Constant, LoadOrStore, LocalType};
use coffer::{Class, ReadWrite};
use coffer::flags::{ClassFlags, MethodFlags};
use coffer::version::JavaVersion;
use terryc_base::ast::{TyKind, BinOpKind};
use terryc_base::data::FxHashMap;
use terryc_base::errors::ErrorReported;
use terryc_base::hir::Literal;
use terryc_base::mir::{self, Body, Operand, Statement, Rvalue};
use terryc_base::{Providers, Context, FileId, sym};

fn codegen(cx: &dyn Context, id: FileId) -> Result<Rc<[u8]>, ErrorReported> {
    let mut writer = ClassWriter::new(cx.mir(id)?);
    writer.gen_main();
    let mut buf = Cursor::new(vec![]);
    writer.class.write_to(&mut buf).unwrap();
    Ok(buf.into_inner().into())
}

pub struct ClassWriter {
    class: Class,
    code: Code,
    body: Rc<Body>,
}


impl ClassWriter {
    fn new(body: Rc<Body>) -> Self {
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
            code: Code::default(),
            body,
        }
    }
    fn add(&mut self, instruction: Instruction) {
        self.code.code.push(instruction);
    }
    fn lower_to_type(ty: TyKind) -> Type {
        match ty {
            TyKind::I32 => Type::Int,
            _ => todo!()
        }
    }
    fn lower_to_local_type(ty: TyKind) -> LocalType {
        match ty {
            TyKind::I32 => LocalType::Int,
            _ => todo!(),
        }
    }
    fn type_ofo(&mut self, op: &Operand) -> Type {
        match op {
            Operand::Copy(l) => Self::lower_to_type(self.body.locals[*l].ty),
            Operand::Const(Literal::String(_)) => Type::string(),
            Operand::Const(Literal::Int(_)) => Type::Int,
            Operand::Const(_) => todo!(),
        }
    }
    fn type_ofr(&mut self, rv: &Rvalue) -> Type {
        match rv {
            Rvalue::BinaryOp(_, o, _) => self.type_ofo(o),
            Rvalue::Use(op) => self.type_ofo(op),
            _ => todo!()
        }
    }
    fn local_type_of(&mut self, rv: &Rvalue) -> LocalType {
        match rv {
            Rvalue::BinaryOp(_, o, _) => {
                match self.type_ofo(o) {
                    Type::Int => LocalType::Int,
                    _ => todo!(),
                }
            }
            _ => todo!()
        }
    }
    fn pushrv(&mut self, rv: &Rvalue) {
        match rv {
            Rvalue::Use(op) => self.pushop(op),
            Rvalue::BinaryOp(BinOpKind::Add, o1, o2) => {
                match self.type_ofo(o1) {
                    Type::Int => {
                        self.pushop(o1);
                        self.pushop(o2);
                        self.add(Instruction::iadd());
                    }
                    _ => todo!()
                }
            }
            _ => todo!()
        }
    }
    fn pushop(&mut self, op: &Operand) {
        self.add(match op {
            Operand::Const(Literal::String(s)) => Instruction::Push(Constant::String(s.get_str().to_owned().into()).into()),
            Operand::Const(Literal::Int(i)) => Instruction::Push(Constant::I32((*i).try_into().unwrap()).into()),
            Operand::Copy(l) => Instruction::load(Self::lower_to_local_type(self.body.locals[*l].ty), l.index() as u16),
            _ => todo!()
        })
    }
    fn gen_main(&mut self) {
        let mut locals = FxHashMap::default();
        self.code.max_locals = 5;
        self.code.max_stack = 5;
        let mut locali = 0usize;
        for (l, ld) in self.body.locals.iter_enumerated() {
            if ld.ty == TyKind::Unit { continue }
            locals.insert(l, locali);
            locali += 1;
        }
        let body = self.body.clone();
        for (bb, data) in body.blocks.iter_enumerated() {
            for stmt in &data.statements {
                match stmt {
                    Statement::Assign(l, rvalue) => {
                        let ty = self.local_type_of(rvalue);
                        self.pushrv(rvalue);
                        self.add(Instruction::store(ty, l.index().try_into().unwrap()))
                    }
                }
            }
            match &data.terminator {
                mir::Terminator::Return(l) => {
                    if self.body.locals[*l].ty != TyKind::Unit {
                        todo!()
                    }

                    self.add(Instruction::Return(None));
                }
                mir::Terminator::Goto(_) => todo!(),
                mir::Terminator::SwitchInt(_, _) => todo!(),
                mir::Terminator::Call { callee: sym::println, args, destination } => {
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
                _ => todo!(),
            }
        }
        self.class.methods.push(Method {
            access: MethodFlags::ACC_FINAL | MethodFlags::ACC_PUBLIC | MethodFlags::ACC_STATIC,
            name: "main".into(),
            descriptor: Type::method([Type::array(1, Type::string())], None),
            attributes: vec![MethodAttribute::Code(take(&mut self.code))],
        })
    }
}

pub fn provide(providers: &mut Providers) {
    *providers = Providers {
        codegen,
        ..*providers
    }
}