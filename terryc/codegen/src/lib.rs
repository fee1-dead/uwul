use std::borrow::Cow;
use std::collections::hash_map::Entry;
use std::io::Cursor;
use std::mem::take;
use std::rc::Rc;

use coffer::prelude::{MemberRef, Type, Code, Instruction, GetOrPut, MemberType, Method, MethodAttribute, Constant};
use coffer::{Class, ReadWrite};
use coffer::flags::{ClassFlags, MethodFlags};
use coffer::version::JavaVersion;
use terryc_base::ast::TyKind;
use terryc_base::data::FxHashMap;
use terryc_base::errors::ErrorReported;
use terryc_base::hir::Literal;
use terryc_base::mir::{self, Body, Operand};
use terryc_base::{Providers, Context, FileId, sym};

fn codegen(cx: &dyn Context, id: FileId) -> Result<Rc<[u8]>, ErrorReported> {
    let mut writer = ClassWriter::default();
    writer.gen_main(&*cx.mir(id)?);
    let mut buf = Cursor::new(vec![]);
    writer.class.write_to(&mut buf).unwrap();
    Ok(buf.into_inner().into())
}

pub struct ClassWriter {
    class: Class,
    code: Code,
}

impl Default for ClassWriter {
    fn default() -> Self {
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
        }
    }
}

impl ClassWriter {
    fn add(&mut self, instruction: Instruction) {
        self.code.code.push(instruction);
    }
    fn type_of(&mut self, op: &Operand) -> Type {
        match op {
            Operand::Copy(_) => todo!(),
            Operand::Const(Literal::String(_)) => Type::string(),
            Operand::Const(Literal::Int(_)) => Type::Int,
            Operand::Const(_) => todo!(),
        }
    }
    fn genop(&mut self, op: &Operand) {
        self.add(match op {
            Operand::Const(Literal::String(s)) => Instruction::Push(Constant::String(s.get_str().to_owned().into()).into()),
            Operand::Const(Literal::Int(i)) => Instruction::Push(Constant::I32((*i).try_into().unwrap()).into()),
            _ => todo!()
        })
    }
    fn gen_main(&mut self, body: &Body) {
        self.code.max_locals = 5;
        self.code.max_stack = 5;
        for (bb, data) in body.blocks.iter_enumerated() {
            for stmt in &data.statements {
                todo!()
            }
            match &data.terminator {
                mir::Terminator::Return(l) => {
                    if body.locals[*l].ty != TyKind::Unit {
                        todo!()
                    }

                    self.add(Instruction::Return(None));
                }
                mir::Terminator::Goto(_) => todo!(),
                mir::Terminator::SwitchInt(_, _) => todo!(),
                mir::Terminator::Call { callee: sym::println, args, destination } => {
                    if let [arg] = &args[..] {
                        if destination.1 == bb + 1 {
                            let t = self.type_of(arg);

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
                            self.genop(arg);
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