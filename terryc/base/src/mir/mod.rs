use core::fmt;
use std::fmt::Debug;
use std::rc::Rc;

use rustc_hash::FxHashMap;

use crate::Id;
use crate::ast::{BinOpKind, TyKind, UnOpKind};
use crate::hir::{Literal, Resolution, Func};
use crate::sym::Symbol;

index_vec::define_index_type! {
    pub struct Local = u32;
    DEBUG_FORMAT = "_{}";
}

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct LocalData {
    pub ty: TyKind,
}

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub enum Operand {
    Copy(Local),
    Const(Literal),
}

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Lt,
    Le,
    Ne,
    Ge,
    Gt,
}

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub enum UnOp {
    Neg,
    Not,
}

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub enum Rvalue {
    Use(Operand),
    BinaryOp(BinOpKind, Operand, Operand),
    UnaryOp(UnOpKind, Operand),
}

#[derive(PartialEq, Eq, Hash, Clone)]
pub enum Statement {
    Assign(Local, Rvalue),
}

impl Debug for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Assign(local, rvalue) => write!(f, "{local:?} = {rvalue:?}"),
        }
    }
}

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct Targets {
    pub values: Vec<i32>,
    pub targets: Vec<BasicBlock>,
}

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub enum Terminator {
    Return(Local),
    Goto(BasicBlock),
    SwitchInt(Rvalue, Targets),
    Call {
        callee: Resolution,
        args: Vec<Rvalue>,
        destination: (Local, BasicBlock),
    },
    ReplacedAfterConstruction,
}

index_vec::define_index_type! {
    pub struct BasicBlock = u32;
}

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct BasicBlockData {
    pub statements: Vec<Statement>,
    pub terminator: Terminator,
}

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct Function {
    pub body: Body,
    pub name: Symbol,
    pub args: Vec<TyKind>,
    pub ret: TyKind,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct MirTree {
    pub functions: Rc<[Function]>,
    pub funcs: FxHashMap<Id, Func>,
}

#[derive(PartialEq, Eq, Hash, Debug, Default, Clone)]
pub struct Body {
    pub blocks: index_vec::IndexVec<BasicBlock, BasicBlockData>,
    pub locals: index_vec::IndexVec<Local, LocalData>,
}

impl Body {
    pub fn expect_last_mut(&mut self) -> &mut BasicBlockData {
        self.blocks.last_mut().expect("expected last basic block")
    }
}
