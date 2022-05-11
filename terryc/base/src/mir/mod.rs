use crate::ast::{BinOpKind, TyKind, UnOpKind};
use crate::hir::Literal;
use crate::sym::Symbol;

index_vec::define_index_type! {
    pub struct Local = u32;
}

#[derive(PartialEq, Eq, Hash, Debug)]
pub struct LocalData {
    pub ty: TyKind,
}

#[derive(PartialEq, Eq, Hash, Debug)]
pub enum Operand {
    Copy(Local),
    Const(Literal),
}

#[derive(PartialEq, Eq, Hash, Debug)]
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

#[derive(PartialEq, Eq, Hash, Debug)]
pub enum UnOp {
    Neg,
    Not,
}

#[derive(PartialEq, Eq, Hash, Debug)]
pub enum Rvalue {
    Use(Operand),
    BinaryOp(BinOpKind, Operand, Operand),
    UnaryOp(UnOpKind, Operand),
}

#[derive(PartialEq, Eq, Hash, Debug)]
pub enum Statement {
    Assign(Local, Rvalue),
}

#[derive(PartialEq, Eq, Hash, Debug)]
pub struct Targets {
    pub values: Vec<i32>,
    pub targets: Vec<BasicBlock>,
}

#[derive(PartialEq, Eq, Hash, Debug)]
pub enum Terminator {
    Return(Local),
    Goto(BasicBlock),
    SwitchInt(Rvalue, Targets),
    Call {
        callee: Symbol,
        args: Vec<Rvalue>,
        destination: (Local, BasicBlock),
    },
    ReplacedAfterConstruction,
}

index_vec::define_index_type! {
    pub struct BasicBlock = u32;
}

#[derive(PartialEq, Eq, Hash, Debug)]
pub struct BasicBlockData {
    pub statements: Vec<Statement>,
    pub terminator: Terminator,
}

#[derive(PartialEq, Eq, Hash, Debug, Default)]
pub struct Body {
    pub blocks: index_vec::IndexVec<BasicBlock, BasicBlockData>,
    pub locals: index_vec::IndexVec<Local, LocalData>,
}

impl Body {
    pub fn expect_last_mut(&mut self) -> &mut BasicBlockData {
        self.blocks.last_mut().expect("expected last basic block")
    }
}
