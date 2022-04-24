use terryc_ast::{DeclId, TyKind};
use terryc_base::Id;
use terryc_base::sym::Symbol;
use terryc_base::lex::Ident;

use crate::Item;

pub enum DefKind {
    Local(LocalDecl),
    Fn(Ident),
}

pub struct HirId {
    pub id: u32,
}

pub struct Block {
    pub statements: Vec<Stmt>,
    pub expr: Option<Box<Expr>>,
}

pub enum Expr {
    Block(Block),
    Call { callee: HirId, args: Vec<Expr> },
    If { cond: Box<Expr>, then: Block },
    While { cond: Box<Expr>, body: Block },
    Assign { to: Id, rvalue: Box<Expr> },
    Literal(Literal),
    Group(Box<Expr>),
    Use(Id),
}

pub enum Literal {
    Int(u128),
    String(Symbol),
    Float(f64),
    Bool(bool),
}

pub enum Stmt {
    Local(LocalDecl),
    Expr(Expr),
    Item(Item),
}

pub struct LocalDecl {
    pub(crate) id: Id,
    pub(crate) ty: TyKind,
    pub(crate) initializer: Option<Expr>,
}

pub struct Local(u32);

pub enum Operand {
    Copy(Local),
    Const(Literal),
}

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

pub enum UnOp {
    Neg,
    Not,
}

pub enum Rvalue {
    Use(Operand),
    BinaryOp(BinOp, Operand, Operand),
    UnaryOp(UnOp, Operand),
}

pub enum Statement {
    Assign(Local, Rvalue),
}

pub struct Targets {
    values: Vec<i32>,
    targets: Vec<BasicBlock>,
}

pub enum Terminator {
    Return(Local),
    Goto(BasicBlock),
    SwitchInt(Operand, Targets),
}

index_vec::define_index_type! {
    pub struct BasicBlock = u32;
}

pub struct BasicBlockData {
    statements: Vec<Statement>,
    terminator: Terminator,
}

pub struct Body {
    blocks: index_vec::IndexVec<BasicBlock, BasicBlockData>,
    locals: Vec<TyKind>,
}