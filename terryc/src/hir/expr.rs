use crate::sym::Symbol;


pub enum Ty {
    I32,
    F32,
    String,
    Unit,
    Bool,
}

pub enum DefKind {
    Local(Local),
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
    Call {
        callee: HirId,
        args: Vec<Expr>,
    },
    If {
        cond: Box<Expr>,
        then: Block,
    },
    While {
        cond: Box<Expr>,
        body: Block,
    },
    Path(HirId),
}

pub enum Literal {
    Integer(i128),
    Float(f64),
    String(String),
}

pub enum Stmt {
    Local(Local),
}

pub enum Item {
    Fn(ItemFn)
}

pub struct ItemFn {
    id: HirId,

}

pub struct Local {
    id: HirId,
    ty: Ty,
    initializer: Option<Expr>,
}