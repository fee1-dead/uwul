use std::fmt;

use super::{Expr, Item, ItemFn, ItemKind, Ty, TyKind};
use crate::lex::Ident;
use crate::{Id, Span};

#[derive(PartialEq, Eq, Hash)]
pub struct Stmt {
    pub kind: StmtKind,
}

#[derive(PartialEq, Eq, Hash)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    /// Optional trailing expression
    pub expr: Option<Box<Expr>>,
    pub span: Span,
}

pub struct Function {
    pub name: Ident,
    pub params: Vec<Ident>,
    pub body: Block,
}

#[derive(PartialEq, Eq, Hash)]
pub enum StmtKind {
    Expr(Expr),
    Let {
        id: Id,
        user_ty: Option<Ty>,
        name: Ident,
        value: Option<Expr>,
    },
    Item(Item),
}

impl fmt::Debug for StmtKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            StmtKind::Expr(expr) => expr.fmt(f),
            StmtKind::Let {
                name,
                user_ty,
                value,
                id: _,
            } => {
                write!(f, "let {name}")?;
                if let Some(Ty { kind, .. }) = user_ty {
                    write!(f, ": {kind}")?;
                }
                if let Some(value) = value {
                    write!(f, " = {value:?}")?;
                }
                Ok(())
            }
            StmtKind::Item(Item {
                kind,
            }) => kind.fmt(f)
        }
    }
}

impl fmt::Debug for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.kind.fmt(f)
    }
}

impl fmt::Debug for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut set = f.debug_set();
        set.entries(&self.stmts);

        if let Some(expr) = &self.expr {
            set.entry(expr);
        }
        set.finish()
    }
}
