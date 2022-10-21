use std::fmt;


use super::{Expr, Item, ItemFn, ItemKind};
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
        name: Ident,
        value: Option<Expr>,
    },
    Item(Item),
}

impl fmt::Debug for StmtKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            StmtKind::Expr(expr) => expr.fmt(f),
            StmtKind::Let { name, value, id: _ } => {
                write!(f, "let {name}")?;
                if let Some(value) = value {
                    f.write_str(" = ")?;
                    value.fmt(f)?;
                }
                Ok(())
            }
            StmtKind::Item(Item {
                kind:
                    ItemKind::Fn(ItemFn {
                        name,
                        id: _,
                        args,
                        ret,
                        body,
                    }),
            }) => {
                write!(f, "fn {name}(")?;
                for (name, ty) in args {
                    write!(f, "{name}: {ty:?},")?;
                }
                write!(f, ") -> {ret:?} ")?;
                body.fmt(f)
            }
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
