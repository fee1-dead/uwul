use std::fmt;

use super::{Block, Ty, Tree};
use crate::lex::Ident;
use crate::{Id, FileId};

#[derive(PartialEq, Eq, Hash, Debug)]
pub struct Item {
    pub kind: ItemKind,
}

#[derive(PartialEq, Eq, Hash, Debug)]
pub struct ItemFn {
    pub name: Ident,
    pub id: Id,
    pub args: Vec<(Ident, Ty)>,
    pub ret: Ty,
    pub body: Block,
}

#[derive(PartialEq, Eq, Hash)]
pub enum ItemKind {
    Fn(ItemFn),
    Mod { name: Ident, tree: Tree },
}

impl fmt::Debug for ItemKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Fn(ItemFn { name, id: _, args, ret, body }) => {
                write!(f, "fn {name}(")?;
                for (name, ty) in args {
                    write!(f, "{name}: {ty:?},")?;
                }
                write!(f, ") -> {ret:?} ")?;
                body.fmt(f)
            }
            Self::Mod { name, tree } => write!(f, "mod {name} {{ {tree:?} }} ")
        }
    }
}
