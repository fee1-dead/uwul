use super::{Block, HirTree};
use crate::ast::TyKind;
use crate::lex::Ident;
use crate::sym::Symbol;
use crate::{Id, FileId};

#[derive(PartialEq, Eq, Hash, Debug)]
pub enum Item {
    Fn(ItemFn),
    Mod {
        name: Ident,
        tree: HirTree,
    }
}

#[derive(PartialEq, Eq, Hash, Debug)]
pub struct ItemFn {
    pub id: Id,
    pub name: Symbol,
    pub args: Vec<FnArg>,
    pub ret: TyKind,
    pub block: Block,
}

#[derive(PartialEq, Eq, Hash, Debug)]
pub struct FnArg {
    pub name: Ident,
    pub ty: TyKind,
    pub id: Id,
}
