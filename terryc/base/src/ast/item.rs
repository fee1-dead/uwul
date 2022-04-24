use crate::Id;
use crate::lex::Ident;

use super::{Block, Ty};

#[derive(PartialEq, Eq, Hash)]
pub struct Item {
    pub kind: ItemKind,
}

#[derive(PartialEq, Eq, Hash)]
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
}