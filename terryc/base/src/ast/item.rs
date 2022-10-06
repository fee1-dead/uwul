use super::{Block, Ty};
use crate::lex::Ident;
use crate::Id;

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

#[derive(PartialEq, Eq, Hash, Debug)]
pub enum ItemKind {
    Fn(ItemFn),
}
