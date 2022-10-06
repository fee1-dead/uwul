use super::Block;
use crate::ast::TyKind;
use crate::lex::Ident;
use crate::Id;
use crate::sym::Symbol;

#[derive(PartialEq, Eq, Hash, Debug)]
pub enum Item {
    Fn(ItemFn),
}

#[derive(PartialEq, Eq, Hash, Debug)]
pub struct ItemFn {
    pub id: Id,
    pub name: Symbol,
    pub args: Vec<(Ident, TyKind)>,
    pub ret: TyKind,
    pub block: Block,
}
