mod expr;
mod item;
pub use expr::*;
pub use item::*;

use crate::ast::TyKind;
use crate::sym::Symbol;
use crate::Id;

#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy)]
pub enum Resolution {
    Builtin(Symbol),
    Local(Id),
}

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct Func {
    pub args: Vec<TyKind>,
    pub ret: TyKind,
}
