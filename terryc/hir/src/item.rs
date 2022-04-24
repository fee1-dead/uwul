use terryc_ast::TyKind;
use terryc_base::lex::Ident;

use super::HirId;

pub enum Item {
    Fn(ItemFn),
}

pub struct ItemFn {
    pub id: HirId,
    pub args: Vec<(Ident, TyKind)>,
}
