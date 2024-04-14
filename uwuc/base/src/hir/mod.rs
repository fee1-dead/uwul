mod expr;
mod item;
use std::hash::Hash;
use std::rc::Rc;

pub use expr::*;
pub use item::*;
use rustc_hash::FxHashMap;

use crate::ast::{Ty, TyKind};
use crate::lex::Ident;
use crate::sym::Symbol;
use crate::Id;

#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy)]
pub enum Resolution {
    Builtin(Symbol),
    Fn(Id),
    Local(Id),
}

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub struct Func {
    pub name: Ident,
    pub args: Vec<Ty>,
    pub ret: TyKind,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct HirTree {
    pub functions: FxHashMap<Id, Func>,
    pub items: Rc<[Item]>,
}

impl Hash for HirTree {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.items.hash(state);
    }
}
