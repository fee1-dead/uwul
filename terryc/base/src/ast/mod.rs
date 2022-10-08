use std::rc::Rc;

mod stmt;
pub use stmt::*;

mod expr;
pub use expr::*;

mod item;
pub use item::*;

mod ty;
pub use ty::*;

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Tree {
    pub items: Rc<[Item]>,
}
