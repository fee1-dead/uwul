mod expr;
mod item;
pub use expr::*;
pub use item::*;

use crate::sym::Symbol;
use crate::Id;

#[derive(PartialEq, Eq, Hash, Debug)]
pub enum Resolution {
    Builtin(Symbol),
    Local(Id),
}
