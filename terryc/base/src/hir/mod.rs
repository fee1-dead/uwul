mod expr;
mod item;
pub use expr::*;
pub use item::*;

use crate::Id;
use crate::sym::Symbol;

#[derive(PartialEq, Eq, Hash, Debug)]
pub enum Resolution {
    Builtin(Symbol),
    Local(Id),
}
