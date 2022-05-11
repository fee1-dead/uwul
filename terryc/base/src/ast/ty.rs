use std::fmt;

use crate::Span;

#[derive(PartialEq, Eq, Hash)]
pub struct Ty {
    pub kind: TyKind,
    pub span: Span,
}

impl fmt::Debug for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.kind.fmt(f)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TyKind {
    I32,
    F32,
    Unit,
    Bool,
    String,
}
