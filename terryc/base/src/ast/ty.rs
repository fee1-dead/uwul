use std::fmt;

use crate::Span;

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub struct Ty {
    pub kind: TyKind,
    pub span: Span,
}

impl fmt::Debug for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.kind.fmt(f)
    }
}

impl fmt::Display for TyKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TyKind::Bool => f.write_str("bool"),
            TyKind::F32 => f.write_str("f32"),
            TyKind::I32 => f.write_str("i32"),
            TyKind::Unit => f.write_str("unit"),
            TyKind::String => f.write_str("string"),
        }
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
