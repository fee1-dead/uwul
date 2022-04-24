use crate::Span;

#[derive(PartialEq, Eq, Hash)]
pub struct Ty {
    pub kind: TyKind,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TyKind {
    I32,
    F32,
    Unit,
    Bool,
    String,
}