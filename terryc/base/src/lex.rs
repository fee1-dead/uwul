use std::fs::File;
use std::{fmt, hash::Hash};

use crate::{Span, FileId};
use crate::sym::Symbol;

#[derive(Clone, Copy)]
pub struct Ident {
    pub span: Span,
    pub symbol: Symbol,
}

impl PartialEq for Ident {
    fn eq(&self, other: &Self) -> bool {
        self.symbol.eq(&other.symbol)
    }
}

impl Eq for Ident {}

impl fmt::Debug for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.symbol.fmt(f)
    }
}

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.symbol.fmt(f)
    }
}

impl Hash for Ident {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.symbol.hash(state);
    }
}

#[derive(Debug)]
pub enum ErrorKind {
    UnexpectedCharacter(char),
    UnterminatedString,
    UnclosedComment,
    InvalidFloat,
    InvalidInt,
}

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub enum TokenKind {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    RArrow,
    Comma,
    Colon,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Star,
    Not,
    NotEq,
    Eq,
    EqEq,
    Greater,
    GreaterEq,
    Less,
    LessEq,
    Slash,
    String(Symbol),
    Integer(u128),
    //    Decimal(f64),
    Keyword(Ident),
    Ident(Ident),
    Eof,
}

#[derive(Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn dummy() -> Self {
        Token {
            kind: TokenKind::Dot,
            span: Span::new(0, 0, FileId::main()),
        }
    }
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.kind.fmt(f)
    }
}