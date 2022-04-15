use crate::lex::ErrorReported;
use crate::lex::TokenKind as T;
use crate::sym;

use super::Parser;

pub enum Ty {
    I32,
}

impl<'a> Parser<'a> {
    pub fn parse_ty(&mut self) -> Result<Ty, ErrorReported> {
        if self.eat(T::Ident(sym::i32)) {
            Ok(Ty::I32)
        } else {
            Err(self.error("expected type"))
        }
    }
}