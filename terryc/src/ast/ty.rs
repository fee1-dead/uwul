use crate::lex::ErrorReported;
use crate::lex::TokenKind as T;
use crate::sym;

use super::Parser;

pub enum Ty {
    I32,
    F32,
    Unit,
    Bool,
    String,
}

impl<'a> Parser<'a> {
    pub fn parse_ty(&mut self) -> Result<Ty, ErrorReported> {
        if self.eat(T::Ident(sym::i32)) {
            Ok(Ty::I32)
        } else if self.eat(T::Ident(sym::unit)) {
            Ok(Ty::Unit)
        } else if self.eat(T::Ident(sym::bool)) {
            Ok(Ty::Bool)
        } else if self.eat(T::Ident(sym::f32)) {
            Ok(Ty::F32)
        } else if self.eat(T::Ident(sym::string)) {
            Ok(Ty::String)
        }
        
        else {
            Err(self.error("expected type"))
        }
    }
}