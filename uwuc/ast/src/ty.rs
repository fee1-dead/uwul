use uwuc_base::ast::{Ty, TyKind};
use uwuc_base::errors::ErrorReported;
use uwuc_base::sym;

use super::Parser;

impl<'a> Parser<'a> {
    pub fn parse_ty(&mut self) -> Result<Ty, ErrorReported> {
        let kind;

        if self.eat_sym(sym::i32) {
            kind = TyKind::I32;
        } else if self.eat_sym(sym::unit) {
            kind = TyKind::Unit;
        } else if self.eat_sym(sym::bool) {
            kind = TyKind::Bool;
        } else if self.eat_sym(sym::f32) {
            kind = TyKind::F32;
        } else if self.eat_sym(sym::string) {
            kind = TyKind::String;
        } else {
            return Err(self.error("expected type"));
        }

        let span = self.prev_token.span;

        Ok(Ty { span, kind })
    }
}
