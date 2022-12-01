use terryc_base::{ast::*, ContextExt};
use terryc_base::errors::ErrorReported;
use terryc_base::lex::{Ident, TokenKind as T};
use terryc_base::sym::kw;

use crate::Parser;

impl Parser<'_> {
    pub(crate) fn parse_item(&mut self) -> Result<Item, ErrorReported> {
        if self.eat_kw(kw::Fn) {
            let name = self.expect_ident()?;
            let args = self.parse_args()?;
            self.expect(T::RArrow)?;
            let ret = self.parse_ty()?;
            let body = self.parse_block()?;

            Ok(Item {
                kind: ItemKind::Fn(ItemFn {
                    name,
                    id: self.mk_id(),
                    args,
                    ret,
                    body,
                }),
            })
        } else if self.eat_kw(kw::Mod) {
            let name = self.expect_ident()?;
            self.expect(T::Semicolon)?;
            let id = self.cx.resolve_mod(self.current_file, name.symbol.get_str());
            let tree = Parser::enter(self.cx, id, |nested| {
                nested.parse()
            })??;
            Ok(Item { kind: ItemKind::Mod { name, tree } })
        } else {
            Err(self.error("expected item"))
        }
    }

    fn parse_args(&mut self) -> Result<Vec<(Ident, Ty)>, ErrorReported> {
        let mut args = Vec::new();
        self.expect(T::LeftParen)?;

        if self.eat(T::Comma) {
            if self.eat(T::RightParen) {
                return Ok(args);
            } else {
                return Err(self.error("expected `)`"));
            }
        } else if self.eat(T::RightParen) {
            return Ok(args);
        }

        loop {
            let name = self.expect_ident()?;
            self.expect(T::Colon)?;
            let ty = self.parse_ty()?;
            args.push((name, ty));

            if self.eat(T::Comma) {
                if self.eat(T::RightParen) {
                    break;
                }
            } else if self.eat(T::RightParen) {
                break;
            }
        }

        Ok(args)
    }
}
