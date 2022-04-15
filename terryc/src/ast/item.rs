use crate::lex::ErrorReported;
use crate::sym::{Symbol, kw};

use super::TokenKind as T;

use super::{Ty, Block, Parser};

pub struct Item {
    pub kind: ItemKind,
}

pub struct ItemFn {
    pub name: Symbol,
    pub args: Vec<(Symbol, Ty)>,
    pub ret: Ty,
    pub body: Block,
}

pub enum ItemKind {
    Fn(ItemFn),
}

impl Parser<'_> {
    pub(crate) fn parse_item(&mut self) -> Result<Item, ErrorReported> {
        if self.eat(T::Keyword(kw::Fn)) {
            let name = self.expect_ident()?;
            let args = self.parse_args()?;
            self.expect(T::RArrow)?;
            let ret = self.parse_ty()?;
            let body = self.parse_block()?;

            Ok(Item {
                kind: ItemKind::Fn(ItemFn {
                    name,
                    args,
                    ret,
                    body,
                }),
            })
        } else {
            Err(self.error("expected item"))
        }
    }

    fn parse_args(&mut self) -> Result<Vec<(Symbol, Ty)>, ErrorReported> {
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