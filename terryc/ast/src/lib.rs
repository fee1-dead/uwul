#![feature(let_chains)]
use std::rc::Rc;

use terryc_base::{FileId, Context, Providers, IdMaker, Id};
use terryc_base::errors::{DiagnosticBuilder, DiagnosticSeverity, ErrorReported};
use terryc_base::sym::{kw, Symbol};
use terryc_base::lex::TokenKind::{self, self as T};
use terryc_base::lex::{Ident, Token};

pub use terryc_base::ast::*;

mod expr;
mod stmt;
mod item;
mod ty;

pub struct Parser<'a> {
    tokens: &'a [Token],
    current: usize,
    pub prev_token: Token,
    pub has_errors: bool,
    maker: IdMaker,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Parser {
            tokens,
            current: 0,
            prev_token: Token::dummy(),
            has_errors: false,
            maker: IdMaker::new(),
        }
    }

    fn mk_id(&mut self) -> Id {
        self.maker.make()
    }

    fn error(&mut self, message: &str) -> ErrorReported {
        self.has_errors = true;
        let tok = self.peek();

        DiagnosticBuilder::new(DiagnosticSeverity::Error, message, tok.span).emit();

        ErrorReported
    }

    fn is_end(&self) -> bool {
        self.peek().kind == T::Eof
    }

    fn peek(&self) -> &Token {
        self.tokens.get(self.current).unwrap()
    }

    fn bump(&mut self) -> &Token {
        if !self.is_end() {
            self.prev_token = self.peek().clone();
            self.current += 1;
        }
        self.peek()
    }

    fn eat(&mut self, kind: TokenKind) -> bool {
        if !self.is_end() && self.peek().kind == kind {
            self.bump();
            true
        } else {
            false
        }
    }

    fn eat_kw(&mut self, s: Symbol) -> bool {
        if let T::Keyword(i) = self.peek().kind && i.symbol == s {
            self.bump();
            true
        } else {
            false
        }
    }

    fn eat_sym(&mut self, s: Symbol) -> bool {
        if let T::Ident(i) = self.peek().kind && i.symbol == s {
            self.bump();
            true
        } else {
            false
        }
    }

    fn expect(&mut self, kind: TokenKind) -> Result<(), ErrorReported> {
        if self.eat(kind.clone()) {
            Ok(())
        } else {
            Err(self.error(&format!("expected {kind:?}")))
        }
    }

    /// like eat but does not consume
    fn check(&mut self, kind: TokenKind) -> bool {
        !self.is_end() && self.peek().kind == kind
    }

    fn eat_filter_map<F: FnOnce(&TokenKind) -> Option<O>, O>(&mut self, f: F) -> Option<O> {
        if !self.is_end() &&  let Some(o) = f(&self.peek().kind) {
            self.bump();
            Some(o)
        } else {
            None
        }
    }

    fn eat_ident(&mut self) -> Option<Ident> {
        self.eat_filter_map(|t| if let T::Ident(s) = t { Some(*s) } else { None })
    }

    fn expect_ident(&mut self) -> Result<Ident, ErrorReported> {
        if let Some(ident) = self.eat_ident() {
            Ok(ident)
        } else {
            Err(self.error("expected identifier"))
        }
    }

    fn eat_any(&mut self, kinds: &[TokenKind]) -> bool {
        let token = self.peek();
        for kind in kinds {
            if &token.kind == kind {
                self.current += 1;
                return true;
            }
        }
        false
    }

    fn synchronize(&mut self) {
        while !self.is_end() {
            if self.peek().kind == T::Semicolon {
                self.bump();
                return;
            }
            match self.peek().kind {
                T::Keyword(Ident {
                    symbol: kw::Fn | kw::Let | kw::For | kw::If | kw::While | kw::Return,
                    ..
                }) => {
                    return;
                }
                _ => {
                    self.bump();
                }
            }
        }
    }
}

fn parse(cx: &dyn Context, id: FileId) -> Result<Rc<[Stmt]>, ErrorReported> {
    cx.lex(id).and_then(|tokens| Parser::new(&tokens).parse_stmts()).map(Rc::from)
}

pub fn provide(providers: &mut Providers) {
    *providers = Providers {
        parse,
        ..*providers
    };
}
