use crate::ariadne_config;
use crate::lex::TokenKind::{self, self as T};
use crate::lex::{ErrorReported, Token, Ident};
use crate::sym::{kw, Symbol};

mod expr;
use ariadne::{Label, ReportKind, Source};
pub use expr::*;

mod stmt;
pub use stmt::*;

mod item;
pub use item::*;

mod ty;
pub use ty::*;

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct DeclId(u32);

pub struct Parser<'a> {
    source: &'a str,
    tokens: &'a [Token],
    current: usize,
    pub prev_token: Token,
    pub has_errors: bool,
    decl_id: u32,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str, tokens: &'a [Token]) -> Self {
        Parser {
            source,
            tokens,
            current: 0,
            prev_token: Token::dummy(),
            has_errors: false,
            decl_id: 0,
        }
    }

    fn mk_id(&mut self) -> DeclId {
        let id = self.decl_id;
        self.decl_id += 1;
        DeclId(id)
    }

    fn error(&mut self, message: &str) -> ErrorReported {
        self.has_errors = true;
        let tok = self.peek();

        let mut report = ariadne::Report::build(ReportKind::Error, (), tok.span.lo());
        report = report.with_config(ariadne_config());
        report.set_message(message);
        report.add_label(Label::new(tok.span));
        report.finish().eprint(Source::from(self.source)).unwrap();
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
                T::Keyword(Ident { symbol: kw::Fn | kw::Let | kw::For | kw::If | kw::While | kw::Return, .. }) => {
                    return;
                }
                _ => {
                    self.bump();
                }
            }
        }
    }
}
