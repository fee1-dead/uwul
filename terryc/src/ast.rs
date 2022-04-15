use crate::lex::TokenKind::{self, self as T};
use crate::lex::{ErrorReported, Token};
use crate::sym::{kw, Symbol};

mod expr;
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
    tokens: &'a [Token<'a>],
    current: usize,
    pub has_errors: bool,
    decl_id: u32,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token<'a>]) -> Self {
        Parser {
            tokens,
            current: 0,
            has_errors: false,
            decl_id: 0
        }
    }

    fn mk_id(&mut self) -> DeclId {
        let id = self.decl_id;
        self.decl_id += 1;
        DeclId(id)
    }

    fn error(&mut self, message: &str) -> ErrorReported {
        self.has_errors = true;
        if let Some(token) = self.peek() {
            println!("error on line {} ({:?}): {message}", token.line, token.kind,);
        } else {
            println!("error on EOF: {message}");
        }
        ErrorReported
    }

    fn is_end(&self) -> bool {
        self.current >= self.tokens.len()
    }

    fn peek(&self) -> Option<&Token<'a>> {
        self.tokens.get(self.current)
    }

    fn advance(&mut self) -> Option<&Token<'a>> {
        if !self.is_end() {
            self.current += 1;
        }
        self.peek()
    }

    fn eat(&mut self, kind: TokenKind<'a>) -> bool {
        if !self.is_end() && self.peek().unwrap().kind == kind {
            self.advance();
            true
        } else {
            false
        }
    }

    fn expect(&mut self, kind: TokenKind<'a>) -> Result<(), ErrorReported> {
        if self.eat(kind.clone()) {
            Ok(())
        } else {
            Err(self.error(&format!("expected {kind:?}")))
        }
    }

    /// like eat but does not consume
    fn check(&mut self, kind: TokenKind<'a>) -> bool {
        !self.is_end() && self.peek().unwrap().kind == kind
    }

    fn eat_filter_map<F: FnOnce(&TokenKind<'a>) -> Option<O>, O>(&mut self, f: F) -> Option<O> {
        if !self.is_end() && let Some(t) = self.peek() && let Some(o) = f(&t.kind) {
            self.advance();
            Some(o)
        } else {
            None
        }
    }

    fn eat_ident(&mut self) -> Option<Symbol> {
        self.eat_filter_map(|t| if let T::Ident(s) = t { Some(*s) } else { None })
    }

    fn expect_ident(&mut self) -> Result<Symbol, ErrorReported> {
        if let Some(ident) = self.eat_ident() {
            Ok(ident)
        } else {
            Err(self.error("expected identifier"))
        }
    }

    fn eat_any(&mut self, kinds: &[TokenKind<'a>]) -> Option<&Token> {
        if let Some(token) = self.peek() {
            for kind in kinds {
                if &token.kind == kind {
                    self.current += 1;
                    return self.get_prev();
                }
            }
        }
        None
    }

    fn get_prev(&self) -> Option<&Token<'a>> {
        self.tokens.get(self.current - 1)
    }

    fn synchronize(&mut self) {
        while !self.is_end() {
            if self.peek().unwrap().kind == T::Semicolon {
                self.advance();
                return;
            }
            match self.peek().unwrap().kind {
                T::Keyword(kw::Fn | kw::Let | kw::For | kw::If | kw::While | kw::Return) => {
                    return;
                }
                _ => {
                    self.advance();
                }
            }
        }
    }
}
