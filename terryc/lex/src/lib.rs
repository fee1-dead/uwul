#![feature(let_chains)]

use std::rc::Rc;
use std::str::FromStr;

use terryc_base::errors::{DiagnosticBuilder, DiagnosticSeverity, ErrorReported};
use terryc_base::lex::{ErrorKind, Ident, Token, TokenKind};
use terryc_base::sym::Symbol;
use terryc_base::{Context, FileId, Providers, Span};

pub mod unescape;

pub struct Lexer<'a> {
    file: FileId,
    src: &'a str,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    has_errors: bool,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str, file: FileId) -> Self {
        Self {
            file,
            src,
            tokens: Vec::new(),
            start: 0,
            current: 0,
            has_errors: false,
        }
    }

    fn error(&mut self, kind: ErrorKind, span: Span) {
        self.has_errors = true;
        DiagnosticBuilder::new(DiagnosticSeverity::Error, &format!("{kind:?}"), span).emit();
    }

    fn is_end(&self) -> bool {
        self.current >= self.src.len()
    }

    fn char_at(&self, idx: usize) -> Option<char> {
        self.src.split_at(idx).1.chars().next()
    }

    fn peek(&self) -> Option<char> {
        self.char_at(self.current)
    }

    /*fn peek2(&self) -> Option<char> {
        self.char_at(self.current + 1)
    }*/

    fn advance(&mut self) -> Option<char> {
        let c = self.peek();
        self.current += c.map_or(0, char::len_utf8);
        c
    }

    fn eat(&mut self, c: char) -> bool {
        if self.peek() != Some(c) {
            return false;
        }
        self.current += c.len_utf8();
        true
    }

    fn string(&mut self) -> Option<TokenKind> {
        while let Some(c) = self.peek() {
            match c {
                '\\' => {
                    self.advance();
                } // escape will ignore one or more chars.
                '"' => break,
                _ => {}
            }
            self.advance();
        }

        if self.is_end() {
            self.error(
                ErrorKind::UnterminatedString,
                Span::new(self.current, self.current, self.file),
            );
            return None;
        }

        self.advance();

        let s = &self.src[self.start + 1..self.current - 1];
        unescape::unescape(s, Span::new(self.start + 1, self.current - 1, self.file))
            .ok()
            .as_deref()
            .map(Symbol::new)
            .map(TokenKind::String)
    }

    fn number(&mut self) -> Option<TokenKind> {
        while let Some(c) = self.peek() && c.is_ascii_digit() {
            self.advance();
        }

        let kind = /*if Some('.') == self.peek()
            && self.peek2().map(|c| c.is_ascii_digit()).unwrap_or_default()
        {
            self.advance();
            while let Some(c) = self.peek() && c.is_ascii_digit() {
                self.advance();
            }

            let s = &self.src[self.start..self.current];
            let Ok(num) = f64::from_str(s).map_err(|_| self.error(ErrorKind::InvalidFloat)) else { return None };
            TokenKind::Decimal(num)
        } else */{
            let s = &self.src[self.start..self.current];
            let Ok(num) = u128::from_str(s).map_err(|_| self.error(ErrorKind::InvalidInt, Span::new(self.start, self.current, self.file))) else { return None };
            TokenKind::Integer(num)
        };

        Some(kind)
    }

    fn identifier(&mut self) -> TokenKind {
        while let Some(c) = self.peek() && c.is_ascii_alphanumeric() {
            self.advance();
        }

        let s = &self.src[self.start..self.current];
        let symbol = Symbol::new(s);
        let span = Span::new(self.start, self.current, self.file);
        if symbol.is_keyword() {
            TokenKind::Keyword(Ident { symbol, span })
        } else {
            TokenKind::Ident(Ident { symbol, span })
        }
    }

    fn scan_token(&mut self) -> Option<TokenKind> {
        use TokenKind::*;

        let c = match self.advance() {
            Some(c) => c,
            None => return None,
        };

        let kind = match c {
            '(' => LeftParen,
            ')' => RightParen,
            '{' => LeftBrace,
            '}' => RightBrace,
            ',' => Comma,
            '.' => Dot,
            '-' if self.eat('>') => RArrow,
            '-' => Minus,
            '+' => Plus,
            ';' => Semicolon,
            '*' => Star,
            ':' => Colon,
            '!' if self.eat('=') => NotEq,
            '!' => Not,
            '=' if self.eat('=') => EqEq,
            '=' => Eq,
            '<' if self.eat('=') => LessEq,
            '<' => Less,
            '>' if self.eat('=') => GreaterEq,
            '>' => Greater,
            '%' => Percent,

            '/' if self.eat('/') => {
                while let Some(c) = self.peek() && c != '\n' {
                    self.advance();
                }
                return None;
            }

            '/' if self.eat('*') => {
                let mut nest = 1;

                while nest > 0 {
                    if self.is_end() {
                        self.error(
                            ErrorKind::UnclosedComment,
                            Span::new(self.current, self.current, self.file),
                        );
                        return None;
                    }
                    while let Some(c) = self.peek() {
                        self.advance();
                        if c == '/' && self.eat('*') {
                            nest += 1;
                        } else if c == '*' && self.eat('/') {
                            nest -= 1;
                            break;
                        }
                    }
                }

                return None;
            }

            '/' => Slash,

            // ignore whitespace.
            ' ' | '\r' | '\t' | '\n' => return None,

            '"' => return self.string(),

            c if c.is_ascii_digit() => return self.number(),
            c if c.is_ascii_alphabetic() || c == '_' => self.identifier(),

            c => {
                self.error(
                    ErrorKind::UnexpectedCharacter(c),
                    Span::new(self.current, self.current, self.file),
                );
                return None;
            }
        };

        Some(kind)
    }

    pub fn scan_tokens(mut self) -> Result<Vec<Token>, ErrorReported> {
        while !self.is_end() {
            self.start = self.current;
            let Some(kind) = self.scan_token() else { continue };
            let span = Span::new(self.start, self.current, self.file);
            self.tokens.push(Token { kind, span })
        }

        self.tokens.push(Token {
            kind: TokenKind::Eof,
            span: Span::new(self.current, self.current, self.file),
        });

        if self.has_errors {
            Err(ErrorReported)
        } else {
            Ok(self.tokens)
        }
    }
}

fn lex(cx: &dyn Context, file: FileId) -> Result<Rc<[Token]>, ErrorReported> {
    let Some(src) = cx.get_file(file) else { return Err(ErrorReported); };
    let lexer = Lexer::new(&src, file);
    lexer.scan_tokens().map(Rc::from)
}

pub fn provide(p: &mut Providers) {
    *p = Providers { lex, ..*p };
}
