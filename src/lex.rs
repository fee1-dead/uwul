use crate::sym::Symbol;
use std::str::FromStr;

#[derive(Debug)]
pub enum ErrorKind {
    UnexpectedCharacter(char),
    UnterminatedString,
    UnclosedComment,
    InvalidFloat,
    InvalidInt,
}

#[derive(Debug)]
pub struct Error {
    line: u32,
    kind: ErrorKind,
}

#[derive(PartialEq, Debug, Clone)]
pub enum TokenKind<'a> {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Star,
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Slash,
    String(&'a str),
    Integer(u128),
    Decimal(f64),
    Keyword(Symbol),
    Ident(Symbol),
}

#[derive(Clone)]
pub struct Token<'a> {
    pub kind: TokenKind<'a>,
    pub line: u32,
}

pub struct Lexer<'a> {
    src: &'a str,
    tokens: Vec<Token<'a>>,
    start: usize,
    current: usize,
    line: u32,
    has_errors: bool,
}

pub struct ErrorReported;

impl Error {
    fn emit(self) {
        println!("{self:?}");
    }
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            src,
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
            has_errors: false,
        }
    }

    fn add(&mut self, kind: TokenKind<'a>) {
        self.tokens.push(Token {
            kind,
            line: self.line,
        });
    }

    fn error(&mut self, kind: ErrorKind) {
        self.has_errors = true;
        Error {
            line: self.line,
            kind,
        }
        .emit()
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

    fn peek2(&self) -> Option<char> {
        self.char_at(self.current + 1)
    }

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

    fn string(&mut self) {
        while let Some(c) = self.peek() {
            if c == '"' {
                break;
            }
            if c == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_end() {
            self.error(ErrorKind::UnterminatedString);
            return;
        }

        self.advance();

        let s = &self.src[self.start + 1..self.current - 1];
        self.add(TokenKind::String(s));
    }

    fn number(&mut self) {
        while let Some(c) = self.peek() && c.is_ascii_digit() {
            self.advance();
        }

        let kind = if Some('.') == self.peek()
            && self.peek2().map(|c| c.is_ascii_digit()).unwrap_or_default()
        {
            self.advance();
            while let Some(c) = self.peek() && c.is_ascii_digit() {
                self.advance();
            }

            let s = &self.src[self.start..self.current];
            let Ok(num) = f64::from_str(s).map_err(|_| self.error(ErrorKind::InvalidFloat)) else { return };
            TokenKind::Decimal(num)
        } else {
            let s = &self.src[self.start..self.current];
            let Ok(num) = u128::from_str(s).map_err(|_| self.error(ErrorKind::InvalidInt)) else { return };
            TokenKind::Integer(num)
        };

        self.add(kind);
    }

    fn identifier(&mut self) {
        while let Some(c) = self.peek() && c.is_ascii_alphanumeric() {
            self.advance();
        }

        let s = &self.src[self.start..self.current];
        let sym = Symbol::new(s);

        let kind = if sym.is_keyword() {
            TokenKind::Keyword(sym)
        } else {
            TokenKind::Ident(sym)
        };

        self.add(kind);
    }

    fn scan_token(&mut self) {
        use TokenKind::*;

        let c = match self.advance() {
            Some(c) => c,
            None => return,
        };

        let kind = match c {
            '(' => LeftParen,
            ')' => RightParen,
            '{' => LeftBrace,
            '}' => RightBrace,
            ',' => Comma,
            '.' => Dot,
            '-' => Minus,
            '+' => Plus,
            ';' => Semicolon,
            '*' => Star,
            '!' if self.eat('=') => BangEqual,
            '!' => Bang,
            '=' if self.eat('=') => EqualEqual,
            '=' => Equal,
            '<' if self.eat('=') => LessEqual,
            '<' => Less,
            '>' if self.eat('=') => GreaterEqual,
            '>' => Greater,

            '/' if self.eat('/') => {
                while let Some(c) = self.peek() && c != '\n' {
                    self.advance();
                }
                return;
            }

            '/' if self.eat('*') => {
                let mut nest = 1;

                while nest > 0 {
                    if self.is_end() {
                        self.error(ErrorKind::UnclosedComment);
                        return;
                    }
                    while let Some(c) = self.peek() {
                        if c == '/' && self.eat('*') {
                            nest += 1;
                        } else if c == '*' && self.eat('/') {
                            nest -= 1;
                            break;
                        } else if c == '\n' {
                            self.line += 1;
                        }

                        self.advance();
                    }
                }

                return;
            }

            '/' => Slash,

            // ignore whitespace.
            ' ' | '\r' | '\t' => return,

            '\n' => {
                self.line += 1;
                return;
            }

            '"' => return self.string(),

            c if c.is_ascii_digit() => return self.number(),
            c if c.is_ascii_alphabetic() || c == '_' => return self.identifier(),

            c => {
                self.error(ErrorKind::UnexpectedCharacter(c));
                return;
            }
        };

        self.add(kind);
    }

    pub fn scan_tokens(mut self) -> Result<Vec<Token<'a>>, ErrorReported> {
        while !self.is_end() {
            self.start = self.current;
            self.scan_token();
        }

        if self.has_errors {
            Err(ErrorReported)
        } else {
            Ok(self.tokens)
        }
    }
}
