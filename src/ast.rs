use std::fmt;

use crate::lex::ErrorReported;
use crate::lex::{
    Token,
    TokenKind::{self, self as T},
};
use crate::sym::kw;

pub struct Expr {
    kind: ExprKind,
}

#[derive(Debug, Clone, Copy)]
pub enum BinOpKind {
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Add,
    Sub,
    Mul,
    Div,
}

impl BinOpKind {
    fn as_str(self) -> &'static str {
        match self {
            BinOpKind::Equal => "==",
            BinOpKind::NotEqual => "!=",
            BinOpKind::Less => "<",
            BinOpKind::LessEqual => "<=",
            BinOpKind::Greater => ">",
            BinOpKind::GreaterEqual => ">=",
            BinOpKind::Add => "+",
            BinOpKind::Sub => "-",
            BinOpKind::Mul => "*",
            BinOpKind::Div => "/",
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum UnOpKind {
    Minus,
    Bang,
}

impl UnOpKind {
    fn as_char(self) -> char {
        match self {
            UnOpKind::Minus => '-',
            UnOpKind::Bang => '!',
        }
    }
}

pub enum LiteralKind {
    Int(u128),
    Str(String),
    Float(f64),
    Bool(bool),
}

impl fmt::Display for LiteralKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LiteralKind::Int(i) => write!(f, "{}", i),
            LiteralKind::Str(s) => write!(f, "\"{}\"", s),
            LiteralKind::Float(float) => write!(f, "{}", float),
            LiteralKind::Bool(b) => write!(f, "{}", b),
        }
    }
}

pub struct Literal {
    kind: LiteralKind,
}


impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.kind.fmt(f)
    }
}

pub enum ExprKind {
    BinOp(BinOpKind, Box<Expr>, Box<Expr>),
    UnOp(UnOpKind, Box<Expr>),
    Literal(Literal),
}

impl fmt::Debug for ExprKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExprKind::BinOp(op, lhs, rhs) => write!(f, "({} {:?} {:?})", op.as_str(), lhs, rhs),
            ExprKind::UnOp(op, rhs) => write!(f, "({}, {:?})", op.as_char(), rhs),
            ExprKind::Literal(lit) => write!(f, "{lit}"),
        }
    }
}

impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.kind.fmt(f)
    }
}

pub struct Parser<'a> {
    tokens: &'a [Token<'a>],
    current: usize,
    pub has_errors: bool,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token<'a>]) -> Self {
        Parser {
            tokens,
            current: 0,
            has_errors: false,
        }
    }

    fn error(&mut self, message: &'static str) {
        self.has_errors = true;
        if let Some(token) = self.peek() {
            println!(
                "error on line {} ({:?}): {message}",
                token.line, token.kind,
            );
        } else {
            println!("error on EOF: {message}");
        }
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

    fn eat_any(&mut self, kinds: &[TokenKind<'a>]) -> bool {
        if let Some(token) = self.peek() {
            for kind in kinds {
                if &token.kind == kind {
                    self.advance();
                    return true;
                }
            }
        }
        false
    }

    fn get_prev(&self) -> Option<&Token<'a>> {
        self.tokens.get(self.current - 1)
    }

    fn equality(&mut self) -> Option<Expr> {
        let mut expr = self.comparison()?;
        while self.eat_any(&[T::EqualEqual, T::BangEqual]) {
            let op = match self.get_prev().unwrap().kind {
                T::Equal => BinOpKind::Equal,
                T::BangEqual => BinOpKind::NotEqual,
                _ => unreachable!(),
            };
            let right = self.comparison()?;
            expr = Expr {
                kind: ExprKind::BinOp(op, Box::new(expr), Box::new(right)),
            };
        }
        Some(expr)
    }

    pub fn parse(mut self) -> Result<Expr, ErrorReported> {
        self.expression().ok_or(ErrorReported)
    }

    fn expression(&mut self) -> Option<Expr> {
        self.equality()
    }

    fn comparison(&mut self) -> Option<Expr> {
        let mut expr = self.term()?;
        while self.eat_any(&[T::Greater, T::GreaterEqual, T::Less, T::LessEqual]) {
            let op = match self.get_prev().unwrap().kind {
                T::Greater => BinOpKind::Greater,
                T::GreaterEqual => BinOpKind::GreaterEqual,
                T::Less => BinOpKind::Less,
                T::LessEqual => BinOpKind::LessEqual,
                _ => unreachable!(),
            };
            let right = self.term()?;
            expr = Expr {
                kind: ExprKind::BinOp(op, Box::new(expr), Box::new(right)),
            };
        }
        Some(expr)
    }

    fn term(&mut self) -> Option<Expr> {
        let mut expr = self.factor()?;

        while self.eat_any(&[T::Minus, T::Plus]) {
            let op = match self.get_prev().unwrap().kind {
                T::Minus => BinOpKind::Sub,
                T::Plus => BinOpKind::Add,
                _ => unreachable!(),
            };
            let right = self.factor()?;
            expr = Expr {
                kind: ExprKind::BinOp(op, Box::new(expr), Box::new(right)),
            };
        }

        Some(expr)
    }

    fn factor(&mut self) -> Option<Expr> {
        let mut expr = self.unary()?;
        while self.eat_any(&[T::Star, T::Slash]) {
            let op = match self.get_prev().unwrap().kind {
                T::Star => BinOpKind::Mul,
                T::Slash => BinOpKind::Div,
                _ => unreachable!(),
            };
            let right = self.unary()?;
            expr = Expr {
                kind: ExprKind::BinOp(op, Box::new(expr), Box::new(right)),
            };
        }
        Some(expr)
    }

    fn unary(&mut self) -> Option<Expr> {
        if self.eat_any(&[T::Minus, T::Bang]) {
            let op = match self.get_prev().unwrap().kind {
                T::Minus => UnOpKind::Minus,
                T::Bang => UnOpKind::Bang,
                _ => unreachable!(),
            };
            Some(Expr {
                kind: ExprKind::UnOp(op, Box::new(self.unary()?)),
            })
        } else {
            self.primary()
        }
    }

    fn primary(&mut self) -> Option<Expr> {
        let expr = match self.peek().unwrap().kind {
            T::Integer(n) => Expr {
                kind: ExprKind::Literal(Literal {
                    kind: LiteralKind::Int(n),
                }),
            },
            T::Keyword(kw::True) => Expr {
                kind: ExprKind::Literal(Literal {
                    kind: LiteralKind::Bool(true),
                }),
            },
            T::Keyword(kw::False) => Expr {
                kind: ExprKind::Literal(Literal {
                    kind: LiteralKind::Bool(false),
                }),
            },
            T::String(s) => Expr {
                kind: ExprKind::Literal(Literal {
                    kind: LiteralKind::Str(s.to_owned()),
                }),
            },
            T::Decimal(f) => Expr {
                kind: ExprKind::Literal(Literal {
                    kind: LiteralKind::Float(f),
                }),
            },
            T::LeftParen => {
                self.advance();
                let expr = self.expression()?;
                if !self.peek().map_or(false, |t| t.kind == T::RightParen) {
                    self.error("expected ')'");
                }
                expr
            }
            _ => {
                self.error("expected expression");
                return None;
            }
        };
        self.advance();
        Some(expr)
    }

    fn synchronize(&mut self) {
        self.advance();
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
