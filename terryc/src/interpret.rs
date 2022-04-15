use std::fmt;

use rustc_hash::FxHashMap;

use crate::ast::{self, BinOpKind, Expr, ExprKind, Literal, Stmt, UnOpKind};
use crate::sym::Symbol;
/*
pub enum Error {
    InvalidOp {
        left: Value,
        right: Option<Value>,
        op: &'static str,
    },
    VariableNotFound(Symbol),
    VariableUninit(Symbol),
    InvalidAssignment(Expr, Expr),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::InvalidOp { left, right, op } => {
                let right = right
                    .as_ref()
                    .map(|v| format!(" and {:?}", v))
                    .unwrap_or_default();
                write!(f, "error: tried to {op} {left:?}{right}",)
            }
            Error::VariableNotFound(name) => {
                write!(f, "error: variable `{name}` not found")
            }
            Error::VariableUninit(name) => {
                write!(f, "error: variable `{name}` has not been initialized")
            }
            Error::InvalidAssignment(lhs, rhs) => {
                write!(f, "error: invalid assignment: {:?} = {:?}", lhs, rhs)
            }
        }
    }
}

impl fmt::Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Int(i128),
    Float(f64),
    String(String),
    Boolean(bool),
}

macro_rules! binops {
    (@var($A:ident)) => (Value::$A);
    (@var($A:ident $B:ident)) => (Value::$B);
    (
        $(fn $name:ident(a, b) = a $tt:tt b ($lit:literal) $(($Variant:ident))?;)+
    ) => {$(
        fn $name(self, other: Value) -> Result<Value, Error> {
            match (self, other) {
                (Value::Int(i), Value::Int(j)) => Ok(binops!(@var(Int $($Variant)?))(i $tt j)),
                (Value::Float(i), Value::Float(j)) => Ok(binops!(@var(Float $($Variant)?))(i $tt j)),
                (left, right) => Err(Error::InvalidOp {
                    left,
                    right: Some(right),
                    op: $lit,
                }),
            }
        }
    )+};
}

impl Value {
    fn neg(self) -> Result<Value, Error> {
        match self {
            Value::Int(i) => Ok(Value::Int(-i)),
            Value::Float(f) => Ok(Value::Float(-f)),
            _ => Err(Error::InvalidOp {
                left: self,
                right: None,
                op: "negate",
            }),
        }
    }

    fn inv(self) -> Result<Value, Error> {
        match self {
            Value::Boolean(b) => Ok(Value::Boolean(!b)),
            _ => Err(Error::InvalidOp {
                left: self,
                right: None,
                op: "invert",
            }),
        }
    }

    fn add(self, other: Value) -> Result<Value, Error> {
        match (self, other) {
            (Value::Int(i), Value::Int(j)) => Ok(Value::Int(i + j)),
            (Value::Float(i), Value::Float(j)) => Ok(Value::Float(i + j)),
            (Value::String(i), Value::String(j)) => Ok(Value::String(i + &j)),
            (left, right) => Err(Error::InvalidOp {
                left,
                right: Some(right),
                op: "add",
            }),
        }
    }

    binops! {
        fn sub(a, b) = a - b ("subtract");
        fn mul(a, b) = a * b ("multiply");
        fn div(a, b) = a / b ("divide");
        fn eq(a, b) = a == b ("equate") (Boolean);
        fn ne(a, b) = a != b ("equate") (Boolean);
        fn gt(a, b) = a > b ("compare (>)") (Boolean);
        fn lt(a, b) = a < b ("compare (<)") (Boolean);
        fn ge(a, b) = a >= b ("compare (>=)") (Boolean);
        fn le(a, b) = a <= b ("compare (<=)") (Boolean);
    }
}

fn lit_to_val(lit: Literal) -> Value {
    use crate::ast::LiteralKind as L;
    match lit.kind {
        L::Int(i) => Value::Int(i as i128),
        L::Float(f) => Value::Float(f),
        L::Str(s) => Value::String(s),
        L::Bool(b) => Value::Boolean(b),
    }
}

pub struct Interpreter {
    env: FxHashMap<Symbol, Option<Value>>,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            env: FxHashMap::default(),
        }
    }

    pub fn eval_binop(&self, op: BinOpKind, left: Expr, right: Expr) -> Result<Value, Error> {
        macro_rules! gen_match {
            ($($name:ident = $fnName:ident),*$(,)?) => {
                match op {
                    $(BinOpKind::$name => self.eval(left)?.$fnName(self.eval(right)?),)+
                }
            }
        }

        gen_match! {
            Add = add,
            Sub = sub,
            Mul = mul,
            Div = div,
            Equal = eq,
            NotEqual = ne,
            Greater = gt,
            Less = lt,
            GreaterEqual = ge,
            LessEqual = le,
        }
    }

    pub fn eval(&self, expr: Expr) -> Result<Value, Error> {
        use crate::ast::ExprKind::*;
        match expr.kind {
            Literal(lit) => Ok(lit_to_val(lit)),
            UnOp(UnOpKind::Minus, a) => self.eval(*a)?.neg(),
            UnOp(UnOpKind::Bang, a) => self.eval(*a)?.inv(),
            BinOp(op, a, b) => self.eval_binop(op, *a, *b),
            Ident(s) => self
                .env
                .get(&s)
                .cloned()
                .ok_or(Error::VariableNotFound(s))?
                .ok_or(Error::VariableUninit(s)),
        }
    }

    pub fn eval_stmt(&mut self, stmt: Stmt) -> Result<(), Error> {
        use crate::ast::StmtKind::*;
        match stmt.kind {
            Expr(expr) => {
                self.eval(expr)?;
            }
            Print(expr) => println!("{:?}", self.eval(expr)?),
            Var { name, value } => {
                let val = value.map(|e| self.eval(e)).transpose()?;
                self.env.insert(name, val);
            }
            Assign { lhs: ast::Expr { kind: ExprKind::Ident(sym) }, value } => {
                let val = self.eval(value)?;
                *self.env.get_mut(&sym).ok_or(Error::VariableNotFound(sym))? = Some(val);
            }
            Assign { lhs, value } => return Err(Error::InvalidAssignment(lhs, value)),
        }
        Ok(())
    }

    pub fn eval_stmts(&mut self, stmts: impl IntoIterator<Item = Stmt>) -> Result<(), Error> {
        for stmt in stmts {
            self.eval_stmt(stmt)?;
        }
        Ok(())
    }
}
*/
