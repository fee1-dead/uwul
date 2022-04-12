#![feature(decl_macro)]
#![cfg_attr(test, feature(iter_order_by))]
#![feature(let_chains, let_else)]

pub mod ast;

pub mod lex;
pub mod sym;

#[cfg(test)]
mod tests;

/*
pub fn eval(expr: &Expr) -> Result<i64, String> {
    match expr {
        Expr::Num(x) => Ok(*x),
        Expr::Neg(a) => Ok(-eval(a)?),
        Expr::Add(a, b) => Ok(eval(a)? + eval(b)?),
        Expr::Sub(a, b) => Ok(eval(a)? - eval(b)?),
        Expr::Mul(a, b) => Ok(eval(a)? * eval(b)?),
        Expr::Div(a, b) => Ok(eval(a)? / eval(b)?),
        _ => todo!(), // We'll handle other cases later
    }
}*/
