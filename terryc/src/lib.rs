#![feature(decl_macro)]
#![cfg_attr(test, feature(iter_order_by))]
#![feature(let_chains, let_else)]

pub mod ast;
pub mod interpret;
pub mod lex;
pub mod sym;
pub mod hir;

#[cfg(test)]
mod tests;
