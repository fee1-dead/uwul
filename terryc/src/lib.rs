#![feature(decl_macro)]
#![cfg_attr(test, feature(iter_order_by))]
#![feature(let_chains, let_else)]
#![feature(once_cell)]

use std::cell::RefCell;
use std::lazy::OnceCell;

pub mod ast;
pub mod codegen;
pub mod hir;
pub mod interpret;
pub mod lex;
pub mod sym;

#[non_exhaustive]
#[derive(Debug)]
pub struct SessionGlobals {
    pub interner: sym::Interner,
    pub use_ascii: bool,
}

thread_local! {
    static GLOBALS: OnceCell<SessionGlobals> = OnceCell::new();
}

pub fn use_ascii() -> bool {
    with_session_globals(|sess| sess.use_ascii)
}

pub fn ariadne_config() -> ariadne::Config {
    ariadne::Config::default()
        .with_char_set(if use_ascii() {
            ariadne::CharSet::Ascii
        } else {
            ariadne::CharSet::Unicode
        })
        .with_color(!use_ascii())
}

pub fn create_session_globals(use_ascii: bool) {
    let globals = SessionGlobals {
        interner: sym::Interner::fresh(),
        use_ascii,
    };

    GLOBALS.with(|cell| {
        cell.set(globals).expect("setting globals twice");
    })
}

pub fn with_session_globals<T>(f: impl FnOnce(&SessionGlobals) -> T) -> T {
    GLOBALS.with(|globals| f(globals.get().expect("globals")))
}

#[cfg(test)]
mod tests;
