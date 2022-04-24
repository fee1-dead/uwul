#![feature(once_cell, let_else, decl_macro)]

use std::fmt::Debug;
use std::lazy::OnceCell;
use std::path::PathBuf;
use std::rc::Rc;

use ast::Stmt;
use errors::ErrorReported;
use lex::Token;
use sym::Interner;

pub mod ast;
pub mod errors;
pub mod sym;
pub mod lex;

pub use errors::Span;

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub struct Id(u32);

#[derive(Default)]
pub struct IdMaker {
    curr: u32,
}

impl IdMaker {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn make(&mut self) -> Id {
        let id = Id(self.curr);
        self.curr += 1;
        id
    }
}

impl Iterator for IdMaker {
    type Item = Id;

    fn next(&mut self) -> Option<Self::Item> {
        Some(self.make())
    }
}

#[non_exhaustive]
#[derive(Debug)]
pub struct SessionGlobals {
    pub interner: sym::Interner,
    pub use_ascii: bool,
}

thread_local! { // TODO use something else than thread local once we have multithreading
    static GLOBAL_CTXT: OnceCell<GlobalCtxt> = OnceCell::new();
}

pub fn ariadne_config() -> ariadne::Config {
    fn use_ascii() -> bool {
        GlobalCtxt::with(|gcx| gcx.options().use_ascii)
    }
    ariadne::Config::default()
        .with_char_set(if use_ascii() {
            ariadne::CharSet::Ascii
        } else {
            ariadne::CharSet::Unicode
        })
        .with_color(!use_ascii())
}

#[derive(Debug)]
pub struct Options {
    pub use_ascii: bool,
    pub path: PathBuf,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct FileId(u32);

impl FileId {
    pub const fn main() -> Self {
        FileId(u32::MAX)
    }
}

#[salsa::query_group(ContextStorage)]
pub trait Context {
    #[salsa::input]
    fn options(&self) -> &'static Options;
    #[salsa::input]
    fn interner(&self) -> &'static sym::Interner;
    #[salsa::input]
    fn providers(&self) -> &'static Providers;
    #[salsa::dependencies]
    fn get_file(&self, id: FileId) -> Option<String>;
    fn file_list(&self) -> &'static [PathBuf];
    fn lex(&self, id: FileId) -> Result<Rc<[Token]>, ErrorReported>;
    fn parse(&self, id: FileId) -> Result<Rc<[Stmt]>, ErrorReported>;
}


dynamic_queries! {
    Providers ->
    fn lex(&self, id: FileId) -> Result<Rc<[Token]>, ErrorReported>;
    fn parse(&self, id: FileId) -> Result<Rc<[Stmt]>, ErrorReported>;
}

macro dynamic_queries(
    $Providers:ident ->
    $(fn $name:ident(&self, $($ident:ident: $ty:ty),*$(,)?) $( -> $retty:ty )?;)*
) {
    #[derive(Clone, Copy)]
    #[allow(unused_parens)]
    pub struct $Providers {
        $(
            pub $name: fn(&dyn Context, $($ty),*) -> ($($retty)?),
        )*
    }

    impl Default for $Providers {
        fn default() -> Self {
            Self {
                $(
                    $name: |_cx, $(_: $ty),*| panic!(concat!(stringify!($name), " is not supported/initialized")),
                )*
            }
        }
    }

    $(
        #[allow(unused_parens)]
        fn $name(cx: &dyn Context, $($ident: $ty,)*) -> ($($retty)?) {
            (cx.providers().$name)(cx, $($ident,)*)
        }
    )*
}

impl Debug for Providers {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Providers").finish_non_exhaustive()
    }
}


#[salsa::database(ContextStorage)]
pub struct GlobalCtxt {
    storage: salsa::Storage<GlobalCtxt>,
}



impl GlobalCtxt {
    pub fn create_and_then(options: Options, f: impl FnOnce(GlobalCtxt) -> GlobalCtxt) {
        let mut ctxt = Self {
            storage: Default::default(),
        };

        ctxt.set_options(Box::leak(Box::new(options)));
        ctxt.set_interner(Box::leak(Box::new(Interner::fresh())));

        GLOBAL_CTXT.with(|cell| cell.set(f(ctxt)).ok().expect("`create` called twice"))
    }

    pub fn with<T>(f: impl FnOnce(&GlobalCtxt) -> T) -> T {
        GLOBAL_CTXT.with(|cell| f(cell.get().expect("`with` called before `create`")))
    }
}

impl salsa::Database for GlobalCtxt {}

pub fn leak<T>(x: T) -> &'static T {
    Box::leak(Box::new(x))
}

fn get_file(gcx: &dyn Context, id: FileId) -> Option<String> {
    if id == FileId::main() {
        let p = &gcx.options().path;
        let res = std::fs::read_to_string(p).ok();
        if res.is_none() {
            eprintln!("ERROR: failed to read file `{}`", p.display());
        }
        res
    } else {
        todo!()
    }
}

fn file_list(gcx: &dyn Context) -> &'static [PathBuf] {
    todo!()
}
