#![feature(once_cell, let_else, decl_macro)]

use std::cell::RefCell;
use std::lazy::OnceCell;
use std::path::{Path, PathBuf};

use sym::Interner;

pub mod errors;
pub mod sym;

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
}

impl Iterator for IdMaker {
    type Item = Id;

    fn next(&mut self) -> Option<Self::Item> {
        let id = Id(self.curr);
        self.curr += 1;
        Some(id)
    }
}

#[non_exhaustive]
#[derive(Debug)]
pub struct SessionGlobals {
    pub interner: sym::Interner,
    pub use_ascii: bool,
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

#[salsa::query_group(InputStorage)]
pub trait Input {
    #[salsa::input]
    fn options(&self) -> &'static Options;
    #[salsa::input]
    fn interner(&self) -> &'static sym::Interner;
    #[salsa::dependencies]
    fn get_file(&self, id: FileId) -> Option<String>;
    fn file_list(&self) -> &'static [PathBuf];
}

#[salsa::database(InputStorage)]
pub struct GlobalCtxt {
    storage: salsa::Storage<GlobalCtxt>,
}

thread_local! { // TODO use something else than thread local once we have multithreading
    static GLOBAL_CTXT: OnceCell<GlobalCtxt> = OnceCell::new();
}

impl GlobalCtxt {
    pub fn create(options: Options) {
        let mut ctxt = Self {
            storage: Default::default(),
        };

        ctxt.set_options(Box::leak(Box::new(options)));
        ctxt.set_interner(Box::leak(Box::new(Interner::fresh())));

        GLOBAL_CTXT.with(|cell| cell.set(ctxt).ok().expect("`create` called twice"))
    }

    pub fn with<T>(f: impl FnOnce(&GlobalCtxt) -> T) -> T {
        GLOBAL_CTXT.with(|cell| f(cell.get().expect("`with` called before `create`")))
    }
}

impl salsa::Database for GlobalCtxt {}

pub fn leak<T>(x: T) -> &'static T {
    Box::leak(Box::new(x))
}

fn get_file(gcx: &dyn Input, id: FileId) -> Option<String> {
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

fn file_list(gcx: &dyn Input) -> &'static [PathBuf] {
    todo!()
}
