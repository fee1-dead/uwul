#![feature(once_cell, decl_macro)]

use std::cell::RefCell;
use std::fmt;
use std::hash::Hash;
use std::ops::Deref;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::sync::OnceLock;

use ast::{Tree, TyKind};
use errors::ErrorReported;
use hir::HirTree;
use lex::Token;

pub mod ast;
pub mod errors;
pub mod hir;
pub mod lex;
pub mod mir;
pub mod sym;

pub use errors::Span;
use rustc_hash::FxHashMap;

pub mod data {
    pub use rustc_hash::FxHashMap;
}

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
    pub use_ascii: bool,
}

thread_local! { // TODO use something else than thread local once we have multithreading
    static GLOBAL_CTXT: OnceLock<GlobalCtxt> = OnceLock::new();
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

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum Mode {
    PrintAst,
    PrintMir,
    Gen,
}

#[derive(Debug)]
pub struct Options {
    pub use_ascii: bool,
    pub dont_print_path: bool,
    pub path: PathBuf,
    pub mode: Mode,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum FileLocator {
    Main,
    Unresolved(PathBuf),
    Resolved(u32),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum FileId {
    Main,
    Other(u32),
}

impl From<FileId> for FileLocator {
    fn from(id: FileId) -> Self {
        match id {
            FileId::Main => FileLocator::Main,
            FileId::Other(resolved) => FileLocator::Resolved(resolved),
        }
    }
}

impl fmt::Display for FileId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        GlobalCtxt::with(|cx| {
            let path = cx.file_path(*self);
            if cx.options().dont_print_path {
                write!(
                    f,
                    "DIR/{}",
                    path.file_name().expect("file name").to_string_lossy()
                )
            } else {
                path.display().fmt(f)
            }
        })
    }
}

pub fn run() {
    GlobalCtxt::with(|cx| match cx.mode() {
        Mode::PrintAst => {
            if let Ok(ast) = cx.parse(FileId::Main) {
                eprintln!("{ast:#?}");
            }
        }
        Mode::PrintMir => {
            let mir = cx.mir(FileId::Main);
            eprintln!("{mir:#?}");
        }
        Mode::Gen => {
            /* let class = */
            let _ = cx.codegen(FileId::Main);
            // fs::write("Main.class", &*class).unwrap();
        }
    });
}

#[derive(PartialEq, Eq, Debug)]
pub struct DefTree {
    pub defs: FxHashMap<Id, Definition>,
}

#[derive(PartialEq, Eq, Debug)]
pub enum DefKind {
    Fn,
}

#[derive(PartialEq, Eq, Debug)]
pub struct Definition {
    pub kind: DefKind,
    pub span: Span,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct TyList(&'static [TyKind]);

impl fmt::Debug for TyList {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl Deref for TyList {
    type Target = [TyKind];
    fn deref(&self) -> &Self::Target {
        self.0
    }
}
// FIXME replace this with a unique arena instead to help faster comparisons
/*
impl PartialEq for TyList {
    fn eq(&self, other: &Self) -> bool {

        std::ptr::eq(self.0, other.0)
    }
}

impl Eq for TyList {}

impl Hash for TyList {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.as_ptr().hash(state)
    }
} */

#[derive(Default)]
pub struct PathResolver {
    pub cur_id: u32,
    pub resolutions: FxHashMap<&'static Path, FileId>,
    pub paths: FxHashMap<FileId, &'static Path>,
}

impl PathResolver {
    pub fn locate(&mut self, locator: FileLocator) -> FileId {
        match locator {
            FileLocator::Main => FileId::Main,
            FileLocator::Resolved(x) => FileId::Other(x),
            FileLocator::Unresolved(path) => {
                let path = &*Box::leak(path.into_boxed_path());
                // cur_id should never be zero.
                self.cur_id += 1;
                let id = FileId::Other(self.cur_id);
                self.resolutions.insert(path, id);
                self.paths.insert(id, path);
                id
            }
        }
    }
}

pub struct Interners {
    pub symbols: sym::Interner,
    pub types: typed_arena::Arena<TyKind>,
    pub paths: RefCell<PathResolver>,
}

impl fmt::Debug for Interners {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Interners").finish_non_exhaustive()
    }
}

impl Interners {
    pub fn fresh() -> Self {
        Self {
            symbols: sym::Interner::fresh(),
            types: Default::default(),
            paths: Default::default(),
        }
    }
}

#[salsa::query_group(ContextStorage)]
pub trait Context {
    #[salsa::input]
    fn options(&self) -> &'static Options;
    fn mode(&self) -> Mode;
    #[salsa::input]
    fn interners(&self) -> &'static Interners;
    #[salsa::input]
    fn providers(&self) -> &'static Providers;
    fn locate(&self, locator: FileLocator) -> FileId;
    #[salsa::dependencies]
    fn get_file(&self, id: FileLocator) -> Option<String>;
    // fn file_list(&self) -> &'static [PathBuf];
    fn file_path(&self, id: FileId) -> &'static Path;
    fn lex(&self, id: FileId) -> Result<Rc<[Token]>, ErrorReported>;
    fn parse(&self, id: FileId) -> Result<Tree, ErrorReported>;
    fn hir(&self, id: FileId) -> Result<HirTree, ErrorReported>;
    fn def_tree(&self) -> Result<Rc<DefTree>, ErrorReported>;
    fn mir(&self, id: FileId) -> Result<mir::MirTree, ErrorReported>;
    fn codegen(&self, id: FileId) -> Result<(), ErrorReported>;
}

pub trait ContextExt: Context {
    fn intern_types(&self, x: impl IntoIterator<Item = TyKind>) -> TyList {
        TyList(self.interners().types.alloc_extend(x))
    }
}

impl<T: Context + ?Sized> ContextExt for T {}

fn mode(cx: &dyn Context) -> Mode {
    cx.options().mode
}

fn locate(cx: &dyn Context, locator: FileLocator) -> FileId {
    cx.interners().paths.borrow_mut().locate(locator)
}

fn def_tree(cx: &dyn Context) -> Result<Rc<DefTree>, ErrorReported> {
    todo!()
}

dynamic_queries! {
    Providers ->
    fn lex(&self, id: FileId) -> Result<Rc<[Token]>, ErrorReported>;
    fn parse(&self, id: FileId) -> Result<Tree, ErrorReported>;
    fn hir(&self, id: FileId) -> Result<HirTree, ErrorReported>;
    fn mir(&self, id: FileId) -> Result<mir::MirTree, ErrorReported>;
    fn codegen(&self, id: FileId) -> Result<(), ErrorReported>;
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

impl fmt::Debug for Providers {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
        ctxt.set_interners(Box::leak(Box::new(Interners::fresh())));

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

fn get_file(gcx: &dyn Context, locator: FileLocator) -> Option<String> {
    if locator == FileLocator::Main {
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

/*
fn file_list(cx: &dyn Context) -> &'static [PathBuf] {
    let paths = vec![cx.options().path.to_owned()];
    Box::leak(paths.into_boxed_slice())
} */

// FIXME: maybe not leak the path?
fn file_path(cx: &dyn Context, id: FileId) -> &'static Path {
    match id {
        FileId::Main => Box::leak(cx.options().path.clone().into_boxed_path()),
        FileId::Other(_) => todo!(),
    }
}
