use std::cell::RefCell;
use std::collections::hash_map::Entry;
use std::fmt;
use std::ops::Index;

use rustc_hash::FxHashMap;

macro_rules! define_symbols {
    (@extract_sym($name:ident)) => (stringify!($name));
    (@extract_sym($_:ident: $real:literal)) => ($real);
    (
        Keywords {
            $(
                $kw:ident $(: $kwr: literal)?
            ),*$(,)?
        }
        Symbols {
            $(
                $sym:ident $(: $symr: literal)?
            ),*$(,)?
        }
    ) => {
        #[allow(non_snake_case, non_camel_case_types, non_upper_case_globals)]
        mod kw_generated {
            #[repr(usize)]
            enum Uh {$($kw),*,$($sym),*}
            $(
                pub const $kw: super::Symbol = super::Symbol(Uh::$kw as usize);
            )*
        }

        #[allow(non_snake_case, non_camel_case_types, non_upper_case_globals)]
        mod sym_generated {
            #[repr(usize)]
            enum Uh {$($kw),*,$($sym),*}
            $(
                pub const $sym: super::Symbol = super::Symbol(Uh::$sym as usize);
            )*
        }

        static SYMS: &[&'static str] = &[
            $(
                define_symbols!(@extract_sym($kw $(: $kwr)?))
            ),*,
            $(
                define_symbols!(@extract_sym($sym $(: $symr)?))
            ),*
        ];
    }
}

define_symbols! {
    Keywords {
        If: "if",
        Fn: "fn",
        Nc: "nc",
        True: "true",
        False: "false",
    }

    Symbols {
        println,
    }
}

pub mod kw {
    pub use super::kw_generated::*;
}

pub use sym_generated::*;

struct InternerInner {
    names: FxHashMap<&'static str, Symbol>,
    strings: Vec<&'static str>,
}

impl Default for InternerInner {
    fn default() -> Self {
        let strings = SYMS.to_vec();
        let names = strings
            .iter()
            .enumerate()
            .map(|(i, s)| (*s, Symbol(i as usize)))
            .collect();
        Self { names, strings }
    }
}

impl InternerInner {
    fn intern(&mut self, name: &'static str) -> Symbol {
        match self.names.entry(name) {
            Entry::Occupied(entry) => *entry.get(),
            Entry::Vacant(entry) => {
                let idx = self.strings.len();
                self.strings.push(name);
                *entry.insert(Symbol(idx))
            }
        }
    }
}

impl Index<Symbol> for InternerInner {
    type Output = &'static str;

    fn index(&self, index: Symbol) -> &&'static str {
        unsafe { self.strings.get_unchecked(index.0) }
    }
}

pub struct Interner {
    inner: RefCell<InternerInner>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Symbol(usize);

impl Symbol {
    pub fn new(s: &str) -> Self {
        INTERNER.with(|it| it.intern(s))
    }

    pub fn get_str(&self) -> &str {
        INTERNER.with(|it| it.get_str(self))
    }
}

impl fmt::Debug for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.pad(self.get_str())
    }
}

impl Interner {
    fn fresh() -> Self {
        Self {
            inner: RefCell::default(),
        }
    }

    fn intern(&self, s: &str) -> Symbol {
        self.inner
            .borrow_mut()
            .intern(Box::leak(s.to_owned().into_boxed_str()))
    }

    fn get_str<'a>(&self, s: &'a Symbol) -> &'a str {
        self.inner.borrow()[*s]
    }
}

thread_local! {
    pub static INTERNER: Interner = Interner::fresh();
}
