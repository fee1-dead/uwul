use std::fmt::Debug;

use chumsky::Parser;

use crate::lex::Tokens;
use crate::lex::{
    lexer,
    Group::{self, *},
    Lit, Token,
};
use crate::sym::{kw, Symbol};

#[test]
fn lex() {
    expect("a b c", [it("a"), it("b"), it("c")]);
    expect("a + b", [it("a"), p('+'), it("b")]);
    expect("a += b;", [it("a"), p('+'), p('='), it("b"), p(';')]);
    expect("'\\u{123}' +", [litc("\\u{123}"), p('+')]);
    expect(
        "fn a() { if true {} }",
        [
            st(kw::Fn),
            it("a"),
            grouped(Paren, []),
            grouped(Brace, [st(kw::If), it("true"), grouped(Brace, [])]),
        ],
    )
}

fn grouped(g: fn(Tokens) -> Group, tokens: impl IntoIterator<Item = Token>) -> Token {
    Token::Group(g(tokens.into_iter().map(|t| (t, 0..0)).collect()))
}

fn litc(s: &str) -> Token {
    Token::Lit(Lit::Char(s.to_owned()))
}

fn p(c: char) -> Token {
    Token::Punct(c)
}

fn st(s: Symbol) -> Token {
    Token::Ident(s)
}

fn it(s: &str) -> Token {
    Token::Ident(Symbol::new(s))
}

fn expect<I: Debug + PartialEq<[Token]>>(src: &str, output: I) {
    let result: Vec<_> = lexer()
        .parse(src)
        .expect("expected lexer to succeed")
        .into_iter()
        .map(|(a, _)| a)
        .collect();

    assert_eq!(output, *result);
}
