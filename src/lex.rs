use std::hash::Hash;
use std::mem::discriminant;

use chumsky::prelude::*;

use crate::sym::Symbol;

pub type Span = std::ops::Range<usize>;

pub type Tokens = Vec<(Token, Span)>;

#[derive(Clone, Debug)]
pub enum Group {
    Paren(Tokens),
    Bracket(Tokens),
    Brace(Tokens),
}

impl Group {
    fn tokens(&self) -> &Tokens {
        match self {
            Self::Paren(p) | Self::Bracket(p) | Self::Brace(p) => p,
        }
    }
}

impl PartialEq for Group {
    fn eq(&self, other: &Group) -> bool {
        if discriminant(self) != discriminant(other) {
            return false;
        }

        self.tokens()
            .iter()
            .map(|(t, _)| t)
            .eq(other.tokens().iter().map(|(t, _)| t))
    }
}

impl Hash for Group {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::mem::discriminant(self).hash(state);
        for (t, _) in self.tokens().iter() {
            t.hash(state);
        }
    }
}

impl Eq for Group {}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Lit {
    Number(String),
    Char(String),
    Str(String),
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Token {
    Lit(Lit),
    Ident(Symbol),
    Punct(char),
    Group(Group),
}

/// lexes comment. Handles recursive block comments.
///
/// # Examples
///
/// ```
/// # use chumsky::prelude::*;
/// use terry::lex::comment;
/// assert_eq!(comment().parse("// hello"), Ok(()));
/// assert_eq!(comment().parse("// hello\n// world"), Ok(()));
/// assert_eq!(comment().parse("/* hello,\n world! */"), Ok(()));
/// assert_eq!(comment().parse("/* hello, \n/* nested!\n */ */"), Ok(()));
///
/// assert!(comment().then_ignore(end()).parse("/* */ */").is_err());
/// assert!(comment().parse("/*").is_err())
/// ```
pub fn comment() -> impl Parser<char, (), Error = Simple<char>> + Clone {
    fn comment_block_inner() -> impl Parser<char, (), Error = Simple<char>> + Clone {
        recursive(|inner| {
            take_until(
                just("/*")
                    .ignored()
                    .then_ignore(inner)
                    .or(just("*/").ignored()),
            )
            .ignored()
        })
    }



    just("//")
        .then(take_until(just("\n").ignored().or(end())))
        .ignored()
        .or(just("/*").then(comment_block_inner()).ignored())
}

pub fn num() -> impl Parser<char, String, Error = Simple<char>> + Clone {
    just('0')
        .chain(choice((just('x').chain(text::int(16)),)))
        .or(text::int(10).chain::<char, _, _>(just('.').chain(text::digits(10)).or_not().flatten()))
        .collect::<String>()
}

pub fn lexer() -> impl Parser<char, Tokens, Error = Simple<char>> {
    recursive(|tokens| {
        let num = just('0')
            .chain(one_of("xbo"))
            .or(text::int(10)
                .chain::<char, _, _>(just('.').chain(text::digits(10)).or_not().flatten()))
            .collect::<String>()
            .map(Lit::Number)
            .map(Token::Lit); // TODO add more number types

        let char = just('\'')
            .ignore_then(
                just('\\').chain(
                    choice((just('\''), just('\\'), just('n'), just('t'), just('r')))
                        .map(|c| vec![c])
                        .or(just('u').chain(just('{').chain(text::int(16)).chain(just('}')))),
                ),
            )
            .then_ignore(just('\''))
            .collect::<String>()
            .map(Lit::Char)
            .map(Token::Lit);

        let str_ = just('"')
            .ignore_then(filter(|c| *c != '"').repeated())
            .then_ignore(just('"'))
            .collect::<String>()
            .map(Lit::Str)
            .map(Token::Lit);

        let punct = one_of("+-*/%=<>!&|^~#;").map(Token::Punct);

        let mk_group = |left: char, right: char, g: fn(Tokens) -> Group| {
            tokens
                .clone()
                .delimited_by(just(left), just(right))
                .map(move |tokens| Token::Group(g(tokens)))
        };
        let groupp = mk_group('(', ')', Group::Paren);
        let groupbrace = mk_group('{', '}', Group::Brace);
        let groupbracket = mk_group('[', ']', Group::Bracket);

        let ident = text::ident()
            .map(|s: String| Symbol::new(&s))
            .map(Token::Ident);

        let token = choice((
            num,
            char,
            str_,
            punct,
            groupp,
            groupbrace,
            groupbracket,
            ident,
        ));

        token
            .padded_by(comment().repeated())
            .map_with_span(|tk, span| (tk, span))
            .padded()
            .repeated()
    })
    .then_ignore(end())
}
