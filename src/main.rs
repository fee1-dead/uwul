#![feature(let_else)]
use std::env::args_os;
use std::{fs, io};

use terry::ast::Parser;
use terry::lex::Lexer;

fn main() -> io::Result<()> {
    match args_os().nth(1) {
        Some(p) => {
            let s = fs::read_to_string(p)?;
            let lexer = Lexer::new(&s);
            let Ok(tokens) = lexer.scan_tokens() else { std::process::exit(1) };
            for token in &tokens {
                println!("{:?}", token.kind);
            }
            let parser = Parser::new(&tokens);
            let Ok(ast) = parser.parse() else { std::process::exit(1) };
            println!("{ast:#?}");
        }
        None => {
            eprintln!("Usage: terry <file>");
            std::process::exit(64);
        }
    }
    Ok(())
}
