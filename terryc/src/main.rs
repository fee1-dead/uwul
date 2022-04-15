#![feature(let_else)]
use std::env::args_os;
use std::{fs, io};

use terryc::ast::Parser;
//use terry::interpret::Interpreter;
use terryc::lex::Lexer;

fn main() -> io::Result<()> {
    match args_os().nth(1) {
        Some(p) => {
            let s = fs::read_to_string(p)?;
            let lexer = Lexer::new(&s);
            let Ok(tokens) = lexer.scan_tokens() else { std::process::exit(1) };
            for token in &tokens {
                //println!("{:?}", token.kind);
            }
            let mut parser = Parser::new(&tokens);
            let Ok(ast) = parser.parse_stmts() else { std::process::exit(1) };
            println!("{ast:#?}");
            //let mut interpreter = Interpreter::new();
            //let ev = interpreter.eval_stmts(ast);
            //println!("{:?}", ev.err());
        }
        None => {
            eprintln!("Usage: terry <file>");
            std::process::exit(64);
        }
    }
    Ok(())
}
