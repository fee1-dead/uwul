#![feature(decl_macro)]

use std::io;
use std::path::PathBuf;

use clap::ValueEnum;
use terryc_base::{Context, Providers};

/// The terry compiler
#[derive(clap::Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    file: PathBuf,

    #[clap(long)]
    use_ascii: bool,

    #[clap(short, value_enum, default_value_t = Mode::Gen)]
    mode: Mode,
}

macro modes($($name:ident),*$(,)?) {
    impl From<terryc_base::Mode> for Mode {
        fn from(m: terryc_base::Mode) -> Self {
            match m {
                $(terryc_base::Mode::$name => Self::$name,)*
            }
        }
    }

    impl From<Mode> for terryc_base::Mode {
        fn from(m: Mode) -> Self {
            match m {
                $(Mode::$name => Self::$name,)*
            }
        }
    }
}

#[derive(ValueEnum, Clone, Copy, Debug)]
pub enum Mode {
    PrintAst,
    PrintMir,
    Gen,
}

modes! {
    PrintAst,
    PrintMir,
    Gen,
}

fn main() -> io::Result<()> {
    let m: Args = clap::Parser::parse();

    let mut providers = Providers::default();
    terryc_lex::provide(&mut providers);
    terryc_ast::provide(&mut providers);
    terryc_mir::provide(&mut providers);
    terryc_hir::provide(&mut providers);
    terryc_codegen::provide(&mut providers);

    terryc_base::GlobalCtxt::create_and_then(
        terryc_base::Options {
            path: m.file,
            use_ascii: m.use_ascii,
            mode: m.mode.into(),
        },
        |mut gcx| {
            gcx.set_providers(terryc_base::leak(providers));
            gcx
        },
    );

    terryc_base::run();

    /*let s = fs::read_to_string(&m.file)?;
        let lexer = Lexer::new(&s);
        let Ok(tokens) = lexer.scan_tokens() else { std::process::exit(1) };
        let mut parser = Parser::new(&s, &tokens);
        let Ok(ast) = parser.parse_stmts() else { std::process::exit(1) };
        println!("{ast:#?}");
    */
    Ok(())
}
