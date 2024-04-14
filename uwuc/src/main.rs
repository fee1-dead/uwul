#![feature(decl_macro)]

use std::io;
use std::path::PathBuf;

use clap::ValueEnum;
use uwuc_base::{Context, Providers};

/// The uwu compiler
#[derive(clap::Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    file: PathBuf,

    #[clap(long)]
    use_ascii: bool,

    #[clap(long)]
    dont_print_path: bool,

    #[clap(short, value_enum, default_value_t = Mode::Gen)]
    mode: Mode,
}

macro modes($($name:ident),*$(,)?) {
    impl From<uwuc_base::Mode> for Mode {
        fn from(m: uwuc_base::Mode) -> Self {
            match m {
                $(uwuc_base::Mode::$name => Self::$name,)*
            }
        }
    }

    impl From<Mode> for uwuc_base::Mode {
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
    uwuc_lex::provide(&mut providers);
    uwuc_ast::provide(&mut providers);
    uwuc_mir::provide(&mut providers);
    uwuc_hir::provide(&mut providers);
    uwuc_codegen::provide(&mut providers);

    uwuc_base::GlobalCtxt::create_and_then(
        uwuc_base::Options {
            path: m.file,
            use_ascii: m.use_ascii,
            dont_print_path: m.dont_print_path,
            mode: m.mode.into(),
        },
        |mut gcx| {
            gcx.set_providers(uwuc_base::leak(providers));
            gcx
        },
    );

    uwuc_base::run();

    /*let s = fs::read_to_string(&m.file)?;
        let lexer = Lexer::new(&s);
        let Ok(tokens) = lexer.scan_tokens() else { std::process::exit(1) };
        let mut parser = Parser::new(&s, &tokens);
        let Ok(ast) = parser.parse_stmts() else { std::process::exit(1) };
        println!("{ast:#?}");
    */
    Ok(())
}
