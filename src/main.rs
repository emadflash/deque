use std::{error::Error, fs};

use deque::env::Env;
use deque::eval::Eval;
use deque::parser::Parser;
use deque::ast::print_ast;

use clap::{App, Arg};

fn main() -> Result<(), Box<dyn Error>> {
    let matches = App::new("deque")
        .version("0.1.0")
        .author("madflash <backbugkaught@gmail.com>")
        .about("deque interpreter")
        .arg(
            Arg::with_name("source_file")
                .value_name("SOURCE_FILE")
                .help("deque source file")
                .required(true)
                .min_values(1),
        )
        .arg(
            Arg::with_name("print_ast")
                .short("a")
                .long("--print-ast")
                .help("Print abstract syntax tree (AST)")
                .takes_value(false),
        )
        .get_matches();

    let source_files_path = matches.values_of_lossy("source_file").unwrap();
    let is_print_ast = matches.is_present("print_ast");

    let source_files = source_files_path
        .iter()
        .map(|input| fs::read_to_string(input))
        .collect::<Result<Vec<_>, _>>()?;

    if is_print_ast {
        let mut parser = Parser::new(&source_files[0]).unwrap();
        let ast = parser.parse().unwrap();
        print_ast(ast);
    } else {
        let mut env = Env::new();
        let mut eval = Eval::new(&mut env);
        eval.eval(&source_files[0])?;
    }

    Ok(())
}
