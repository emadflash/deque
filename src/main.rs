use std::{error::Error, fs};

use deque::env::Envirnoment;
use deque::interpreter::Interpreter;
use deque::parser::Parser;
use deque::lexer::print_lexical_analysis;
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
                .help("Prints abstract syntax tree (AST)")
                .takes_value(false),
        )
        .arg(
            Arg::with_name("lex")
                .short("l")
                .long("--lex")
                .help("Prints lexical analysis of the file")
                .takes_value(false),
        )
        .get_matches();

    let source_files_path = matches.values_of_lossy("source_file").unwrap();
    let is_print_ast = matches.is_present("print_ast");
    let is_lex = matches.is_present("lex");

    let source_files = source_files_path
        .iter()
        .map(|input| fs::read_to_string(input))
        .collect::<Result<Vec<_>, _>>()?;

    if is_print_ast {
        let mut parser = Parser::new(&source_files[0]).unwrap();
        let ast = parser.parse().unwrap();
        print_ast(ast);
    }

    if is_lex {
        print_lexical_analysis(&source_files[0]);
    } else {
        let mut env = Envirnoment::new();
        let mut interpreter = Interpreter::new(&mut env);
        interpreter.interpret(&source_files[0])?;
    }

    Ok(())
}
