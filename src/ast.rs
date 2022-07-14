use std::{fmt, fmt::Display};
use ansi_term::Color;

// --------------------------------------------------------------------------
//                          - Expr -
// --------------------------------------------------------------------------
#[derive(Debug, PartialEq, Clone)]
pub enum Expr<'src> {
    Number { num: f32 },
    String { text: &'src str },
    Boolean(bool),
    Op { op: &'src str },
    PushLeft { expr: Box<Expr<'src>> },
    PushRight { expr: Box<Expr<'src>> },
}

impl<'src> Display for Expr<'src> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Number { num } => write!(f, "{}", num),
            Expr::String { text } => write!(f, "\"{}\"", text),
            Expr::Boolean(value) => write!(f, "{}", value),
            Expr::Op { op } => write!(f, "{}", op),
            Expr::PushLeft { expr } => write!(f, "PushLeft({})", expr),
            Expr::PushRight { expr } => write!(f, "PushRight({})", expr),
        }
    }
}

// --------------------------------------------------------------------------
//                          - Stmt -
// --------------------------------------------------------------------------
#[derive(Debug, PartialEq)]
pub enum Stmt<'src> {
    Expr { expr: Expr<'src> },
    Body { body: Vec<Stmt<'src>> },
    If {
        main: Expr<'src>,
        conditions: Vec<Expr<'src>>,
        body: Box<Stmt<'src>>,
    },
    IfElse {
        master: Box<Stmt<'src>>,
        alternates: Vec<Stmt<'src>>,
    },
    While {
        main: Expr<'src>,
        conditions: Vec<Expr<'src>>,
        body: Box<Stmt<'src>>,
    },
    Program { stmts: Vec<Stmt<'src>> }
}

impl<'src> Stmt<'src> {
    pub fn unwrap_body(&self) -> &Vec<Stmt<'src>> {
        match self {
            Stmt::Body { ref body } => body,
            _ => unreachable!("unwraping body requires body!"),
        }
    }

    pub fn unwrap_if(&self) -> (&Expr<'src>, &Vec<Expr<'src>>, &Box<Stmt<'src>>) {
        match self {
            Stmt::If { ref main, ref conditions, ref body } => (main, conditions, body),
            _ => unreachable!("should be used for unwraping if-stmt")
        }
    }

    pub fn unwrap_program(&self) -> &Vec<Stmt<'src>> {
        match self {
            Stmt::Program { ref stmts } => stmts,
            _ => unreachable!("should be used for unwraping program")
        }
    }
}

fn _print_ast<'src>(_stmt: &Stmt<'src>, mut indent: usize) {
    macro_rules! colorize_stmt {
        ($a:expr) => {
            Color::Cyan.bold().paint(stringify!($a))
        }
    }

    macro_rules! colorize_attr {
        ($a:expr) => {
            Color::Green.paint(stringify!($a))
        }
    }

    match _stmt {
        Stmt::Expr { expr } => println!("{:indent$}{}", "", expr, indent = indent),
        Stmt::Body { body } => {
            println!("{:indent$}{} {{", "", colorize_stmt!(BLOCK), indent=indent);
            indent += 4;
            for stmt in body {
                _print_ast(stmt, indent);
            }
            indent -= 4;
            println!("{:indent$}}}", "", indent=indent);
        }
        Stmt::If { main, conditions, body } => {
            println!("{:indent$}{} {} {{", "", colorize_stmt!(IF_STMT), main, indent=indent);
            indent += 4;
            println!("{:indent$}{} {{", "", colorize_attr!(Conditions), indent=indent);
            indent += 4;
            for condition in conditions {
                println!("{:indent$}{}", "", condition, indent=indent);
            }
            indent -= 4;
            println!("{:indent$}}}", "", indent=indent);
            _print_ast(&*body, indent);
            indent -= 4;
            println!("{:indent$}}}", "", indent=indent);
        }
        Stmt::IfElse { master, alternates } => {
            println!("{:indent$}{} {{", "", colorize_stmt!(IF_ELSE_STMT), indent=indent);
            indent += 4;
            println!("{:indent$}{} {{", "", colorize_attr!(Master), indent=indent);
            indent += 4;
            _print_ast(&*master, indent);
            indent -= 4;
            println!("{:indent$}}}", "", indent=indent);
            println!("{:indent$}{} {{", "", colorize_attr!(Alternates), indent=indent);
            indent += 4;
            for alternate in alternates {
                _print_ast(alternate, indent);
            }
            indent -= 4;
            println!("{:indent$}}}", "", indent=indent);
            indent -= 4;
            println!("{:indent$}}}", "", indent=indent);
        }
        Stmt::While { main, conditions, body } => {
            println!("{:indent$}{} {} {{", "", colorize_stmt!(WHILE_STMT), main);
            indent += 4;
            println!("{:indent$}{} {{", "", colorize_attr!(Conditions), indent=indent);
            indent += 4;
            for condition in conditions {
                println!("{:indent$}{}", "", condition, indent=indent);
            }
            indent -= 4;
            println!("{:indent$}}}", "", indent=indent);
            _print_ast(&*body, indent);
            indent -= 4;
            println!("{:indent$}}}", "", indent=indent);
        }

        _ => (),
    };
}

pub fn print_ast<'src>(program: Stmt<'src>) {
    let stmts = program.unwrap_program();
    stmts.iter().for_each(|stmt| _print_ast(stmt, 0));
}
