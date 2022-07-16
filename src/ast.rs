use std::{fmt, fmt::Display};
use crate::lexer::Token;
use ansi_term::Color;

// --------------------------------------------------------------------------
//                          - Helper macros -
// --------------------------------------------------------------------------
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

// --------------------------------------------------------------------------
//                          - Expr -
// --------------------------------------------------------------------------
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum Expr<'src> {
    Number { num: f32 },
    String { text: &'src str },
    Boolean(bool),
    Iden { iden: &'src str },
    FnCall { main: Token<'src>, name: &'src str },
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
            Expr::Iden { iden } => write!(f, "{}", iden),
            Expr::Op { op } => write!(f, "{}", op),
            Expr::FnCall { main: _, name } => write!(f, "{} {}()", colorize_attr!(FN_CALL), name),
            Expr::PushLeft { expr } => write!(f, "PushLeft({})", expr),
            Expr::PushRight { expr } => write!(f, "PushRight({})", expr),
        }
    }
}

// --------------------------------------------------------------------------
//                          - Stmt -
// --------------------------------------------------------------------------
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum Stmt<'src> {
    Program { stmts: Vec<Stmt<'src>> },
    Expr { expr: Expr<'src> },
    Block { stmts: Vec<Stmt<'src>> },
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
    Let { main: Expr<'src>, token: Token<'src>, iden: &'src str },
    Fn { main: Token<'src>, name: &'src str, args: Vec<&'src str>, body: Box<Stmt<'src>> }
}

impl<'src> Stmt<'src> {
    pub fn unwrap_body(&self) -> &Vec<Stmt<'src>> {
        match self {
            Stmt::Block { ref stmts } => stmts,
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
    macro_rules! nest {
        ($a:block) => {
            indent += 4;
            $a;
            indent -= 4;
        }
    }

    match _stmt {
        Stmt::Expr { expr } => println!("{:indent$}{}", "", expr, indent = indent),
        Stmt::Block { stmts } => {
            println!("{:indent$}{} {{", "", colorize_stmt!(BLOCK), indent=indent);
            nest!({
                for stmt in stmts {
                    _print_ast(stmt, indent);
                }
            });
            println!("{:indent$}}}", "", indent=indent);
        }
        Stmt::If { main, conditions, body } => {
            println!("{:indent$}{} {} {{", "", colorize_stmt!(IF_STMT), main, indent=indent);
            nest!({
                println!("{:indent$}{} {{", "", colorize_attr!(Conditions), indent=indent);
                nest!({
                    for condition in conditions {
                        println!("{:indent$}{}", "", condition, indent=indent);
                    }
                });
                println!("{:indent$}}}", "", indent=indent);
                _print_ast(&*body, indent);
            });
            println!("{:indent$}}}", "", indent=indent);
        }
        Stmt::IfElse { master, alternates } => {
            println!("{:indent$}{} {{", "", colorize_stmt!(IF_ELSE_STMT), indent=indent);
            nest!({
                println!("{:indent$}{} {{", "", colorize_attr!(Master), indent=indent);

                nest!({
                    _print_ast(&*master, indent);
                });

                println!("{:indent$}}}", "", indent=indent);
                println!("{:indent$}{} {{", "", colorize_attr!(Alternates), indent=indent);

                nest!({
                    for alternate in alternates {
                        _print_ast(alternate, indent);
                    }
                });

                println!("{:indent$}}}", "", indent=indent);
            });

            println!("{:indent$}}}", "", indent=indent);
        }
        Stmt::While { main, conditions, body } => {
            println!("{:indent$}{} {} {{", "", colorize_stmt!(WHILE_STMT), main);
            nest!({
                println!("{:indent$}{} {{", "", colorize_attr!(Conditions), indent=indent);
                nest!({
                    for condition in conditions {
                        println!("{:indent$}{}", "", condition, indent=indent);
                    }
                });
                println!("{:indent$}}}", "", indent=indent);
                _print_ast(&*body, indent);
            });
            println!("{:indent$}}}", "", indent=indent);
        }
        Stmt::Let { main, token, iden } => {
            println!("{:indent$}{} {} {{", "", colorize_stmt!(LET_STMT), main);
            nest!({
                println!("{:indent$}{}: {}", "", colorize_attr!(Token), token, indent=indent);
                println!("{:indent$}{}: {}", "", colorize_attr!(Identifier), iden, indent=indent);
            });
            println!("{:indent$}}}", "", indent=indent);
        }
        Stmt::Fn { main: _, name, args, body } => {
            println!("{:indent$}{} {} {{", "", colorize_stmt!(FUNC_DECL), name);
            nest!({
                println!("{:indent$}{}: {:?}", "", colorize_attr!(Args), args, indent=indent);
                _print_ast(&*body, indent);
            });
            println!("{:indent$}}}", "", indent=indent);
        }

        _ => (),
    };
}

pub fn print_ast<'src>(program: Stmt<'src>) {
    let stmts = program.unwrap_program();
    stmts.iter().for_each(|stmt| _print_ast(stmt, 0));
}
