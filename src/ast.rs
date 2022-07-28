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
//                          - OpKind -
// --------------------------------------------------------------------------
#[derive(Debug, PartialEq, Clone)]
pub enum OpKind {
    _Drop,
    Dup,
    Let,
    Return,
    If,
    While,
    _Eq,
    Inc,
    Dec,
    Print,
    Println,
    Plus,
    Minus,
    Mod,
    GreaterThan,
    LessThan,
}

// --------------------------------------------------------------------------
//                          - Expr -
// --------------------------------------------------------------------------
#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    Number { num: f32 },
    String { text: String },
    Bool(bool),
    Iden { iden: String },
    Op { kind: OpKind },
    Call { name: String },
    PushLeft { expr: Box<Expr> },
    PushRight { expr: Box<Expr> },
}

impl Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Number { num } => write!(f, "{}", num),
            Expr::String { text } => write!(f, "\"{}\"", text),
            Expr::Bool(value) => write!(f, "{}", value),
            Expr::Iden { iden } => write!(f, "{}", iden),
            Expr::Op { kind } => write!(f, "{:?}", kind),
            Expr::Call { name } => write!(f, "Call({})", name),
            Expr::PushLeft { expr } => write!(f, "PushLeft({})", expr),
            Expr::PushRight { expr } => write!(f, "PushRight({})", expr),
        }
    }
}

// --------------------------------------------------------------------------
//                          - Stmt -
// --------------------------------------------------------------------------
#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    Program { stmts: Vec<Stmt> },
    Expr { expr: Expr },
    Block { stmts: Vec<Stmt> },
    If {
        main: Expr,
        conditions: Vec<Expr>,
        body: Box<Stmt>,
    },
    IfElse {
        master: Box<Stmt>,
        alternates: Vec<Stmt>,
    },
    While {
        main: Expr,
        conditions: Vec<Expr>,
        body: Box<Stmt>,
    },
    Let { main: Expr, name: String, token: Token },
    Fn { main: Token, name: String, args: Vec<String>, body: Box<Stmt> },
    Call { name: String },
}

impl Stmt {
    pub fn unwrap_body(&self) -> &Vec<Stmt> {
        match self {
            Stmt::Block { ref stmts } => stmts,
            _ => unreachable!("unwraping body requires body!"),
        }
    }

    pub fn unwrap_if(&self) -> (&Expr, &Vec<Expr>, &Box<Stmt>) {
        match self {
            Stmt::If { ref main, ref conditions, ref body } => (main, conditions, body),
            _ => unreachable!("should be used for unwraping if-stmt")
        }
    }

    pub fn unwrap_program(&self) -> &Vec<Stmt> {
        match self {
            Stmt::Program { ref stmts } => stmts,
            _ => unreachable!("should be used for unwraping program")
        }
    }
}

fn _print_ast(_stmt: &Stmt, mut indent: usize) {
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
        Stmt::Let { main, name, token } => {
            println!("{:indent$}{} {} {{", "", colorize_stmt!(LET_STMT), main);
            nest!({
                println!("{:indent$}{}: {}", "", colorize_attr!(Identifier), name, indent=indent);
                println!("{:indent$}{}: {}", "", colorize_attr!(Token), token, indent=indent);
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

pub fn print_ast(program: Stmt) {
    let stmts = program.unwrap_program();
    stmts.iter().for_each(|stmt| _print_ast(stmt, 0));
}
