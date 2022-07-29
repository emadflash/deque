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
}


// --------------------------------------------------------------------------
//                          - Program -
// --------------------------------------------------------------------------
#[derive(Debug, PartialEq)]
pub struct Program { pub stmts: Vec<Stmt> }
impl Program {
    pub fn new(stmts: Vec<Stmt>) -> Self {
        Self {
            stmts
        }
    }

    pub fn print_ast(&self) {
        self.stmts.iter().for_each(|stmt| Program::print_stmt_with_lvl(stmt, 0));
    }

    fn print_stmt_with_lvl(stmt: &Stmt, mut indent_lvl: usize) {
        macro_rules! nest {
            ($a:block) => {
                indent_lvl += 4;
                $a;
                indent_lvl -= 4;
            }
        }

        match stmt {
            Stmt::Expr { expr } => println!("{:indent_lvl$}{}", "", expr, indent_lvl = indent_lvl),
            Stmt::Block { stmts } => {
                println!("{:indent_lvl$}{} {{", "", colorize_stmt!(BLOCK), indent_lvl=indent_lvl);
                nest!({
                    for stmt in stmts {
                        Program::print_stmt_with_lvl(stmt, indent_lvl);
                    }
                });
                println!("{:indent_lvl$}}}", "", indent_lvl=indent_lvl);
            }
            Stmt::If { main, conditions, body } => {
                println!("{:indent_lvl$}{} {} {{", "", colorize_stmt!(IF_STMT), main, indent_lvl=indent_lvl);
                nest!({
                    println!("{:indent_lvl$}{} {{", "", colorize_attr!(Conditions), indent_lvl=indent_lvl);
                    nest!({
                        for condition in conditions {
                            println!("{:indent_lvl$}{}", "", condition, indent_lvl=indent_lvl);
                        }
                    });
                    println!("{:indent_lvl$}}}", "", indent_lvl=indent_lvl);
                    Program::print_stmt_with_lvl(&*body, indent_lvl);
                });
                println!("{:indent_lvl$}}}", "", indent_lvl=indent_lvl);
            }
            Stmt::IfElse { master, alternates } => {
                println!("{:indent_lvl$}{} {{", "", colorize_stmt!(IF_ELSE_STMT), indent_lvl=indent_lvl);
                nest!({
                    println!("{:indent_lvl$}{} {{", "", colorize_attr!(Master), indent_lvl=indent_lvl);

                    nest!({
                        Program::print_stmt_with_lvl(&*master, indent_lvl);
                    });

                    println!("{:indent_lvl$}}}", "", indent_lvl=indent_lvl);
                    println!("{:indent_lvl$}{} {{", "", colorize_attr!(Alternates), indent_lvl=indent_lvl);

                    nest!({
                        for alternate in alternates {
                            Program::print_stmt_with_lvl(alternate, indent_lvl);
                        }
                    });

                    println!("{:indent_lvl$}}}", "", indent_lvl=indent_lvl);
                });

                println!("{:indent_lvl$}}}", "", indent_lvl=indent_lvl);
            }
            Stmt::While { main, conditions, body } => {
                println!("{:indent_lvl$}{} {} {{", "", colorize_stmt!(WHILE_STMT), main);
                nest!({
                    println!("{:indent_lvl$}{} {{", "", colorize_attr!(Conditions), indent_lvl=indent_lvl);
                    nest!({
                        for condition in conditions {
                            println!("{:indent_lvl$}{}", "", condition, indent_lvl=indent_lvl);
                        }
                    });
                    println!("{:indent_lvl$}}}", "", indent_lvl=indent_lvl);
                    Program::print_stmt_with_lvl(&*body, indent_lvl);
                });
                println!("{:indent_lvl$}}}", "", indent_lvl=indent_lvl);
            }
            Stmt::Let { main, name, token } => {
                println!("{:indent_lvl$}{} {} {{", "", colorize_stmt!(LET_STMT), main);
                nest!({
                    println!("{:indent_lvl$}{}: {}", "", colorize_attr!(Identifier), name, indent_lvl=indent_lvl);
                    println!("{:indent_lvl$}{}: {}", "", colorize_attr!(Token), token, indent_lvl=indent_lvl);
                });
                println!("{:indent_lvl$}}}", "", indent_lvl=indent_lvl);
            }
            Stmt::Fn { main: _, name, args, body } => {
                println!("{:indent_lvl$}{} {} {{", "", colorize_stmt!(FUNC_DECL), name);
                nest!({
                    println!("{:indent_lvl$}{}: {:?}", "", colorize_attr!(Args), args, indent_lvl=indent_lvl);
                    Program::print_stmt_with_lvl(&*body, indent_lvl);
                });
                println!("{:indent_lvl$}}}", "", indent_lvl=indent_lvl);
            }

            _ => (),
        };
    }
}
