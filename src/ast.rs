use std::{fmt, fmt::Display};

// --------------------------------------------------------------------------
//                          - Expr -
// --------------------------------------------------------------------------
#[derive(Debug, PartialEq, Clone)]
pub(crate) enum Expr<'src> {
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
            Expr::Number { num } => write!(f, "Expr::Number({})", num),
            Expr::String { text } => write!(f, "Expr::String({})", text),
            Expr::Boolean(value) => write!(f, "Expr::Boolean({})", value),
            Expr::Op { op } => write!(f, "Expr::Op({})", op),
            Expr::PushLeft { expr } => write!(f, "Expr::PushLeft({})", expr),
            Expr::PushRight { expr } => write!(f, "Expr::PushLeft({})", expr),
        }
    }
}

// --------------------------------------------------------------------------
//                          - Stmt -
// --------------------------------------------------------------------------
#[derive(Debug, PartialEq)]
pub(crate) enum Stmt<'src> {
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
    pub(crate) fn unwrap_body(&self) -> &Vec<Stmt<'src>> {
        match self {
            Stmt::Body { ref body } => body,
            _ => unreachable!("unwraping body requires body!"),
        }
    }

    pub(crate) fn unwrap_if(&self) -> (&Expr<'src>, &Vec<Expr<'src>>, &Box<Stmt<'src>>) {
        match self {
            Stmt::If { ref main, ref conditions, ref body } => (main, conditions, body),
            _ => unreachable!("should be used for unwraping if-stmt")
        }
    }

    pub(crate) fn unwrap_program(&self) -> &Vec<Stmt<'src>> {
        match self {
            Stmt::Program { ref stmts } => stmts,
            _ => unreachable!("should be used for unwraping program")
        }
    }
}

impl<'src> Display for Stmt<'src> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Stmt::Expr { expr } => write!(f, "{:?}", expr),
            Stmt::Body { body } => {
                writeln!(f, "[")?;
                for stmt in body {
                    writeln!(f, "{}", stmt)?;
                }
                writeln!(f, "]")
            }
            Stmt::If { main, conditions, body } => {
                write!(f, "{}", main)?;
                writeln!(f, "If [")?;
                for condition in conditions {
                    writeln!(f, "{}", condition)?;
                }
                writeln!(f, "]")?;
                writeln!(f, "{}", body)
            }
            Stmt::IfElse { master, alternates } => {
                writeln!(f, "{}", master)?;
                for alternate in alternates {
                    writeln!(f, "{}", alternate)?;
                }
                Ok(())
            }
            Stmt::While { main, conditions, body } => {
                writeln!(f, "{}", main)?;
                for condition in conditions {
                    writeln!(f, "{}", condition)?;
                }
                writeln!(f, "]")?;
                writeln!(f, "{}", body)
            }
            Stmt::Program { stmts } => {
                for stmt in stmts {
                    writeln!(f, "{}", stmt)?;
                }
                Ok(())
            }
        }
    }
}
