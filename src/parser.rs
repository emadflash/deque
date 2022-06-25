use std::{fmt, fmt::Display};

use crate::lexer::{lex, Token, TokenKind};
use thiserror::Error;

#[derive(Error, Debug, PartialEq)]
pub(crate) enum ParseError<'src> {
    #[error("missing expression")]
    MissingExpr,

    #[error("missing op")]
    MissingOp,

    #[error("invaild op kind (found: {found:?})")]
    InvaildOpKind { found: Token<'src> },

    #[error("invaild token kind (expected: {expected:?}, found: {found:?}")]
    InvaildTokenKind { expected: TokenKind<'src>, found: TokenKind<'src> }
}

#[derive(Debug, PartialEq)]
pub(crate) enum Expr<'src> {
    Number { num: f32 },
    String { text: &'src str },
    Op { op: &'src str },
    PushLeft { expr: Box<Expr<'src>> },
    PushRight { expr: Box<Expr<'src>> },
}

impl<'src> Display for Expr<'src> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Number { num } => write!(f, "Expr::Number({})", num),
            Expr::String { text } => write!(f, "Expr::String({})", text),
            Expr::Op { op } => write!(f, "Expr::Op({})", op),
            Expr::PushLeft { expr } => write!(f, "Expr::PushLeft({})", expr),
            Expr::PushRight { expr } => write!(f, "Expr::PushLeft({})", expr),
        }
    }
}

#[derive(Debug, PartialEq)]
pub(crate) enum Stmt<'src> {
    Expr { expr: Expr<'src> },
    If { stmt: Expr<'src>, body: Vec<Stmt<'src>> },
    IfElse { if_arm: Box<Stmt<'src>>, else_arm: Vec<Stmt<'src>> },
    Label { name: &'src str },
}

#[derive(Debug)]
pub(crate) struct Parser<'src> {
    tokens: Vec<Token<'src>>,
    curr: usize,
}

impl<'src> Parser<'src> {
    pub(crate) fn from(text: &'src str) -> anyhow::Result<Parser> {
        Ok(Self {
            tokens: lex(&text)?,
            curr: 0,
        })
    }

    fn current(&self) -> &Token<'src> {
        return &self.tokens[self.curr - 1];
    }

    fn next(&mut self) -> Option<&Token<'src>> {
        if self.curr == self.tokens.len() - 1 {
            return None;
        }

        let tok = &self.tokens[self.curr];
        self.curr += 1;
        Some(tok)
    }

    fn peek(&self) -> Option<&Token<'src>> {
        if self.curr == self.tokens.len() - 1 {
            return None;
        }

        let tok = &self.tokens[self.curr];
        Some(tok)
    }

    fn eof(&self) -> &Token<'src> {
        assert_eq!(self.curr, self.tokens.len() - 1);
        &self.tokens[self.curr]
    }

    fn expect(&mut self, expected: &TokenKind<'src>) -> Result<(), ParseError<'src>> {
        if let Some(tok) = self.peek() {
            if !matches!(&tok.kind, _expected) {
                return Err(ParseError::InvaildTokenKind { expected: expected.clone(), found: tok.kind.clone() });
            }
        }

        self.next();
        Ok(())
    }

    fn skip_label_if_any(&mut self) -> Option<&'src str> {
        match self.current().kind {
            TokenKind::Iden { iden } => {
                if matches!(
                    self.peek(),
                    Some(Token {
                        kind: TokenKind::Sym { sym: ":" },
                        ..
                    })
                ) {
                    self.next();
                    return Some(iden);
                }
                None
            }

            _ => None,
        }
    }

    fn parse_expr(&mut self) -> anyhow::Result<Expr<'src>, ParseError<'src>> {
        match self.current().kind {
            TokenKind::Sym { sym: "!" } => match self.next() {
                Some(arg) => match arg.kind {
                    TokenKind::Number { num, .. } => Ok(Expr::PushLeft {
                        expr: Box::new(Expr::Number { num }),
                    }),

                    TokenKind::Keyword { kw } => match &kw {
                        &"add" | &"sub" | &"dup" | &"print" | &"if" => Ok(Expr::PushLeft {
                            expr: Box::new(Expr::Op { op: kw }),
                        }),
                        _ => unreachable!(),
                    },

                    _ => unreachable!(),
                },

                None => Err(ParseError::MissingExpr),
            },

            TokenKind::Keyword { kw } => match &kw {
                &"add" | &"sub" | &"dup" | &"print" | &"if" => match self.next() {
                    Some(op) => match op.kind {
                        TokenKind::Sym { sym: "!" } => Ok(Expr::PushRight {
                            expr: Box::new(Expr::Op { op: kw }),
                        }),

                        _ => Err(ParseError::InvaildOpKind { found: op.clone() }),
                    },
                    None => Err(ParseError::MissingOp),
                },
                _ => unreachable!(),
            },

            TokenKind::Number { num, .. } => match self.next() {
                Some(arg) => match arg.kind {
                    TokenKind::Sym { sym: "!" } => Ok(Expr::PushRight {
                        expr: Box::new(Expr::Number { num }),
                    }),
                    _ => Err(ParseError::InvaildOpKind { found: arg.clone() }),
                },

                None => Err(ParseError::InvaildOpKind {
                    found: self.eof().clone(),
                }),
            },

            _ => unreachable!(),
        }
    }

    fn parse_if_stmt(&mut self, stmt: Expr<'src>) -> anyhow::Result<Stmt<'src>, ParseError<'src>> {
        assert!(matches!(self.current(), Token { kind: TokenKind::Keyword { kw: "if" }, .. })
            || matches!(self.current(), Token { kind: TokenKind::Sym { sym: "!" }, .. }));

        self.expect(&TokenKind::Sym { sym: "{" })?;
        let mut body: Vec<Stmt<'src>> = Vec::new();

        while let Some(tok) = self.next() {
            if matches!(tok, Token { kind: TokenKind::Sym { sym: "}" }, .. }) {
                break;
            } else {
                body.push(self.parse_stmt()?);
            }
        }

        if !matches!(self.current(), Token { kind: TokenKind::Sym { sym: "}" }, .. }) {
            return Err(ParseError::InvaildTokenKind { expected: TokenKind::Sym { sym: "}" }, found: self.current().clone().kind } );
        }

        if matches!(self.peek(), Some(&Token { kind: TokenKind::Keyword { kw: "else" }, ..})) {
            self.next();
            self.expect(&TokenKind::Sym { sym: "{" })?;

            let mut else_arm: Vec<Stmt<'src>> = Vec::new();
            while let Some(tok) = self.next() {
                if matches!(tok, Token { kind: TokenKind::Sym { sym: "}" }, .. }) {
                    break;
                } else {
                    else_arm.push(self.parse_stmt()?);
                }
            }

            if !matches!(self.current(), Token { kind: TokenKind::Sym { sym: "}" }, .. }) {
                return Err(ParseError::InvaildTokenKind { expected: TokenKind::Sym { sym: "}" }, found: self.current().clone().kind } );
            }

            return Ok(Stmt::IfElse { if_arm: Box::new(Stmt::If { stmt, body }), else_arm }) 
        }

        Ok(Stmt::If { stmt, body }) 
    }

    fn parse_stmt(&mut self) -> anyhow::Result<Stmt<'src>, ParseError<'src>> {
        if let Some(name) = self.skip_label_if_any() {
            Ok(Stmt::Label { name })
        } else {
            let stmt = self.parse_expr()?;

            match stmt {
                Expr::PushLeft { ref expr } | Expr::PushRight { ref expr } => {
                    match **expr {
                        Expr::Op { op: "if" } => return self.parse_if_stmt(stmt),
                        _ => (),
                    };
                },

                _ => ()
            };

            Ok(Stmt::Expr { expr: stmt })
        }
    }

    pub(crate) fn parse(&mut self) -> anyhow::Result<Vec<Stmt<'src>>, ParseError<'src>> {
        let mut stmts: Vec<Stmt> = Vec::new();

        while let Some(_) = self.next() {
            stmts.push(self.parse_stmt()?);
        }

        Ok(stmts)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn _parse() {
        let text: &str = "!add !sub !2 loop: !sub end: dup!";
        let mut parser = Parser::from(text).unwrap();
        assert_eq!(
            parser.parse(),
            Ok(vec![
                Stmt::Expr {
                    expr: Expr::PushLeft {
                        expr: Box::new(Expr::Op { op: "add" })
                    }
                },
                Stmt::Expr {
                    expr: Expr::PushLeft {
                        expr: Box::new(Expr::Op { op: "sub" })
                    }
                },
                Stmt::Expr {
                    expr: Expr::PushLeft {
                        expr: Box::new(Expr::Number { num: 2.0 })
                    }
                },
                Stmt::Label { name: "loop" },
                Stmt::Expr {
                    expr: Expr::PushLeft {
                        expr: Box::new(Expr::Op { op: "sub" })
                    }
                },
                Stmt::Label { name: "end" },
                Stmt::Expr {
                    expr: Expr::PushRight {
                        expr: Box::new(Expr::Op { op: "dup" })
                    }
                }
            ])
        );
    }

    #[test]
    fn parse_if_block() {
        let text: &str = "!1 !if { !1 !2 }";
        let mut parser = Parser::from(text).unwrap();
        assert_eq!(
            parser.parse(),
            Ok(vec![
                Stmt::Expr {
                    expr: Expr::PushLeft {
                        expr: Box::new(Expr::Number { num: 1.0 })
                    }
                },
                Stmt::If {
                    stmt: Expr::PushLeft {
                        expr: Box::new(Expr::Op { op: "if" })
                    },
                    body: vec![
                        Stmt::Expr {
                            expr: Expr::PushLeft {
                                expr: Box::new(Expr::Number { num: 1.0 })
                            }
                        },
                        Stmt::Expr {
                            expr: Expr::PushLeft {
                                expr: Box::new(Expr::Number { num: 2.0 })
                            }
                        },
                    ]
                },
            ]));
    }

    #[test]
    fn parse_if_else_block() {
        let text: &str = "!1 !if { !1 !2 } else { !2 !1 }";
        let mut parser = Parser::from(text).unwrap();
        assert_eq!(
            parser.parse(),
            Ok(vec![
                Stmt::Expr {
                    expr: Expr::PushLeft {
                        expr: Box::new(Expr::Number { num: 1.0 })
                    }
                },
                Stmt::IfElse {
                    if_arm: Box::new(Stmt::If {
                        stmt: Expr::PushLeft {
                            expr: Box::new(Expr::Op { op: "if" })
                        },
                        body: vec![
                            Stmt::Expr {
                                expr: Expr::PushLeft {
                                    expr: Box::new(Expr::Number { num: 1.0 })
                                }
                            },
                            Stmt::Expr {
                                expr: Expr::PushLeft {
                                    expr: Box::new(Expr::Number { num: 2.0 })
                                }
                            },
                        ]
                    }),

                    else_arm: vec![
                        Stmt::Expr {
                            expr: Expr::PushLeft {
                                expr: Box::new(Expr::Number { num: 2.0 })
                            }
                        },
                        Stmt::Expr {
                            expr: Expr::PushLeft {
                                expr: Box::new(Expr::Number { num: 1.0 })
                            }
                        },
                    ]
                    },
            ]));
    }
}
