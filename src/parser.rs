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
    InvaildTokenKind {
        expected: TokenKind<'src>,
        found: TokenKind<'src>,
    },
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
    Block { body: Vec<Stmt<'src>> },
    If {
        main: Expr<'src>,
        condition: Vec<Expr<'src>>,
        body: Box<Stmt<'src>>,
    },
    IfElse {
        master: Box<Stmt<'src>>,
        alternates: Vec<Stmt<'src>>,
    },
    While {
        main: Expr<'src>,
        condition: Vec<Expr<'src>>,
        body: Vec<Stmt<'src>>,
    },
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

    fn next(&mut self) -> Option<&Token<'src>> {
        if self.curr + 1 == self.tokens.len() {
            return None;
        }

        self.curr += 1;
        Some(&self.tokens[self.curr])
    }

    fn peek(&self) -> Option<&Token<'src>> {
        if self.curr + 1 == self.tokens.len() {
            return None;
        }

        Some(&self.tokens[self.curr + 1])
    }

    fn eof(&self) -> &Token<'src> {
        assert_eq!(self.curr, self.tokens.len() - 1);
        &self.tokens[self.curr]
    }

    fn expect(&mut self, expected: &TokenKind<'src>) -> Result<(), ParseError<'src>> {
        if !matches!(&self.current().kind, _expected) {
            return Err(ParseError::InvaildTokenKind {
                expected: expected.clone(),
                found: self.current().kind.clone(),
            });
        }

        self.next();
        Ok(())
    }

    // --------------------------------------------------------------------------
    //                          - Expr -
    // --------------------------------------------------------------------------
    fn parse_expr(&mut self) -> anyhow::Result<Expr<'src>, ParseError<'src>> {
        let expr = match self.current().kind {
            TokenKind::Sym { sym: "!" } => match self.next() {
                Some(arg) => match arg.kind {
                    TokenKind::Number { num, .. } => Ok(Expr::PushLeft {
                        expr: Box::new(Expr::Number { num }),
                    }),

                    TokenKind::Keyword { kw } => match &kw {
                        &"dup" | &"pud" | &"drop" | &"print" | &"println" | &"if" | &"elif" | &"while" | &"eq" => {
                            Ok(Expr::PushLeft {
                                expr: Box::new(Expr::Op { op: kw }),
                            })
                        }
                        _ => unreachable!(),
                    },

                    TokenKind::Sym { sym } => match &sym {
                        &"+" | &"-" | &"%" | &"<" | &">" => Ok(Expr::PushLeft {
                            expr: Box::new(Expr::Op { op: sym }),
                        }),
                        _ => unreachable!(),
                    },

                    _ => unreachable!(),
                },

                None => Err(ParseError::MissingExpr),
            },

            TokenKind::Keyword { kw } => match &kw {
                &"dup" | &"pud" | &"drop" | &"print" | &"println" | &"if" | &"elif" | &"while" | &"eq" => {
                    match self.next() {
                        Some(op) => match op.kind {
                            TokenKind::Sym { sym: "!" } => Ok(Expr::PushRight {
                                expr: Box::new(Expr::Op { op: kw }),
                            }),

                            _ => Err(ParseError::InvaildOpKind { found: op.clone() }),
                        },
                        None => Err(ParseError::MissingOp),
                    }
                }
                _ => unreachable!(),
            },

            TokenKind::Sym { sym } => match &sym {
                &"+" | &"-" | &"%" | &"<" | &">" => match self.next() {
                    Some(op) => match op.kind {
                        TokenKind::Sym { sym: "!" } => Ok(Expr::PushRight {
                            expr: Box::new(Expr::Op { op: sym }),
                        }),

                        _ => Err(ParseError::InvaildOpKind { found: op.clone() }),
                    },
                    None => Err(ParseError::MissingOp),
                },
                _ => unreachable!("{}, prev: {:?}", sym,  self.tokens[self.curr - 2]),
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
        };

        self.next();
        expr
    }

    // --------------------------------------------------------------------------
    //                          - Block -
    // --------------------------------------------------------------------------
    fn parse_block(&mut self) -> anyhow::Result<Stmt<'src>, ParseError<'src>> {
        assert!(matches!(self.current(), Token { kind: TokenKind::Sym { sym: "{" }, .. }));
        let mut body: Vec<Stmt<'src>> = Vec::new();

        while let Some(tok) = self.next() {
            if matches!(tok, Token { kind: TokenKind::Sym { sym: "}" }, .. }) {
                break;
            }
            body.push(self.parse_stmt()?);
        }

        self.expect(&TokenKind::Sym { sym: "}" })?;
        Ok(Stmt::Block { body })
    }

    // --------------------------------------------------------------------------
    //                          - If -
    // --------------------------------------------------------------------------
    fn is_elif_start(&mut self) -> bool {
        if matches!(self.current(), Token { kind: TokenKind::Sym { sym: "!" }, .. }) && 
            matches!(self.peek(), Some(Token { kind: TokenKind::Keyword { kw: "elif" }, .. })) {
                return true;
        }

        if matches!(self.current(), Token { kind: TokenKind::Sym { sym: "elif" }, .. }) && 
            matches!(self.peek(), Some(Token { kind: TokenKind::Keyword { kw: "!" }, .. })) {
                return true;
        }

        false
    }

    fn parse_if_block(&mut self, main: Expr<'src>) -> anyhow::Result<Stmt<'src>, ParseError<'src>> {
        let mut condition: Vec<Expr<'src>> = Vec::new();

        while let Some(tok) = self.next() {
            if matches!(tok, Token { kind: TokenKind::Sym { sym: "{" }, ..}) {
                break;
            }
            condition.push(self.parse_expr()?);
        }

        self.expect(&TokenKind::Sym { sym: "{" })?;
        Ok(Stmt::If { main, condition, body: Box::new(self.parse_block()?) })
    }

    fn parse_if_stmt(&mut self, main: Expr<'src>) -> anyhow::Result<Stmt<'src>, ParseError<'src>> {
        let master = self.parse_if_block(main)?;
        let mut alternates: Vec<Stmt<'src>> = Vec::new();

        while self.is_elif_start() {
            let main = self.parse_expr()?;
            alternates.push(self.parse_if_block(main)?);
            self.next();
        }

        if matches!(self.peek(), Some(&Token { kind: TokenKind::Keyword { kw: "else" }, .. })) {
            self.next();
            self.next();
            alternates.push(self.parse_block()?);
        }

        if alternates.is_empty() {
            return Ok(master);
        }
        Ok(Stmt::IfElse { master: Box::new(master), alternates })
    }

    fn parse_while_stmt(&mut self, main: Expr<'src>) -> anyhow::Result<Stmt<'src>, ParseError<'src>> {
        let mut condition: Vec<Expr<'src>> = Vec::new();
        let mut body: Vec<Stmt<'src>> = Vec::new();

        while let Some(tok) = self.next() {
            if matches!(tok, Token { kind: TokenKind::Sym { sym: "{" }, .. }) {
                break;
            }
            condition.push(self.parse_expr()?);
        }

        self.expect(&TokenKind::Sym { sym: "{" })?;

        while let Some(tok) = self.next() {
            if matches!(tok, Token { kind: TokenKind::Sym { sym: "}" }, .. }) {
                break;
            } else {
                body.push(self.parse_stmt()?);
            }
        }

        self.expect(&TokenKind::Sym { sym: "{" })?;
        Ok(Stmt::While {
            main,
            condition,
            body,
        })
    }

    fn parse_stmt(&mut self) -> anyhow::Result<Stmt<'src>, ParseError<'src>> {
        let stmt = self.parse_expr()?;

        match stmt {
            Expr::PushLeft { ref expr } | Expr::PushRight { ref expr } => {
                match **expr {
                    Expr::Op { op: "if" } => return self.parse_if_stmt(stmt),
                    Expr::Op { op: "while" } => return self.parse_while_stmt(stmt),
                    _ => (),
                };
            }

            _ => (),
        };

        Ok(Stmt::Expr { expr: stmt })
    }

    pub(crate) fn parse(&mut self) -> anyhow::Result<Vec<Stmt<'src>>, ParseError<'src>> {
        let mut stmts: Vec<Stmt> = Vec::new();

        while !matches!(self.current(), Token { kind: TokenKind::Eof, .. }) {
            stmts.push(self.parse_stmt()?);
        }

        Ok(stmts)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn misc_exprs() {
        let text: &str = "!+ !- !2 !- dup! !> <!";
        let mut parser = Parser::from(text).unwrap();
        assert_eq!(
            parser.parse(),
            Ok(vec![
                Stmt::Expr {
                    expr: Expr::PushLeft {
                        expr: Box::new(Expr::Op { op: "+" })
                    }
                },
                Stmt::Expr {
                    expr: Expr::PushLeft {
                        expr: Box::new(Expr::Op { op: "-" })
                    }
                },
                Stmt::Expr {
                    expr: Expr::PushLeft {
                        expr: Box::new(Expr::Number { num: 2.0 })
                    }
                },
                Stmt::Expr {
                    expr: Expr::PushLeft {
                        expr: Box::new(Expr::Op { op: "-" })
                    }
                },
                Stmt::Expr {
                    expr: Expr::PushRight {
                        expr: Box::new(Expr::Op { op: "dup" })
                    }
                },
                Stmt::Expr {
                    expr: Expr::PushLeft {
                        expr: Box::new(Expr::Op { op: ">" })
                    }
                },
                Stmt::Expr {
                    expr: Expr::PushRight {
                        expr: Box::new(Expr::Op { op: "<" })
                    }
                },
            ])
        );
    }

    #[test]
    fn parse_only_if_stmt() {
        let text: &str = "!if !1 { !1 !2 } !69";
        let mut parser = Parser::from(text).unwrap();
        assert_eq!(
            parser.parse(),
            Ok(vec![
                Stmt::If {
                    main: Expr::PushLeft {
                        expr: Box::new(Expr::Op { op: "if" })
                    },
                    condition: vec![
                         Expr::PushLeft {
                             expr: Box::new(Expr::Number { num: 1.0 })
                         }
                    ],
                    body: Box::new(Stmt::Block {
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
                    })
                },
                Stmt::Expr {
                    expr: Expr::PushLeft {
                        expr: Box::new(Expr::Number { num: 69.0 })
                    }
                },
            ])
        );
    }

    #[test]
    fn parse_if_elif_block() {
        let text: &str = "!if !1 { } !elif !dup !> { } !elif dup! { 69! } !68";
        let mut parser = Parser::from(text).unwrap();
        assert_eq!(
            parser.parse(),
            Ok(vec![
                Stmt::IfElse {
                    master: Box::new(Stmt::If {
                        main: Expr::PushLeft {
                            expr: Box::new(Expr::Op { op: "if" })
                        },
                        condition: vec![
                            Expr::PushLeft {
                                expr: Box::new(Expr::Number { num: 1.0}),
                            },
                        ],
                        body: Box::new(Stmt::Block {
                            body: vec![]
                        })
                    }),
                    alternates: vec![
                        Stmt::If {
                            main: Expr::PushLeft {
                                expr: Box::new(Expr::Op { op: "elif" })
                            },
                            condition: vec![
                                Expr::PushLeft {
                                    expr: Box::new(Expr::Op { op: "dup" }),
                                },
                                Expr::PushLeft {
                                    expr: Box::new(Expr::Op { op: ">" }),
                                },
                            ],
                            body: Box::new(Stmt::Block { body: vec![] })
                        },
                        Stmt::If {
                            main: Expr::PushLeft {
                                expr: Box::new(Expr::Op { op: "elif" })
                            },
                            condition: vec![Expr::PushRight {
                                expr: Box::new(Expr::Op { op: "dup" }),
                            },],
                            body: Box::new(Stmt::Block {
                                body: vec![
                                    Stmt::Expr {
                                        expr: Expr::PushRight {
                                            expr: Box::new(Expr::Number { num: 69.0 })
                                        }
                                    }
                                ]
                            })
                        }
                    ]
                },
                Stmt::Expr {
                    expr: Expr::PushRight {
                        expr: Box::new(Expr::Number { num: 68.0 })
                    }
                }
            ])
        );
    }

    //#[test]
    //fn parse_if_else_block() {
        //let text: &str = "!1 !if { !1 !2 } else { !2 !1 }";
        //let mut parser = Parser::from(text).unwrap();
        //assert_eq!(
            //parser.parse(),
            //Ok(vec![
                //Stmt::Expr {
                    //expr: Expr::PushLeft {
                        //expr: Box::new(Expr::Number { num: 1.0 })
                    //}
                //},
                //Stmt::IfElse {
                    //if_block: Box::new(Stmt::If {
                        //main: Expr::PushLeft {
                            //expr: Box::new(Expr::Op { op: "if" })
                        //},
                        //body: vec![
                            //Stmt::Expr {
                                //expr: Expr::PushLeft {
                                    //expr: Box::new(Expr::Number { num: 1.0 })
                                //}
                            //},
                            //Stmt::Expr {
                                //expr: Expr::PushLeft {
                                    //expr: Box::new(Expr::Number { num: 2.0 })
                                //}
                            //},
                        //]
                    //}),

                    //else_block: vec![
                        //Stmt::Expr {
                            //expr: Expr::PushLeft {
                                //expr: Box::new(Expr::Number { num: 2.0 })
                            //}
                        //},
                        //Stmt::Expr {
                            //expr: Expr::PushLeft {
                                //expr: Box::new(Expr::Number { num: 1.0 })
                            //}
                        //},
                    //]
                //},
            //])
        //);
    //}

    #[test]
    fn parse_while_block() {
        let text: &str = "!while !dup { !1 }";
        let mut parser = Parser::from(text).unwrap();
        assert_eq!(
            parser.parse(),
            Ok(vec![Stmt::While {
                main: Expr::PushLeft {
                    expr: Box::new(Expr::Op { op: "while" }),
                },
                condition: vec![Expr::PushLeft {
                    expr: Box::new(Expr::Op { op: "dup" }),
                },],
                body: vec![Stmt::Expr {
                    expr: Expr::PushLeft {
                        expr: Box::new(Expr::Number { num: 1.0 }),
                    },
                }],
            }])
        );
    }
}
