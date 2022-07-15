use std::iter::Peekable;

use crate::lexer::{TokenKind, Lexer, LexerError};
use crate::ast::{Expr, Stmt};

// --------------------------------------------------------------------------
//                          - ParseError -
// --------------------------------------------------------------------------
#[derive(thiserror::Error, Debug, PartialEq)]
pub enum ParseError<'src> {
    #[error("Lexer error")]
    LexerError(#[from] LexerError),

    #[error("Missing expression")]
    MissingExpr,

    #[error("Invaild token kind (expected: {expected:?}, found: {found:?}")]
    InvaildTokenKind {
        expected: TokenKind<'src>,
        found: TokenKind<'src>,
    },
}

impl<'src> From<&LexerError> for ParseError<'src> {
    fn from(e: &LexerError) -> Self {
        ParseError::LexerError(e.clone())
    }
}

// --------------------------------------------------------------------------
//                          - Parser -
// --------------------------------------------------------------------------
#[derive(Debug)]
pub struct Parser<'src> {
    lexer: Peekable<Lexer<'src>>
}

impl<'src> Parser<'src> {
    pub fn new(src: &'src str) -> anyhow::Result<Parser> {
        Ok(Self {
            lexer: Lexer::new(src).peekable(),
        })
    }

    // --------------------------------------------------------------------------
    //                          - Helpers -
    // --------------------------------------------------------------------------
    fn match_next_token(&mut self, expected_kind: TokenKind<'src>) -> bool {
        let tok = self.lexer.peek().expect("Token for peeking");
        match tok {
            Err(_) => false,
            Ok(tok) => {
                if tok.kind == expected_kind {
                    return true;
                }
                false
            }
        }
    }

    fn expect(&mut self, expected_kind: TokenKind<'src>) -> Result<(), ParseError<'src>> {
        let tok = self.lexer.peek().expect("Token for peeking");
        if tok.as_ref()?.kind == expected_kind {
            self.lexer.next();
            return Ok(());
        } else {
            return Err(ParseError::InvaildTokenKind { expected: expected_kind, found: tok.as_ref()?.kind.clone() })
        }
    }

    // --------------------------------------------------------------------------
    //                          - Expr -
    // --------------------------------------------------------------------------
    fn parse_expr(&mut self) -> anyhow::Result<Expr<'src>, ParseError<'src>> {
        let tok = self.lexer.next().expect("expression")?;

        let expr = match tok.kind {
            TokenKind::Sym { sym: "!" } => match self.lexer.next() {
                Some(arg) => match arg?.kind {
                    TokenKind::Number { num, .. } => Ok(Expr::PushLeft {
                        expr: Box::new(Expr::Number { num }),
                    }),

                    TokenKind::String { text } => Ok(Expr::PushLeft {
                        expr: Box::new(Expr::String { text }),
                    }),

                    TokenKind::Boolean(value) => Ok(Expr::PushLeft {
                        expr: Box::new(Expr::Boolean(value)),
                    }),

                    TokenKind::Iden { iden } => Ok(Expr::PushLeft {
                        expr: Box::new(Expr::Iden { iden })
                    }),

                    TokenKind::Keyword { kw } => match &kw {
                        &"dup" | &"drop" | &"let" | &"print" | &"println" | &"if" | &"while" | &"eq" | &"inc" | &"dec" => {
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
                &"dup" | &"drop" | &"let" | &"print" | &"println" | &"if" | &"while" | &"eq" | &"inc" | &"dec" => {
                    self.expect(TokenKind::Sym { sym: "!" })?;
                    Ok(Expr::PushRight {
                        expr: Box::new(Expr::Op { op: kw }),
                    })
                }
                _ => unreachable!(),
            },

            TokenKind::Sym { sym } => match &sym {
                &"+" | &"-" | &"%" | &"<" | &">" => {
                    self.expect(TokenKind::Sym { sym: "!" })?;
                    Ok(Expr::PushRight{
                        expr: Box::new(Expr::Op { op: sym }),
                    })
                },
                _ => unreachable!("this: {:?}", tok),
            },

            TokenKind::Number { num, .. } => {
                // TODO: Provide more context, error like expecting a right op
                self.expect(TokenKind::Sym { sym: "!" })?;
                Ok(Expr::PushRight {
                    expr: Box::new(Expr::Number { num }),
                })
            },

            TokenKind::String { text } => {
                self.expect(TokenKind::Sym { sym: "!" })?;
                Ok(Expr::PushRight {
                    expr: Box::new(Expr::String { text }),
                })
            }

            TokenKind::Boolean(value) => {
                self.expect(TokenKind::Sym { sym: "!" })?;
                Ok(Expr::PushRight {
                    expr: Box::new(Expr::Boolean(value)),
                })
            },

            TokenKind::Iden { iden } => {
                self.expect(TokenKind::Sym { sym: "!" })?;
                Ok(Expr::PushLeft {
                    expr: Box::new(Expr::Iden { iden })
                })
            }
            _ => unreachable!("{:?}", tok),
        };

        expr
    }

    // --------------------------------------------------------------------------
    //                          - Body/Block -
    // --------------------------------------------------------------------------
    fn parse_block(&mut self) -> anyhow::Result<Stmt<'src>, ParseError<'src>> {
        self.expect(TokenKind::Sym { sym: "{" })?;
        let mut stmts: Vec<Stmt<'src>> = Vec::new();

        while let Some(tok) = self.lexer.peek() {
            if matches!(tok.as_ref()?.kind, TokenKind::Sym { sym: "}" }) {
                break;
            }
            stmts.push(self.parse_stmt()?);
        }

        self.expect(TokenKind::Sym { sym: "}" })?;
        Ok(Stmt::Block { stmts })
    }

    // --------------------------------------------------------------------------
    //                          - If -
    // --------------------------------------------------------------------------
    fn parse_if_block(&mut self, main: Expr<'src>) -> anyhow::Result<Stmt<'src>, ParseError<'src>> {
        let mut conditions: Vec<Expr<'src>> = Vec::new();

        while let Some(tok) = self.lexer.peek() {
            if matches!(tok.as_ref()?.kind, TokenKind::Sym { sym: "{" }) {
                break;
            }
            conditions.push(self.parse_expr()?);
        }

        if conditions.is_empty() {
            panic!("expected conditions in if stmt");
        }

        Ok(Stmt::If { main, conditions, body: Box::new(self.parse_block()?) })
    }

    fn parse_if_stmt(&mut self, main: Expr<'src>) -> anyhow::Result<Stmt<'src>, ParseError<'src>> {
        let master = self.parse_if_block(main.clone())?;
        let mut alternates: Vec<Stmt<'src>> = Vec::new();

        while self.match_next_token(TokenKind::Keyword { kw: "elif" }) {
            self.lexer.next();
            alternates.push(self.parse_if_block(main.clone())?);
        }

        if self.match_next_token(TokenKind::Keyword { kw: "else" }) {
            self.lexer.next();
            alternates.push(self.parse_block()?);
        }

        if alternates.is_empty() {
            return Ok(master);
        }
        Ok(Stmt::IfElse { master: Box::new(master), alternates })
    }

    fn parse_while_stmt(&mut self, main: Expr<'src>) -> anyhow::Result<Stmt<'src>, ParseError<'src>> {
        let mut conditions: Vec<Expr<'src>> = Vec::new();

        while let Some(tok) = self.lexer.peek() {
            if matches!(tok.as_ref()?.kind, TokenKind::Sym { sym: "{" }) {
                break;
            }
            conditions.push(self.parse_expr()?);
        }

        Ok(Stmt::While {
            main,
            conditions,
            body: Box::new(self.parse_block()?),
        })
    }

    fn parse_let_stmt(&mut self, main: Expr<'src>) -> anyhow::Result<Stmt<'src>, ParseError<'src>> {
        if let Some(tok) = self.lexer.next() {
            let tok = tok?;
            match tok.kind {
                TokenKind::Iden { iden } => return Ok(Stmt::Let {
                    main,
                    token: tok.clone(),
                    iden
                }),
                _ => panic!("expected a identifier"),
            }
        }

        panic!()
    }

    fn parse_stmt(&mut self) -> anyhow::Result<Stmt<'src>, ParseError<'src>> {
        if self.match_next_token(TokenKind::Sym { sym: "{" }) {
            return self.parse_block();
        }

        let stmt = self.parse_expr()?;
        match stmt {
            Expr::PushLeft { ref expr } | Expr::PushRight { ref expr } => {
                match **expr {
                    Expr::Op { op: "if" } => return self.parse_if_stmt(stmt),
                    Expr::Op { op: "while" } => return self.parse_while_stmt(stmt),
                    Expr::Op { op: "let" } => return self.parse_let_stmt(stmt),
                    _ => (),
                };
            }

            _ => (),
        };

        Ok(Stmt::Expr { expr: stmt })
    }

    pub fn parse(&mut self) -> anyhow::Result<Stmt<'src>, ParseError<'src>> {
        let mut stmts: Vec<Stmt> = Vec::new();

        while let Some(tok) = self.lexer.peek() {
            if tok.as_ref()?.kind == TokenKind::Eof {
                break;
            }
            stmts.push(self.parse_stmt()?);
        }

        Ok(Stmt::Program { stmts })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! program {
        ($a:expr )=> {
           Stmt::Program { stmts: $a }
       }
    }

    #[test]
    fn parse_miscellaneous_exprs() {
        let text: &str = "!+ !- !2 !- dup! !> <!";
        let mut parser = Parser::new(text).unwrap();
        assert_eq!(
            parser.parse(),
            Ok(program!(vec![
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
            ]))
        );
    }

    #[test]
    fn parse_if_stmt_with_no_branches() {
        let text: &str = "!if !1 { !1 !2 } !69";
        let mut parser = Parser::new(text).unwrap();
        assert_eq!(
            parser.parse(),
            Ok(program!(vec![
                Stmt::If {
                    main: Expr::PushLeft {
                        expr: Box::new(Expr::Op { op: "if" })
                    },
                    conditions: vec![
                         Expr::PushLeft {
                             expr: Box::new(Expr::Number { num: 1.0 })
                         }
                    ],
                    body: Box::new(Stmt::Block {
                        stmts: vec![
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
            ]))
        );
    }

    #[test]
    fn parse_if_elif_block() {
        let text: &str = "!if !1 { } elif !dup !> { } elif dup! { 69! } !eq";
        let mut parser = Parser::new(text).unwrap();
        assert_eq!(
            parser.parse(),
            Ok(program!(vec![
                Stmt::IfElse {
                    master: Box::new(Stmt::If {
                        main: Expr::PushLeft {
                            expr: Box::new(Expr::Op { op: "if" })
                        },
                        conditions: vec![
                            Expr::PushLeft {
                                expr: Box::new(Expr::Number { num: 1.0}),
                            },
                        ],
                        body: Box::new(Stmt::Block {
                            stmts: vec![]
                        })
                    }),
                    alternates: vec![
                        Stmt::If {
                            main: Expr::PushLeft {
                                expr: Box::new(Expr::Op { op: "if" })
                            },
                            conditions: vec![
                                Expr::PushLeft {
                                    expr: Box::new(Expr::Op { op: "dup" }),
                                },
                                Expr::PushLeft {
                                    expr: Box::new(Expr::Op { op: ">" }),
                                },
                            ],
                            body: Box::new(Stmt::Block { stmts: vec![] })
                        },
                        Stmt::If {
                            main: Expr::PushLeft {
                                expr: Box::new(Expr::Op { op: "if" })
                            },
                            conditions: vec![Expr::PushRight {
                                expr: Box::new(Expr::Op { op: "dup" }),
                            },],
                            body: Box::new(Stmt::Block {
                                stmts: vec![
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
                    expr: Expr::PushLeft {
                        expr: Box::new(Expr::Op { op: "eq" }),
                    },
                }
            ]))
        );
    }

    #[test]
    fn parse_if_else_block() {
        // NOTE(madflash) - This !eq is here only to test, if we are successfully eating the
        // while stmt and then moving to eat the next stmt nicely!
        let text: &str = "!if !1 { !2 } else { 2! 1! } !eq";
        let mut parser = Parser::new(text).unwrap();
        assert_eq!(
            parser.parse(),
            Ok(program!(vec![
                Stmt::IfElse {
                    master: Box::new(Stmt::If {
                        main: Expr::PushLeft {
                            expr: Box::new(Expr::Op { op: "if" })
                        },
                        conditions: vec![
                            Expr::PushLeft {
                                expr: Box::new(Expr::Number { num: 1.0 })
                            }
                        ],
                        body: Box::new(Stmt::Block {
                            stmts: vec![
                                Stmt::Expr {
                                    expr: Expr::PushLeft {
                                        expr: Box::new(Expr::Number { num: 2.0 })
                                    }
                                }
                            ]
                        })
                    }),
                    alternates: vec![
                        Stmt::Block {
                            stmts: vec![
                                Stmt::Expr {
                                    expr: Expr::PushRight {
                                        expr: Box::new(Expr::Number { num: 2.0 })
                                    }
                                },
                                Stmt::Expr {
                                    expr: Expr::PushRight {
                                        expr: Box::new(Expr::Number { num: 1.0 })
                                    }
                                }
                            ]
                        }
                    ]
                },
                Stmt::Expr {
                    expr: Expr::PushLeft {
                        expr: Box::new(Expr::Op { op: "eq" }),
                    },
                }
            ]))
        );
    }

    #[test]
    fn parse_while_block() {
        // NOTE(madflash) - This !eq is here only to test, if we are successfully eating the
        // while stmt and then moving to eat the next stmt nicely!
        let text: &str = "!while !dup { !1 } !eq";
        let mut parser = Parser::new(text).unwrap();

        assert_eq!(
            parser.parse(),
            Ok(program!(vec![Stmt::While {
                main: Expr::PushLeft {
                    expr: Box::new(Expr::Op { op: "while" }),
                },
                conditions: vec![Expr::PushLeft {
                    expr: Box::new(Expr::Op { op: "dup" }),
                },],
                body: Box::new(Stmt::Block {
                    stmts: vec![Stmt::Expr {
                        expr: Expr::PushLeft {
                            expr: Box::new(Expr::Number { num: 1.0 }),
                        },
                    }]
                })
            },
            // NOTE(madflash) - This !eq is here only to test, if we are successfully eating the
            // while stmt and then moving to eat the next stmt nicely!
            Stmt::Expr {
                expr: Expr::PushLeft {
                    expr: Box::new(Expr::Op { op: "eq" }),
                },
            }
            ]))
        );
    }
}
