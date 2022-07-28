use std::iter::Peekable;

use crate::lexer::*;
use crate::ast::*;

// --------------------------------------------------------------------------
//                          - ParseError -
// --------------------------------------------------------------------------
#[derive(thiserror::Error, Debug, PartialEq)]
pub enum ParseError {
    #[error("Lexer error")]
    LexerError(#[from] LexerError),

    #[error("Missing expression")]
    MissingExpr,

    #[error("Invaild token kind (expected: {expected:?}, found: {found:?}")]
    InvaildTokenKind {
        expected: TokenKind,
        found: TokenKind,
    },

    #[error("Invaild token kind (expected: {expected:?}, found: {found:?}")]
    InvaildTokenKind2 {
        expected: String,
        found: TokenKind,
    },

    #[error("Invaild stmt: {msg:?}")]
    InvaildStmt { msg: String }
}

impl From<&LexerError> for ParseError {
    fn from(e: &LexerError) -> Self {
        ParseError::LexerError(e.clone())
    }
}

// --------------------------------------------------------------------------
//                          - Parser -
// --------------------------------------------------------------------------
#[derive(Debug)]
pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>
}

impl<'a, 'src> Parser<'a> {
    pub fn new(src: &'a str) -> anyhow::Result<Parser> {
        Ok(Self {
            lexer: Lexer::new(src).peekable(),
        })
    }

    // --------------------------------------------------------------------------
    //                          - Helpers -
    // --------------------------------------------------------------------------
    fn match_next_token(&mut self, expected_kind: TokenKind) -> bool {
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

    fn expect(&mut self, expected_kind: TokenKind) -> Result<(), ParseError> {
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
    fn parse_expr(&mut self) -> anyhow::Result<Expr, ParseError> {
        let tok = self.lexer.next().expect("expression")?;

        let expr = match tok.kind {
            TokenKind::Punctuation { kind: PunctuationKind::Bang, .. } => match self.lexer.next() {
                Some(arg) => match arg?.kind {
                    TokenKind::Number { num, .. } => Ok(Expr::PushLeft {
                        expr: Box::new(Expr::Number { num }),
                    }),

                    TokenKind::String { text } => Ok(Expr::PushLeft {
                        expr: Box::new(Expr::String { text }),
                    }),

                    TokenKind::Bool(value) => Ok(Expr::PushLeft {
                        expr: Box::new(Expr::Bool(value)),
                    }),

                    TokenKind::Iden { iden } => {
                        if self.match_next_token(TokenKind::Punctuation { kind: PunctuationKind::LeftParen, ch: '(' }) {
                            self.lexer.next();
                            self.expect(TokenKind::Punctuation { kind: PunctuationKind::RightParen, ch: ')' })?;
                            return Ok(Expr::PushLeft {
                                expr: Box::new(Expr::Call { name: iden })
                            })
                        }
                        Ok(Expr::PushLeft {
                            expr: Box::new(Expr::Iden { iden })
                        })
                    }

                    TokenKind::Keyword { kind } => match &kind {
                        KwKind::Dup => Ok(Expr::PushLeft {
                                expr: Box::new(Expr::Op { kind: OpKind::Dup }),
                        }),
                        KwKind::_Drop => Ok(Expr::PushLeft {
                                expr: Box::new(Expr::Op { kind: OpKind::_Drop }),
                        }),
                        KwKind::_Eq => Ok(Expr::PushLeft {
                                expr: Box::new(Expr::Op { kind: OpKind::_Eq }),
                        }),
                        KwKind::Let => Ok(Expr::PushLeft {
                                expr: Box::new(Expr::Op { kind: OpKind::Let }),
                        }),
                        KwKind::Return => Ok(Expr::PushLeft {
                                expr: Box::new(Expr::Op { kind: OpKind::Return }),
                        }),
                        KwKind::If => Ok(Expr::PushLeft {
                                expr: Box::new(Expr::Op { kind: OpKind::If }),
                        }),
                        KwKind::While => Ok(Expr::PushLeft {
                                expr: Box::new(Expr::Op { kind: OpKind::While }),
                        }),
                        KwKind::Inc => Ok(Expr::PushLeft {
                                expr: Box::new(Expr::Op { kind: OpKind::Inc }),
                        }),
                        KwKind::Dec => Ok(Expr::PushLeft {
                                expr: Box::new(Expr::Op { kind: OpKind::Dec }),
                        }),
                        KwKind::Print => Ok(Expr::PushLeft {
                                expr: Box::new(Expr::Op { kind: OpKind::Print }),
                        }),
                        KwKind::Println => Ok(Expr::PushLeft {
                                expr: Box::new(Expr::Op { kind: OpKind::Println }),
                        }),
                        _ => unreachable!(),
                    },

                    TokenKind::Punctuation { kind, .. } => match &kind {
                        PunctuationKind::Plus => Ok(Expr::PushLeft {
                                expr: Box::new(Expr::Op { kind: OpKind::Plus }),
                        }),
                        PunctuationKind::Minus => Ok(Expr::PushLeft {
                                expr: Box::new(Expr::Op { kind: OpKind::Minus }),
                        }),
                        PunctuationKind::Mod => Ok(Expr::PushLeft {
                                expr: Box::new(Expr::Op { kind: OpKind::Mod }),
                        }),
                        PunctuationKind::GreaterThan => Ok(Expr::PushLeft {
                                expr: Box::new(Expr::Op { kind: OpKind::GreaterThan }),
                        }),
                        PunctuationKind::LessThan => Ok(Expr::PushLeft {
                                expr: Box::new(Expr::Op { kind: OpKind::LessThan }),
                        }),
                        _ => unreachable!(),
                    },

                    _ => unreachable!(),
                },

                None => Err(ParseError::MissingExpr),
            },

            TokenKind::Keyword { kind, .. } => {
                self.expect(TokenKind::Punctuation { kind: PunctuationKind::Bang, ch: '!' })?;
                let op = match &kind {
                    KwKind::Dup => Ok(Expr::PushRight {
                            expr: Box::new(Expr::Op { kind: OpKind::Dup }),
                    }),
                    KwKind::_Drop => Ok(Expr::PushRight {
                            expr: Box::new(Expr::Op { kind: OpKind::_Drop }),
                    }),
                    KwKind::_Eq => Ok(Expr::PushLeft {
                            expr: Box::new(Expr::Op { kind: OpKind::_Eq }),
                        }),
                    KwKind::Let => Ok(Expr::PushRight {
                            expr: Box::new(Expr::Op { kind: OpKind::Let }),
                    }),
                    KwKind::Return => Ok(Expr::PushRight {
                            expr: Box::new(Expr::Op { kind: OpKind::Return }),
                    }),
                    KwKind::If => Ok(Expr::PushRight {
                            expr: Box::new(Expr::Op { kind: OpKind::If }),
                    }),
                    KwKind::While => Ok(Expr::PushRight {
                            expr: Box::new(Expr::Op { kind: OpKind::While }),
                    }),
                    KwKind::Inc => Ok(Expr::PushRight {
                            expr: Box::new(Expr::Op { kind: OpKind::Inc }),
                    }),
                    KwKind::Dec => Ok(Expr::PushRight {
                            expr: Box::new(Expr::Op { kind: OpKind::Dec }),
                    }),
                    KwKind::Print => Ok(Expr::PushRight {
                            expr: Box::new(Expr::Op { kind: OpKind::Print }),
                    }),
                    KwKind::Println => Ok(Expr::PushRight {
                            expr: Box::new(Expr::Op { kind: OpKind::Println }),
                    }),
                    _ => unreachable!(),
                };
                op
            }

            TokenKind::Punctuation { kind, .. } => {
                self.expect(TokenKind::Punctuation { kind: PunctuationKind::Bang, ch: '!' })?;
                let op = match &kind {
                    PunctuationKind::Plus => Ok(Expr::PushRight {
                            expr: Box::new(Expr::Op { kind: OpKind::Plus }),
                    }),
                    PunctuationKind::Minus => Ok(Expr::PushRight {
                            expr: Box::new(Expr::Op { kind: OpKind::Minus }),
                    }),
                    PunctuationKind::Mod => Ok(Expr::PushRight {
                            expr: Box::new(Expr::Op { kind: OpKind::Mod }),
                    }),
                    PunctuationKind::GreaterThan => Ok(Expr::PushRight {
                            expr: Box::new(Expr::Op { kind: OpKind::GreaterThan }),
                    }),
                    PunctuationKind::LessThan => Ok(Expr::PushRight {
                            expr: Box::new(Expr::Op { kind: OpKind::LessThan }),
                    }),
                    _ => unreachable!(),
                };

                op
            }

            TokenKind::Number { num, .. } => {
                // TODO: Provide more context, error like expecting a right op
                self.expect(TokenKind::Punctuation { kind: PunctuationKind::Bang, ch: '!' })?;
                Ok(Expr::PushRight {
                    expr: Box::new(Expr::Number { num }),
                })
            },

            TokenKind::String { text } => {
                self.expect(TokenKind::Punctuation { kind: PunctuationKind::Bang, ch: '!' })?;
                Ok(Expr::PushRight {
                    expr: Box::new(Expr::String { text }),
                })
            }

            TokenKind::Bool(value) => {
                self.expect(TokenKind::Punctuation { kind: PunctuationKind::Bang, ch: '!' })?;
                Ok(Expr::PushRight {
                    expr: Box::new(Expr::Bool(value)),
                })
            },

            TokenKind::Iden { iden } => {
                if self.match_next_token(TokenKind::Punctuation { kind: PunctuationKind::LeftParen, ch: '(' }) {
                    self.lexer.next();
                    self.expect(TokenKind::Punctuation { kind: PunctuationKind::RightParen, ch: ')' })?;
                    self.expect(TokenKind::Punctuation { kind: PunctuationKind::Bang, ch: '!' })?;
                    return Ok(Expr::PushRight {
                        expr: Box::new(Expr::Call { name: iden })
                    })
                }

                self.expect(TokenKind::Punctuation { kind: PunctuationKind::Bang, ch: '!' })?;
                Ok(Expr::PushRight {
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
    fn parse_block(&mut self) -> anyhow::Result<Stmt, ParseError> {
        self.expect(TokenKind::Punctuation { kind: PunctuationKind::LeftCurly, ch: '{' })?;
        let mut stmts: Vec<Stmt> = Vec::new();

        while let Some(tok) = self.lexer.peek() {
            if matches!(tok.as_ref()?.kind, TokenKind::Punctuation { kind: PunctuationKind::RightCurly, ch: '}' }) {
                break;
            }
            stmts.push(self.parse_stmt()?);
        }

        self.expect(TokenKind::Punctuation { kind: PunctuationKind::RightCurly, ch: '}' })?;
        Ok(Stmt::Block { stmts })
    }

    // --------------------------------------------------------------------------
    //                          - If -
    // --------------------------------------------------------------------------
    fn parse_if_block(&mut self, main: Expr) -> anyhow::Result<Stmt, ParseError> {
        let mut conditions: Vec<Expr> = Vec::new();

        while let Some(tok) = self.lexer.peek() {
            if matches!(tok.as_ref()?.kind, TokenKind::Punctuation {  kind: PunctuationKind::LeftCurly, ch: '{'}) {
                break;
            }
            conditions.push(self.parse_expr()?);
        }

        if conditions.is_empty() {
            return Err(ParseError::InvaildStmt { msg: "missing condition in if stmt".to_owned() });
        }

        Ok(Stmt::If { main, conditions, body: Box::new(self.parse_block()?) })
    }

    fn parse_if_stmt(&mut self, main: Expr) -> anyhow::Result<Stmt, ParseError> {
        let master = self.parse_if_block(main.clone())?;
        let mut alternates: Vec<Stmt> = Vec::new();

        while self.match_next_token(TokenKind::Keyword { kind: KwKind::Elif }) {
            self.lexer.next();
            alternates.push(self.parse_if_block(main.clone())?);
        }

        if self.match_next_token(TokenKind::Keyword { kind: KwKind::Else}) {
            self.lexer.next();
            alternates.push(self.parse_block()?);
        }

        if alternates.is_empty() {
            return Ok(master);
        }
        Ok(Stmt::IfElse { master: Box::new(master), alternates })
    }

    fn parse_while_stmt(&mut self, main: Expr) -> anyhow::Result<Stmt, ParseError> {
        let mut conditions: Vec<Expr> = Vec::new();

        while let Some(tok) = self.lexer.peek() {
            if matches!(tok.as_ref()?.kind, TokenKind::Punctuation {  kind: PunctuationKind::LeftCurly, ch: '{'}) {
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

    fn parse_let_stmt(&mut self, main: Expr) -> anyhow::Result<Stmt, ParseError> {
        if let Some(tok) = self.lexer.next() {
            let tok = tok?;
            match tok.kind {
                TokenKind::Iden { ref iden } => return Ok(Stmt::Let {
                    name: iden.to_string(),
                    main,
                    token: tok.clone(),
                }),
                _ => return Err(ParseError::InvaildTokenKind2 { expected: "'variable name' after let".to_owned(), found: tok.kind.clone()})
            }
        }

        Err(ParseError::InvaildStmt { msg: "missing variable name after let stmt".to_owned() })
    }

    fn parse_fn_args(&mut self) -> anyhow::Result<Vec<String>, ParseError> {
        self.expect(TokenKind::Punctuation { kind: PunctuationKind::LeftParen, ch: '(' })?;
        let mut args: Vec<String> = Vec::new();

        if self.match_next_token(TokenKind::Punctuation { kind: PunctuationKind::LeftParen, ch: ')' }) {
            return Ok(args);
        }

        let tok = self.lexer.next().unwrap();
        let tok = tok?;
        match tok.kind {
            TokenKind::Iden { iden } => args.push(iden),
            _ => return Err(ParseError::InvaildTokenKind2 { expected: "function name".to_owned(), found: tok.kind.clone()})
        }

        while let Some(tok) = self.lexer.peek() {
            if matches!(tok.as_ref()?.kind, TokenKind::Punctuation { kind: PunctuationKind::RightParen, ch: ')' }) {
                break;
            } else if matches!(tok.as_ref()?.kind, TokenKind::Punctuation { kind: PunctuationKind::Comma, ch: ',' }) {
                self.lexer.next();
            }

            let __tok = self.lexer.next().unwrap();
            let __tok = __tok?;
            match __tok.kind {
                TokenKind::Iden { iden } => {
                    args.push(iden);
                },
                _ => return Err(ParseError::InvaildTokenKind2 { expected: "function parameter".to_owned(), found: __tok.kind.clone() })
            }
        }

        self.expect(TokenKind::Punctuation { kind: PunctuationKind::RightParen, ch: ')' })?;
        Ok(args)
    }

    fn parse_fn_decl(&mut self) -> anyhow::Result<Stmt, ParseError> {
        self.expect(TokenKind::Keyword { kind: KwKind::_Fn})?;
        if let Some(tok) = self.lexer.next() {
            let tok = tok?;
            match tok.kind {
                TokenKind::Iden { ref iden } => {
                    return Ok(Stmt::Fn {
                        name: iden.to_string(),
                        main: tok,
                        args: self.parse_fn_args()?,
                        body: Box::new(self.parse_block()?) })
                }
                _ => return Err(ParseError::InvaildStmt { msg: " function name".to_owned() }),
            }
        }
        Err(ParseError::InvaildStmt { msg: "Expected function name".to_owned() })
    }

    fn parse_stmt(&mut self) -> anyhow::Result<Stmt, ParseError> {
        if self.match_next_token(TokenKind::Punctuation { kind: PunctuationKind::LeftCurly, ch: '{' }) {
            return self.parse_block();
        } else if self.match_next_token(TokenKind::Keyword { kind: KwKind::_Fn}) {
            return self.parse_fn_decl();
        } else {
            let stmt = self.parse_expr()?;
            match stmt {
                Expr::PushLeft { ref expr } | Expr::PushRight { ref expr } => {
                    match **expr {
                        Expr::Op { kind: OpKind::If} => return self.parse_if_stmt(stmt),
                        Expr::Op { kind: OpKind::While } => return self.parse_while_stmt(stmt),
                        Expr::Op { kind: OpKind::Let} => return self.parse_let_stmt(stmt),
                        _ => (),
                    };
                }

                _ => (),
            };

            Ok(Stmt::Expr { expr: stmt })
        }
    }

    pub fn parse(&mut self) -> anyhow::Result<Stmt, ParseError> {
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
                        expr: Box::new(Expr::Op { kind: OpKind::Plus})
                    }
                },
                Stmt::Expr {
                    expr: Expr::PushLeft {
                        expr: Box::new(Expr::Op { kind: OpKind::Minus})
                    }
                },
                Stmt::Expr {
                    expr: Expr::PushLeft {
                        expr: Box::new(Expr::Number { num: 2.0 })
                    }
                },
                Stmt::Expr {
                    expr: Expr::PushLeft {
                        expr: Box::new(Expr::Op {  kind: OpKind::Minus})
                    }
                },
                Stmt::Expr {
                    expr: Expr::PushRight {
                        expr: Box::new(Expr::Op { kind: OpKind::Dup})
                    }
                },
                Stmt::Expr {
                    expr: Expr::PushLeft {
                        expr: Box::new(Expr::Op { kind: OpKind::GreaterThan })
                    }
                },
                Stmt::Expr {
                    expr: Expr::PushRight {
                        expr: Box::new(Expr::Op { kind: OpKind::LessThan })
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
                        expr: Box::new(Expr::Op { kind: OpKind::If})
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
                            expr: Box::new(Expr::Op { kind: OpKind::If})
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
                                expr: Box::new(Expr::Op { kind: OpKind::If})
                            },
                            conditions: vec![
                                Expr::PushLeft {
                                    expr: Box::new(Expr::Op { kind: OpKind::Dup})
                                },
                                Expr::PushLeft {
                                    expr: Box::new(Expr::Op { kind: OpKind::GreaterThan })
                                },
                            ],
                            body: Box::new(Stmt::Block { stmts: vec![] })
                        },
                        Stmt::If {
                            main: Expr::PushLeft {
                                expr: Box::new(Expr::Op { kind: OpKind::If})
                            },
                            conditions: vec![Expr::PushRight {
                                expr: Box::new(Expr::Op { kind: OpKind::Dup})
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
                        expr: Box::new(Expr::Op { kind: OpKind::_Eq})
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
                            expr: Box::new(Expr::Op { kind: OpKind::If})
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
                        expr: Box::new(Expr::Op { kind: OpKind::_Eq})
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
                    expr: Box::new(Expr::Op { kind: OpKind::While}),
                },
                conditions: vec![Expr::PushLeft {
                    expr: Box::new(Expr::Op { kind: OpKind::Dup})
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
                    expr: Box::new(Expr::Op { kind: OpKind::_Eq})
                },
            }
            ]))
        );
    }
}
