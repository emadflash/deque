use std::fmt;
use thiserror::Error;

#[derive(Error, Debug, PartialEq)]
pub(crate) enum LexerError {
    #[error("missing ending quotes")]
    MissingEndOfStringQuote,
    #[error("unknown character error")]
    UnknowCharacter(char),
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum TokenKind<'src> {
    Sym { sym: &'src str },
    Iden { iden: &'src str },
    Keyword { kw: &'src str },
    Number { text: &'src str, num: f32 },
    String { text: &'src str },
    Eof,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Token<'src> {
    pub(crate) kind: TokenKind<'src>,
    pub(crate) pos: (usize, usize),
}

impl<'src> Token<'src> {
    fn new(kind: TokenKind<'src>, pos: (usize, usize)) -> Self {
        Self { kind, pos }
    }

    pub(crate) fn to_string(&self) -> String {
        self.kind.to_string()
    }
}

impl<'src> TokenKind<'src> {
    fn to_token(self, pos: (usize, usize)) -> Token<'src> {
        Token::new(self, pos)
    }

    fn to_string(&self) -> String {
        match self {
            TokenKind::Sym { sym } => sym.to_string(),
            TokenKind::Iden { iden } => iden.to_string(),
            TokenKind::Keyword { kw } => kw.to_string(),
            TokenKind::Number { text, .. } => text.to_string(),
            TokenKind::String { text } => text.to_string(),
            TokenKind::Eof => "EOF".to_string(),
        }
    }
}

impl<'src> fmt::Display for TokenKind<'src> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TokenKind::Sym { sym } => write!(f, "TokenKind::Sym({})", sym),
            TokenKind::Iden { iden } => write!(f, "TokenKind::Iden({})", iden),
            TokenKind::Keyword { kw } => write!(f, "TokenKind::Keyword({})", kw),
            TokenKind::Number { text, .. } => write!(f, "TokenKind::Number({})", text),
            TokenKind::String { text } => write!(f, "TokenKind::String({})", text),
            TokenKind::Eof => write!(f, "TokenKind::Eof"),
        }
    }
}

pub(crate) fn lex<'src>(s: &'src str) -> anyhow::Result<Vec<Token<'src>>, LexerError> {
    let mut row: usize = 0;
    let mut col: usize = 0;
    let mut tokens: Vec<Token<'src>> = Vec::new();
    let mut it = s.char_indices().peekable();

    while let Some((index, ch)) = it.next() {
        match ch {
            // --------------------------------------------------------------------------
            //                          - Whitespace -
            // --------------------------------------------------------------------------
            ' ' => {
                while let Some((_, w)) = it.peek() {
                    if !w.is_whitespace() {
                        break;
                    }

                    col += 1;
                    it.next();
                }
            }

            // --------------------------------------------------------------------------
            //                          - Change line -
            // --------------------------------------------------------------------------
            '\n' => {
                col = 0;
                row += 1;
            }

            // --------------------------------------------------------------------------
            //                          - String -
            // --------------------------------------------------------------------------
            '"' => {
                let mut success = false;
                let index = index + 1;
                let mut end = index;

                while let Some((_, a)) = it.peek() {
                    match a {
                        '"' => {
                            success = true;
                            it.next();
                            break;
                        }

                        _ => {
                            end += 1;
                            it.next();
                        }
                    };
                }

                if !success {
                    return Err(LexerError::MissingEndOfStringQuote);
                }

                tokens.push(
                    TokenKind::String {
                        text: &s[index..end],
                    }
                    .to_token((row, col)),
                );
                col += end - index + 3;
            }

            // --------------------------------------------------------------------------
            //                          - Number -
            // --------------------------------------------------------------------------
            '0'..='9' => {
                let mut end = index;

                while let Some((_, a)) = it.peek() {
                    match a {
                        '0'..='9' => {
                            end += 1;
                            it.next();
                        }

                        _ => break,
                    };
                }

                let text = &s[index..=end];
                let num = text.parse::<f32>().unwrap();
                tokens.push(TokenKind::Number { text, num }.to_token((row, col)));
                col += end - index;
            }

            // --------------------------------------------------------------------------
            //                          - Sym -
            // --------------------------------------------------------------------------
            '!' | ':' => {
                tokens.push(
                    TokenKind::Sym {
                        sym: &s[index..index + 1],
                    }
                    .to_token((row, col)),
                );
            }

            // --------------------------------------------------------------------------
            //                          - Iden or Keyword -
            // --------------------------------------------------------------------------
            'a'..='z' => {
                let mut end = index;

                while let Some((_, a)) = it.peek() {
                    match a {
                        'a'..='z' | '0'..='9' => {
                            end += 1;
                            it.next();
                        }

                        _ => break,
                    };
                }

                let text = &s[index..=end];
                match text {
                    "add" | "sub" | "dup" | "print" => {
                        tokens.push(TokenKind::Keyword { kw: text }.to_token((row, col)))
                    }
                    _ => tokens.push(TokenKind::Iden { iden: text }.to_token((row, col))),
                };

                col += end - index;
            }

            _ => return Err(LexerError::UnknowCharacter(ch)),
        };

        col += 1;
    }

    tokens.push(TokenKind::Eof.to_token((row, col)));
    Ok(tokens)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lex_() {
        let text: &str = "!add !sub !2";
        assert_eq!(
            lex(&text),
            Ok(vec![
                Token::new(TokenKind::Sym { sym: "!" }, (0, 0)),
                Token::new(TokenKind::Keyword { kw: "add" }, (0, 1)),
                Token::new(TokenKind::Sym { sym: "!" }, (0, 5)),
                Token::new(TokenKind::Keyword { kw: "sub" }, (0, 6)),
                Token::new(TokenKind::Sym { sym: "!" }, (0, 10)),
                Token::new(
                    TokenKind::Number {
                        text: "2",
                        num: 2.0
                    },
                    (0, 11)
                ),
                Token::new(TokenKind::Eof, (0, 12))
            ])
        );
    }
    #[test]
    fn lex_strings() {
        let text: &str = "\"string 1\"   \"string 2\"";
        assert_eq!(
            lex(&text),
            Ok(vec![
                Token::new(TokenKind::String { text: "string 1" }, (0, 0)),
                Token::new(TokenKind::String { text: "string 2" }, (0, 15)),
                Token::new(TokenKind::Eof, (0, 27))
            ])
        );
    }
}
