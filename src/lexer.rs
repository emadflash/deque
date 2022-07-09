use std::fmt;
use std::iter::Peekable;
use std::str::CharIndices;
use thiserror::Error;

#[derive(Error, Debug, PartialEq)]
pub(crate) enum LexerError {
    #[error("missing ending quotes")]
    MissingEndOfStringQuote,
    #[error("unknown character error: {ch:?}")]
    UnknowCharacter { ch: char },
}

// --------------------------------------------------------------------------
//                          - TokenKind -
// --------------------------------------------------------------------------
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum TokenKind<'src> {
    Sym { sym: &'src str },
    Iden { iden: &'src str },
    Keyword { kw: &'src str },
    Number { text: &'src str, num: f32 },
    String { text: &'src str },
    Eof,
}

// --------------------------------------------------------------------------
//                          - Token -
// --------------------------------------------------------------------------
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Token<'src> {
    pub(crate) kind: TokenKind<'src>,
    pub(crate) pos: (usize, usize),
}

impl<'src> Token<'src> {
    fn new(kind: TokenKind<'src>, pos: (usize, usize)) -> Self {
        Self { kind, pos }
    }
}

impl<'src> TokenKind<'src> {
    fn to_token(self, pos: (usize, usize)) -> Token<'src> {
        Token::new(self, pos)
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

// --------------------------------------------------------------------------
//                          - Lexer -
// --------------------------------------------------------------------------
#[derive(Debug)]
pub(crate) struct Lexer<'src> {
    row: usize,
    col: usize,
    is_finished: bool,
    src: &'src str,
    chars: Peekable<CharIndices<'src>>
}

impl<'src> Lexer<'src> {
    pub(crate) fn new(src: &'src str) -> Self {
        let chars = src.char_indices().peekable();
        Self {
            row: 0,
            col: 0,
            is_finished: false,
            src,
            chars
        }
    }

    pub(crate) fn next_token(&mut self) -> Result<Token<'src>, LexerError> {
        assert!(!self.is_finished, "Lexer has already finished!");

        while let Some((index, ch)) = self.chars.next() {
            match ch {
                // --------------------------------------------------------------------------
                //                          - Whitespace -
                // --------------------------------------------------------------------------
                ' ' => {
                    while let Some((_, w)) = self.chars.peek() {
                        if !w.is_whitespace() {
                            break;
                        }

                        self.col += 1;
                        self.chars.next();
                    }
                }

                // --------------------------------------------------------------------------
                //                          - Comments -
                // --------------------------------------------------------------------------
                '-' => {
                    if matches!(self.chars.peek(), Some(&(_, '-'))) {
                        while let Some((_, w)) = self.chars.peek() {
                            if w == &'\n' {
                                break;
                            }

                            self.col += 1;
                            self.chars.next();
                        }
                    } else {
                        // --------------------------------------------------------------------------
                        //                          - Minus -
                        // --------------------------------------------------------------------------
                        let tok = TokenKind::Sym {sym: &self.src[index..index + 1]}.to_token((self.row, self.col));
                        self.col += 1;
                        return Ok(tok);
                    }
                }

                // --------------------------------------------------------------------------
                //                          - Change line -
                // --------------------------------------------------------------------------
                '\n' => {
                    self.col = 0;
                    self.row += 1;
                }

                // --------------------------------------------------------------------------
                //                          - String -
                // --------------------------------------------------------------------------
                '"' => {
                    let mut success = false;
                    let index = index + 1;
                    let mut end = index;

                    while let Some((_, a)) = self.chars.peek() {
                        match a {
                            '"' => {
                                success = true;
                                self.chars.next();
                                break;
                            }

                            _ => {
                                end += 1;
                                self.chars.next();
                            }
                        };
                    }

                    if !success {
                        return Err(LexerError::MissingEndOfStringQuote);
                    }

                    let tok = TokenKind::String { text: &self.src[index..end] }.to_token((self.row, self.col));
                    self.col += end - index + 2;
                    return Ok(tok);
                }

                // --------------------------------------------------------------------------
                //                          - Number -
                // --------------------------------------------------------------------------
                '0'..='9' => {
                    let mut end = index;

                    while let Some((_, a)) = self.chars.peek() {
                        match a {
                            '0'..='9' => {
                                end += 1;
                                self.chars.next();
                            }

                            _ => break,
                        };
                    }

                    let text = &self.src[index..=end];
                    let num = text.parse::<f32>().unwrap();

                    let tok = TokenKind::Number { text, num }.to_token((self.row, self.col));
                    self.col += end - index + 1;
                    return Ok(tok);
                }

                // --------------------------------------------------------------------------
                //                          - Sym -
                // --------------------------------------------------------------------------
                '!' | ':' | '{' | '}' | '+' | '%' | '>' | '<' => {
                    let tok = TokenKind::Sym {
                        sym: &self.src[index..index + 1],
                    }
                    .to_token((self.row, self.col));
                    self.col += 1;
                    return Ok(tok);
                }

                // --------------------------------------------------------------------------
                //                          - Iden or Keyword -
                // --------------------------------------------------------------------------
                'a'..='z' => {
                    let mut end = index;

                    while let Some((_, a)) = self.chars.peek() {
                        match a {
                            'a'..='z' | '0'..='9' => {
                                end += 1;
                                self.chars.next();
                            }

                            _ => break,
                        };
                    }

                    let text = &self.src[index..=end];
                    let tok = match text {
                        "dup" | "pud" | "drop" | "print" | "println" | "if" | "elif" | "else" | "while"
                        | "eq" => TokenKind::Keyword { kw: text }.to_token((self.row, self.col)),
                        _ => TokenKind::Iden { iden: text }.to_token((self.row, self.col))
                    };

                    self.col += end - index;
                    return Ok(tok);
                }

                _ => return Err(LexerError::UnknowCharacter { ch }),
            };
            self.col += 1;
        }

        self.is_finished = true;
        Ok(TokenKind::Eof.to_token((self.row, self.col)))
    }
}

impl<'src> Iterator for Lexer<'src> {
    type Item = Result<Token<'src>, LexerError>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.is_finished {
            None
        } else {
            Some(self.next_token())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lex_miscellaneous() {
        let lexer = Lexer::new("!+ !- !2 >!");
        let tokens = lexer.map(|tok| tok.unwrap()).collect::<Vec<_>>();
        assert_eq!(
            tokens,
            vec![
                Token::new(TokenKind::Sym { sym: "!" }, (0, 0)),
                Token::new(TokenKind::Sym { sym: "+" }, (0, 1)),
                Token::new(TokenKind::Sym { sym: "!" }, (0, 3)),
                Token::new(TokenKind::Sym { sym: "-" }, (0, 4)),
                Token::new(TokenKind::Sym { sym: "!" }, (0, 6)),
                Token::new(TokenKind::Number { text: "2", num: 2.0 }, (0, 7)),
                Token::new(TokenKind::Sym { sym: ">" }, (0, 9)),
                Token::new(TokenKind::Sym { sym: "!" }, (0, 10)),
                Token::new(TokenKind::Eof, (0, 11))
            ]
        );
    }

    #[test]
    fn lex_strings() {
        let lexer = Lexer::new("\"string 1\"   \"string 2\" 69");
        let tokens = lexer.map(|tok| tok.unwrap()).collect::<Vec<_>>();
        assert_eq!(
            tokens,
            vec![
                Token::new(TokenKind::String { text: "string 1" }, (0, 0)),
                Token::new(TokenKind::String { text: "string 2" }, (0, 13)),
                Token::new(TokenKind::Number { text: "69", num: 69.0 }, (0, 24)),
                Token::new(TokenKind::Eof, (0, 26))
            ]
        );
    }
}
