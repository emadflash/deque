use std::fmt;
use std::iter::Peekable;
use std::str::CharIndices;

use ansi_term::Color;

#[derive(thiserror::Error, Debug, PartialEq, Clone)]
pub enum LexerError {
    #[error("missing ending quotes")]
    MissingEndOfStringQuote,
    #[error("unknown character error: {ch:?}")]
    UnknowCharacter { ch: char },
}

// --------------------------------------------------------------------------
//                          - TokenKind -
// --------------------------------------------------------------------------
#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind<'src> {
    Sym { sym: &'src str },
    Iden { iden: &'src str },
    Keyword { kw: &'src str },
    Number { text: &'src str, num: f32 },
    String { text: &'src str },
    Boolean(bool),
    Eof,
}

// --------------------------------------------------------------------------
//                          - Token -
// --------------------------------------------------------------------------
#[derive(Debug, Clone, PartialEq)]
pub struct Token<'src> {
    pub kind: TokenKind<'src>,
    pub pos: (usize, usize),
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
            TokenKind::Sym { sym } => write!(f, "Symbol({})", sym),
            TokenKind::Iden { iden } => write!(f, "Identifier({})", iden),
            TokenKind::Keyword { kw } => write!(f, "Keyword({})", kw),
            TokenKind::Number { text, .. } => write!(f, "Number({})", text),
            TokenKind::String { text } => write!(f, "String({})", text),
            TokenKind::Boolean(val) => write!(f, "Bool({})", val),
            TokenKind::Eof => write!(f, "END_OF_FILE"),
        }
    }
}

impl<'src> fmt::Display for Token<'src> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {}, {}: {:?}", Color::Cyan.bold().paint("KIND"), self.kind,
            Color::Cyan.bold().paint("POS"), self.pos)
    }
}

// --------------------------------------------------------------------------
//                          - Lexer -
// --------------------------------------------------------------------------
#[derive(Debug)]
pub struct Lexer<'src> {
    row: usize,
    col: usize,
    is_finished: bool,
    src: &'src str,
    chars: Peekable<CharIndices<'src>>
}

impl<'src> Lexer<'src> {
    pub fn new(src: &'src str) -> Self {
        let chars = src.char_indices().peekable();
        Self {
            row: 0,
            col: 0,
            is_finished: false,
            src,
            chars
        }
    }

    pub fn next_token(&mut self) -> Result<Token<'src>, LexerError> {
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
                            '\n' => break,
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
                        "dup" | "drop" | "let" | "print" | "println" | "if" | "elif" | "else" | "while"
                        | "eq" | "inc" | "dec" => TokenKind::Keyword { kw: text }.to_token((self.row, self.col)),

                        "true" => TokenKind::Boolean(true).to_token((self.row, self.col)),
                        "false" => TokenKind::Boolean(false).to_token((self.row, self.col)),
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

// --------------------------------------------------------------------------
//                          - Print Lexical analysis -
// --------------------------------------------------------------------------
pub fn print_lexical_analysis<'src>(src: &'src str) {
    let lexer = Lexer::new(src);
    lexer
        .collect::<Vec<_>>()
        .iter()
        .for_each(|tok| {
            match tok {
                Err(e) => println!("{} {}", Color::Red.bold().underline().paint("ERROR:"), e),
                Ok(tok) => println!("{}", tok),
            }
        });
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
