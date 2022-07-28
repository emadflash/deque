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
//                          - KwKind -
// --------------------------------------------------------------------------
#[derive(Debug, Clone, PartialEq)]
pub enum KwKind {
    _Drop,
    Dup,
    Let,
    _Fn,
    Return,
    Print,
    Println,
    If,
    Elif,
    Else,
    While,
    _Eq,
    Inc,
    Dec
}

// --------------------------------------------------------------------------
//                          - Punctuations -
// --------------------------------------------------------------------------
#[derive(Debug, Clone, PartialEq)]
pub enum PunctuationKind {
    Bang,
    Colon,
    Comma,
    LeftParen,
    RightParen,
    LeftCurly,
    RightCurly,

    // Operators
    Plus,
    Minus,
    Mod,
    GreaterThan,
    LessThan,
}

// --------------------------------------------------------------------------
//                          - TokenKind -
// --------------------------------------------------------------------------
#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Number { num: f32 },
    String { text: String },
    Bool(bool),
    Iden { iden: String },
    Keyword { kind: KwKind },
    Punctuation { ch: char, kind: PunctuationKind},
    Eof,
}

// --------------------------------------------------------------------------
//                          - Token -
// --------------------------------------------------------------------------
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub pos: (usize, usize),
}

impl Token {
    fn new(kind: TokenKind, pos: (usize, usize)) -> Self {
        Self { kind, pos }
    }
}

impl TokenKind {
    fn to_token(self, pos: (usize, usize)) -> Token {
        Token::new(self, pos)
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {:?}, {}: {:?}", Color::Cyan.bold().paint("KIND"), self.kind,
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
                
    fn mk_punc(&mut self, ch: char, kind: PunctuationKind) -> Result<Token, LexerError> {
        let tok = TokenKind::Punctuation {
            ch,
            kind
        }
        .to_token((self.row, self.col));
        self.col += 1;
        return Ok(tok);
    }

    pub fn next_token(&mut self) -> Result<Token, LexerError> {
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
                        return self.mk_punc(ch, PunctuationKind::Minus);
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

                    let tok = TokenKind::String { text: self.src[index..end].to_string() }.to_token((self.row, self.col));
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

                    let tok = TokenKind::Number { num }.to_token((self.row, self.col));
                    self.col += end - index + 1;
                    return Ok(tok);
                }

                // --------------------------------------------------------------------------
                //                          - Punctuation -
                // --------------------------------------------------------------------------
                '!' => return self.mk_punc(ch, PunctuationKind::Bang),
                ',' => return self.mk_punc(ch, PunctuationKind::Comma),
                ':' => return self.mk_punc(ch, PunctuationKind::Colon),
                '{' => return self.mk_punc(ch, PunctuationKind::LeftCurly),
                '}' => return self.mk_punc(ch, PunctuationKind::RightCurly),
                '(' => return self.mk_punc(ch, PunctuationKind::LeftParen),
                ')' => return self.mk_punc(ch, PunctuationKind::RightParen),
                '+' => return self.mk_punc(ch, PunctuationKind::Plus),
                '%' => return self.mk_punc(ch, PunctuationKind::Mod),
                '>' => return self.mk_punc(ch, PunctuationKind::GreaterThan),
                '<' => return self.mk_punc(ch, PunctuationKind::LessThan),

                // --------------------------------------------------------------------------
                //                          - Iden or Keyword -
                // --------------------------------------------------------------------------
                'a'..='z' | '_' => {
                    let mut end = index;

                    while let Some((_, a)) = self.chars.peek() {
                        match a {
                            'a'..='z' | '_' | '0'..='9' => {
                                end += 1;
                                self.chars.next();
                            }

                            _ => break,
                        };
                    }

                    let text = &self.src[index..=end];
                    let tok = match text {
                        "dup" => TokenKind::Keyword { kind: KwKind::Dup },
                        "drop" =>TokenKind::Keyword { kind: KwKind::_Drop }, 
                        "let" =>TokenKind::Keyword { kind: KwKind::Let }, 
                        "fn" =>TokenKind::Keyword { kind: KwKind::_Fn }, 
                        "return" =>TokenKind::Keyword { kind: KwKind::Return }, 
                        "print" =>TokenKind::Keyword { kind: KwKind::Print }, 
                        "println" =>TokenKind::Keyword { kind: KwKind::Println }, 
                        "if" =>TokenKind::Keyword { kind: KwKind::If }, 
                        "elif" =>TokenKind::Keyword { kind: KwKind::Elif }, 
                        "else" =>TokenKind::Keyword { kind: KwKind::Else }, 
                        "while" =>TokenKind::Keyword { kind: KwKind::While }, 
                        "eq" =>TokenKind::Keyword { kind: KwKind::_Eq }, 
                        "inc" =>TokenKind::Keyword { kind: KwKind::Inc}, 
                        "dec" =>TokenKind::Keyword { kind: KwKind::Dec }, 

                        "true" => TokenKind::Bool(true),
                        "false" => TokenKind::Bool(false),
                        _ => TokenKind::Iden { iden: text.to_string() }
                    };

                    self.col += end - index;
                    return Ok(tok.to_token((self.row, self.col)));
                }

                _ => return Err(LexerError::UnknowCharacter { ch }),
            };
            self.col += 1;
        };

        self.is_finished = true;
        Ok(TokenKind::Eof.to_token((self.row, self.col)))
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token, LexerError>;

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
                Token::new(TokenKind::Punctuation { ch: '!', kind: PunctuationKind::Bang }, (0, 0)),
                Token::new(TokenKind::Punctuation { ch: '+', kind: PunctuationKind::Plus }, (0, 1)),
                Token::new(TokenKind::Punctuation { ch: '!', kind: PunctuationKind::Bang }, (0, 3)),
                Token::new(TokenKind::Punctuation { ch: '-', kind: PunctuationKind::Minus }, (0, 4)),
                Token::new(TokenKind::Punctuation { ch: '!', kind: PunctuationKind::Bang }, (0, 6)),
                Token::new(TokenKind::Number { num: 2.0 }, (0, 7)),
                Token::new(TokenKind::Punctuation { ch: '>', kind: PunctuationKind::GreaterThan }, (0, 9)),
                Token::new(TokenKind::Punctuation { ch: '!', kind: PunctuationKind::Bang }, (0, 10)),
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
                Token::new(TokenKind::String { text: "string 1".to_string() }, (0, 0)),
                Token::new(TokenKind::String { text: "string 2".to_string() }, (0, 13)),
                Token::new(TokenKind::Number { num: 69.0 }, (0, 24)),
                Token::new(TokenKind::Eof, (0, 26))
            ]
        );
    }
}
