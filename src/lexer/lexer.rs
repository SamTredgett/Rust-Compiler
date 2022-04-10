extern crate thiserror;

use std::io;

use thiserror::Error;

#[derive(Error, Debug)]
pub enum LexerError {
    #[error("IO Error")]
    FileIO(#[from] io::Error),

    #[error("Was expecting {expected:?}, found {found:?}")]
    MissingExpectedSymbol { expected: TokenType, found: Token },

    #[error("Can't create numerical literal due to invalid character {raw:?}")]
    NumericaLiteralInvalidChar { raw: String },
    #[error("Can't find opening symbol ({open:?}) for {symbol:?}")]
    MisbalancedSymbol { symbol: char, open: char },

    #[error("Found unknown symbol {symbol:?}")]
    UnknownSymbol { symbol: String },
}

pub type Token = TokenType;

pub struct Punctuation {
    pub raw: char,
    pub kind: PunctuationKind,
}
#[derive(Debug)]
pub enum NumericHint {
    Integer,
    FloatingPoint,
}

#[derive(Debug)]
pub enum TokenType {
    /* End of Token Stream */
    EOF,

    /** Punctuation like , ( [  */
    Punctuation {
        raw: char,
        kind: PunctuationKind,
    },

    /**  Operators are 'actions' that you take on an entity i.e. '*', '->' */
    Operators(String),

    /** A sequence of characters  */
    Identifier(String),

    /** A single character('a') => unicode codepoint (32 bits). */
    Char(char),

    /** a number 0-9 */
    Numeric {
        raw: String,
        hint: NumericHint 
    },
    

    /** For errors */
    Unknown(char),
}
#[derive(Debug)]
pub enum PunctuationKind {
    /** '(' and '[' */
    Open(BalancingDepthType),

    /** ')' and ']' */
    Close(BalancingDepthType),

    /** ',' and ';' */
    Seperator,
}

type BalancingDepthType = i32;

pub struct Lexer<'a> {
    /**Human readable formats */
    pub line: usize,
    pub cur_col: usize,

    /** 'raw' format / offset within the file (in terms of 'codepoints') */
    pub codepoint_offset: usize,

    chars: std::iter::Peekable<std::str::Chars<'a>>,
    balancing_state: std::collections::HashMap<char, BalancingDepthType>,
}

impl<'a> Lexer<'a> {
    pub fn new(chars: &'a str) -> Lexer<'a> {
        Lexer {
            cur_col: 1,
            line: 1,

            codepoint_offset: 0,
            chars: chars.chars().peekable(),
            balancing_state: std::collections::HashMap::new(),
        }
    }
    fn map_balance(c: &char) -> char {
        match c {
            '}' => '{',
            '{' => '}',
            ']' => '[',
            '[' => ']',
            ')' => '(',
            '(' => ')',
            _ => panic!(
                "AHHHHH don't map a balancing character that isn't a balancing character....:("
            ),
        }
    }

    fn push_symbol(&mut self, c: &char) -> BalancingDepthType {
        if let Some(v) = self.balancing_state.get_mut(&c) {
            *v += 1;
            *v - 1
        } else {
            self.balancing_state.insert(*c, 1);
            0
        }
    }

    fn pop_symbol(&mut self, c: &char) -> Result<BalancingDepthType, LexerError> {
        if let Some(v) = self.balancing_state.get_mut(&Lexer::map_balance(&c)) {
            if *v >= 1 {
                *v -= 1;
                Ok(*v)
            } else {
                Err(LexerError::MisbalancedSymbol {
                    symbol: *c,
                    open: Lexer::map_balance(&c),
                })
            }
        } else {
            Err(LexerError::MisbalancedSymbol {
                symbol: *c,
                open: Lexer::map_balance(&c),
            })
        }
    }

    fn consume_digit(&mut self, raw: &String, radix: u32) -> Result<char, LexerError> {
        match self.chars.peek() {
            // Ugly solution
            None => Err(LexerError::NumericaLiteralInvalidChar { raw: raw.to_string() }),
            Some(c) if !c.is_digit(radix) => {
                Err(LexerError::NumericaLiteralInvalidChar { raw: raw.to_string() })
            },
            Some(c) => Ok(*c)
        }
    }

    // Handling seeing numbers
    fn parse_number(&mut self, start: char) -> Result<TokenType, LexerError> {
        let mut seen_dot = false;
        let mut seen_exp = false;
        let mut num = start.to_string();
        let radix = 10;

        if start == '.' {
            num.push(self.consume_digit(&num, radix)?);
            seen_dot = true;
        }

        loop {
            match self.chars.peek() {
                Some(c) if *c == '.' && !seen_dot => {
                    num.push(*c);
                    self.consume_char();
                    seen_dot = true;
                }
                Some(c) if *c == 'e' || *c == 'E' => {
                    num.push(*c);
                    self.consume_char();
                    seen_exp = true;

                    let exp_radix = 10;
                    match self.chars.peek() {
                        Some(c) if *c == '+' || *c == '-' => {
                            num.push(*c);
                            self.consume_char();
                        }
                        _ => {}
                    }
                    match self.chars.peek() {
                        // Ugly solution
                        None => break Err(LexerError::NumericaLiteralInvalidChar { raw: num }),
                        Some(c) if !c.is_digit(exp_radix) => {
                            break Err(LexerError::NumericaLiteralInvalidChar { raw: num })
                        }
                        _ => {}
                    }

                    num.push(self.consume_digit(&num, exp_radix)?);
                }
                Some(c) if c.is_digit(radix) => {
                    num.push(*c);
                    self.consume_char();
                }
                Some(c) if c.is_ascii_alphabetic() || c.is_digit(10) => {
                    // is_digit(10) is future proofing for when radix is !== 10
                    num.push(*c); // Pushing for error
                    return Err(LexerError::NumericaLiteralInvalidChar { raw: num });
                }
                _ => {
                    // Exit condition
                    break Ok(TokenType::Numeric { raw: num, hint: if seen_dot || seen_exp {  NumericHint::FloatingPoint} else { NumericHint::Integer } });
                }
            }
        }
    }

    fn transform_to_type(&mut self, c: char) -> Result<TokenType, LexerError> {
        match c {
            '(' | '[' | '{' => Ok(TokenType::Punctuation {
                raw: c,
                kind: PunctuationKind::Open(self.push_symbol(&c)),
            }),
            ')' | ']' | '}' => Ok(TokenType::Punctuation {
                raw: c,
                kind: PunctuationKind::Close(self.pop_symbol(&c)?),
            }),
            '0'..='9' | '.' => self.parse_number(c),
            _ => Err(LexerError::UnknownSymbol {
                symbol: c.to_string(),
            }),
        }
    }
    // Peeking characters in string
    fn consume_char(&mut self) -> Option<char> {
        match self.chars.next() {
            Some(c) => {
                self.cur_col += 1;
                if c == '\n' {
                    self.cur_col += 1;
                    self.line = 1;
                }
                self.codepoint_offset += 1;

                Some(c)
            }
            None => None,
        }
    }

    // ignore whitespace
    fn skip_whitespace(&mut self) {
        while self.chars.next_if(|c| c.is_whitespace()).is_some() {}
        while let Some(c) = self.chars.peek() {
            if !c.is_whitespace() {
                break;
            }
            self.consume_char();
        }
    }

    pub fn next_token(&mut self) -> Result<TokenType, LexerError> {
        self.skip_whitespace();

        if let Some(c) = self.consume_char() {
            self.transform_to_type(c)
        } else {
            Ok(TokenType::EOF)
        }
    }
}
