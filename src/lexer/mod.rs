// MIT License
// Copyright (c) 2018 Victor Bankowski
// Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
// The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
use std::char::from_u32;

use common::{State, run_machine};
use common::{Source, Sink};
use common::Direction;

use self::{TokenType::*, Operator::*};

/// Tokens are lexical units of code such as identifiers, keywords, operators etc.
pub struct Token {
    /// token_type is the type of the token such as an identifier, keyword, operator etc.
    token_type: TokenType,

    ///Position is the position of the token in the source code.
    position: Position,
}

impl Token {
    fn new(token_type: TokenType, position: Position) -> Self {
        Token {
            token_type,
            position,
        }
    }
}

pub struct Position {
    /// line is the line in which the token or error is located in the source code.
    line: usize,

    /// column is a tuple that that contains in which colums the token or error is located in the source code.
    /// the first element of the tuple is the starting column and the second element is ending column.
    column: (usize, usize),
}

enum TokenType {
    KeyWord(KeyWord),
    Identifier(String),
    Literal(Literal),
    Operator(Operator),
    Period,
    Comma,
    Colon,
    SemiColon,
    Assignment,
    Bracket(Bracket, Direction),
}

enum Bracket {
    Normal,
    Square,
}

enum Literal {
    Integer(i64),
    Real(f64),
    String(String),
}

enum Operator {
    Plus,
    Minus,
    Mult,
    Div,
    Mod,
    // Or, And, and Not are keywords however since they are also operators I put them here for convenience.
    // They will be handled in the keyword recognizing part of the lexer however.
    Or,
    And,
    Not,
    //
    Eq,
    NEq,
    Gt,
    GEq,
    Lt,
    LEq,
}

enum KeyWord {
    If,
    Then,
    Else,
    Of,
    While,
    Do,
    Begin,
    End,
    Var,
    Array,
    Procedure,
    Function,
    Program,
    Assert,
    Return,
}

/// Lexer is the struct that holds in the data and data structures needed during 
/// lexing. Methods(Functions that take in self in some form) that represent the
/// different states lexer can be in are implemented for it.
/// 
/// Together with the State struct they form a state machine.
struct Lexer<'a, O>
where
    O: Sink<Result<Token, LexError>> + 'a
{
    buffer: String,
    escape_buffer: String,
    tokens: &'a mut O,
    line: usize,
    current_column: usize,
    starting_column: Option<usize>,
}

pub fn lex<I, O>(characters: &mut I, tokens: &mut O)
where
    I: Source<char>,
    O: Sink<Result<Token, LexError>>,
{
    let lexer = Lexer::new(tokens);
    run_machine(characters, lexer, State(Lexer::normal));
}

impl<'a, O> Lexer<'a, O>
where
    O: Sink<Result<Token, LexError>>,
{
    fn new(tokens: &'a mut O) -> Self {
        Lexer {
            buffer: String::new(),
            escape_buffer: String::new(),
            tokens,
            line: 1,
            current_column: 0,
            starting_column: None,
        }
    }
    /// normal is the default state of the lexer. It outputs tokens for single character tokens 
    /// and for multi char tokens it does some preparations and switches to the approriate state.
    fn normal(&mut self, c: char) -> State<Self, char> {
        self.current_column += 1;
        match c {
            // These individual characters correspond directly to some token
            '+' | '-' | '*' | '/' | '%' | ';' | ',' | '.' | '(' | ')' | '[' | ']' => {
                // we match the character to a TokenType.
                let token_type = match c {
                    '+' => Operator(Plus),
                    '-' => Operator(Minus),
                    '*' => Operator(Mult),
                    '/' => Operator(Div),
                    '%' => Operator(Mod),
                    '=' => Operator(Eq),
                    ';' => SemiColon,
                    ',' => Comma,
                    '.' => Period,
                    '(' => Bracket(Bracket::Normal, Direction::Left),
                    ')' => Bracket(Bracket::Normal, Direction::Right),
                    '[' => Bracket(Bracket::Square, Direction::Left),
                    ']' => Bracket(Bracket::Square, Direction::Right),
                    _ => unreachable!(),
                };
                // fetch the position of the token in the code.
                let position = self.get_current_position();
                // Create and output the token.
                self.tokens.put(Ok(Token::new(token_type, position)));
                // continue to the next character.
                State(Self::normal)
            },

            // We're expecting a string. Set the starting column to this point and enter string scanning.
            '"' => {
                self.starting_column = Some(self.current_column);
                State(Self::string)
            },

            // We're expecting some comparative operator. We can't tell yet if it's a single character one
            // or a two character one so we push the current character to the buffer and switch to the
            // approriate state.
            '<' | '>' => {
                self.buffer.push(c);
                self.starting_column = Some(self.current_column);
                State(Self::comparative_operator)
            },

            ':' => {
                self.starting_column = Some(self.current_column);
                State(Self::assignment_or_colon)
            },

            // The character is a letter so the character is added to the buffer and
            // and the state is changed to the the identifier and keyword handling one.
            a if a.is_alphabetic() && a.is_ascii() => {
                self.buffer.push(a);
                self.starting_column = Some(self.current_column);
                State(Self::identifier_or_keyword)
            },

            n if n.is_digit(10) => {
                self.buffer.push(n);
                self.starting_column = Some(self.current_column);
                State(Self::integer_or_real)
            },
            
            w if w.is_whitespace() => {
                // newlines are a special case of whitespace since they affect line and 
                // column counting.
                self.check_newline(c);
                // otherwise whitespaces are completely ignored.
                State(Self::normal)
            }

            _ => {
                let position = self.get_current_position();
                self.tokens.put(Err(LexError::InvalidCharacter(position)));
                State(Self::normal)
            }
        }
    }

    fn identifier_or_keyword(&mut self, c: char) -> State<Self, char> {
        match c {
            a if (a.is_alphabetic() && a.is_ascii()) || a == '_' || a.is_digit(10) => {
                self.current_column += 1; 
                self.buffer.push(c);
                State(Self::identifier_or_keyword)
            },
            _ => {
                let token = self.parse_keyword_or_identifier();
                self.tokens.put(Ok(token));
                self.normal(c)
            },
        }
    }

    fn parse_keyword_or_identifier(&mut self) -> Token {
        use self::KeyWord::*;
        let token_type = match &*self.buffer {
            "or" => TokenType::Operator(Or),
            "and" => TokenType::Operator(And),
            "not" => TokenType::Operator(Not),
            "if" => TokenType::KeyWord(If),
            "then" => TokenType::KeyWord(Then),
            "else" => TokenType::KeyWord(Else),
            "of" => TokenType::KeyWord(Of),
            "while" => TokenType::KeyWord(While),
            "do" => TokenType::KeyWord(Do),
            "begin" => TokenType::KeyWord(Begin),
            "end" => TokenType::KeyWord(End),
            "var" => TokenType::KeyWord(Var),
            "array" => TokenType::KeyWord(Array),
            "procedure" => TokenType::KeyWord(Procedure),
            "function" => TokenType::KeyWord(Function),
            "program" => TokenType::KeyWord(Program),
            "assert" => TokenType::KeyWord(Assert),
            "return" => TokenType::KeyWord(Return),
            _ => TokenType::Identifier(self.buffer.clone()),
        };
        self.buffer.clear();
        let position = self.get_current_position();
        self.starting_column = None;
        Token::new(token_type, position)
    }

    fn integer_or_real(&mut self, c: char) -> State<Self, char> {
        match c {
            d if d.is_digit(10) => {
                self.current_column += 1;
                self.buffer.push(c);
                State(Self::integer_or_real)
            },

            '.' => {
                self.current_column += 1;
                self.buffer.push(c);
                State(Self::real)
            },

            _ => {
                let position = self.get_current_position();
                let integer = match i64::from_str_radix(&self.buffer, 10) {
                    Ok(i) => i,
                    Err(err) => {
                        let message = format!("Parsing an integer literal failed. Error given by from_str_radix : {:?}", err);
                        let error = LexError::IntegerLexError(position, message);
                        self.tokens.put(Err(error));
                        self.buffer.clear();
                        self.starting_column = None;
                        return self.normal(c);
                    },
                };
                let token_type = TokenType::Literal(Literal::Integer(integer));
                self.buffer.clear();

                let position = self.get_current_position();
                self.starting_column = None;
                let token = Token::new(token_type, position);
                self.tokens.put(Ok(token));
                self.normal(c)
            },
        }
    }

    fn real(&mut self, c: char) -> State<Self, char> {
        match c {
            n if n.is_digit(10) || n == 'e' => {
                self.current_column += 1;
                self.buffer.push(c);
                State(Self::real)
            },
            _ => {

            }
        }
    }

    fn string(&mut self, c: char) -> State<Self, char> {
        self.current_column += 1;
        match c {
            '\\' => State(Self::some_escape),
            '"' => {
                // The string has ended so create a new token out of the contents of the 
                // buffer and put it into the token sink.
                let token_type = Literal(Literal::String(self.buffer.clone()));
                let position = self.get_current_position();
                let token = Token::new(token_type, position);
                self.tokens.put(Ok(token));

                // Clean up the buffer and the starting column field since their contents
                // are now essentially junk.
                self.buffer.clear();
                self.starting_column = None;

                // Switch back to normal state.
                State(Self::normal) 
            },
            _ => {
                // The character not a special character so we add it to the buffer and 
                // continue to the next character.
                self.check_newline(c);
                self.buffer.push(c);
                State(Self::string)
            },
        }
    }

    fn some_escape(&mut self, c: char) -> State<Self, char> {
        self.current_column += 1;
        let escaped_char = match c {
            // These are single character escapes so the corresponding character can be matched for these
            'a' => '\x07',
            'b' => '\x08',
            'f' => '\x0C',
            'n' => '\n',
            'r' => '\r',
            't' => '\t',
            'v' => '\x0B',
            '\\' | '\'' | '"' | '?' => c,
            // Theśe are multicharacter escapes so we need to transition to a different state to handle them.
            // With octal escapes the first character after the backslash is part of the octal we are parsing 
            // so we need save the character in the buffer before we do a state transition. 
            '0'...'7' => {
                self.escape_buffer.push(c);
                return State(Self::octal_escape)
            } 
            // With hex and unicode the first character is not needed in the parsing of the escape we only need
            // to do a state transition.
            'x' | 'U' | 'u' => {
                return match c {
                    'x' => State(Self::hex_escape),
                    'U' => State(Self::eight_char_unicode_escape),
                    'u' => State(Self::four_char_unicode_escape),
                    _ => unreachable!(),
                };
            }
            _ => {
                let position = self.get_current_position();
                self.tokens.put(Err(LexError::InvalidEscape(
                    position,
                    format!("\\{}", c),
                )));
                return State(Self::string);
            },
        };
        // The escape has been handled. Push the character to the buffer and go back to string lexing state.
        self.buffer.push(escaped_char);
        self.escape_buffer.clear();
        State(Self::string)
    }

    //TODO: The multicharacter escape functions look really similar so I should figure out a way to
    // do code reuse. possibly a function.z
    fn octal_escape(&mut self, c: char) -> State<Self, char> {
        let mut stop = false;
        match c {
            '0'...'7' => self.escape_buffer.push(c),
            _ => stop = true,
        }

        if self.escape_buffer.len() == 3 || stop {
            let escaped = u8::from_str_radix(&self.escape_buffer[..], 8);
            match escaped {
                Ok(chr) => self.buffer.push(chr as char),
                Err(e) => self.send_escape_error(format!("Parsing an octal escape failed. An IntParseError occured: {:?}", e)),
            }
            self.escape_buffer.clear();
            match stop {
                true => self.string(c),
                false => State(Self::string),
            }
        } else {
            State(Self::octal_escape)
        }
    }

    fn hex_escape(&mut self, c: char) -> State<Self, char> {
        let mut stop = false;
        match c {
            '0'...'9' | 'A'...'F' | 'a'...'f' => self.escape_buffer.push(c),
            _ => stop = true,
        }
        if self.escape_buffer.len() == 2 || stop {
            let escaped = u8::from_str_radix(&self.escape_buffer[..], 16);
            match escaped {
                Ok(chr) => self.buffer.push(chr as char),
                Err(e) => self.send_escape_error(
                    format!(
                        "Parsing an hex escape failed. An IntParseError occured: {:?}", e
                    ), 
                ),
            }
            self.escape_buffer.clear();
            match stop {
                true => self.string(c),
                false => State(Self::string),
            }
        } else {
            State(Self::hex_escape)
        }
    }

    fn four_char_unicode_escape(&mut self, c: char) -> State<Self, char> {
        self.char_unicode_escape(c, 4)
    }

    fn eight_char_unicode_escape(&mut self, c: char) -> State<Self, char> {
        self.char_unicode_escape(c, 8)
    }
    
    fn char_unicode_escape(&mut self, c: char, char_amount: usize) -> State<Self, char> {
        self.current_column += 1;
        match c {
            '0'...'9' | 'A'...'F' | 'a'...'f' => self.escape_buffer.push(c),
            _ => {
                self.send_escape_error(
                    format!("Parsing and 8 char unicode escape failed. {} is not a valid hex digit.", c)
                );
                return State(Self::string);
            },
        }
        if self.escape_buffer.len() == char_amount {
            match self.parse_unicode_escape_from_string() {
                Ok(chara) => {
                    self.buffer.push(chara);
                    self.escape_buffer.clear();
                }
                Err(err) => {
                    self.tokens.put(Err(err));
                    self.escape_buffer.clear();
                }
            };
            State(Self::string)
        } else {
            match char_amount {
                4 => State(Self::four_char_unicode_escape),
                8 => State(Self::eight_char_unicode_escape),
                _ => unreachable!(),
            }
        }
    }

    fn parse_unicode_escape_from_string(&mut self) -> Result<char, LexError> {
        let number = match u32::from_str_radix(&self.escape_buffer[..], 16) {
            Ok(number) => number,
            Err(err) => return Err(
                LexError::InvalidEscape(
                    self.get_current_position(),
                    format!(
                        "Error parsing an unicode escape. Could not convert the escape into an integer. {:?}",
                        err,
                    ),
                )
            ),
        };
        match from_u32(number) {
            Some(chara) => Ok(chara),
            None => return Err(
                LexError::InvalidEscape(
                    self.get_current_position(),
                    format!(
                        "Error parsing an unicode escape. Escape value is not valid unicode."
                    ),
                )
            ),
        }
    }

    fn send_escape_error(&mut self, message: String) {
        let position = self.get_current_position();
        let error = Err(LexError::InvalidEscape(position, message));
        self.tokens.put(error);
        self.escape_buffer.clear();
    }

    fn comparative_operator(&mut self, c: char) -> State<Self, char> {
        self.current_column += 1;
        State(Self::comparative_operator)
    }

    fn assignment_or_colon(&mut self, c: char) -> State<Self, char> {
        self.current_column += 1;
        State(Self::assignment_or_colon)
    }

    fn check_newline(&mut self, c: char) {
        if c == '\n' {
            self.line += 1;
            self.current_column = 0;
        }
    }

    fn get_current_position(&self) -> Position {
        let starting_column = match self.starting_column {
            Some(n) => n,
            None => self.current_column,
        };
        Position{
            line: self.line,
            column: (starting_column, self.current_column),
        }
    }
}

pub enum LexError {
    InvalidEscape(Position, String),
    InvalidCharacter(Position),
    IntegerLexError(Position, String),
}