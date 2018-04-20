// MIT License
// Copyright (c) 2018 Victor Bankowski
// Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
// The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

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
            tokens,
            line: 0,
            current_column: 0,
            starting_column: None,
        }
    }

    fn normal(&mut self, c: char) -> State<Self, char> {
        match c {
            // These individual characters correspond directly to some token
            '+' | '-' | '*' | '/' | '%' | ';' | ',' | '.' | '(' | ')' | '[' | ']' => {
                let token_type = match c {
                    '+' => Operator(Plus),
                    '-' => Operator(Minus),
                    '*' => Operator(Mult),
                    '/' => Operator(Div),
                    '%' => Operator(Mod),
                    ';' => SemiColon,
                    ',' => Comma,
                    '.' => Period,
                    '(' => Bracket(Bracket::Normal, Direction::Left),
                    ')' => Bracket(Bracket::Normal, Direction::Right),
                    '[' => Bracket(Bracket::Square, Direction::Left),
                    ']' => Bracket(Bracket::Square, Direction::Right),
                    _ => unreachable!(),
                };
                let position = self.get_current_position();
                self.tokens.put(Ok(Token::new(token_type, position)));
                State(Self::normal)
            },

            '"' => State(Self::string),

            '=' | '<' | '>' => {
                self.buffer.push(c);
                State(Self::logical_operator)
            }

            ':' => {
                State(Self::assignment_or_colon)
            },

            a if a.is_alphabetic() && a.is_ascii() => {
                self.buffer.push(a);
                State(Self::identifier_or_keyword)
            },

            n if n.is_digit(10) => {
                self.buffer.push(n);
                State(Self::integer_or_real)
            },

            _ => {
                let position = self.get_current_position();
                self.tokens.put(Err(LexError::InvalidCharacter(position)));
                State(Self::normal)
            }
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

    fn identifier_or_keyword(&mut self, c: char) -> State<Self, char> {
        State(Self::identifier_or_keyword)
    }

    fn integer_or_real(&mut self, c: char) -> State<Self, char> {
        State(Self::integer_or_real)
    }

    fn string(&mut self, c: char) -> State<Self, char> {
        match c {
            '\\' => State(Self::escape),
            '"' => {
                let token_type = Literal(Literal::String(self.buffer.clone()));
                let position = self.get_current_position();
                let token = Token::new(token_type, position);
                self.tokens.put(Ok(token));
                State(Self::normal) 
            },
            _ => {
                self.buffer.push(c);
                State(Self::string)
            },
        }
    }

    fn escape(&mut self, c: char) -> State<Self, char> {
        State(Self::escape)
    }

    fn logical_operator(&mut self, c: char) -> State<Self, char> {
        State(Self::logical_operator)
    }

    fn assignment_or_colon(&mut self, c: char) -> State<Self, char> {
        State(Self::assignment_or_colon)
    }
}

pub enum LexError {
    InvalidEscape(Position),
    InvalidCharacter(Position),
}