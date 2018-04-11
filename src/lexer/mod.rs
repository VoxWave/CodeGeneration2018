// MIT License

// Copyright (c) 2018 Victor Bankowski

// Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

use num_bigint::BigInt;

use common::Direction;

/// Tokens are lexical units of code such as identifiers, keywords, operators etc.
struct Token {
    /// token_type is the type of the token such as an identifier, keyword, operator etc.
    token_type: TokenType,
    /// line is the line in which the token is located in the source code.
    line: usize,
    /// column is a tuple that that contains in which colums the token is located in the source code.
    /// the first element of the tuple is the colum from which the token begins and the second element is where token ends.
    column: (usize, usize),
}


enum TokenType {
    Identifier(String),
    Literal(Literal),
    Operator(Operator),
    SemiColon,
    Assignment,
    Bracket(Direction),  
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
