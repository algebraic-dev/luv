//! A module for lexing input into tokens, handling comments, whitespace, and various types of tokens.

use std::{iter::Peekable, str::Chars};

use crate::{
    span::{Span, Spanned},
    compiler::syntax::token::{Comment, Token, TokenInfo, TokenKind},
};

/// Checks if a character can be part of an atom.
fn is_atom(c: &char) -> bool {
    !"()[]; \t\n\r\"".contains(*c)
}

/// A lexer for tokenizing input strings.
pub struct Lexer<'input> {
    peekable: Peekable<Chars<'input>>,
    input: &'input str,
    start: usize,
    index: usize,
}
/// A utility enum for handling two possible types.
pub enum Either<A, B> {
    Left(A),
    Right(B),
}

type Lexeme = String;

impl<'input> Lexer<'input> {
    /// Creates a new `Lexer` for the given input.
    pub fn new(input: &'input str) -> Self {
        Self {
            peekable: input.chars().peekable(),
            input,
            start: 0,
            index: 0,
        }
    }

    /// Advances the lexer and returns the next character.
    fn advance(&mut self) -> Option<char> {
        let c = self.peekable.next()?;
        self.index += c.len_utf8();
        Some(c)
    }

    /// Advances the lexer while the predicate is true.
    fn advance_while(&mut self, pred: fn(&char) -> bool) {
        while let Some(&c) = self.peekable.peek() {
            if pred(&c) {
                self.advance();
            } else {
                break;
            }
        }
    }

    /// Saves the current index as the start of the next token.
    fn save(&mut self) {
        self.start = self.index;
    }

    /// Creates a spanned object with the given data.
    fn spanned<T>(&self, data: T) -> Spanned<T> {
        Spanned::new(data, Span::new(self.start, self.index))
    }

    /// Consumes whitespace and returns it as a spanned string.
    fn whitespace(&mut self) -> Spanned<String> {
        self.save();
        self.advance_while(|c| c.is_ascii_whitespace());
        let whitespace = self.input[self.start..self.index].to_owned();
        self.spanned(whitespace)
    }

    /// Consumes comments and returns either a `Comment` or spanned whitespace.
    fn comment(&mut self) -> Either<Comment, Spanned<String>> {
        let whitespace = self.whitespace();
        self.save();

        if let Some(';') = self.peekable.peek() {
            self.advance_while(|c| *c != '\n');
            let comment = self.input[self.start..self.index].to_owned();
            let comment = self.spanned(comment);
            Either::Left(Comment { whitespace, comment })
        } else {
            Either::Right(whitespace)
        }
    }

    /// Consumes multiple comments and returns them along with trailing whitespace.
    fn comments(&mut self) -> (Vec<Comment>, Spanned<String>) {
        let mut comments = vec![];
        loop {
            match self.comment() {
                Either::Left(comment) => comments.push(comment),
                Either::Right(whitespace) => break (comments, whitespace),
            }
        }
    }

    /// Returns the next token from the input.
    pub fn next_token(&mut self) -> Token {
        let (comments, whitespace) = self.comments();
        self.save();

        let (kind, lexeme) = self.token();
        let lexeme = self.spanned(lexeme);
        Token {
            kind,
            info: TokenInfo {
                comments,
                whitespace,
                lexeme,
            },
        }
    }

    /// Determines the next token kind and its lexeme.
    fn token(&mut self) -> (TokenKind, Lexeme) {
        let kind = if let Some(c) = self.advance() {
            match c {
                '(' => TokenKind::LParens,
                ')' => TokenKind::RParens,
                '[' => TokenKind::LBracket,
                ']' => TokenKind::RBracket,
                '\'' => TokenKind::SimpleQuote,
                '"' => return self.string(),
                c if c.is_ascii_alphabetic() => {
                    self.advance_while(is_atom);
                    TokenKind::Identifier
                }
                c if c.is_ascii_digit() => {
                    self.advance_while(|c| c.is_ascii_digit());
                    TokenKind::Number
                }
                _ => TokenKind::Error,
            }
        } else {
            TokenKind::Eof
        };

        let lexeme = self.input[self.start..self.index].to_owned();
        (kind, lexeme)
    }

    /// Consumes a string token, handling escape characters and errors.
    fn string(&mut self) -> (TokenKind, Lexeme) {
        let mut s = String::new();
        while let Some(&c) = self.peekable.peek() {
            match c {
                '"' => break,
                _ => s.push(self.advance().unwrap()),
            }
        }
        if let Some('"') = self.advance() {
            (TokenKind::String, s)
        } else {
            (TokenKind::Error, s)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Lexer;

    #[test]
    fn read_token_with_comment() {
        let input = r#"

; here
; comment

fly

"#;
        let mut lexer = Lexer::new(input);
        let token = lexer.next_token();
        println!("{:?}", token);
    }
}