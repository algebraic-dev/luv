//! A module for lexing input into tokens, handling comments, whitespace, and various types of tokens.

use std::{iter::Peekable, str::Chars};

use crate::span::{Position, Span, Spanned};

use super::syntax::{Syntax, SyntaxKind};

/// Checks if a character can be part of an atom.
fn is_atom(c: &char) -> bool {
    !"()[]; \t\n\r\"".contains(*c)
}

/// A lexer for tokenizing input strings.
pub struct Lexer<'input> {
    peekable: Peekable<Chars<'input>>,
    input: &'input str,
    start: Position,
    current: Position,
}

impl<'input> Lexer<'input> {
    /// Creates a new `Lexer` for the given input.
    pub fn new(input: &'input str) -> Self {
        Self {
            peekable: input.chars().peekable(),
            input,
            start: Position::zeroed(),
            current: Position::zeroed(),
        }
    }

    /// Advances the lexer and returns the next character.
    fn advance(&mut self) -> Option<char> {
        let c = self.peekable.next()?;
        self.current.advance(c);
        Some(c)
    }

    /// Advances the lexer while the predicate is true.
    fn accumulate(&mut self, pred: fn(&char) -> bool) {
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
        self.start = self.current.clone();
    }

    /// Creates a spanned object with the given data.
    fn spanned<T>(&self, data: T) -> Spanned<T> {
        Spanned::new(data, Span::new(self.start.clone(), self.current.clone()))
    }

    /// Consumes whitespace and returns it as a spanned string.
    fn whitespace(&mut self) -> SyntaxKind {
        self.accumulate(|c| c.is_ascii_whitespace());
        SyntaxKind::Whitespace
    }

    /// Consumes comments and returns either a `Comment` or spanned whitespace.
    fn comment(&mut self) -> SyntaxKind {
        self.accumulate(|c| *c != '\n');
        SyntaxKind::Comment
    }

    /// Determines the next token kind and its lexeme.
    fn token(&mut self) -> Syntax {
        let kind = if let Some(c) = self.advance() {
            match c {
                '(' => SyntaxKind::LPar,
                ')' => SyntaxKind::RPar,
                '\'' => SyntaxKind::SimpleQuote,
                ';' => self.comment(),
                '"' => self.string(),
                c if c.is_ascii_whitespace() => self.whitespace(),
                c if c.is_ascii_alphabetic() => {
                    self.accumulate(is_atom);
                    SyntaxKind::Identifier
                }
                c if c.is_ascii_digit() => {
                    self.accumulate(|c| c.is_ascii_digit());
                    SyntaxKind::Number
                }
                _ => SyntaxKind::Error,
            }
        } else {
            SyntaxKind::Eof
        };

        let lexeme = self.input[self.start.index..self.current.index].to_owned();
        (kind, self.spanned(lexeme))
    }

    /// Consumes a string token, handling escape characters and errors.
    fn string(&mut self) -> SyntaxKind {
        let mut s = String::new();
        while let Some(&c) = self.peekable.peek() {
            match c {
                '"' => break,
                _ => s.push(self.advance().unwrap()),
            }
        }

        if let Some('"') = self.advance() {
            SyntaxKind::String
        } else {
            SyntaxKind::Error
        }
    }

    /// Returns the next token from the input.
    pub fn bump(&mut self) -> Syntax {
        self.save();
        self.token()
    }

    pub fn peekable(self) -> Peekable<Self> {
        std::iter::Iterator::peekable(self)
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Syntax;

    fn next(&mut self) -> Option<Self::Item> {
        match self.bump() {
            (SyntaxKind::Eof, _) => None,
            other => Some(other),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let mut result = Lexer::new("(ata    \"ata\" 232 (4 5 \"b\")))) (a 2 3)");
        println!("{:?}", result.bump());
        println!("{:?}", result.bump());
        println!("{:?}", result.bump());
        println!("{:?}", result.bump());
        println!("{:?}", result.bump());
        println!("{:?}", result.bump());
        println!("{:?}", result.bump());
        println!("{:?}", result.bump());
        println!("{:?}", result.bump());
        println!("{:?}", result.bump());
        println!("{:?}", result.bump());
        println!("{:?}", result.bump());
        println!("{:?}", result.bump());
        println!("{:?}", result.bump());
        println!("{:?}", result.bump());
        println!("{:?}", result.bump());
    }
}
