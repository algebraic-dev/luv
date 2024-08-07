//! A module for lexing input into tokens, handling comments, whitespace, and various types of tokens.

use std::{iter::Peekable, str::Chars};

use crate::span::{Point, Span, Spanned};

use super::syntax::{Syntax, SyntaxKind};

/// Checks if a character can be part of an atom.
fn is_atom(c: &char) -> bool {
    !"()[]; \t\n\r\"".contains(*c)
}

/// A lexer for tokenizing input strings.
pub struct Lexer<'input> {
    peekable: Peekable<Chars<'input>>,
    input: &'input str,
    start: Point,
    current: Point,
    st_idx: usize,
    cr_idx: usize
}

impl<'input> Lexer<'input> {
    /// Creates a new `Lexer` for the given input.
    pub fn new(input: &'input str) -> Self {
        Self {
            peekable: input.chars().peekable(),
            input,
            start: Point::zeroed(),
            current: Point::zeroed(),
            st_idx: 0,
            cr_idx: 0,
        }
    }

    /// Advances the lexer and returns the next character.
    fn advance(&mut self) -> Option<char> {
        let c = self.peekable.next()?;
        self.current.advance(c);
        self.cr_idx += c.len_utf8();
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
        self.st_idx = self.cr_idx;
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

        let lexeme = self.input[self.st_idx..self.cr_idx].to_owned();
        (kind, self.spanned(lexeme))
    }

    /// Consumes a string token, handling escape characters and errors.
    fn string(&mut self) -> SyntaxKind {
        while let Some(&c) = self.peekable.peek() {
            match c {
                '"' => break,
                _ => {
                    self.advance();
                    continue
                }
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