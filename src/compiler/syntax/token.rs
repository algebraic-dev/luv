//! A module for handling tokens, their kinds, and related information, including comments and whitespace.
//!
//! The module provides the following structures:
//!
//! - [Token] : Represents a token with a kind and associated information.
//! - [TokenKind] : Enumerates the possible kinds of tokens.
//! - [Comment] : Represents a comment with preceding whitespace.
//! - [TokenInfo] : Holds detailed information about a token, including comments, whitespace, and the lexeme.
//!

use crate::span::Spanned;

/// Represents a token with a kind and associated information.
#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub info: TokenInfo,
}

impl Token {
    /// Creates a new `Token` with the given kind and information.
    pub fn new(kind: TokenKind, info: TokenInfo) -> Self {
        Self { kind, info }
    }
}

/// Enumerates the possible kinds of tokens.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenKind {
    Identifier,
    Number,
    String,
    LParens,
    RParens,
    LBracket,
    RBracket,
    SimpleQuote,
    Error,
    Eof,
}

/// Represents a comment with preceding whitespace.
#[derive(Debug)]
pub struct Comment {
    pub whitespace: Spanned<String>,
    pub comment: Spanned<String>,
}

impl Comment {
    /// Creates a new `Comment` with the given whitespace and comment.
    pub fn new(whitespace: Spanned<String>, comment: Spanned<String>) -> Self {
        Self {
            whitespace,
            comment,
        }
    }
}

/// Holds detailed information about a token, including comments, whitespace, and the lexeme.
#[derive(Debug)]
pub struct TokenInfo {
    pub comments: Vec<Comment>,
    pub whitespace: Spanned<String>,
    pub lexeme: Spanned<String>,
}

impl TokenInfo {
    /// Creates a new `TokenInfo` with the given comments, whitespace, and lexeme.
    pub fn new(
        comments: Vec<Comment>,
        whitespace: Spanned<String>,
        lexeme: Spanned<String>,
    ) -> Self {
        Self {
            comments,
            whitespace,
            lexeme,
        }
    }
}
