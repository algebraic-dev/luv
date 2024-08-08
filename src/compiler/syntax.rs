//! Defines all the pieces of syntax that a firefly CST can have.

use core::fmt;

use crate::span::Spanned;

/// All the types of Syntax that a ?
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u16)]
pub enum SyntaxKind {
    LPar = 0,
    RPar,
    Identifier,
    Number,
    String,
    Comment,
    Whitespace,
    SimpleQuote,
    Error,
    Eof,

    // Composite
    List,
    Literal,

    Root,
}

impl fmt::Display for SyntaxKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SyntaxKind::LPar => write!(f, "lpar"),
            SyntaxKind::RPar => write!(f, "rpar"),
            SyntaxKind::Identifier => write!(f, "identifier"),
            SyntaxKind::Number => write!(f, "number"),
            SyntaxKind::String => write!(f, "string"),
            SyntaxKind::Comment => write!(f, "comment"),
            SyntaxKind::Whitespace => write!(f, "ws"),
            SyntaxKind::SimpleQuote => write!(f, "simple"),
            SyntaxKind::Error => write!(f, "error"),
            SyntaxKind::Eof => write!(f, "eof"),
            SyntaxKind::List => write!(f, "list"),
            SyntaxKind::Literal => write!(f, "literal"),
            SyntaxKind::Root => write!(f, "root")
        }
    }
}

pub type Syntax = (SyntaxKind, Spanned<String>);
