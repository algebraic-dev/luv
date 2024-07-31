//! Defines all the pieces of syntax that a firefly CST can have.

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

pub type Syntax = (SyntaxKind, Spanned<String>);

impl From<SyntaxKind> for rowan::SyntaxKind {
    fn from(kind: SyntaxKind) -> Self {
        Self(kind as u16)
    }
}
