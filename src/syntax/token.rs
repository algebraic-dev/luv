use crate::span::Spanned;

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub info: TokenInfo,
}

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

#[derive(Debug)]
pub struct Comment {
    pub whitespace: Spanned<String>,
    pub comment: Spanned<String>,
}

#[derive(Debug)]
pub struct TokenInfo {
    pub comments: Vec<Comment>,
    pub whitespace: Spanned<String>,
    pub lexeme: Spanned<String>,
}
