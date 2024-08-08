//! A module for parsing tokens into an concrete syntax tree.

use std::iter::Peekable;

use super::{
    concrete::{GreenNodeBuilder, SyntaxNode},
    lexer::Lexer,
    syntax::{Syntax, SyntaxKind},
};
use crate::span::{Span, Spanned};

/// The response of parsing a single expression.
pub enum Response {
    Ok,
    Eof,
    RParen,
}

/// A parser for converting tokens into a Concrete Syntax Tree (CST).
pub struct Parser<'input> {
    lexer: Peekable<Lexer<'input>>,
    builder: GreenNodeBuilder,
    errors: Vec<Spanned<String>>,
    span: Span,
}

impl<'input> Parser<'input> {
    /// Creates a new parser instance from a given lexer.
    pub fn new(lexer: Lexer<'input>) -> Self {
        Self {
            lexer: lexer.peekable(),
            builder: GreenNodeBuilder::new(),
            errors: Vec::new(),
            span: Span::empty(),
        }
    }

    fn start_node(&mut self, kind: SyntaxKind) {
        let span = self.current_span();
        self.builder.start_node(kind, span)
    }

    fn finish_node(&mut self, span: Span) {
        self.builder.finish_node(span)
    }

    /// Returns the current token's kind, if any.
    fn current(&mut self) -> Option<SyntaxKind> {
        self.lexer.peek().map(|(kind, _)| kind).copied()
    }

    /// Returns the current token's span, if any.
    fn current_span(&mut self) -> Span {
        let start = self.lexer.peek().map(|(_, s)| s.span.clone());
        self.span = start.unwrap_or(self.span.clone());
        self.span.clone()
    }

    /// Advances to the next token in the input stream.
    fn bump(&mut self) -> Syntax {
        let (kind, spanned) = self.lexer.next().unwrap();
        self.builder
            .token(kind.into(), spanned.data.as_str(), spanned.span.clone());

        (kind, spanned)
    }

    /// Skips whitespace and comments in the input stream.
    fn skip_whitespace(&mut self) {
        while let Some(SyntaxKind::Whitespace | SyntaxKind::Comment) = self.current() {
            self.bump();
        }
    }

    /// Parses a string literal.
    fn string(&mut self) {
        self.start_node(SyntaxKind::String.into());
        let span = self.current_span();
        self.bump();
        self.finish_node(span);
    }

    /// Parses an identifier.
    fn identifier(&mut self) {
        self.start_node(SyntaxKind::Identifier.into());
        let span = self.current_span();
        self.bump();
        self.finish_node(span);
    }

    /// Parses a numeric literal.
    fn number(&mut self) {
        self.start_node(SyntaxKind::Number.into());
        let span = self.current_span();
        self.bump();
        self.finish_node(span);
    }

    /// Records a parsing error.
    fn error(&mut self, message: impl Into<String>) {
        let span = self.current_span();
        self.start_node(SyntaxKind::Error.into());
        self.errors.push(Spanned::new(message.into(), span));
        let span = self.current_span();
        self.bump();
        self.finish_node(span);
    }

    /// Parses a list of expressions.
    fn list(&mut self, start_span: Span) {
        self.start_node(SyntaxKind::List.into());
        self.bump();
        loop {
            let span = self.current_span();
            match self.expr() {
                Response::Ok => (),
                Response::Eof => {
                    self.errors
                        .push(Spanned::new("unmatched".into(), start_span.clone()));
                    self.finish_node(span);
                    break;
                }
                Response::RParen => {
                    self.bump();
                    self.finish_node(span);
                    break;
                }
            }
        }
    }

    /// Parses a list of expressions.
    fn quote(&mut self, start_span: Span) {
        self.start_node(SyntaxKind::Quote.into());
        self.bump();
        let span = self.current_span();
        match self.expr() {
            Response::Ok => {
                self.finish_node(span);
            }
            _ => {
                self.errors
                    .push(Spanned::new("no expression".into(), start_span.clone()));
                self.finish_node(span);
            }
        }
    }

    /// Parses an expression.
    pub fn expr(&mut self) -> Response {
        self.skip_whitespace();

        let (kind, span) = match self.current().zip(Some(self.current_span())) {
            None => return Response::Eof,
            Some((SyntaxKind::RPar, _)) => return Response::RParen,
            Some((other, s)) => (other, s),
        };

        match kind {
            SyntaxKind::LPar => self.list(span),
            SyntaxKind::String => self.string(),
            SyntaxKind::Identifier => self.identifier(),
            SyntaxKind::Number => self.number(),
            SyntaxKind::SimpleQuote => self.quote(span),
            SyntaxKind::Error => {
                _ = self.bump();
                self.errors
                    .push(Spanned::new("unfinished string".to_owned(), span));
            }
            k => todo!("{k:?}"),
        }
        Response::Ok
    }

    /// Parses the entire input stream and returns the resulting CST and any errors encountered.
    pub fn parse(mut self) -> (SyntaxNode, Vec<Spanned<String>>) {
        self.start_node(SyntaxKind::Root.into());
        loop {
            self.skip_whitespace();
            match self.current().zip(Some(self.current_span())) {
                None => break,
                Some((SyntaxKind::LPar, span)) => self.list(span),
                Some(_) => self.error("Expected (".to_string()),
            }
        }
        (self.builder.finish(), self.errors)
    }
}

/// Parses a string to a syntax node and a vector of errors.
pub fn parse(code: &str) -> (SyntaxNode, Vec<Spanned<String>>) {
    Parser::new(Lexer::new(code)).parse()
}

#[cfg(test)]
mod test {
    use crate::compiler::lexer::Lexer;

    use super::Parser;

    #[test]
    fn lexer_test() {
        let input = r#"(def a (b c) d e 3 "ata")"#;
        let parser = Parser::new(Lexer::new(input));
        let (syntax, errors) = parser.parse();

        println!("errors = {errors:?}");
        println!("{}", syntax);
    }
}
