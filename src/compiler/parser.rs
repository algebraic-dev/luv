//! A module for parsing tokens into an concrete syntax tree.

use rowan::GreenNodeBuilder;
use std::iter::Peekable;

use super::syntax::{Syntax, SyntaxKind};
use crate::{
    compiler::lexer::Lexer,
    span::{Position, Span, Spanned},
};

/// A parser for converting tokens into an CST.
pub struct Parser<'input> {
    lexer: Peekable<Lexer<'input>>,
    builder: GreenNodeBuilder<'static>,
    errors: Vec<Spanned<String>>,
    span: Span,
}

impl<'input> Parser<'input> {
    /// Creates a new `Parser` for the given lexer.
    pub fn new(lexer: Lexer<'input>) -> Self {
        Self {
            lexer: lexer.peekable(),
            builder: GreenNodeBuilder::new(),
            errors: Vec::new(),
            span: Span::new(Position::zeroed(), Position::zeroed()),
        }
    }
}

/// The
pub enum Response {
    Ok,
    Eof,
    RParen,
}

impl<'input> Parser<'input> {
    /// Gets the current `SyntaxKind` of the token that we are analysing.
    fn current(&mut self) -> Option<SyntaxKind> {
        self.lexer.peek().map(|(kind, _)| kind).copied()
    }

    /// Advances one token in the lexer.
    fn bump(&mut self) -> Syntax {
        let (kind, spanned) = self.lexer.next().unwrap();
        self.builder.token(kind.into(), spanned.data.as_str());
        self.span = spanned.span.clone();

        (kind, spanned)
    }

    /// Removes all the whitespaces.
    fn skip_whitespace(&mut self) {
        while let Some(SyntaxKind::Whitespace | SyntaxKind::Comment) = self.current() {
            self.bump();
        }
    }

    fn string(&mut self) {
        self.builder.start_node(SyntaxKind::String.into());
        self.bump();
        self.builder.finish_node();
    }

    fn identifier(&mut self) {
        self.builder.start_node(SyntaxKind::Identifier.into());
        self.bump();
        self.builder.finish_node();
    }

    fn number(&mut self) {
        self.builder.start_node(SyntaxKind::Number.into());
        self.bump();
        self.builder.finish_node();
    }

    fn error(&mut self, message: impl Into<String>) {
        let span = self.span.clone();
        self.builder.start_node(SyntaxKind::Error.into());
        self.errors.push(Spanned::new(message.into(), span));
        self.bump();
        self.builder.finish_node();
    }

    fn list(&mut self) {
        self.builder.start_node(SyntaxKind::List.into());
        self.bump();
        loop {
            match self.expr() {
                Response::Ok => (),
                Response::Eof => {
                    self.error("unmatched )");
                    break;
                }
                Response::RParen => {
                    self.bump();
                    break;
                }
            }
        }
        self.builder.finish_node();
    }

    pub fn expr(&mut self) -> Response {
        self.skip_whitespace();

        let t = match self.current() {
            None => return Response::Eof,
            Some(SyntaxKind::RPar) => return Response::RParen,
            Some(other) => other,
        };

        match t {
            SyntaxKind::LPar => self.list(),
            SyntaxKind::String => self.string(),
            SyntaxKind::Identifier => self.identifier(),
            SyntaxKind::Number => self.number(),
            SyntaxKind::Error => _ = self.bump(),
            k => todo!("{k:?}"),
        }
        Response::Ok
    }

    pub fn parse(mut self) -> (SyntaxNode, Vec<Spanned<String>>) {
        self.builder.start_node(SyntaxKind::Root.into());
        loop {
            self.skip_whitespace();
            match self.current() {
                None => break,
                Some(SyntaxKind::LPar) => self.list(),
                Some(_) => self.error("Expected (".to_string()),
            }
        }
        self.builder.finish_node();
        (
            SyntaxNode::new_root(self.builder.finish().clone()),
            self.errors,
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Lang {}
impl rowan::Language for Lang {
    type Kind = SyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        assert!(raw.0 <= Self::Kind::Root as u16);
        unsafe { std::mem::transmute::<u16, SyntaxKind>(raw.0) }
    }

    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        kind.into()
    }
}

pub type SyntaxNode = rowan::SyntaxNode<Lang>;
pub type SyntaxToken = rowan::SyntaxToken<Lang>;
pub type SyntaxElement = rowan::NodeOrToken<SyntaxNode, SyntaxToken>;

#[cfg(test)]
mod test {
    use crate::compiler::lexer::Lexer;

    use super::Parser;

    #[test]
    fn lexer_test() {
        let input = r#"(42
            ; ata po
            "a"
            42)

            3  2 3
            "#;
        let parser = Parser::new(Lexer::new(input));
        let (syntax, errors) = parser.parse();

        println!("errors = {errors:?}");

        println!("{:?}", syntax);
        for child in syntax.children_with_tokens() {
            println!("{:?}", child);
            if let Some(child) = child.as_node() {
                for children in child.children_with_tokens() {
                    println!("  {:?}", children);
                }
            }
        }
    }
}
