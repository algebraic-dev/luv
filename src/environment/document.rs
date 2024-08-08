//! The `Document` module provides functionalities for managing and tracking changes in a text
//! document within a text editor. It includes structures and methods to represent and modify a
//! document's content, compute its syntax tree, and track changes.

use std::{
    collections::HashMap,
    mem,
};

use crate::{
    compiler::{
        compare::{self, Change},
        concrete::{self, SyntaxNode},
        parser,
        syntax::SyntaxKind,
    },
    span::{Span, Spanned},
};

/// Represents a change made to a section of text.
pub struct TextChange {
    pub span: Span,
    pub text: String,
}

/// An identifier for a document.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Id(pub usize);

/// Represents a document within a text editor.
pub struct Document {
    pub id: Id,
    pub code: String,
    pub old_syn: SyntaxNode,
    pub new_syn: SyntaxNode,
    pub version: usize,
    pub errors: Vec<Spanned<String>>,
    pub scopes: HashMap<concrete::Id, HashMap<Span, HashMap<String, Span>>>,
}

impl Document {
    /// Creates a new `Document` instance with the given ID and initial code.
    pub fn new(id: Id, code: String) -> Self {
        Self {
            id,
            code,
            old_syn: SyntaxNode::new(SyntaxKind::Root, vec![], Span::empty()),
            new_syn: SyntaxNode::new(SyntaxKind::Root, vec![], Span::empty()),
            version: 0,
            errors: vec![],
            scopes: HashMap::default(),
        }
    }

    /// Updates the document's content based on a list of text changes.
    pub fn update(&mut self, changes: &[TextChange]) -> Vec<Change> {
        for TextChange { span, text } in changes {
            let start = span.start.offset(&self.code);
            let end = span.end.offset(&self.code);
            self.code.replace_range(start..end, text);
            self.version = self.version.wrapping_add(1);
        }

        self.compute(&changes.iter().map(|x| x.span.clone()).collect::<Vec<_>>())
    }

    /// Computes a new CST.
    pub fn compute(&mut self, changes: &[Span]) -> Vec<Change> {
        let (syntax, errors) = parser::parse(&self.code);
        self.errors = errors;

        // Just to avoid copying stuff around.
        mem::swap(&mut self.old_syn, &mut self.new_syn);
        self.new_syn = syntax;

        compare::compare(&self.old_syn, &self.new_syn, changes)
    }
}
