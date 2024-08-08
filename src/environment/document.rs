//! A document is just a reference to the entire page of something. It's used to manage, clear and
//! track things when it's being used in a text editor.

use std::mem;

use crate::{compiler::{compare::{self, Change}, concrete::SyntaxNode, parser, syntax::SyntaxKind}, span::{Span, Spanned}};

/// A change in a part of the text.
pub struct TextChange {
    pub span: Span,
    pub text: String
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Id(pub usize);

pub struct Document {
    pub id: Id,
    pub code: String,
    pub old_syn: SyntaxNode,
    pub new_syn: SyntaxNode,
    pub version: usize,
    pub errors: Vec<Spanned<String>>,
}

impl Document {
    pub fn new(id: Id, code: String) -> Self {
        Self {
            id,
            code,
            old_syn: SyntaxNode::new(SyntaxKind::Root, vec![], Span::empty()),
            new_syn: SyntaxNode::new(SyntaxKind::Root, vec![], Span::empty()),
            version: 0,
            errors: vec![]
        }
    }

    /// Sets a new text to the code without parsing the new CST.
    pub fn update(&mut self, changes: &[TextChange]) -> Vec<Change> {
        for TextChange { span, text } in changes {
            let start = span.start.offset(&self.code);
            let end = span.end.offset(&self.code);
            self.code.replace_range(start..end, &text);
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