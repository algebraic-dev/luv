//! This module defines an untyped concrete syntax tree, which stores every piece of data for easy
//! manipulation with an LSP.

use crate::span::{Span, Spanned};
use core::fmt;
use std::collections::HashMap;
use std::hash::{DefaultHasher, Hash, Hasher};
use std::slice::Iter;

/// All the types of syntax that a piece of text can have.
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
    List,
    Quote,
    Literal,
    Root,
}

/// A token is a piece of text with the classification.
pub type Token = (SyntaxKind, Spanned<String>);

/// The identifier of a [SyntaxNode], used for lightweight comparison of nodes.
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Id(Span, u64);

/// The syntax node express an artificial boundary in the tokens creating a syntatic meaning on them.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SyntaxNode {
    pub kind: SyntaxKind,
    pub children: Vec<SyntaxElement>,
    pub span: Span,
    pub hash: u64,
}

impl SyntaxNode {
    /// Constructs a new SyntaxNode with computed hash.
    pub fn new(kind: SyntaxKind, children: Vec<SyntaxElement>, span: Span) -> Self {
        let mut hasher = DefaultHasher::new();

        kind.hash(&mut hasher);
        for child in &children {
            child.hash(&mut hasher);
        }
        let hash = hasher.finish();

        SyntaxNode {
            kind,
            children,
            hash,
            span,
        }
    }

    /// Converts the node to a map of its nodes.
    pub fn to_map(&self) -> HashMap<Id, SyntaxNode> {
        self.clone().get_nodes().map(|x| (x.get_id(), x)).collect()
    }

    /// Returns the unique identifier for the node.
    pub fn get_id(&self) -> Id {
        Id(self.span.clone(), self.hash)
    }

    /// Returns the kind of the syntax node.
    pub fn kind(&self) -> SyntaxKind {
        self.kind
    }

    /// Returns the children of the syntax node.
    pub fn children(&self) -> Iter<'_, SyntaxElement> {
        self.children.iter()
    }

    /// Returns an iterator over the child nodes.
    pub fn nodes(&self) -> impl Iterator<Item = &SyntaxNode> {
        self.children.iter().filter_map(|p| {
            if let SyntaxElement::Node(n) = p {
                Some(n)
            } else {
                None
            }
        })
    }

    /// Consumes the node and returns an iterator over the child nodes.
    pub fn get_nodes(self) -> impl Iterator<Item = SyntaxNode> {
        self.children.into_iter().filter_map(|p| {
            if let SyntaxElement::Node(n) = p {
                Some(n)
            } else {
                None
            }
        })
    }

    /// Returns an iterator over the child tokens.
    pub fn tokens(&self) -> impl Iterator<Item = &SyntaxToken> {
        self.children.iter().filter_map(|p| {
            if let SyntaxElement::Token(n) = p {
                Some(n)
            } else {
                None
            }
        })
    }

    /// Returns the first token if available.
    pub fn first_token(&self) -> Option<&SyntaxToken> {
        self.tokens().nth(0)
    }

    pub fn compare<'a>(&'a self, other: &'a SyntaxNode, changed: &[Span]) -> Vec<Change<'a>> {
        let mut changes = Vec::new();
        Self::compare_top_level_nodes(self, other, &mut changes, changed);
        changes
    }

    fn compare_top_level_nodes<'a>(
        a: &'a SyntaxNode,
        b: &'a SyntaxNode,
        changes: &mut Vec<Change<'a>>,
        changed_spans: &[Span],
    ) {
        let a_nodes = filter_top_level_children(a).collect::<Vec<_>>();
        let b_nodes = filter_top_level_children(b).collect::<Vec<_>>();

        let mut a_map = HashMap::new();
        let mut b_map = HashMap::new();

        for a_node in a_nodes {
            if is_affected_by_changed(&a_node.span, changed_spans) {
                let adjusted_span = adjust_span(&a_node.span, changed_spans);
                a_map.insert((a_node.hash, adjusted_span.clone()), a_node);
            }
        }

        for b_node in b_nodes {
            if is_affected_by_changed(&b_node.span, changed_spans) {
                let adjusted_span = adjust_span(&b_node.span, changed_spans);
                b_map.insert((b_node.hash, adjusted_span.clone()), b_node);
            }
        }

        for (b_node, value) in &b_map {
            if !a_map.contains_key(b_node) {
                changes.push(Change::Added(value));
            }
        }

        for (a_node, value) in &a_map {
            if !b_map.contains_key(a_node) {
                changes.push(Change::Removed(value));
            }
        }
    }
}

impl Hash for SyntaxNode {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.hash.hash(state);
    }
}

/// A piece of text inside the syntax that matches some [SyntaxKind]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SyntaxToken {
    pub kind: SyntaxKind,
    pub text: String,
    pub span: Span,
    pub hash: u64,
}

impl SyntaxToken {
    // Constructor that computes and stores the hash
    pub fn new(kind: SyntaxKind, text: String, span: Span) -> Self {
        let mut hasher = DefaultHasher::new();
        kind.hash(&mut hasher);
        text.hash(&mut hasher);
        let hash = hasher.finish();

        SyntaxToken {
            kind,
            text,
            span,
            hash,
        }
    }
}

/// A syntax piece that can be a [SyntaxNode] or a [SyntaxToken]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SyntaxElement {
    Node(SyntaxNode),
    Token(SyntaxToken),
}

impl Hash for SyntaxElement {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            SyntaxElement::Node(node) => {
                0.hash(state);
                node.hash.hash(state);
            }
            SyntaxElement::Token(token) => {
                1.hash(state);
                token.hash.hash(state);
            }
        }
    }
}

impl SyntaxElement {
    /// Returns the kind of the syntax node or token.
    pub fn kind(&self) -> SyntaxKind {
        match self {
            SyntaxElement::Node(n) => n.kind,
            SyntaxElement::Token(t) => t.kind,
        }
    }

    /// Returns the hash value of the node or token.
    pub fn hash_value(&self) -> u64 {
        match self {
            SyntaxElement::Node(n) => n.hash,
            SyntaxElement::Token(t) => t.hash,
        }
    }
}

#[derive(Debug)]
pub struct SyntaxBuilder {
    stack: Vec<(SyntaxKind, Vec<SyntaxElement>, Span)>,
}

impl Default for SyntaxBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl SyntaxBuilder {
    /// Initializes a new GreenNodeBuilder.
    pub fn new() -> Self {
        Self { stack: Vec::new() }
    }

    /// Starts a new syntax node of the given kind.
    pub fn start_node(&mut self, kind: SyntaxKind, span: Span) {
        self.stack.push((kind, Vec::new(), span));
    }

    /// Finishes the current syntax node, adding it to its parent node.
    pub fn finish_node(&mut self, end_span: Span) {
        let (kind, children, start_span) = self.stack.pop().expect("No node to finish");
        let span = start_span.merge(end_span);

        let node = SyntaxElement::Node(SyntaxNode::new(kind, children, span.clone()));

        if let Some((_, parent, _)) = self.stack.last_mut() {
            parent.push(node);
        } else {
            self.stack.push((kind, vec![node], span));
        }
    }

    /// Adds a token to the current node being built.
    pub fn token(&mut self, kind: SyntaxKind, text: &str, span: Span) {
        let token = SyntaxElement::Token(SyntaxToken::new(kind, text.to_string(), span));

        if let Some((_, parent, _)) = self.stack.last_mut() {
            parent.push(token);
        }
    }

    /// Completes the building process and returns the constructed green node.
    pub fn finish(mut self) -> SyntaxNode {
        let (kind, children, span) = self.stack.pop().expect("No root node to finish");
        SyntaxNode::new(kind, children, span)
    }
}

trait PrettyPrint {
    fn pretty_print(&self, f: &mut fmt::Formatter, prefix: &str, is_last: bool) -> fmt::Result;
}

const BOLD: &str = "";
const DIM: &str = "";
const RESET: &str = "";

impl PrettyPrint for SyntaxNode {
    fn pretty_print(&self, f: &mut fmt::Formatter, prefix: &str, is_last: bool) -> fmt::Result {
        let connector = if is_last { "└── " } else { "├── " };
        writeln!(
            f,
            "{}{}{}{:?} {} {}",
            prefix, connector, BOLD, self.kind, self.span, RESET
        )?;

        let new_prefix = if is_last {
            prefix.to_string() + "    "
        } else {
            prefix.to_string() + "│   "
        };

        let mut children = self.children.iter().peekable();
        while let Some(child) = children.next() {
            let is_last_child = children.peek().is_none();
            match child {
                SyntaxElement::Node(node) => {
                    node.pretty_print(f, &new_prefix, is_last_child).unwrap();
                }
                SyntaxElement::Token(token) => {
                    let token_connector = if is_last_child {
                        "└── "
                    } else {
                        "├── "
                    };
                    writeln!(
                        f,
                        "{}{}{}{:?} {:?} {} {}",
                        new_prefix, token_connector, DIM, token.kind, token.text, token.span, RESET
                    )?;
                }
            }
        }
        Ok(())
    }
}

impl PrettyPrint for SyntaxToken {
    fn pretty_print(&self, f: &mut fmt::Formatter, prefix: &str, is_last: bool) -> fmt::Result {
        let connector = if is_last { "└── " } else { "├── " };
        writeln!(
            f,
            "{}{}Token({:?}, \"{}\")",
            prefix, connector, self.kind, self.text
        )
    }
}

impl fmt::Display for SyntaxNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.pretty_print(f, "", true)
    }
}

impl fmt::Display for SyntaxElement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SyntaxElement::Node(tkn) => tkn.pretty_print(f, "", true),
            SyntaxElement::Token(tkn) => tkn.pretty_print(f, "", true),
        }
    }
}

#[derive(Debug)]
pub enum Change<'a> {
    Added(&'a SyntaxNode),
    Removed(&'a SyntaxNode),
}

fn filter_top_level_children(node: &SyntaxNode) -> impl Iterator<Item = &SyntaxNode> {
    node.nodes()
        .filter(|child| !matches!(child.kind(), SyntaxKind::Whitespace | SyntaxKind::Comment))
}

fn is_affected_by_changed(span: &Span, changed_spans: &[Span]) -> bool {
    changed_spans
        .iter()
        .any(|changed| span.starts_after(changed) || span.overlap(changed))
}

fn adjust_span(span: &Span, changed_spans: &[Span]) -> Span {
    let mut adjusted_span = span.clone();
    for changed in changed_spans {
        if adjusted_span.starts_after(changed) {
            let diff = changed.end.subtract(&changed.start);
            let diff_start = adjusted_span.start.subtract(&diff);
            let diff_end = adjusted_span.end.subtract(&diff);
            adjusted_span = Span::new(diff_start, diff_end);
        }
    }
    adjusted_span
}
