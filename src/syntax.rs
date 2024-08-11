//! This module defines an untyped concrete syntax tree, which stores every piece of data for easy
//! manipulation with an LSP.

use core::fmt;
use std::collections::HashMap;
use std::hash::{DefaultHasher, Hash, Hasher};
use std::slice::Iter;

use crate::prettytree::{PrettyPrint, Tree};
use crate::span::{Span, Spanned};

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
            SyntaxKind::Quote => write!(f, "quote"),
            SyntaxKind::Root => write!(f, "root"),
        }
    }
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

    /// Compare two syntax trees and return the difference between them.
    pub fn compare<'a>(&'a self, other: &'a SyntaxNode, changed: &[Span]) -> Vec<Change<'a>> {
        let mut changes = Vec::new();

        let a_nodes = filter_top_level_children(self).collect::<Vec<_>>();
        let b_nodes = filter_top_level_children(other).collect::<Vec<_>>();

        let mut a_map = HashMap::new();
        let mut b_map = HashMap::new();

        for a_node in a_nodes {
            if is_affected_by_changed(&a_node.span, changed) {
                let adjusted_span = adjust_span(&a_node.span, changed);
                a_map.insert((a_node.hash, adjusted_span.clone()), a_node);
            }
        }

        for b_node in b_nodes {
            if is_affected_by_changed(&b_node.span, changed) {
                let adjusted_span = adjust_span(&b_node.span, changed);
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

        changes
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
    pub fn finish(mut self, end_span: Span) -> SyntaxNode {
        let (kind, children, span) = self.stack.pop().expect("No root node to finish");
        SyntaxNode::new(kind, children, span.merge(end_span))
    }
}

impl PrettyPrint for SyntaxNode {
    fn to_tree(&self) -> Tree {
        let mut tree = Tree::label(format!("{} {}", self.kind, self.span));

        for child in &self.children {
            tree = tree.add(child.to_tree());
        }

        tree
    }
}

impl PrettyPrint for SyntaxToken {
    fn to_tree(&self) -> Tree {
        Tree::label(format!("{} {:?} {}", self.kind, self.text, self.span))
    }
}

impl PrettyPrint for SyntaxElement {
    fn to_tree(&self) -> Tree {
        match self {
            SyntaxElement::Node(node) => node.to_tree(),
            SyntaxElement::Token(node) => node.to_tree(),
        }
    }
}

impl fmt::Display for SyntaxNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}", self.to_tree())
    }
}

impl fmt::Display for SyntaxElement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}", self.to_tree())
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

#[cfg(test)]
mod tests {
    use crate::{
        parser::parse,
        span::{Point, Span},
        syntax::Change,
    };

    #[test]
    fn test_no_changes() {
        let input1 = "(a)";
        let input2 = "(a)";
        let (syn1, errors1) = parse(input1);
        let (syn2, errors2) = parse(input2);

        assert_eq!(errors1.len(), 0);
        assert_eq!(errors2.len(), 0);

        let changes = syn1.compare(&syn2, &[]);
        assert!(changes.is_empty());
    }

    #[test]
    fn test_added_node() {
        let input1 = "(a)";
        let input2 = "(a)(b)";
        let (syn1, errors1) = parse(input1);
        let (syn2, errors2) = parse(input2);

        assert_eq!(errors1.len(), 0);
        assert_eq!(errors2.len(), 0);

        let changes = syn1.compare(&syn2, &[Span::new(Point::new(0, 3), Point::new(0, 3))]);
        assert_eq!(changes.len(), 1);
        matches!(changes[0], Change::Added(_));
    }

    #[test]
    fn test_removed_node() {
        let input1 = "(a)(b)";
        let input2 = "(a)";
        let (syn1, errors1) = parse(input1);
        let (syn2, errors2) = parse(input2);

        assert_eq!(errors1.len(), 0);
        assert_eq!(errors2.len(), 0);

        let changes = syn1.compare(&syn2, &[Span::new(Point::new(0, 3), Point::new(0, 3))]);
        assert_eq!(changes.len(), 1);
        matches!(changes[0], Change::Removed(_));
    }

    #[test]
    fn test_changed_node() {
        let input1 = "(a)";
        let input2 = "(b)";
        let (syn1, errors1) = parse(input1);
        let (syn2, errors2) = parse(input2);

        assert_eq!(errors1.len(), 0);
        assert_eq!(errors2.len(), 0);

        let changes = syn1.compare(&syn2, &[Span::new(Point::new(0, 1), Point::new(0, 1))]);
        assert_eq!(changes.len(), 2);

        assert!(matches!(changes[1], Change::Removed(_)));
        assert!(matches!(changes[0], Change::Added(_)));
    }

    #[test]
    fn test_changed_whitespace() {
        let input1 = "(a) ";
        let input2 = "(a)";
        let (syn1, errors1) = parse(input1);
        let (syn2, errors2) = parse(input2);

        assert_eq!(errors1.len(), 0);
        assert_eq!(errors2.len(), 0);

        let changes = syn1.compare(&syn2, &[Span::new(Point::new(0, 3), Point::new(0, 3))]);
        assert!(changes.is_empty());
    }

    #[test]
    fn test_changed_comment() {
        let input1 = "(a) ; comment";
        let input2 = "(a)";
        let (syn1, errors1) = parse(input1);
        let (syn2, errors2) = parse(input2);

        assert_eq!(errors1.len(), 0);
        assert_eq!(errors2.len(), 0);

        let changes = syn1.compare(&syn2, &[Span::new(Point::new(0, 4), Point::new(0, 4))]);
        assert!(changes.is_empty());
    }

    #[test]
    fn test_adjusted_span() {
        let input1 = "(a)";
        let input2 = "(a)(b)";
        let (syn1, errors1) = parse(input1);
        let (syn2, errors2) = parse(input2);

        assert_eq!(errors1.len(), 0);
        assert_eq!(errors2.len(), 0);

        let changes = syn1.compare(&syn2, &[Span::new(Point::new(0, 0), Point::new(0, 1))]);
        assert_eq!(changes.len(), 1);
        matches!(changes[0], Change::Added(_));
    }

    #[test]
    fn test_id() {
        let input1 = "a";
        let input2 = "ab";
        let (syn1, _) = parse(input1);
        let (syn2, _) = parse(input2);

        let changes = syn1.compare(&syn2, &[Span::new(Point::new(0, 1), Point::new(0, 1))]);
        assert_eq!(changes.len(), 2);
    }
}
