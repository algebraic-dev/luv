//! This module defines an untyped concrete syntax tree, which stores every piece of data for easy
//! manipulation with an LSP.

use core::fmt;
use std::collections::HashSet;
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
    Atom,
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
            SyntaxKind::Atom => write!(f, "atom"),
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

    /// Creates an empty ROOT syntax node.
    pub fn empty() -> Self {
        Self {
            kind: SyntaxKind::Root,
            children: vec![],
            span: Span::empty(),
            hash: 0,
        }
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
    pub fn compare_hashes<'a>(&'a self, other: &'a SyntaxNode) -> Vec<Change<'a>> {
        let mut changes = Vec::new();

        let a_nodes = filter_top_level_children(self).collect::<HashSet<_>>();
        let b_nodes = filter_top_level_children(other).collect::<HashSet<_>>();

        for value in b_nodes.difference(&a_nodes) {
            changes.push(Change::Added(vec![value]));
        }

        for value in a_nodes.difference(&b_nodes) {
            changes.push(Change::Removed(vec![value]));
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
            tree = tree.with(child.to_tree());
        }

        tree
    }
}

impl PrettyPrint for &SyntaxNode {
    fn to_tree(&self) -> Tree {
        (*self).to_tree()
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
    Added(Vec<&'a SyntaxNode>),
    Removed(Vec<&'a SyntaxNode>),
}

impl<'a> PrettyPrint for Change<'a> {
    fn to_tree(&self) -> Tree {
        match self {
            Change::Added(node) => Tree::label("Added").add_child(node),
            Change::Removed(node) => Tree::label("Removed").add_child(node),
        }
    }
}

fn filter_top_level_children(node: &SyntaxNode) -> impl Iterator<Item = &SyntaxNode> {
    node.nodes()
        .filter(|child| !matches!(child.kind(), SyntaxKind::Whitespace | SyntaxKind::Comment))
}
