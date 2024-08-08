//! This module defines an untyped concrete syntax tree. Its useful because it stores every single
//! piece of data that the code has in order to make it easy to work with a LSP.

use core::fmt;
use std::collections::HashMap;
use std::hash::Hash;
use std::{
    hash::{DefaultHasher, Hasher},
    slice::Iter,
};

use crate::span::Span;

/// The identifier of a [SyntaxNode], its used as a lightweight way to compare different nodes.
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Id(Span, u64);

use super::syntax::SyntaxKind;

/// The syntax node express an artificial boundary in the tokens creating a syntatic meaning on them.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SyntaxNode {
    pub kind: SyntaxKind,
    pub children: Vec<SyntaxNodeOrToken>,
    pub span: Span,
    pub hash: u64,
}

impl SyntaxNode {
    pub fn get_id(&self) -> Id {
        Id(self.span.clone(), self.hash)
    }
}

impl Hash for SyntaxNode {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.hash.hash(state);
    }
}

impl SyntaxNode {
    // Constructor that computes and stores the hash
    pub fn new(kind: SyntaxKind, children: Vec<SyntaxNodeOrToken>, span: Span) -> Self {
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

    pub fn to_map(&self) -> HashMap<(u64, Span), SyntaxNode> {
        self.clone()
            .get_nodes()
            .map(|x| ((x.hash, x.span.clone()), x))
            .collect()
    }
}

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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SyntaxNodeOrToken {
    Node(SyntaxNode),
    Token(SyntaxToken),
}

impl Hash for SyntaxNodeOrToken {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            SyntaxNodeOrToken::Node(node) => {
                0.hash(state); // Unique tag for Node
                node.hash.hash(state); // Use stored hash
            }
            SyntaxNodeOrToken::Token(token) => {
                1.hash(state); // Unique tag for Token
                token.hash.hash(state); // Use stored hash
            }
        }
    }
}

impl SyntaxNodeOrToken {
    pub fn kind(&self) -> SyntaxKind {
        match self {
            SyntaxNodeOrToken::Node(n) => n.kind,
            SyntaxNodeOrToken::Token(t) => t.kind,
        }
    }

    pub fn hash_value(&self) -> u64 {
        match self {
            SyntaxNodeOrToken::Node(n) => n.hash,
            SyntaxNodeOrToken::Token(t) => t.hash,
        }
    }
}

#[derive(Debug)]
pub struct GreenNodeBuilder {
    stack: Vec<(SyntaxKind, Vec<SyntaxNodeOrToken>, Span)>,
}

impl GreenNodeBuilder {
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
        let span = start_span.mix(end_span);

        let node = SyntaxNodeOrToken::Node(SyntaxNode::new(kind, children, span.clone()));

        if let Some((_, parent, _)) = self.stack.last_mut() {
            parent.push(node);
        } else {
            self.stack.push((kind, vec![node], span));
        }
    }

    /// Adds a token to the current node being built.
    pub fn token(&mut self, kind: SyntaxKind, text: &str, span: Span) {
        let token = SyntaxNodeOrToken::Token(SyntaxToken::new(kind, text.to_string(), span));

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

impl SyntaxNode {
    /// Returns the kind of the syntax node.
    pub fn kind(&self) -> SyntaxKind {
        self.kind
    }

    /// Returns the children of the syntax node.
    pub fn children(&self) -> Iter<'_, SyntaxNodeOrToken> {
        self.children.iter()
    }

    pub fn nodes(&self) -> impl Iterator<Item = &SyntaxNode> {
        self.children.iter().filter_map(|p| {
            if let SyntaxNodeOrToken::Node(n) = p {
                Some(n)
            } else {
                None
            }
        })
    }

    pub fn get_nodes(self) -> impl Iterator<Item = SyntaxNode> {
        self.children.into_iter().filter_map(|p| {
            if let SyntaxNodeOrToken::Node(n) = p {
                Some(n)
            } else {
                None
            }
        })
    }

    pub fn tokens(&self) -> impl Iterator<Item = &SyntaxToken> {
        self.children.iter().filter_map(|p| {
            if let SyntaxNodeOrToken::Token(n) = p {
                Some(n)
            } else {
                None
            }
        })
    }

    pub fn first_token(&self) -> Option<&SyntaxToken> {
        self.tokens().nth(0)
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
                SyntaxNodeOrToken::Node(node) => {
                    node.pretty_print(f, &new_prefix, is_last_child).unwrap();
                }
                SyntaxNodeOrToken::Token(token) => {
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

impl fmt::Display for SyntaxNodeOrToken {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            SyntaxNodeOrToken::Node(tkn) => tkn.pretty_print(f, "", true),
            SyntaxNodeOrToken::Token(tkn) => tkn.pretty_print(f, "", true),
        }
    }
}
