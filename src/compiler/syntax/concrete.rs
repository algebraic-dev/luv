//! A module for handling various types of expression nodes and their associated tokens.

use super::token::Token;
use crate::span::Spanned;

/// Represents a string node with an associated token.
#[derive(Debug)]
pub struct StringNode {
    pub token: Token,
}

impl StringNode {
    /// Creates a new `StringNode` with the given token.
    pub fn new(token: Token) -> Self {
        Self { token }
    }
}

/// Represents an atom node with an associated token.
#[derive(Debug)]
pub struct AtomNode {
    pub token: Token,
}

impl AtomNode {
    /// Creates a new `AtomNode` with the given token.
    pub fn new(token: Token) -> Self {
        Self { token }
    }
}

/// Represents a number node with an associated token.
#[derive(Debug)]
pub struct NumberNode {
    pub token: Token,
}

impl NumberNode {
    /// Creates a new `NumberNode` with the given token.
    pub fn new(token: Token) -> Self {
        Self { token }
    }
}

/// Represents a list node with parentheses tokens and a list of expressions.
#[derive(Debug)]
pub struct ListNode {
    pub lparens: Token,
    pub items: Vec<Expr>,
    pub rparens: Token,
}

impl ListNode {
    /// Creates a new `ListNode` with the given tokens and items.
    pub fn new(lparens: Token, items: Vec<Expr>, rparens: Token) -> Self {
        Self {
            lparens,
            items,
            rparens,
        }
    }
}

/// Represents a vector node with bracket tokens and a list of expressions.
#[derive(Debug)]
pub struct VecNode {
    pub lbracket: Token,
    pub items: Vec<Expr>,
    pub rbracket: Token,
}

impl VecNode {
    /// Creates a new `VecNode` with the given tokens and items.
    pub fn new(lbracket: Token, items: Vec<Expr>, rbracket: Token) -> Self {
        Self {
            lbracket,
            items,
            rbracket,
        }
    }
}

/// Enumerates the possible types of expression nodes.
#[derive(Debug)]
pub enum ExprNode {
    Atom(AtomNode),
    Number(NumberNode),
    String(StringNode),
    List(ListNode),
    Vec(VecNode),
}

impl ExprNode {
    /// Creates a new `ExprNode` for an atom.
    pub fn new_atom(token: Token) -> Self {
        ExprNode::Atom(AtomNode::new(token))
    }

    /// Creates a new `ExprNode` for a number.
    pub fn new_number(token: Token) -> Self {
        ExprNode::Number(NumberNode::new(token))
    }

    /// Creates a new `ExprNode` for a string.
    pub fn new_string(token: Token) -> Self {
        ExprNode::String(StringNode::new(token))
    }

    /// Creates a new `ExprNode` for a list.
    pub fn new_list(lparens: Token, items: Vec<Expr>, rparens: Token) -> Self {
        ExprNode::List(ListNode::new(lparens, items, rparens))
    }

    /// Creates a new `ExprNode` for a vector.
    pub fn new_vec(lbracket: Token, items: Vec<Expr>, rbracket: Token) -> Self {
        ExprNode::Vec(VecNode::new(lbracket, items, rbracket))
    }
}

/// Type alias for a spanned expression node.
pub type Expr = Spanned<ExprNode>;
