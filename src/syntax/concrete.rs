use crate::span::Spanned;

use super::token::Token;

pub struct StringNode {
    pub token: Token,
}

pub struct AtomNode {
    pub token: Token,
}

pub struct NumberNode {
    pub token: Token,
}

pub struct ListNode {
    pub lparens: Token,
    pub items: Vec<Expr>,
    pub rparens: Token,
}

pub struct VecNode {
    pub lbracket: Token,
    pub items: Vec<Expr>,
    pub rbracket: Token,
}

pub enum ExprNode {
    Atom(AtomNode),
    Number(NumberNode),
    String(StringNode),
    List(ListNode),
}

pub type Expr = Spanned<ExprNode>;
