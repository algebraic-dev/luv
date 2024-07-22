use crate::span::Spanned;

use super::token::Token;

#[derive(Debug)]
pub struct StringNode {
    pub token: Token,
}

#[derive(Debug)]
pub struct AtomNode {
    pub token: Token,
}

#[derive(Debug)]
pub struct NumberNode {
    pub token: Token,
}

#[derive(Debug)]
pub struct ListNode {
    pub lparens: Token,
    pub items: Vec<Expr>,
    pub rparens: Token,
}

#[derive(Debug)]
pub struct VecNode {
    pub lbracket: Token,
    pub items: Vec<Expr>,
    pub rbracket: Token,
}

#[derive(Debug)]
pub enum ExprNode {
    Atom(AtomNode),
    Number(NumberNode),
    String(StringNode),
    List(ListNode),
    Vec(VecNode),
}

pub type Expr = Spanned<ExprNode>;
