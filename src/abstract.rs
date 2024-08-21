use std::collections::HashSet;

use crate::{
    id,
    span::{Span, Spanned},
};

/// A spanned string type, representing text with associated position information.
pub type Text = Spanned<String>;

/// Represents different kinds of literals that can be used in expressions.
pub enum LiteralKind {
    String,
    Number,
    Atom,
}

/// A spanned literal kind, representing a literal with associated position information.
pub type Literal = Spanned<LiteralKind>;

/// Represents different kinds of expressions in the language.
pub enum ExprKind {
    Let(LetExpr),
    If(IfExpr),
    Call(CallExpr),
    Lambda(LambdaExpr),
    Literal(Literal),
    Identifier(Text),
    Local(Text),
    Reference(id::Id<id::File>, Text),
    Error,
}

/// A spanned expression kind, representing an expression with associated position information.
pub type Expr = Spanned<ExprKind>;

/// Represents a `let` expression, which binds values to names within an expression.
pub struct LetExpr {
    pub name: Text,
    pub body: Box<Expr>,
}

/// Represents an `if` expression, which conditionally executes one of two branches.
pub struct IfExpr {
    pub condition: Box<Expr>,
    pub true_branch: Box<Expr>,
    pub false_branch: Box<Expr>,
}

/// Represents a function or method call.
pub struct CallExpr {
    pub callee: Text,
    pub local: Option<id::Id<id::File>>,
    pub arguments: Vec<Expr>,
}

/// Represents a lambda expression, which is an anonymous function.
pub struct LambdaExpr {
    pub parameters: Vec<String>,
    pub body: Vec<Expr>,
}

/// Represents different kinds of top-level forms in the language.
pub enum TopLevelKind {
    Require(Require),
    Defn(Defn),
    Def(Def),
    Eval(Eval),
    External(External),
    Error,
}

/// A spanned top-level kind, representing a top-level form with associated position information.
pub struct TopLevel {
    pub span: Span,
    pub data: TopLevelKind,
    pub refs: HashSet<(id::Id<id::File>, String)>,
}

impl TopLevel {
    pub fn new(data: TopLevelKind, span: Span) -> TopLevel {
        TopLevel { span, data, refs: Default::default() }
    }
}

/// Represents a `require` form, used to include external libraries or modules.
pub struct Require {
    pub name: Text,
    pub options: Vec<(Text, Text)>,
}

/// Represents a `defn` form, used to define a named function.
pub struct Defn {
    pub name: Text,
    pub params: Vec<Text>,
    pub body: Vec<Expr>,
}

/// Represents a `require` form, used to define a external function.
pub struct External {
    pub name: Text,
    pub params: Vec<Text>,
    pub body: Text,
}

/// Represents a `def` form, used to define a named variable or constant.
pub struct Def {
    pub name: Text,
    pub value: Box<Expr>,
}

/// Represents an `eval` form, used to evaluate an expression.
pub struct Eval {
    pub expr: Vec<Expr>,
}

/// Program
pub struct Program {
    pub vec: Vec<TopLevel>,
    pub span: Span,
}
