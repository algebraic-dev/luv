use crate::span::{Span, Spanned};

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
    Error
}

/// A spanned expression kind, representing an expression with associated position information.
pub type Expr = Spanned<ExprKind>;

/// Represents a `let` expression, which binds values to names within an expression.
pub struct LetExpr {
    pub name: Text,
    pub body: Box<Expr>
}

/// Represents an `if` expression, which conditionally executes one of two branches.
pub struct IfExpr {
    pub condition: Box<Expr>,
    pub true_branch: Box<Expr>,
    pub false_branch: Box<Expr>,
}

pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div
}

/// Represents a binary operation, such as addition or multiplication.
pub struct BinaryExpr {
    pub operator: BinaryOp,
    pub lhs: Box<Expr>,
}

/// Represents a function or method call.
pub struct CallExpr {
    pub callee: Box<Expr>,
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
    Error,
}

/// A spanned top-level kind, representing a top-level form with associated position information.
pub type TopLevel = Spanned<TopLevelKind>;

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
    pub span: Span
}