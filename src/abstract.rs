//! This module provides a wrapper around the CST (Concrete Syntax Tree) to offer a higher-level abstraction
//! with verified types and utility functions for working with syntax elements.

use crate::errors::Error;
use crate::span::Span;
use crate::syntax::{SyntaxElement, SyntaxKind, SyntaxNode, SyntaxToken};

type Result<T> = std::result::Result<T, Error>;

macro_rules! def_token {
    ($name:ident, $method:ident, $kind:expr) => {
        /// Represents a token.
        pub struct $name<'a>(&'a SyntaxNode);

        impl<'a> $name<'a> {
            /// Creates an instance of the type from a `SyntaxNode`.
            /// Returns an error if the node kind does not match kind.
            pub fn from_node(node: &'a SyntaxNode) -> Result<Self> {
                if node.kind != $kind {
                    return Err(Error::new(
                        format!("expected {} but got {}", $kind, node.kind),
                        node.span.clone(),
                    ));
                }

                Ok($name(node))
            }

            /// Retrieves the text associated with the token.
            pub fn $method(&self) -> Result<&str> {
                extract_token_text(&self.0, $kind)
            }

            /// Returns the span of the token.
            pub fn span(&self) -> Span {
                self.0.span.clone()
            }
        }
    };
}

/// Extracts the text of a token if its kind matches the expected kind.
pub fn extract_token_text(node: &SyntaxNode, expected_kind: SyntaxKind) -> Result<&str> {
    if let Some(token) = node.first_token() {
        if expected_kind == token.kind {
            return Ok(&token.text);
        }
    }

    Err(Error::new(
        format!("Expected a {} but got something else.", expected_kind),
        node.span.clone(),
    ))
}

/// Checks if a `SyntaxNode` has the expected kind.
pub fn validate_node_kind(node: &SyntaxNode, expected_kind: SyntaxKind) -> Result<()> {
    if let Some(SyntaxNode { kind, .. }) = node.nodes().next() {
        if expected_kind == *kind {
            return Ok(());
        }
    }

    Err(Error::new(
        format!("Expected a {} but got something else.", expected_kind),
        node.span.clone(),
    ))
}

/// Asserts that the given `SyntaxElement` is a `SyntaxNode`.
pub fn assert_node_element(element: &SyntaxElement) -> Result<&SyntaxNode> {
    match element {
        SyntaxElement::Node(node) => Ok(node),
        SyntaxElement::Token(token) => Err(Error::new(
            "Expected a node but found a token.",
            token.span.clone(),
        )),
    }
}

/// Asserts that the given `SyntaxElement` is a `SyntaxToken`.
pub fn assert_token_element(element: &SyntaxElement) -> Result<&SyntaxToken> {
    match element {
        SyntaxElement::Token(token) => Ok(token),
        SyntaxElement::Node(node) => Err(Error::new(
            "Expected a token but found a node.",
            node.span.clone(),
        )),
    }
}

/// Asserts that the node is an `Identifier` token and matches the expected keyword.
pub fn assert_keyword_node(node: &SyntaxNode, expected_keyword: &str) -> Result<()> {
    let token = Identifier::from_node(node)?;
    let text = token.text()?;

    if text == expected_keyword {
        Ok(())
    } else {
        Err(Error::new(
            format!(
                "Expected keyword `{}` but found `{}`.",
                expected_keyword, text
            ),
            token.span(),
        ))
    }
}

// Basics

def_token!(Identifier, text, SyntaxKind::Identifier);

def_token!(Number, number, SyntaxKind::Number);

def_token!(Str, string, SyntaxKind::String);

/// Represents a list of syntax nodes.
pub struct NodeList<'a>(usize, &'a SyntaxNode);

impl<'a> NodeList<'a> {
    /// Creates a `NodeList` from a `SyntaxNode`.
    /// Returns an error if the node kind is not `SyntaxKind::List`.
    pub fn from_node(node: &'a SyntaxNode) -> Result<Self> {
        if node.kind != SyntaxKind::List {
            return Err(Error::new(
                "Expected a list but got something else.".to_owned(),
                node.span.clone(),
            ));
        }

        Ok(NodeList(0, node))
    }

    /// Checks if the list is empty.
    pub fn is_empty(&self) -> bool {
        self.0 >= self.1.children.len()
    }

    /// Retrieves the next syntax node in the list, skipping tokens.
    pub fn bump(&mut self) -> Option<&'a SyntaxNode> {
        while self.0 < self.1.children.len() {
            let child = self.1.children.get(self.0).unwrap();
            match child {
                SyntaxElement::Node(node) => {
                    self.0 += 1;
                    return Some(node);
                }
                SyntaxElement::Token(_) => {
                    self.0 += 1;
                }
            }
        }
        None
    }

    /// Resets the iterator to the beginning of the list.
    pub fn reset(&mut self) {
        self.0 = 0;
    }

    /// Retrieves the next syntax node in the list, expecting a specific error message.
    pub fn next_with_error(&mut self, error_message: &str) -> Result<&'a SyntaxNode> {
        self.bump()
            .ok_or_else(|| Error::new(error_message.to_string(), self.1.span.clone()))
    }

    /// Helper function to retrieve the next node and convert it using a provided closure.
    pub fn retrieve_optional<T>(
        &mut self,
        convert: impl FnOnce(&'a SyntaxNode) -> Result<T>,
    ) -> Result<Option<T>> {
        if let Some(node) = self.bump() {
            let result = convert(node)?;
            Ok(Some(result))
        } else {
            Ok(None)
        }
    }
}

/// Represents a quoted syntax node.
pub struct Quote<'a>(&'a SyntaxNode);

impl<'a> Quote<'a> {
    /// Creates a `Quote` from a `SyntaxNode`.
    /// Returns an error if the node kind is not `SyntaxKind::Quote`.
    pub fn from_node(node: &'a SyntaxNode) -> Result<Self> {
        if node.kind != SyntaxKind::Quote {
            return Err(Error::new(
                "Expected a quote but got something else.".to_owned(),
                node.span.clone(),
            ));
        }

        Ok(Quote(node))
    }

    /// Retrieves the first child node of the quote.
    /// Returns an error if the node does not have any children.
    pub fn node(&self) -> Result<SyntaxNode> {
        self.0
            .nodes()
            .nth(0)
            .ok_or_else(|| Error::new("Expected a child node.".to_owned(), self.0.span.clone()))
            .cloned()
    }
}

/// Represents the parameters of a function.
pub struct Params<'a>(NodeList<'a>);

impl<'a> Params<'a> {
    /// Creates a `Params` from a `SyntaxNode`.
    /// Returns an error if the node kind is not `SyntaxKind::List`.
    pub fn from_node(node: &'a SyntaxNode) -> Result<Self> {
        Ok(Self(NodeList::from_node(node)?))
    }

    /// Retrieves the function name from the parameters.
    pub fn name(&'a mut self) -> Result<Option<Identifier<'a>>> {
        if let Some(res) = self.0.bump() {
            let id = Identifier::from_node(res)?;
            Ok(Some(id))
        } else {
            Ok(None)
        }
    }
}

/// Represents a function definition.
pub struct Def<'a>(NodeList<'a>);

impl<'a> Def<'a> {
    /// Creates a `Def` from a `SyntaxNode` representing a list.
    pub fn from_node(node: &'a SyntaxNode) -> Result<Self> {
        Ok(Self(NodeList::from_node(node)?))
    }

    /// Creates an instance of a List
    pub fn from_list(syn: NodeList<'a>) -> Self {
        Self(syn)
    }

    /// Retrieves the span of the definition.
    pub fn span(&self) -> Span {
        self.0 .1.span.clone()
    }

    /// Retrieves the definition name.
    /// Returns an error if the name is not found.
    pub fn name(&mut self) -> Result<Identifier<'a>> {
        let name_node = self.0.next_with_error("expected definition name")?;
        Identifier::from_node(name_node)
    }

    /// Retrieves the value of the definition, if available.
    pub fn value(&mut self) -> Result<Stmt<'a>> {
        let name_node = self.0.next_with_error("expected value of the definition")?;
        Stmt::from_node(name_node)
    }
}

/// Represents a function definition.
pub struct Defn<'a>(NodeList<'a>);

impl<'a> Defn<'a> {
    /// Creates a `Defn` from a `SyntaxNode` representing a list.
    pub fn from_node(node: &'a SyntaxNode) -> Result<Self> {
        Ok(Self(NodeList::from_node(node)?))
    }

    /// Creates an instance of a List
    pub fn from_list(syn: NodeList<'a>) -> Self {
        Self(syn)
    }

    /// Retrieves the span of the function definition.
    pub fn span(&self) -> Span {
        self.0 .1.span.clone()
    }

    /// Retrieves the function name from the definition.
    /// Returns an error if the name is not found.
    pub fn name(&mut self) -> Result<Identifier<'a>> {
        let name_node = self.0.next_with_error("expected function name")?;
        Identifier::from_node(name_node)
    }

    /// Retrieves the parameters from the function definition.
    /// Returns an error if parameters are not found.
    pub fn parameters(&mut self) -> Result<Params<'a>> {
        let params_node = self.0.next_with_error("expected parameters")?;
        Params::from_node(params_node)
    }

    /// Retrieves the body of the function definition.
    /// Returns an error if the body is not found.
    pub fn body(&mut self) -> Result<Option<Stmt<'a>>> {
        self.0.retrieve_optional(Stmt::from_node)
    }
}

/// Represents an `eval` statement.
pub struct Eval<'a>(NodeList<'a>);

impl<'a> Eval<'a> {
    /// Creates an `Eval` instance from a `List`.
    pub fn from_list(syn: NodeList<'a>) -> Self {
        Self(syn)
    }

    /// Retrieves the span of the `Eval` node.
    pub fn span(&self) -> Span {
        self.0 .1.span.clone()
    }

    /// Retrieves the statement from the `Eval` node.
    pub fn stmt(&mut self) -> Result<Stmt<'a>> {
        let node = self.0.next_with_error("expected statement")?;
        Stmt::from_node(node)
    }
}

/// Represents a `set-option` statement.
pub struct SetOption<'a>(NodeList<'a>);

impl<'a> SetOption<'a> {
    /// Creates a `SetOption` instance from a `List`.
    pub fn from_list(syn: NodeList<'a>) -> Self {
        Self(syn)
    }

    /// Retrieves the name from the `SetOption` node.
    pub fn name(&mut self) -> Result<Identifier<'a>> {
        let name_node = self.0.next_with_error("expected option name")?;
        Identifier::from_node(name_node)
    }

    /// Retrieves the statement from the `SetOption` node.
    pub fn stmt(&mut self) -> Result<Stmt<'a>> {
        let stmt_node = self.0.next_with_error("expected statement")?;
        Stmt::from_node(stmt_node)
    }
}

/// Represents a `require` statement.
pub struct Require<'a>(NodeList<'a>);

impl<'a> Require<'a> {
    /// Creates a `Require` instance from a `List`.
    pub fn from_list(syn: NodeList<'a>) -> Self {
        Self(syn)
    }

    /// Retrieves the module name from the `Require` node.
    pub fn name(&mut self) -> Result<Str<'a>> {
        let name_node = self.0.next_with_error("expected a module name")?;
        Str::from_node(name_node)
    }
}

/// Represents different top-level constructs in the syntax.
pub enum TopLevel<'a> {
    Def(Def<'a>),
    Defn(Defn<'a>),
    Eval(Eval<'a>),
    SetOption(SetOption<'a>),
    Require(Require<'a>),
}

impl<'a> TopLevel<'a> {
    /// Creates a `TopLevel` variant from a `SyntaxNode`.
    pub fn from_node(syn: &'a SyntaxNode) -> Result<Self> {
        let mut list = NodeList::from_node(syn)?;
        let syn_node = list.next_with_error("expected a function name")?;
        let kw = Identifier::from_node(syn_node)?;
        let text = kw.text()?;

        match text {
            "def" => Ok(TopLevel::Def(Def::from_list(list))),
            "defn" => Ok(TopLevel::Defn(Defn::from_list(list))),
            "eval" => Ok(TopLevel::Eval(Eval::from_list(list))),
            "set-option" => Ok(TopLevel::SetOption(SetOption::from_list(list))),
            "require" => Ok(TopLevel::Require(Require::from_list(list))),
            _ => Err(Error::new(
                "unexpected keyword".to_owned(),
                syn.span.clone(),
            )),
        }
    }
}

/// Represents an `if` expression.
pub struct If<'a>(NodeList<'a>);

impl<'a> If<'a> {
    /// Creates an `If` instance from a `List`.
    pub fn from_list(syn: NodeList<'a>) -> Self {
        Self(syn)
    }

    /// Retrieves the condition expression from the `If` node.
    pub fn cond(&mut self) -> Result<Expr<'a>> {
        let node = self.0.next_with_error("expected condition expression")?;
        Expr::from_node(node)
    }

    /// Retrieves the `then` expression from the `If` node.
    pub fn then(&mut self) -> Result<Expr<'a>> {
        let node = self.0.next_with_error("expected then expression")?;
        Expr::from_node(node)
    }

    /// Retrieves the `else` expression from the `If` node.
    pub fn else_(&mut self) -> Result<Expr<'a>> {
        let node = self.0.next_with_error("expected else expression")?;
        Expr::from_node(node)
    }
}

/// Represents a block of statements.
pub struct Block<'a>(NodeList<'a>);

impl<'a> Block<'a> {
    /// Creates a `Block` instance from a `List`.
    pub fn from_list(syn: NodeList<'a>) -> Self {
        Self(syn)
    }

    /// Retrieves the span of the `Block` node.
    pub fn span(&self) -> Span {
        self.0 .1.span.clone()
    }

    /// Retrieves the body of the block.
    pub fn body(&mut self) -> Result<Option<Stmt<'a>>> {
        self.0.retrieve_optional(Stmt::from_node)
    }
}

/// Represents a function definition.
pub struct Fn<'a>(NodeList<'a>);

impl<'a> Fn<'a> {
    /// Creates a `Fn` instance from a `List`.
    pub fn from_list(syn: NodeList<'a>) -> Self {
        Self(syn)
    }

    /// Retrieves the span of the `Fn` node.
    pub fn span(&self) -> Span {
        self.0 .1.span.clone()
    }

    /// Retrieves the parameters of the function.
    pub fn params(&mut self) -> Result<Params<'a>> {
        let node = self.0.next_with_error("expected function parameters")?;
        Params::from_node(node)
    }

    /// Retrieves the body of the function.
    pub fn body(&mut self) -> Result<Option<Stmt<'a>>> {
        self.0.retrieve_optional(Stmt::from_node)
    }
}

/// Represents a function application.
pub struct App<'a>(NodeList<'a>);

impl<'a> App<'a> {
    /// Creates an `App` instance from a `List`.
    pub fn from_list(syn: NodeList<'a>) -> Self {
        Self(syn)
    }

    /// Retrieves the function name from the `App` node.
    pub fn name(&mut self) -> Result<Identifier<'a>> {
        let name_node = self.0.next_with_error("expected function name")?;
        Identifier::from_node(name_node)
    }

    /// Retrieves the argument for the function application, if present.
    pub fn argument(&mut self) -> Result<Option<Expr<'a>>> {
        self.0.retrieve_optional(Expr::from_node)
    }
}

/// Represents various types of expressions in the syntax.
pub enum Expr<'a> {
    If(If<'a>),
    Fn(Fn<'a>),
    App(App<'a>),
    Block(Block<'a>),
    Quote(Quote<'a>),
    Identifier(Identifier<'a>),
    Number(Number<'a>),
    Str(Str<'a>),
}

impl<'a> Expr<'a> {
    /// Creates an `Expr` variant from a `SyntaxNode`.
    pub fn from_node(syn: &'a SyntaxNode) -> Result<Self> {
        match syn.kind {
            SyntaxKind::Identifier => return Ok(Expr::Identifier(Identifier::from_node(syn)?)),
            SyntaxKind::Number => return Ok(Expr::Number(Number::from_node(syn)?)),
            SyntaxKind::String => return Ok(Expr::Str(Str::from_node(syn)?)),
            SyntaxKind::Quote => return Ok(Expr::Quote(Quote::from_node(syn)?)),
            _ => (),
        };

        let mut list = NodeList::from_node(syn)?;

        let syn = list.next_with_error("expected an expression type.")?;
        let kw = Identifier::from_node(syn)?;
        let text = kw.text()?;

        match text {
            "if" => Ok(Expr::If(If::from_list(list))),
            "fn" => Ok(Expr::Fn(Fn::from_list(list))),
            "block" => Ok(Expr::Block(Block::from_list(list))),
            _ => {
                list.reset();
                Ok(Expr::App(App::from_list(list)))
            }
        }
    }
}

/// Represents a `let` statement.
pub struct Let<'a>(NodeList<'a>);

impl<'a> Let<'a> {
    /// Creates a `Let` instance from a `List`.
    pub fn from_list(syn: NodeList<'a>) -> Self {
        Self(syn)
    }

    /// Retrieves the variable name from the `Let` node.
    pub fn name(&mut self) -> Result<Identifier<'a>> {
        let name_node = self.0.next_with_error("expected variable name")?;
        Identifier::from_node(name_node)
    }

    /// Retrieves the value expression from the `Let` node.
    pub fn value(&mut self) -> Result<Expr<'a>> {
        let value_node = self.0.next_with_error("expected value expression")?;
        Expr::from_node(value_node)
    }
}

/// Represents a statement in the syntax.
pub enum Stmt<'a> {
    Let(Let<'a>),
    Expr(Expr<'a>),
}

impl<'a> Stmt<'a> {
    /// Creates a `Stmt` variant from a `SyntaxNode`.
    pub fn from_node(syn: &'a SyntaxNode) -> Result<Self> {
        match syn.kind {
            SyntaxKind::Identifier
            | SyntaxKind::Number
            | SyntaxKind::String
            | SyntaxKind::Quote => return Ok(Stmt::Expr(Expr::from_node(syn)?)),
            _ => (),
        };

        let mut list = NodeList::from_node(syn)?;
        let syn_node = list.next_with_error("expected a statement type")?;
        let kw = Identifier::from_node(syn_node)?;
        let text = kw.text()?;

        match text {
            "let" => Ok(Stmt::Let(Let::from_list(list))),
            _ => Ok(Stmt::Expr(Expr::from_node(list.1)?)),
        }
    }
}
