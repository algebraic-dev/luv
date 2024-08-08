//! Wrapper around the CST for a better abstraction with verified things.

use crate::span::{Span, Spanned};

use super::{
    concrete::{SyntaxNode, SyntaxNodeOrToken, SyntaxToken},
    syntax::SyntaxKind,
};

type Result<T> = std::result::Result<T, Spanned<String>>;

macro_rules! def_token {
    ($name:ident, $method:ident, $kind:expr) => {
        pub struct $name(SyntaxNode);

        impl $name {
            pub fn from_node(node: SyntaxNode) -> Result<Self> {
                if node.kind != $kind {
                    return Err(Spanned::new(
                        format!("expected {} but got {}", $kind, node.kind),
                        node.span.clone(),
                    ));
                }

                Ok($name(node))
            }

            pub fn $method(&self) -> Result<&str> {
                check_token(&self.0, $kind)
            }

            pub fn span(&self) -> Span {
                self.0.span.clone()
            }
        }
    };
}

pub fn check_token(node: &SyntaxNode, kind: SyntaxKind) -> Result<&str> {
    if let Some(SyntaxToken {
        kind: tkn, text, ..
    }) = node.first_token()
    {
        if kind == *tkn {
            return Ok(text);
        }
    }

    Err(Spanned::new(
        format!("expected a {}.", kind),
        node.span.clone(),
    ))
}

pub fn check_node(node: SyntaxNode, kind: SyntaxKind) -> Result<()> {
    if let Some(SyntaxNode { kind: tkn, .. }) = node.nodes().nth(0) {
        if kind == *tkn {
            return Ok(());
        }
    }

    Err(Spanned::new(
        format!("expected a {}.", kind),
        node.span.clone(),
    ))
}

pub fn assert_node(not: SyntaxNodeOrToken) -> Result<SyntaxNode> {
    match not {
        SyntaxNodeOrToken::Node(node) => Ok(node),
        SyntaxNodeOrToken::Token(token) => {
            Err(Spanned::new(format!("unexpected."), token.span.clone()))
        }
    }
}

pub fn assert_token(not: SyntaxNodeOrToken) -> Result<SyntaxToken> {
    match not {
        SyntaxNodeOrToken::Token(node) => Ok(node),
        SyntaxNodeOrToken::Node(token) => {
            Err(Spanned::new(format!("unexpected."), token.span.clone()))
        }
    }
}

pub fn assert_keyword(node: SyntaxNode, key: &str) -> Result<()> {
    let span = node.span.clone();
    let token = Identifier::from_node(node)?;
    let text = token.text()?;

    if text == key {
        Ok(())
    } else {
        Err(Spanned::new(format!("expected keyword {key}."), span))
    }
}

// Basics

def_token!(Identifier, text, SyntaxKind::Identifier);

def_token!(Number, number, SyntaxKind::Number);

def_token!(Str, string, SyntaxKind::String);

pub struct List(pub usize, pub SyntaxNode);

impl List {
    pub fn from_node(node: SyntaxNode) -> Result<Self> {
        if node.kind != SyntaxKind::List {
            return Err(Spanned::new(
                "expected a list.".to_owned(),
                node.span.clone(),
            ));
        }

        Ok(List(0, node))
    }

    pub fn is_empty(&self) -> bool {
        self.0 >= self.1.children.len()
    }

    pub fn next(&mut self, err: &str) -> Result<SyntaxNode> {
        while self.0 < self.1.children.len() {
            let n = self.1.children.get(self.0).unwrap();
            match n {
                SyntaxNodeOrToken::Node(n) => {
                    self.0 += 1;
                    return Ok(n.clone());
                }
                SyntaxNodeOrToken::Token(_) => {
                    self.0 += 1;
                }
            }
        }
        return Err(Spanned::new(err.to_string(), self.1.span.clone()));
    }

    pub fn reset(&mut self) {
        self.0 = 0;
    }
}

pub struct Quote(SyntaxNode);

impl Quote {
    pub fn from_node(node: SyntaxNode) -> Result<Self> {
        if node.kind != SyntaxKind::Quote {
            return Err(Spanned::new(
                "expected a quote.".to_owned(),
                node.span.clone(),
            ));
        }

        Ok(Quote(node))
    }

    pub fn node(&self) -> Result<SyntaxNode> {
        self.0
            .nodes()
            .nth(0)
            .ok_or_else(|| {
                Spanned::new("expected a `def` keyword.".to_owned(), self.0.span.clone())
            })
            .cloned()
    }
}

pub struct Params(List);

impl Params {
    pub fn from_node(syn: SyntaxNode) -> Result<Self> {
        Ok(Self(List::from_node(syn)?))
    }

    pub fn name(&mut self) -> Result<Identifier> {
        let name = self.0.next("expected function name")?;
        Identifier::from_node(name)
    }

    pub fn names(&mut self) -> Vec<Result<Identifier>> {
        let mut vec = Vec::new();

        while let Ok(res) = self.0.next("cannot find next") {
            vec.push(Identifier::from_node(res))
        }

        vec
    }
}

pub struct Def(pub List);

impl Def {
    pub fn span(&self) -> Span {
        self.0.1.span.clone()
    }

    pub fn from_list(syn: List) -> Self {
        Self(syn)
    }

    pub fn name(&mut self) -> Result<Identifier> {
        let name = self.0.next("expected function name")?;
        Identifier::from_node(name)
    }

    pub fn params(&mut self) -> Result<Params> {
        let node = self.0.next("expected parameters")?;
        Params::from_node(node)
    }

    pub fn body(&mut self) -> Result<Stmt> {
        let name = self.0.next("expected expression")?;
        Stmt::from_node(name)
    }
}

pub struct Defn(pub List);

impl Defn {
    pub fn span(&self) -> Span {
        self.0.1.span.clone()
    }

    pub fn from_list(syn: List) -> Self {
        Self(syn)
    }

    pub fn name(&mut self) -> Result<Identifier> {
        let name = self.0.next("expected function name")?;
        Identifier::from_node(name)
    }

    pub fn parameters(&mut self) -> Result<Params> {
        let node = self.0.next("expected parameters")?;
        Params::from_node(node)
    }

    pub fn body(&mut self) -> Result<Stmt> {
        let node = self.0.next("expected body")?;
        Stmt::from_node(node)
    }
}

pub struct Eval(pub List);

impl Eval {
    pub fn from_list(syn: List) -> Self {
        Self(syn)
    }

    pub fn span(&self) -> Span {
        self.0.1.span.clone()
    }

    pub fn stmt(&mut self) -> Result<Stmt> {
        let node = self.0.next("expected statement")?;
        Stmt::from_node(node)
    }
}

pub struct SetOption(List);

impl SetOption {
    pub fn from_list(syn: List) -> Self {
        Self(syn)
    }

    pub fn name(&mut self) -> Result<Identifier> {
        let name = self.0.next("expected option name")?;
        Identifier::from_node(name)
    }

    pub fn stmt(&mut self) -> Result<Stmt> {
        let node = self.0.next("expected statement")?;
        Stmt::from_node(node)
    }
}

pub enum TopLevel {
    Def(Def),
    Defn(Defn),
    Eval(Eval),
    SetOption(SetOption),
    Require(Require),
}

impl TopLevel {
    pub fn from_node(syn: SyntaxNode) -> Result<Self> {
        let mut list = List::from_node(syn)?;
        let syn = list.next("expected a function name.")?;
        let span = syn.span.clone();
        let kw = Identifier::from_node(syn)?;
        let text = kw.text()?;

        match text {
            "def" => Ok(TopLevel::Def(Def::from_list(list))),
            "defn" => Ok(TopLevel::Defn(Defn::from_list(list))),
            "eval" => Ok(TopLevel::Eval(Eval::from_list(list))),
            "set-option" => Ok(TopLevel::SetOption(SetOption::from_list(list))),
            "require" => Ok(TopLevel::Require(Require::from_list(list))),
            _ => Err(Spanned::new("unexpected keyword".to_owned(), span)),
        }
    }
}

pub struct If(List);

impl If {
    pub fn from_list(syn: List) -> Self {
        Self(syn)
    }

    pub fn cond(&mut self) -> Result<Expr> {
        let node = self.0.next("expected condition expression")?;
        Expr::from_node(node)
    }

    pub fn then(&mut self) -> Result<Expr> {
        let node = self.0.next("expected then expression")?;
        Expr::from_node(node)
    }

    pub fn else_(&mut self) -> Result<Expr> {
        let node = self.0.next("expected else expression")?;
        Expr::from_node(node)
    }
}

pub struct Block(List);

impl Block {
    pub fn from_list(syn: List) -> Self {
        Self(syn)
    }

    pub fn span(&self) -> Span {
        self.0.1.span.clone()
    }

    pub fn stmt(&mut self) -> Vec<Result<Stmt>> {
        let mut vec = Vec::new();

        while let Ok(node) = self.0.next("expected condition expression") {
            vec.push(Stmt::from_node(node))
        }

        vec
    }
}

pub struct Fn(pub List);

impl Fn {
    pub fn span(&self) -> Span {
        self.0.1.span.clone()
    }

    pub fn from_list(syn: List) -> Self {
        Self(syn)
    }

    pub fn params(&mut self) -> Result<Params> {
        let node = self.0.next("expected function parameters")?;
        Params::from_node(node)
    }

    pub fn body(&mut self) -> Result<Stmt> {
        let node = self.0.next("expected function body")?;
        Stmt::from_node(node)
    }
}

pub struct App(List);

impl App {
    pub fn from_list(syn: List) -> Self {
        Self(syn)
    }

    pub fn name(&mut self) -> Result<Identifier> {
        let name = self.0.next("expected function name")?;
        Identifier::from_node(name)
    }

    pub fn argument(&mut self) -> Result<Option<Expr>> {
        let node = self.0.next("expected arguments");
        if let Ok(node) = node {
            Expr::from_node(node).map(Some)
        } else {
            Ok(None)
        }
    }
}

pub enum Expr {
    If(If),
    Fn(Fn),
    App(App),
    Block(Block),
    Quote(Quote),
    Identifier(Identifier),
    Number(Number),
    Str(Str),
}

impl Expr {
    pub fn from_node(syn: SyntaxNode) -> Result<Self> {
        match syn.kind {
            SyntaxKind::Identifier => return Ok(Expr::Identifier(Identifier::from_node(syn)?)),
            SyntaxKind::Number => return Ok(Expr::Number(Number::from_node(syn)?)),
            SyntaxKind::String => return Ok(Expr::Str(Str::from_node(syn)?)),
            SyntaxKind::Quote => return Ok(Expr::Quote(Quote::from_node(syn)?)),
            _ => (),
        };

        let mut list = List::from_node(syn)?;
        let syn = list.next("expected an expression type.")?;
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

pub struct Let(pub List);

impl Let {
    pub fn from_list(syn: List) -> Self {
        Self(syn)
    }

    pub fn name(&mut self) -> Result<Identifier> {
        let name = self.0.next("expected variable name")?;
        Identifier::from_node(name)
    }

    pub fn value(&mut self) -> Result<Expr> {
        let node = self.0.next("expected value expression")?;
        Expr::from_node(node)
    }
}

pub enum Stmt {
    Let(Let),
    Expr(Expr),
}

impl Stmt {
    pub fn from_node(syn: SyntaxNode) -> Result<Self> {
        match syn.kind {
            SyntaxKind::Identifier
            | SyntaxKind::Number
            | SyntaxKind::String
            | SyntaxKind::Quote => return Ok(Stmt::Expr(Expr::from_node(syn)?)),
            _ => (),
        };

        let mut list = List::from_node(syn)?;
        let syn = list.next("expected a statement type.")?;
        let kw = Identifier::from_node(syn)?;
        let text = kw.text()?;

        match text {
            "let" => Ok(Stmt::Let(Let::from_list(list))),
            _ => Ok(Stmt::Expr(Expr::from_node(list.1)?)),
        }
    }
}

pub struct Require(List);

impl Require {
    pub fn from_list(syn: List) -> Self {
        Self(syn)
    }

    pub fn name(&mut self) -> Result<Identifier> {
        let name = self.0.next("expected a module name")?;
        Identifier::from_node(name)
    }
}
