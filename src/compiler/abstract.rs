//! Wrapper around the CST for a better abstraction with verified things.

use crate::span::Spanned;

use super::{
    concrete::{SyntaxNode, SyntaxNodeOrToken, SyntaxToken},
    syntax::SyntaxKind,
};

type Result<T> = std::result::Result<T, Spanned<String>>;

macro_rules! def_token {
    ($name:ident, $method:ident, $kind:expr) => {
        pub struct $name<'a>(&'a SyntaxNode);

        impl<'a> $name<'a> {
            pub fn from_node(node: &'a SyntaxNode) -> Result<Self> {
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

pub fn check_node(node: &SyntaxNode, kind: SyntaxKind) -> Result<()> {
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

pub fn assert_node(not: &SyntaxNodeOrToken) -> Result<&SyntaxNode> {
    match not {
        SyntaxNodeOrToken::Node(node) => Ok(node),
        SyntaxNodeOrToken::Token(token) => {
            Err(Spanned::new(format!("unexpected."), token.span.clone()))
        }
    }
}

pub fn assert_token(not: &SyntaxNodeOrToken) -> Result<&SyntaxToken> {
    match not {
        SyntaxNodeOrToken::Token(node) => Ok(node),
        SyntaxNodeOrToken::Node(token) => {
            Err(Spanned::new(format!("unexpected."), token.span.clone()))
        }
    }
}

pub fn assert_keyword(node: &SyntaxNode, key: &str) -> Result<()> {
    let token = Identifier::from_node(node)?;
    let text = token.text()?;

    if text == key {
        Ok(())
    } else {
        Err(Spanned::new(
            format!("expected keyword {key}."),
            node.span.clone(),
        ))
    }
}

// Basics

def_token!(Identifier, text, SyntaxKind::Identifier);

def_token!(Number, number, SyntaxKind::Number);

def_token!(Str, string, SyntaxKind::String);

pub struct List<'a>(usize, &'a SyntaxNode);

impl<'a> List<'a> {
    pub fn from_node(node: &'a SyntaxNode) -> Result<Self> {
        if node.kind != SyntaxKind::List {
            return Err(Spanned::new(
                "expected a list.".to_owned(),
                node.span.clone(),
            ));
        }

        Ok(List(0, node))
    }

    pub fn next(&mut self, err: &str) -> Result<&SyntaxNode> {
        while self.0 < self.1.children.len() {
            let n = self.1.children.get(self.0).unwrap();
            match n {
                SyntaxNodeOrToken::Node(n) => {
                    self.0 += 1;
                    return Ok(n);
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

pub struct Quote<'a>(&'a SyntaxNode);

impl<'a> Quote<'a> {
    pub fn from_node(node: &'a SyntaxNode) -> Result<Self> {
        if node.kind != SyntaxKind::Quote {
            return Err(Spanned::new(
                "expected a quote.".to_owned(),
                node.span.clone(),
            ));
        }

        Ok(Quote(node))
    }

    pub fn node(&self) -> Result<&SyntaxNode> {
        self.0.nodes().nth(0).ok_or_else(|| {
            Spanned::new("expected a `def` keyword.".to_owned(), self.0.span.clone())
        })
    }
}

pub struct Params<'a>(List<'a>);

impl<'a> Params<'a> {
    pub fn from_node(syn: &'a SyntaxNode) -> Result<Self> {
        Ok(Self(List::from_node(syn)?))
    }

    pub fn name(&mut self) -> Result<Identifier> {
        let name = self.0.next("expected function name")?;
        Identifier::from_node(name)
    }
}

pub struct Def<'a>(List<'a>);

impl<'a> Def<'a> {
    pub fn from_list(syn: List<'a>) -> Self {
        Self(syn)
    }

    pub fn name(&'a mut self) -> Result<Identifier> {
        let name = self.0.next("expected function name")?;
        Identifier::from_node(name)
    }

    pub fn params(&'a mut self) -> Result<Params<'a>> {
        let node = self.0.next("expected parameters")?;
        Params::from_node(node)
    }

    pub fn body(&'a mut self) -> Result<Expr<'a>> {
        let name = self.0.next("expected expression")?;
        Expr::from_node(name)
    }
}

pub struct Defn<'a>(List<'a>);

impl<'a> Defn<'a> {
    pub fn from_list(syn: List<'a>) -> Self {
        Self(syn)
    }

    pub fn name(&'a mut self) -> Result<Identifier> {
        let name = self.0.next("expected function name")?;
        Identifier::from_node(name)
    }

    pub fn parameters(&'a mut self) -> Result<Params<'a>> {
        let node = self.0.next("expected parameters")?;
        Params::from_node(node)
    }

    pub fn body(&'a mut self) -> Result<Stmt<'a>> {
        let node = self.0.next("expected body")?;
        Stmt::from_node(node)
    }
}

pub struct Eval<'a>(List<'a>);

impl<'a> Eval<'a> {
    pub fn from_list(syn: List<'a>) -> Self {
        Self(syn)
    }

    pub fn stmt(&'a mut self) -> Result<Stmt<'a>> {
        let node = self.0.next("expected statement")?;
        Stmt::from_node(node)
    }
}

pub struct SetOption<'a>(List<'a>);

impl<'a> SetOption<'a> {
    pub fn from_list(syn: List<'a>) -> Self {
        Self(syn)
    }

    pub fn name(&'a mut self) -> Result<Identifier> {
        let name = self.0.next("expected option name")?;
        Identifier::from_node(name)
    }

    pub fn stmt(&'a mut self) -> Result<Stmt<'a>> {
        let node = self.0.next("expected statement")?;
        Stmt::from_node(node)
    }
}

pub enum TopLevel<'a> {
    Def(Def<'a>),
    Defn(Defn<'a>),
    Eval(Eval<'a>),
    SetOption(SetOption<'a>),
    Require(Require<'a>),
}

impl<'a> TopLevel<'a> {
    pub fn from_node(syn: &'a SyntaxNode) -> Result<Self> {
        let mut list = List::from_node(syn)?;
        let syn = list.next("expected a function name.")?;
        let kw = Identifier::from_node(syn)?;
        let text = kw.text()?;

        match text {
            "def" => Ok(TopLevel::Def(Def::from_list(list))),
            "defn" => Ok(TopLevel::Defn(Defn::from_list(list))),
            "eval" => Ok(TopLevel::Eval(Eval::from_list(list))),
            "set-option" => Ok(TopLevel::SetOption(SetOption::from_list(list))),
            "require" => Ok(TopLevel::Require(Require::from_list(list))),
            _ => Err(Spanned::new(
                "unexpected keyword".to_owned(),
                syn.span.clone(),
            )),
        }
    }
}

pub struct If<'a>(List<'a>);

impl<'a> If<'a> {
    pub fn from_list(syn: List<'a>) -> Self {
        Self(syn)
    }

    pub fn cond(&'a mut self) -> Result<Expr<'a>> {
        let node = self.0.next("expected condition expression")?;
        Expr::from_node(node)
    }

    pub fn then(&'a mut self) -> Result<Expr<'a>> {
        let node = self.0.next("expected then expression")?;
        Expr::from_node(node)
    }

    pub fn else_(&'a mut self) -> Result<Expr<'a>> {
        let node = self.0.next("expected else expression")?;
        Expr::from_node(node)
    }
}

pub struct Fn<'a>(List<'a>);

impl<'a> Fn<'a> {
    pub fn from_list(syn: List<'a>) -> Self {
        Self(syn)
    }

    pub fn params(&'a mut self) -> Result<Params<'a>> {
        let node = self.0.next("expected function parameters")?;
        Params::from_node(node)
    }

    pub fn body(&'a mut self) -> Result<Stmt<'a>> {
        let node = self.0.next("expected function body")?;
        Stmt::from_node(node)
    }
}

pub struct App<'a>(List<'a>);

impl<'a> App<'a> {
    pub fn from_list(syn: List<'a>) -> Self {
        Self(syn)
    }

    pub fn name(&'a mut self) -> Result<Identifier> {
        let name = self.0.next("expected function name")?;
        Identifier::from_node(name)
    }

    pub fn arguments(&'a mut self) -> Result<Expr<'a>> {
        let node = self.0.next("expected arguments")?;
        Expr::from_node(node)
    }
}

pub enum Expr<'a> {
    If(If<'a>),
    Fn(Fn<'a>),
    App(App<'a>),
    Quote(Quote<'a>),
    Identifier(Identifier<'a>),
    Number(Number<'a>),
    Str(Str<'a>),
}

impl<'a> Expr<'a> {
    pub fn from_node(syn: &'a SyntaxNode) -> Result<Self> {
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
            _ => {
                list.reset();
                Ok(Expr::App(App::from_list(list)))
            }
        }
    }
}

pub struct Let<'a>(List<'a>);

impl<'a> Let<'a> {
    pub fn from_list(syn: List<'a>) -> Self {
        Self(syn)
    }

    pub fn name(&'a mut self) -> Result<Identifier> {
        let name = self.0.next("expected variable name")?;
        Identifier::from_node(name)
    }

    pub fn value(&'a mut self) -> Result<Expr<'a>> {
        let node = self.0.next("expected value expression")?;
        Expr::from_node(node)
    }
}

pub enum Stmt<'a> {
    Let(Let<'a>),
    Expr(Expr<'a>),
}

impl<'a> Stmt<'a> {
    pub fn from_node(syn: &'a SyntaxNode) -> Result<Self> {
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

pub struct Require<'a>(List<'a>);

impl<'a> Require<'a> {
    pub fn from_list(syn: List<'a>) -> Self {
        Self(syn)
    }

    pub fn name(&mut self) -> Result<Identifier> {
        let name = self.0.next("expected a module name")?;
        Identifier::from_node(name)
    }
}
