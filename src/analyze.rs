//! This module is used to analyze an AST (Abstract Syntax Tree) into a collection of data.  It
//! provides structures and implementations to manage scopes and context while visiting  the nodes
//! of the AST, allowing for the organization of variable information and spans.

use im_rc::HashMap;

use crate::{r#abstract::{Identifier, Params}, hierarchy::HierarchyBuilder, prettytree::{PrettyPrint, Tree}, span::Span, visitor::Visitor};

/// Represents a scope within the AST analysis.
#[derive(Default)]
pub struct Scope {
    /// A map that associates variable names (as `String`) with their corresponding [Span].
    vars: HashMap<String, Span>,
}

impl PrettyPrint for Scope {
    fn to_tree(&self) -> crate::prettytree::Tree {
        let mut children = Tree::label(format!("scope {}", self.vars.len()));

        for (k, v) in &self.vars {
            children = children.add(Tree::label(format!("{}: {}", k, v)))
        }

        children
    }
}


impl Scope {
    /// Adds a variable to the current scope along with its associated span.
    pub fn add<T: Into<String>>(&mut self, item: T, span: Span) {
        self.vars.insert(item.into(), span);
    }
}

/// Represents the context during AST analysis.
#[derive(Default)]
pub struct Context {
    /// A builder for managing the hierarchical structure of scopes.
    pub scopes: HierarchyBuilder<Scope>,
}

impl Context {
    /// Creates a new context.
    pub fn new(span: Span) -> Self {
        Self {
            scopes: HierarchyBuilder::new(span)
        }
    }
}

impl<'a> Visitor<'a> for Context {
    fn visit_identifier(&mut self, id: Identifier<'a>) -> Option<()> {
        let txt = id.text().unwrap();

        println!("Text: {}", txt);

        Some(())
    }

    fn visit_params(&mut self, mut s: Params<'a>) -> Option<()> {
        while let Ok(Some(name)) = s.name() {
            let text = name.text().unwrap();
            let scope = self.scopes.get();
            scope.add(text, name.span());
        }

        Some(())
    }

    fn visit_defn(&mut self, mut defn: crate::r#abstract::Defn<'a>) -> Option<()> {
        if let Ok(params) = defn.name() {
            self.visit_identifier(params);
        }

        self.scopes.open(defn.span());

        if let Ok(params) = defn.parameters() {
            self.visit_params(params);
        }

        while let Ok(Some(body)) = defn.body() {
            body.visit(self)?;
        }

        self.scopes.close();

        Some(())
    }

    fn visit_let(&mut self, mut let_stmt: crate::r#abstract::Let<'a>) -> Option<()> {
        if let Ok(value) = let_stmt.name() {
            let name = value.text().ok()?;
            let scope = self.scopes.get();
            scope.add(name, value.span())
        }

        if let Ok(value) = let_stmt.value() {
            value.visit(self);
        }

        Some(())
    }

    fn visit_block(&mut self, mut block: crate::r#abstract::Block<'a>) -> Option<()> {
        self.scopes.open(block.span());

        while let Ok(Some(body)) = block.body() {
            body.visit(self)?;
        }

        self.scopes.close();

        Some(())
    }
}
