//! Scopes information.

use std::collections::HashMap;

use crate::{prettytree::{PrettyPrint, Tree}, span::Span};

/// Represents a scope within the AST analysis.
#[derive(Default)]
pub struct Scope {
    /// A map that associates variable names (as `String`) with their corresponding [Span].
    vars: HashMap<String, Vec<Span>>,
}

impl PrettyPrint for Scope {
    fn to_tree(&self) -> crate::prettytree::Tree {
        let mut children = Tree::label(format!("scope {}", self.vars.len()));

        for (k, v) in &self.vars {
            children = children.with(Tree::label(format!("{}: {}", k, v.first().unwrap())))
        }

        children
    }
}

impl Scope {
    /// Adds a variable to the current scope along with its associated span.
    pub fn add<T: Into<String>>(&mut self, item: T, span: Span) {
        self.vars.entry(item.into()).or_default().push(span);
    }
}
