//! Scopes information.

use std::collections::HashMap;

use crate::{
    hierarchy::Hierarchy, prettytree::{PrettyPrint, Tree}, span::Span
};

/// Represents a scope within the AST analysis.
#[derive(Default, Debug)]
pub struct Scope {
    /// A map that associates variable names (as `String`) with their corresponding [Span].
    vars: HashMap<String, Vec<Span>>,
}

impl Hierarchy<Scope> {
    pub fn find(&self, data: &str, span: &Span) -> Option<Span> {
        if self.site.span.contains(span) {
            for range in &self.forest {
                if let Some(range) = range.find(data, span) {
                    return Some(range)
                }
            }

            let data = self.site.data.vars.get(data)?;
            let last = data.last().unwrap();
            Some(last.clone())
        } else {
            None
        }
    }
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
