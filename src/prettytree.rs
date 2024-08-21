//! Pretty printing tree and trait module.

use std::fmt::{self, Display};

/// Represents a node in a tree structure with a label and a list of children.
pub struct Tree {
    label: String,
    child: Vec<Tree>,
}

impl Tree {
    /// Creates a new tree node with the given label.
    pub fn label<T: Into<String>>(name: T) -> Self {
        Self {
            label: name.into(),
            child: Vec::new(),
        }
    }

    /// Adds a child node to the current node and returns the modified node.
    pub fn with(mut self, child: Tree) -> Self {
        self.child.push(child);
        self
    }

    pub fn add_child<T: PrettyPrint>(mut self, child: &[T]) -> Self {
        self.child = child.iter().map(|x| x.to_tree()).collect();
        self
    }

    /// Helper method to recursively format the tree structure with indentation.
    fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, indent: &str, last: bool) -> fmt::Result {
        writeln!(
            f,
            "{}{} {}",
            indent,
            if last { "└──" } else { "├──" },
            self.label
        )?;
        let new_indent = if last {
            format!("{}    ", indent)
        } else {
            format!("{}│   ", indent)
        };

        for (i, child) in self.child.iter().enumerate() {
            child.fmt_with_indent(f, &new_indent, i == self.child.len() - 1)?;
        }
        Ok(())
    }
}

impl Display for Tree {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_indent(f, "", true)
    }
}

/// A trait that defines a method to convert a type into a `Tree`.
pub trait PrettyPrint {
    /// Converts the implementing type into a `Tree`.
    fn to_tree(&self) -> Tree;
}
