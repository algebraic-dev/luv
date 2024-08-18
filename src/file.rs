//! A File in the definition of things that need to be saved.

use crate::{errors::Error, syntax::SyntaxNode};
#[derive(Debug)]
pub struct File {
    pub old_tree: SyntaxNode,
    pub new_tree: SyntaxNode,
    pub source: String,
    pub syntax_errors: Vec<Error>
}

impl File {
    pub fn new(new_tree: SyntaxNode, source: String, syntax_errors: Vec<Error>) -> Self {
        File {
            old_tree: SyntaxNode::empty(),
            new_tree,
            source,
            syntax_errors
        }
    }
}