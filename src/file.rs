//! A File in the definition of things that need to be saved.

use std::collections::{HashMap, HashSet};

use crate::{r#abstract::Name, errors::Error, id::AnId, syntax::SyntaxNode};
#[derive(Debug)]
pub struct File {
    pub old_tree: SyntaxNode,
    pub new_tree: SyntaxNode,
    pub source: String,
    pub syntax_errors: Vec<Error>,
    pub names: HashMap<Name, HashSet<AnId>>
}

impl File {
    pub fn new(new_tree: SyntaxNode, source: String, syntax_errors: Vec<Error>) -> Self {
        File {
            old_tree: SyntaxNode::empty(),
            new_tree,
            source,
            syntax_errors,
            names: Default::default()
        }
    }
}