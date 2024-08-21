//! A File in the definition of things that need to be saved.

use std::collections::HashSet;

use crate::{
    errors::Error,
    hierarchy::Hierarchy,
    id::{self, Id},
    r#abstract::{Program, Text},
    scope::Scope,
    span::Span,
    syntax::SyntaxNode,
};
pub struct File {
    pub old_tree: SyntaxNode,
    pub new_tree: SyntaxNode,
    pub source: String,
    pub errors: Vec<Error>,
    pub imports: HashSet<Id<id::File>>,
    pub names: HashSet<Text>,
    pub ast: Program,
    pub scopes: Hierarchy<Scope>,
}

impl File {
    pub fn new(new_tree: SyntaxNode, source: String, syntax_errors: Vec<Error>) -> Self {
        File {
            old_tree: SyntaxNode::empty(),
            new_tree,
            source,
            errors: syntax_errors,
            imports: Default::default(),
            names: Default::default(),
            ast: Program {
                vec: vec![],
                span: Span::empty(),
            },
            scopes: Hierarchy::new(Span::empty(), Scope::default()),
        }
    }
}
