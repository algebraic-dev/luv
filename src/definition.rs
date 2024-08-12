
use crate::{errors::Error, hierarchy::Hierarchy, id::{self, Id}, scope::Scope, syntax::SyntaxNode};

/// Definition

/// A definition.
#[derive(Debug)]
pub struct Definition {
    pub name: String,
    pub syn: SyntaxNode,
    pub scope: Hierarchy<Scope>,
    pub errors: Vec<Error>,
    pub conflicted: bool,
    pub file: Id<id::File>
}