//! The manager of graphs and entities during a coding session.

use crate::{hierarchy::Hierarchy, id, scope::Scope, storage::Storage};

pub struct Definition {
    scope: Hierarchy<Scope>,
}

pub struct Env {
    definitions: Storage<id::Definition, Definition>,
}
