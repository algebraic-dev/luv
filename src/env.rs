//! The manager of graphs and entities during a coding session.

use std::mem;

use crate::{
    file::File,
    id::{self, AnId, Id},
    parser::parse,
    relation::Relations,
    span::Edit,
    storage::Storage,
};

/// The environment.
#[derive(Debug, Default)]
pub struct Env {
    pub file_storage: Storage<id::File, File>,

    pub created: Relations<AnId>
}

impl Env {
    pub fn new() -> Self {
        Self {
            file_storage: Storage::default(),
            created: Relations::default()
        }
    }

    pub fn add_file(&mut self, source: String) -> Id<id::File> {
        let (new_tree, syntax_errors) = parse(&source);

        let file = File::new(new_tree, source, syntax_errors);
        let id = self.file_storage.add(file);
        self.created.add_node(id::AnId::File(id));

        id
    }

    pub fn modify_file(&mut self, id: Id<id::File>, edits: &[Edit]) -> Option<()> {
        let file = self.file_storage.get_mut(&id)?;

        for edit in edits {
            let start = edit.span.start.to_offset(&file.source);
            let end = edit.span.end.to_offset(&file.source);

            file.source.replace_range(start..end, &edit.data);
        }

        let (new_syntax, new_syntax_errors) = parse(&file.source);

        mem::swap(&mut file.old_tree, &mut file.new_tree);
        file.new_tree = new_syntax;
        file.syntax_errors = new_syntax_errors;

        Some(())
    }
}
