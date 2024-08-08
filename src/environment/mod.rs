//! This is the module for the language server. It is used as a middle man between the LSP or other services.

use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
    mem,
};

use document::TextChange;

use crate::{
    check,
    compiler::{
        compare::Change,
        concrete::{self, SyntaxNode},
    },
    span::{Point, Span, Spanned},
};

pub mod document;

/// the manager structure is used to manage a bunch of thigns like documents.
#[derive(Default)]
pub struct Manager {
    documents: HashMap<document::Id, document::Document>,
    documents_id: usize,

    pub definitions: HashMap<String, (Span, String)>,
}

impl Manager {
    fn check_changes(
        &mut self,
        removed: Vec<SyntaxNode>,
        added: Vec<SyntaxNode>,
    ) -> (
        Vec<Spanned<String>>,
        Vec<concrete::Id>,
        Vec<(concrete::Id, HashMap<Span, HashMap<String, Span>>)>,
    ) {
        let mut errors = Vec::new();

        let mut to_remove = vec![];
        let mut to_rem_id = vec![];
        let mut to_add = vec![];

        let mut check_context = check::Context::new(self, &mut errors);

        for removed in removed {
            let id = removed.get_id();
            to_rem_id.push(id.clone());
            if let Some(name) = check_context.get_ref(removed) {
                to_remove.push((name.data, id));
            }
        }

        for add in added.clone() {
            let id = add.get_id();
            if let Some(name) = check_context.get_ref(add) {
                to_add.push((name, id));
            }
        }

        drop(check_context);

        for (to_remove, _) in to_remove {
            self.definitions.remove(&to_remove);
        }

        let mut adds = Vec::new();

        for (to_add, _) in to_add {
            self.definitions
                .insert(to_add.data, (to_add.span, "function".to_string()));
        }

        let mut check_context = check::Context::new(self, &mut errors);

        for added in added {
            let id = added.get_id();
            check_context.check(added);
            let s = mem::take(&mut check_context.scopes);
            adds.push((id, s))
        }

        (errors, to_rem_id, adds)
    }

    /// Creates a new document.
    pub fn new_document(&mut self, code: String) -> document::Id {
        let id = document::Id(self.documents_id);

        let mut doc = document::Document::new(id, code);
        let changes = doc.compute(&[Span::empty()]);
        let (removed, added) = split(changes);

        let (mut errors, to_rem_id, to_add_id) = self.check_changes(removed, added);

        for id in to_rem_id {
            doc.scopes.remove(&id);
        }

        for (id, map) in to_add_id {
            doc.scopes.insert(id, map);
        }

        doc.errors.append(&mut errors);

        self.documents.insert(id, doc);
        return id;
    }

    /// Updates the document.
    pub fn update_document(
        &mut self,
        id: document::Id,
        changes: &[TextChange],
    ) -> Vec<Spanned<String>> {
        let (removed, added) = if let Some(doc) = self.get_document(id) {
            let changes = doc.update(&changes);
            split(changes)
        } else {
            return vec![];
        };

        let (mut errors, to_rem_id, to_add_id) = self.check_changes(removed, added);

        if let Some(doc) = self.get_document(id) {
            for id in to_rem_id {
                doc.scopes.remove(&id);
            }

            for (id, map) in to_add_id {
                doc.scopes.insert(id, map);
            }

            doc.errors.append(&mut errors);
            return doc.errors.clone();
        }

        return vec![];
    }

    /// Deletes a document from the store.
    pub fn remove_document(&mut self, id: &document::Id) {
        self.documents.remove(id);
    }

    /// Gets a mutable reference to the document
    pub fn get_document(&mut self, id: document::Id) -> Option<&mut document::Document> {
        self.documents.get_mut(&id)
    }

    pub fn get_completions(
        &mut self,
        id: document::Id,
        position: Point,
    ) -> HashMap<String, (Span, String)> {
        let mut completed = HashMap::default();

        for (def, span) in &self.definitions {
            completed.insert(def.clone(), span.clone());
        }

        if let Some(doc) = self.get_document(id) {
            for val in doc.scopes.values() {
                for (span, values) in val {
                    if span.contains_point(&position) {
                        for (value, span) in values {
                            completed.insert(value.clone(), (span.clone(), "variable".to_string()));
                        }
                    }
                }
            }
        }

        completed
    }
}

pub fn split(changes: Vec<Change>) -> (Vec<SyntaxNode>, Vec<SyntaxNode>) {
    let mut removed = Vec::new();
    let mut added = Vec::new();

    for change in changes {
        change.split(&mut removed, &mut added);
    }

    (removed, added)
}
