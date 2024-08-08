//! This is the module for the language server. It is used as a middle man between the LSP or other services.

use std::collections::HashMap;

pub mod document;

/// the manager structure is used to manage a bunch of thigns like documents.
#[derive(Default)]
pub struct Manager {
    documents: HashMap<document::Id, document::Document>,
    documents_id: usize,
}

impl Manager {
    /// Creates a new document.
    pub fn new_document(&mut self, code: String) -> document::Id {
        let id = document::Id(self.documents_id);
        self.documents.insert(id, document::Document::new(id, code));
        return id
    }

    /// Deletes a document from the store.
    pub fn remove_document(&mut self, id: &document::Id) {
        self.documents.remove(id);
    }

    /// Gets a mutable reference to the document
    pub fn get_document(&mut self, id: document::Id) -> Option<&mut document::Document> {
        self.documents.get_mut(&id)
    }
}