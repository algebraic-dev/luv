//! This is the module for the languge server.

use std::collections::HashMap;
use tokio::sync::RwLock;

use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::MessageType;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};

use crate::environment::document::TextChange;
use crate::environment::{document, Manager};
use crate::span::{Point, Span, Spanned};

pub struct Backend {
    client: Client,
    manager: RwLock<Manager>,
    doc_map: RwLock<HashMap<Url, document::Id>>,
}

impl Backend {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            manager: Default::default(),
            doc_map: Default::default(),
        }
    }

    /// Compile all the errors into diagnostics.
    pub fn transform_errors(&self, errors: &[Spanned<String>]) -> Vec<Diagnostic> {
        let mut errs = Vec::new();

        for err in errors {
            let start = err.span.start.clone();
            let end = err.span.end.clone();

            errs.push(Diagnostic {
                range: Range {
                    start: Position::new(start.line as u32, start.column as u32),
                    end: Position::new(end.line as u32, end.column as u32),
                },
                severity: Some(DiagnosticSeverity::ERROR),
                code: None,
                code_description: None,
                source: Some("firefly".to_string()),
                message: err.data.clone(),
                related_information: None,
                tags: None,
                data: None,
            })
        }

        return errs;
    }

    /// Change all change events to text ranges.
    pub fn changes_to_ranges(
        &self,
        changes: Vec<TextDocumentContentChangeEvent>,
    ) -> Vec<TextChange> {
        let mut vec = Vec::new();

        for change in changes.into_iter() {
            if let Some(range) = change.range {
                let start = Point::new(range.start.line as usize, range.start.character as usize);
                let end = Point::new(range.end.line as usize, range.end.character as usize);
                let span = Span::new(start, end);
                vec.push(TextChange {
                    span,
                    text: change.text,
                })
            }
        }

        vec
    }

    pub async fn get_document_id(&self, uri: &Url) -> Option<document::Id> {
        let docstore = self.doc_map.read().await;
        docstore.get(&uri).copied()
    }

    pub async fn publish_diagnostics(&self, uri: &Url, errs: Vec<Diagnostic>) {
        self.client
            .publish_diagnostics(uri.clone(), errs, None)
            .await;
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::INCREMENTAL,
                )),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "Server initialized!")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        let doc = params.text_document.text;

        let mut manager = self.manager.write().await;
        let id = manager.new_document(doc);
        let doc = manager.get_document(id).unwrap();

        let mut docstore = self.doc_map.write().await;
        docstore.insert(uri.clone(), id);
        drop(docstore);

        doc.compute(&[Span::empty()]);

        let errs = self.transform_errors(&doc.errors);
        self.publish_diagnostics(&uri, errs).await;
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri;

        let mut docstore = self.doc_map.write().await;

        if let Some(id) = docstore.remove(&uri) {
            let mut manager = self.manager.write().await;
            manager.remove_document(&id);
        }
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        let changes = params.content_changes;

        let changes = self.changes_to_ranges(changes);

        let mut manager = self.manager.write().await;
        let id = self.get_document_id(&uri).await.unwrap();

        let doc = manager.get_document(id).unwrap();
        doc.update(&changes);

        let errs = self.transform_errors(&doc.errors);
        self.publish_diagnostics(&uri, errs).await;
    }
}
