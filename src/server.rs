//! This is the module for the languge server.

use std::collections::HashMap;

use tokio::sync::RwLock;

use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::MessageType;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};

use crate::analyze::Analysis;
use crate::env::Env;
use crate::errors::Error;
use crate::id::{self, Id};
use crate::span::{Edit, Point, Span};

pub struct Backend {
    client: Client,
    manager: RwLock<Env>,
    docs: RwLock<HashMap<Url, id::Id<id::File>>>
}

impl Backend {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            docs: Default::default(),
            manager: Default::default(),
        }
    }

    /// Compile all the errors into diagnostics.
    pub fn transform_errors(&self, errors: &[Error]) -> Vec<Diagnostic> {
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
                message: err.message.clone().into_owned(),
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
    ) -> Vec<Edit> {
        let mut vec = Vec::new();

        for change in changes.into_iter() {
            if let Some(range) = change.range {
                let start = Point::new(range.start.line as usize, range.start.character as usize);
                let end = Point::new(range.end.line as usize, range.end.character as usize);
                let span = Span::new(start, end);
                let value = Edit {
                    span,
                    data: change.text,
                };
                vec.push(value)
            }
        }

        vec
    }

    pub async fn get_document_id(&self, uri: &Url) -> Option<Id<id::File>> {
        let docstore = self.docs.read().await;
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
        let id = manager.add_file("".to_string());

        let mut docstore = self.docs.write().await;
        docstore.insert(uri.clone(), id);
        drop(docstore);

        manager.modify_file(id, &[Edit::new(doc, Span::empty())]);

        let doc = manager.file_storage.get(&id).unwrap();
        let mut errors = doc.syntax_errors.clone();

        let mut analysis = Analysis::default();
        analysis.parse_program(doc.new_tree.nodes());
        errors.append(&mut analysis.errors);

        let errs = self.transform_errors(&errors);
        self.publish_diagnostics(&uri, errs).await;
    }

    async fn did_close(&self, _params: DidCloseTextDocumentParams) {
        ()
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        let changes = params.content_changes;

        let changes = self.changes_to_ranges(changes);

        let mut manager = self.manager.write().await;

        let docstore = self.docs.read().await;
        let id = docstore.get(&uri).cloned().unwrap();
        drop(docstore);

        manager.modify_file(id, &changes);

        let doc = manager.file_storage.get(&id).unwrap();
        let mut errors = doc.syntax_errors.clone();

        let mut analysis = Analysis::default();
        analysis.parse_program(doc.new_tree.nodes());
        errors.append(&mut analysis.errors);

        let errs = self.transform_errors(&errors);
        self.publish_diagnostics(&uri, errs).await;
    }
}