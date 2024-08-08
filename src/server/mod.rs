//! This is the module for the languge server.

use std::collections::HashMap;
use tokio::sync::RwLock;

use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::MessageType;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};

use crate::compiler::syntax::SyntaxKind;
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
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(true),
                    trigger_characters: None,
                    all_commit_characters: None,
                    work_done_progress_options: WorkDoneProgressOptions::default(),
                    completion_item: None,
                }),
                definition_provider: Some(OneOf::Left(true)),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                folding_range_provider: Some(FoldingRangeProviderCapability::Simple(true)),
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
        let errors = manager.update_document(id, &changes);

        let errs = self.transform_errors(&errors);
        self.publish_diagnostics(&uri, errs).await;
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = params.text_document_position.text_document.uri;
        let pos = params.text_document_position.position;
        let point = Point::new(pos.line as usize, pos.character as usize);

        let mut manager = self.manager.write().await;
        let id = self.get_document_id(&uri).await.unwrap();

        let mut completions = Vec::new();
        let mut s = Vec::new();

        for (data, _) in manager.get_completions(id, point) {
            s.push(data.to_string());
            completions.push(CompletionItem {
                label: data,
                kind: Some(CompletionItemKind::FUNCTION),
                ..Default::default()
            })
        }

        self.client
            .log_message(MessageType::INFO, format!("get {}", s.join(", ")))
            .await;

        if completions.is_empty() {
            Ok(None)
        } else {
            Ok(Some(CompletionResponse::Array(completions)))
        }
    }

    async fn completion_resolve(&self, params: CompletionItem) -> Result<CompletionItem> {
        Ok(params)
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;
        let point = Point::new(pos.line as usize, pos.character as usize);

        let mut manager = self.manager.write().await;
        let id = self.get_document_id(&uri).await.unwrap();
        let data = manager.get_completions(id, point.clone());

        if let Some(doc) = manager.get_document(id) {
            if let Some(token) = doc.new_syn.find_token_containing_point(&point) {
                if token.kind == SyntaxKind::Identifier {
                    let text = token.text;
                    if let Some((span, _)) = data.get(&text) {
                        return Ok(Some(GotoDefinitionResponse::Scalar(Location {
                            uri,
                            range: Range {
                                start: Position {
                                    line: span.start.line as u32,
                                    character: span.start.column as u32,
                                },
                                end: Position {
                                    line: span.end.line as u32,
                                    character: span.end.column as u32,
                                },
                            },
                        })));
                    }
                }
            }
        }

        Ok(None)
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;
        let point = Point::new(pos.line as usize, pos.character as usize);

        let mut manager = self.manager.write().await;
        let id = self.get_document_id(&uri).await.unwrap();

        if let Some(doc) = manager.get_document(id) {
            if let Some(token) = doc.new_syn.find_token_containing_point(&point) {
                if token.kind == SyntaxKind::Identifier {
                    if let Some((span, typ)) = manager.definitions.get(&token.text) {
                        return Ok(Some(Hover {
                            contents: HoverContents::Markup(MarkupContent {
                                kind: MarkupKind::Markdown,
                                value: format!("{} is a {typ}", token.text),
                            }),
                            range: Some(Range {
                                start: Position {
                                    line: span.start.line as u32,
                                    character: span.start.column as u32,
                                },
                                end: Position {
                                    line: span.end.line as u32,
                                    character: span.end.column as u32,
                                },
                            }),
                        }));
                    }
                }
            }
        }

        Ok(None)
    }

    async fn folding_range(&self, params: FoldingRangeParams) -> Result<Option<Vec<FoldingRange>>> {
        let uri = params.text_document.uri;
        let mut manager = self.manager.write().await;
        let id = self.get_document_id(&uri).await.unwrap();
        let mut folding_ranges = Vec::new();

        if let Some(doc) = manager.get_document(id) {
            for token in doc.new_syn.tokens() {
                if token.kind == SyntaxKind::Comment {
                    folding_ranges.push(FoldingRange {
                        start_line: token.span.start.line as u32,
                        start_character: Some(token.span.start.column as u32),
                        end_line: token.span.end.line as u32,
                        end_character: Some(token.span.end.column as u32),
                        kind: Some(FoldingRangeKind::Region),
                        collapsed_text: None,
                    })
                }
            }

            for node in doc.new_syn.nodes() {
                if node.kind == SyntaxKind::List {
                    folding_ranges.push(FoldingRange {
                        start_line: node.span.start.line as u32,
                        start_character: Some(node.span.start.column as u32),
                        end_line: node.span.end.line as u32,
                        end_character: Some(node.span.end.column as u32),
                        kind: Some(FoldingRangeKind::Region),
                        collapsed_text: None,
                    })
                }
            }
        }

        if folding_ranges.is_empty() {
            return Ok(None);
        } else {
            return Ok(Some(folding_ranges));
        }
    }
}
