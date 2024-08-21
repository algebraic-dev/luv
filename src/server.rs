//! This is the module for the languge server.

use std::collections::{HashMap, HashSet};
use std::path::PathBuf;
use std::sync::Arc;

use tokio::sync::mpsc::Sender;
use tokio::sync::RwLock;

use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::MessageType;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};

use crate::env::{self, Env};
use crate::errors::Error;
use crate::find::Found;
use crate::id::{self, Id};
use crate::r#abstract::TopLevelKind;
use crate::span::{Edit, Point, Span};

#[derive(Default)]
pub struct BiMap {
    from: HashMap<Url, id::Id<id::File>>,
    to: HashMap<id::Id<id::File>, Url>,
}

impl BiMap {
    pub fn insert(&mut self, f: id::Id<id::File>, t: Url) {
        self.from.insert(t.clone(), f);
        self.to.insert(f, t);
    }
}

#[derive(Clone)]
pub struct Backend {
    client: Client,
    manager: Arc<RwLock<Env>>,
    docs: Arc<RwLock<BiMap>>,
}

impl Backend {
    pub fn new(client: Client, event_channel: Sender<env::Event>) -> Self {
        Self {
            client,
            docs: Default::default(),
            manager: Arc::new(RwLock::new(Env::new(event_channel))),
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

        errs
    }

    /// Change all change events to text ranges.
    pub fn changes_to_ranges(&self, changes: Vec<TextDocumentContentChangeEvent>) -> Vec<Edit> {
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
            } else {
                panic!("cannot use mepty range.")
            }
        }

        vec
    }

    pub async fn get_document_id(&self, uri: &Url) -> Option<Id<id::File>> {
        let docstore = self.docs.read().await;
        docstore.from.get(uri).copied()
    }

    pub async fn publish_diagnostics(&self, uri: &Url, errs: Vec<Diagnostic>) {
        self.client
            .publish_diagnostics(uri.clone(), errs, None)
            .await;
    }
}

impl Backend {
    pub async fn handle_recompile(&self, id: Id<id::File>) {
        let mut manager = self.manager.write().await;
        let path = manager.files.get(&id).unwrap();

        let uri = Url::from_file_path(path).unwrap();

        manager.compile(id).await;

        let doc = manager.file_storage.get(&id).unwrap();
        let errors = doc.errors.clone();

        let errs = self.transform_errors(&errors);
        self.publish_diagnostics(&uri, errs).await;
    }

    pub async fn handle_env_log(&self, message: String) {
        self.client
            .log_message(MessageType::INFO, format!("env: {message}"))
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
                    resolve_provider: Some(false),
                    trigger_characters: None,
                    work_done_progress_options: Default::default(),
                    all_commit_characters: None,
                    completion_item: None,
                }),
                definition_provider: Some(tower_lsp::lsp_types::OneOf::Right(DefinitionOptions {
                    work_done_progress_options: tower_lsp::lsp_types::WorkDoneProgressOptions {
                        work_done_progress: None,
                    },
                })),
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

    async fn folding_range(&self, params: FoldingRangeParams) -> Result<Option<Vec<FoldingRange>>> {
        let uri = params.text_document.uri;

        let manager = self.manager.read().await;
        let id = self.get_document_id(&uri).await.unwrap();
        let doc = manager.file_storage.get(&id).unwrap();

        let mut folding = Vec::new();

        for ast in &doc.ast.vec {
            match ast.data {
                TopLevelKind::Defn(_) | TopLevelKind::Def(_) | TopLevelKind::Eval(_) => folding
                    .push(FoldingRange {
                        start_line: ast.span.start.line as u32,
                        start_character: Some(ast.span.start.column as u32),
                        end_line: ast.span.end.line as u32,
                        end_character: Some(ast.span.end.column as u32),
                        kind: None,
                        collapsed_text: None,
                    }),
                _ => (),
            }
        }

        Ok(Some(folding))
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        let doc = params.text_document.text;

        let mut manager = self.manager.write().await;

        let path = PathBuf::from(uri.path()).canonicalize().unwrap();
        let id = manager.ensure_file(path.clone(), "".to_string());

        let mut docstore = self.docs.write().await;
        docstore.insert(id, uri.clone());

        manager.update_file(id, doc);
        manager.compile(id).await;

        let doc = manager.file_storage.get(&id).unwrap();
        let errors = doc.errors.clone();

        let errs = self.transform_errors(&errors);

        manager.open.insert(id);

        self.publish_diagnostics(&uri, errs).await;
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri;

        let docstore = self.docs.write().await;

        let mut manager = self.manager.write().await;
        let id = docstore.from.get(&uri).unwrap();
        manager.open.insert(*id);
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = params.text_document_position.text_document.uri;
        let pos = params.text_document_position.position;
        let point = Point::new(pos.line as usize, pos.character as usize);

        let mut manager = self.manager.write().await;
        let mut completions = Vec::new();
        let id = self.get_document_id(&uri).await.unwrap();
        let doc = manager.file_storage.get(&id).unwrap();

        let mut vec = HashSet::new();
        doc.ast.find(&point, &mut vec);

        let find_map = vec.get(&Found::Expr);

        self.client
            .log_message(MessageType::INFO, format!("changes: {:?}", vec))
            .await;

        if find_map.is_some() {
            for data in manager.available_names_in_point(id, point).await {
                completions.push(CompletionItem {
                    label: data,
                    kind: Some(CompletionItemKind::FUNCTION),
                    ..Default::default()
                })
            }
        }

        if completions.is_empty() {
            Ok(None)
        } else {
            Ok(Some(CompletionResponse::Array(completions)))
        }
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        let changes = params.content_changes;

        let mut manager = self.manager.write().await;

        let docstore = self.docs.read().await;
        let id = docstore.from.get(&uri).cloned().unwrap();
        drop(docstore);

        if let Some(true) = changes.first().map(|x| x.range.is_some()) {
            let edits = self.changes_to_ranges(changes);
            manager.apply_edits(id, &edits).await;
        } else {
            manager.update_file(id, changes.first().unwrap().text.clone());
        }

        manager.visited = HashSet::default();
        manager.compile(id).await;

        let doc = manager.file_storage.get(&id).unwrap();
        let errors = doc.errors.clone();

        let errs = self.transform_errors(&errors);

        self.publish_diagnostics(&uri, errs).await;
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = params.text_document_position_params.text_document.uri;
        let p = params.text_document_position_params.position;
        let point = Point::new(p.line as usize, p.character as usize);

        let docstore = self.docs.read().await;
        let id = docstore.from.get(&uri).cloned().unwrap();

        let manager = self.manager.read().await;
        let doc = manager.file_storage.get(&id).unwrap();

        let mut vec = HashSet::new();
        doc.ast.find(&point, &mut vec);

        let find_map = vec
            .iter()
            .find_map(|x| if let Found::Id(x) = x { Some(x) } else { None });

        if let Some(name) = find_map {
            if let Some(data) = doc.scopes.find(&name.data, &name.span) {
                return Ok(Some(GotoDefinitionResponse::Scalar(Location {
                    uri,
                    range: Range {
                        start: Position {
                            line: data.start.line as u32,
                            character: data.start.column as u32,
                        },
                        end: Position {
                            line: data.end.line as u32,
                            character: data.end.column as u32,
                        },
                    },
                })));
            }
        }

        Ok(None)
    }
}
