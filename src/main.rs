use std::collections::HashMap;

use firefly::compiler::lexer::Lexer;
use firefly::compiler::parser;
use tokio::sync::RwLock;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

#[derive(Debug)]
struct Backend {
    client: Client,
    documents: RwLock<HashMap<Url, String>>,
}

fn offset_to_position(doc: &str, offset: usize) -> Position {
    let mut remaining_offset = offset;
    for (line_num, line) in doc.lines().enumerate() {
        if remaining_offset <= line.len() {
            return Position::new(line_num as u32, remaining_offset as u32);
        }
        remaining_offset -= line.len() + 1;
    }
    let last_line_num = doc.lines().count().saturating_sub(1);
    let last_line_len = doc.lines().last().map_or(0, |line| line.len());
    Position::new(last_line_num as u32, last_line_len as u32)
}

fn position_to_offset(doc: &str, pos: Position) -> usize {
    let mut offset = 0;
    for (line_num, line) in doc.lines().enumerate() {
        if line_num == pos.line as usize {
            offset += pos.character as usize;
            break;
        }
        offset += line.len() + 1; // +1 for newline character
    }
    offset
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::INCREMENTAL,
                )),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                completion_provider: Some(CompletionOptions::default()),
                ..Default::default()
            },
            ..Default::default()
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "server initialized!")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn completion(&self, _: CompletionParams) -> Result<Option<CompletionResponse>> {
        Ok(Some(CompletionResponse::Array(vec![
            CompletionItem::new_simple("Bello".to_string(), "Some detail".to_string()),
            CompletionItem::new_simple("Bye".to_string(), "More detail".to_string()),
        ])))
    }

    async fn hover(&self, _: HoverParams) -> Result<Option<Hover>> {
        Ok(Some(Hover {
            contents: HoverContents::Scalar(MarkedString::String("You're hovering!!!".to_string())),
            range: None,
        }))
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        let doc = params.text_document.text;

        if let Err((str, span)) = parser::Parser::new(Lexer::new(&doc)).parse() {
            let start = offset_to_position(&doc, span.0);
            let end = offset_to_position(&doc, span.1);

            self.client
                .publish_diagnostics(
                    uri.clone(),
                    vec![Diagnostic {
                        range: Range {
                            start: Position::new(start.line as u32, start.character as u32),
                            end: Position::new(end.line as u32, end.character as u32),
                        },
                        severity: Some(DiagnosticSeverity::ERROR),
                        code: None,
                        code_description: None,
                        source: Some("firefly".to_string()),
                        message: str,
                        related_information: None,
                        tags: None,
                        data: None,
                    }],
                    None,
                )
                .await;
        } else {
            self.client
                .publish_diagnostics(uri.clone(), vec![], None)
                .await;
        }

        self.documents.write().await.insert(uri.clone(), doc);

        self.client
            .log_message(MessageType::INFO, format!("Document opened: {}", uri))
            .await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        let changes = params.content_changes;

        let mut documents = self.documents.write().await;
        if let Some(doc) = documents.get_mut(&uri) {
            for change in changes {
                if let Some(range) = change.range {
                    let start = position_to_offset(doc, range.start);
                    let end = position_to_offset(doc, range.end);
                    doc.replace_range(start..end, &change.text);
                } else {
                    *doc = change.text;
                }
            }

            if let Err((str, span)) = parser::Parser::new(Lexer::new(doc)).parse() {
                let start = offset_to_position(doc, span.0);
                let end = offset_to_position(doc, span.1);

                self.client
                    .publish_diagnostics(
                        uri,
                        vec![Diagnostic {
                            range: Range {
                                start: Position::new(start.line as u32, start.character as u32),
                                end: Position::new(end.line as u32, end.character as u32),
                            },
                            severity: Some(DiagnosticSeverity::ERROR),
                            code: None,
                            code_description: None,
                            source: Some("firefly".to_string()),
                            message: str,
                            related_information: None,
                            tags: None,
                            data: None,
                        }],
                        None,
                    )
                    .await;
            } else {
                self.client.publish_diagnostics(uri, vec![], None).await;
            }

            self.client
                .log_message(MessageType::INFO, format!("Document changed: {}", doc))
                .await;
        }
    }
}

#[tokio::main]
async fn main() {
    let (stdin, stdout) = (tokio::io::stdin(), tokio::io::stdout());
    let (service, socket) = LspService::new(|client| Backend {
        client,
        documents: RwLock::new(HashMap::new()),
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}
