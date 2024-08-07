/*

use std::collections::HashMap;

use firefly::compiler::compare::compare_syntax_trees;
use firefly::compiler::concrete::SyntaxNode;
use firefly::compiler::lexer::Lexer;
use firefly::compiler::parser::{self};
use firefly::span::Spanned;
use tokio::sync::RwLock;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

#[derive(Debug)]
struct Backend {
    client: Client,
    documents: RwLock<HashMap<Url, (String, SyntaxNode)>>,
}

fn position_to_offset(doc: &mut String, pos: Position) -> usize {
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
            .log_message(MessageType::INFO, "server initialized!!!!")
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
        self.client
        .log_message(MessageType::INFO, "did open")
        .await;

        
        let uri = params.text_document.uri;
        let doc = params.text_document.text;

        let (nodes, result) = parser::Parser::new(Lexer::new(&doc)).parse();
        //let mut errs = Vec::new();

        /* 
        for Spanned { data: doc, span } in result {
            let start = span.start;
            let end = span.end;

            errs.push(Diagnostic {
                range: Range {
                    start: Position::new(start.line as u32, start.column as u32),
                    end: Position::new(end.line as u32, end.column as u32),
                },
                severity: Some(DiagnosticSeverity::ERROR),
                code: None,
                code_description: None,
                source: Some("firefly".to_string()),
                message: doc.clone(),
                related_information: None,
                tags: None,
                data: None,
            })
        }

        self.client
            .publish_diagnostics(uri.clone(), errs, None)
            .await;

        self.documents
            .write()
            .await
            .insert(uri.clone(), (doc, nodes));

        self.client
            .log_message(MessageType::INFO, format!("Document opened: {}", uri))
            .await; */
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "did change")
            .await;
        /*
        let uri = params.text_document.uri;
        let changes = params.content_changes;

        let mut documents = self.documents.write().await;
        if let Some((doc, gn)) = documents.get_mut(&uri) {
            for change in changes {
                if let Some(range) = change.range {
                    let start = position_to_offset(doc, range.start);
                    let end = position_to_offset(doc, range.end);
                    doc.replace_range(start..end, &change.text);
                } else {
                    *doc = change.text;
                }
            }

            let (syn, result) = parser::Parser::new(Lexer::new(doc)).parse();
            
            let s = {
                let mut changes = Vec::new();
                compare_syntax_trees(gn, &syn, &mut changes);
                format!("Vai ter show: {:?}", changes)
            };

            self.client
                .log_message(MessageType::INFO, s)
                .await;
            
            let mut errs = Vec::new();

            for Spanned { data: doc, span } in result {
                let start = span.start;
                let end = span.end;

                errs.push(Diagnostic {
                    range: Range {
                        start: Position::new(start.line as u32, start.column as u32),
                        end: Position::new(end.line as u32, end.column as u32),
                    },
                    severity: Some(DiagnosticSeverity::ERROR),
                    code: None,
                    code_description: None,
                    source: Some("firefly".to_string()),
                    message: doc.clone(),
                    related_information: None,
                    tags: None,
                    data: None,
                })
            }

            *gn = syn;

            self.client.publish_diagnostics(uri, errs, None).await;
        }*/
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

*/

use firefly::{compiler::{compare::compare_top_level_nodes, lexer::Lexer, parser::Parser}, span::{Point, Span}};

fn main() {
    let input = "(A)\n(c)\n(a)";
    let input2 = "(A)\n(cd)\n(a)";
    let span = Span {
        start: Point {
            line: 1,
            column: 0,
        },
        end: Point {
            line: 1,
            column: 4,
        },
    };

    let mut e = Vec::new();

    let parser = Parser::new(Lexer::new(input));
    let (syntax, errors1) = parser.parse();

    let parser = Parser::new(Lexer::new(input2));
    let (syntax2, errors2) = parser.parse();

    println!("{:?}", errors1);
    println!("{:?}", errors2);

    compare_top_level_nodes(&syntax, &syntax2, &mut e, &span);

    for change in e {
        println!("{}", change)
    }
}