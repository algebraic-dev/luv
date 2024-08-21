use std::env;

use lovelace::{env::Event, server::Backend};
use tokio::sync::mpsc;
use tower_lsp::{LspService, Server};

#[tokio::main]
async fn main() {
    env::set_var("RUST_BACKTRACE", "1");

    let (stdin, stdout) = (tokio::io::stdin(), tokio::io::stdout());
    let (sender, mut receiver) = mpsc::channel(8);
    let (service, socket) = LspService::new(|client| Backend::new(client, sender));
    let backend = service.inner().clone();

    // Spawn a task to handle the receiver
    tokio::spawn(async move {
        while let Some(message) = receiver.recv().await {
            match message {
                Event::Recompiled(rec) => backend.handle_recompile(rec).await,
                Event::Log(log) => backend.handle_env_log(log).await,
                _ => (),
            }
        }
    });

    Server::new(stdin, stdout, socket).serve(service).await;
}
