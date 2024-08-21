use std::env;

use lovelace::{env::Event, server::Backend};
use tokio::sync::mpsc;
use tower_lsp::{LspService, Server};

#[tokio::main]
async fn main2() {
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

use anyhow::Result;
use wasmtime::*;

fn main() -> Result<()> {
    // Modules can be compiled through either the text or binary format
    let engine = Engine::default();
    
    let wat = r#"
        (module
            (import "host" "host_func" (func $host_hello (param i32)))

            (func (export "hello")
                i32.const 3
                call $host_hello)
        )
    "#;

    let modu = [
        0, 97, 115, 109, 1, 0, 0, 0, 1, 8, 2, 96, 1, 127, 0, 96, 0, 0, 2, 18, 1, 4, 104, 111, 115,
        116, 9, 104, 111, 115, 116, 95, 102, 117, 110, 99, 0, 0, 3, 2, 1, 1, 7, 9, 1, 5, 104, 101,
        108, 108, 111, 0, 1, 10, 8, 1, 6, 0, 65, 32, 16, 0, 11,
    ];

    let module = Module::from_binary(&engine, &modu)?;

    // Create a `Linker` which will be later used to instantiate this module.
    // Host functionality is defined by name within the `Linker`.
    let mut linker = Linker::new(&engine);

    linker.func_wrap(
        "host",
        "host_func",
        |caller: Caller<'_, u32>, param: i32| {
            println!("Got {} from WebAssembly", param);
            println!("my host state is: {}", caller.data());
        },
    )?;

    // All wasm objects operate within the context of a "store". Each
    // `Store` has a type parameter to store host-specific data, which in
    // this case we're using `4` for.
    let mut store = Store::new(&engine, 4);
    let instance = linker.instantiate(&mut store, &module)?;
    let hello = instance.get_typed_func::<(), ()>(&mut store, "hello")?;

    // And finally we can call the wasm!
    hello.call(&mut store, ())?;

    Ok(())
}
