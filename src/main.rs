mod application;
mod args;
mod bytecode;
mod client;
mod code_action;
mod completion_cache;
mod config;
mod connection;
mod controller;
mod dispatch;
mod document;
mod editor;
mod error;
mod fuzzy;
mod handlers;
mod job;
mod logging;
mod lsp;
mod lsp_ext;
mod main_loop;
mod msg;
mod registry;
mod req_queue;
mod syntax;
mod thread;
mod utils;

use anyhow::{Context, Result};
use args::Args;
use config::{initialize_config_file, initialize_log_file};
use log::{error, info};
use logging::init_tracing;

use crate::{connection::Connection, main_loop::main_loop};

fn setup_logging(verbosity: u64) -> Result<()> {
    // Only use tracing for all logging
    init_tracing(verbosity)?;

    Ok(())
}

fn main() {
    if let Err(err) = try_main() {
        error!("Unexpected error: {}", err);
        eprintln!("{}", err);
    }
}

fn try_main() -> Result<()> {
    let args = Args::parse_args().context("could not parse arguments")?;
    
    // Handle help and version flags
    if args.show_help {
        Args::print_help();
        return Ok(());
    }
    
    if args.show_version {
        Args::print_version();
        return Ok(());
    }
    
    // Check if --stdio flag is provided
    if !args.stdio {
        eprintln!("Error: --stdio flag is required to start the server");
        eprintln!("Use --help for usage information");
        std::process::exit(1);
    }
    
    initialize_config_file(args.config_file);
    initialize_log_file(args.log_file);
    
    if let Err(e) = setup_logging(args.log_level) {
        eprintln!("Failed to setup logging: {}", e);
        eprintln!("Error chain:");
        let mut source = e.source();
        while let Some(err) = source {
            eprintln!("  Caused by: {}", err);
            source = err.source();
        }
        return Err(e).context("failed to initialize logging");
    }
    
    info!("Server starting...");
    with_extra_thread("LspProxy", run_server)?;

    Ok(())
}

const STACK_SIZE: usize = 1024 * 1024 * 8;

fn with_extra_thread(
    thread_name: impl Into<String>,
    f: impl FnOnce() -> Result<()> + Send + 'static,
) -> Result<()> {
    let handle = std::thread::Builder::new()
        .name(thread_name.into())
        .stack_size(STACK_SIZE)
        .spawn(f)?;

    match handle.join() {
        Ok(res) => res,
        Err(panic) => std::panic::resume_unwind(panic),
    }
}

// #[tokio::main]
fn run_server() -> Result<()> {
    let (connect, io_threads) = Connection::stdio();
    let syn_loader_config = config::default_syntax_loader();
    main_loop(connect, syn_loader_config).unwrap();
    info!("Server started successfully.");
    io_threads.join()?;
    Ok(())
}
