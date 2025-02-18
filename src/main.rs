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
use config::{initialize_config_file, initialize_log_file, log_file};
use log::{error, info};

use crate::{connection::Connection, main_loop::main_loop};

fn setup_logging(verbosity: u64) -> Result<()> {
    let mut base_config = fern::Dispatch::new();

    base_config = match verbosity {
        0 => base_config.level(log::LevelFilter::Warn),
        1 => base_config.level(log::LevelFilter::Info),
        2 => base_config.level(log::LevelFilter::Debug),
        _3_or_more => base_config.level(log::LevelFilter::Trace),
    };

    let file_config = fern::Dispatch::new()
        .format(|out, message, record| {
            out.finish(format_args!(
                "{} {} [{}] {}",
                chrono::Local::now().format("%Y-%m-%dT%H:%M:%S%.3f"),
                record.target(),
                record.level(),
                message
            ))
        })
        .chain(fern::log_file(log_file())?);

    base_config.chain(file_config).apply()?;

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
    initialize_config_file(args.config_file);
    initialize_log_file(args.log_file);
    setup_logging(args.log_level).context("failed to initialize logging")?;
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
