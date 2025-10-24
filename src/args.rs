use anyhow::Result;
use std::path::PathBuf;

#[derive(Default, Debug)]
pub struct Args {
    pub config_file: Option<PathBuf>,
    pub log_level: u64,
    pub log_file: Option<PathBuf>,
    pub stdio: bool,
    pub show_help: bool,
    pub show_version: bool,
    pub max_item_num: usize,
    pub enable_bytecode: bool,
}

impl Args {
    pub fn parse_args() -> Result<Args> {
        let mut args = Args::default();
        let mut argv = std::env::args().peekable();
        argv.next(); // skip the program, we don't care about that;

        while let Some(arg) = argv.next() {
            match arg.as_str() {
                "-c" | "--config" => match argv.next().as_deref() {
                    Some(path) => args.config_file = Some(path.into()),
                    None => anyhow::bail!("--config must specify a path to read"),
                },
                "--max-item" => match argv.next().as_deref() {
                    Some(n) => {
                        args.max_item_num = match n.parse() {
                            Ok(n) => n,
                            Err(_) => 20,
                        }
                    }
                    None => args.max_item_num = 20,
                },
                "--log-level" => match argv.next().as_deref() {
                    Some(level) => {
                        args.log_level = match level.parse() {
                            Ok(num) => num,
                            Err(_) => 1,
                        }
                    }
                    None => anyhow::bail!("--log-level must specify to a level"),
                },
                "--log" => match argv.next().as_deref() {
                    Some(path) => args.log_file = Some(path.into()),
                    None => anyhow::bail!("--log must specify path to write"),
                },
                "--stdio" => args.stdio = true,
                "--bytecode" => args.enable_bytecode = true,
                "-h" | "--help" => {
                    args.show_help = true;
                    return Ok(args);
                }
                "-V" | "--version" => {
                    args.show_version = true;
                    return Ok(args);
                }
                _ => anyhow::bail!("unknown argument: {}", arg),
            }
        }
        Ok(args)
    }

    pub fn print_help() {
        println!("emacs-lsp-proxy {}", env!("CARGO_PKG_VERSION"));
        println!("{}", env!("CARGO_PKG_DESCRIPTION"));
        println!();
        println!("USAGE:");
        println!("    emacs-lsp-proxy [OPTIONS] --stdio");
        println!();
        println!("OPTIONS:");
        println!("    -c, --config <FILE>       Set configuration file path");
        println!("        --log <FILE>          Set log file path");
        println!("        --log-level <LEVEL>   Set log level (0-3, default: 1)");
        println!("        --stdio               Enable stdio communication mode (required)");
        println!("        --bytecode            Enable bytecode optimization for JSON-RPC");
        println!("    -h, --help               Print help information");
        println!("    -V, --version            Print version information");
    }

    pub fn print_version() {
        println!("emacs-lsp-proxy {}", env!("CARGO_PKG_VERSION"));
    }
}
