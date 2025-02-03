use anyhow::Result;
use std::path::PathBuf;

#[derive(Default, Debug)]
pub struct Args {
    pub config_file: Option<PathBuf>,
    pub log_level: u64,
    pub log_file: Option<PathBuf>,
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
                _ => break,
            }
        }
        Ok(args)
    }
}
