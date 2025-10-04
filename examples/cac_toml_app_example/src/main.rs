use clap::{Arg, Command};
use serde_json::{Map, Value};
use std::process;
use superposition_toml::SuperpositionToml;

fn main() {
    let args = Command::new("CAC Demo App")
        .arg(
            Arg::new("dimension")
                .long("dimension")
                .short('d')
                .value_name("KEY=VALUE")
                .action(clap::ArgAction::Append)
                .help("Sets a key-value pair")
                .num_args(1),
        )
        .arg(
            Arg::new("file")
                .long("file")
                .short('f')
                .help("take a cac config file as input")
                .required(true)
                .num_args(1),
        )
        .get_matches();

    let mut dimensions: Map<String, Value> = Map::new();

    if let Some(values) = args.get_many::<String>("dimension") {
        for value in values {
            let parts: Vec<&str> = value.split('=').collect();
            if parts.len() == 2 {
                dimensions
                    .insert(parts[0].to_string(), Value::String(parts[1].to_string()));
            }
        }
    }

    let file: String = args.get_one::<String>("file").unwrap().to_string();

    let config = SuperpositionToml::parse(&file).unwrap_or_else(|_err| {
        eprintln!("Could not parse file at {}", file);
        process::exit(-1);
    });

    println!("{:#?}", config.get_resolved_config(&dimensions));
    process::exit(0);
}
