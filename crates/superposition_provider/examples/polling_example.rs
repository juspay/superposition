/// Demonstrates the Polling refresh strategy with LocalResolutionProvider
/// using the OpenFeature client interface.
///
/// This example connects to a Superposition server via HTTP, polls for config
/// changes every 10 seconds, and prints a config value in a loop using the
/// standard OpenFeature client API. Change the config on the server and watch
/// the printed value update automatically.
///
/// Usage:
///   RUST_LOG=info cargo run --example polling_example
///
/// Environment variables (all optional, with defaults shown):
///   SUPERPOSITION_ENDPOINT  http://localhost:8080
///   SUPERPOSITION_TOKEN     token
///   SUPERPOSITION_ORG_ID    localorg
///   SUPERPOSITION_WORKSPACE dev
///   POLL_INTERVAL           10        (seconds between server polls)
///   PRINT_INTERVAL          5         (seconds between printing the value)
///   CONFIG_KEY              max_connections   (the config key to watch)
use std::env;

use open_feature::{EvaluationContext, OpenFeature};
use superposition_provider::{
    data_source::http::HttpDataSource, local_provider::LocalResolutionProvider, PollingStrategy,
    RefreshStrategy, SuperpositionOptions,
};
use tokio::time::{sleep, Duration};

fn env_or(key: &str, default: &str) -> String {
    env::var(key).unwrap_or_else(|_| default.to_string())
}

#[tokio::main]
async fn main() {
    env_logger::init();

    let endpoint = env_or("SUPERPOSITION_ENDPOINT", "http://localhost:8080");
    let token = env_or("SUPERPOSITION_TOKEN", "token");
    let org_id = env_or("SUPERPOSITION_ORG_ID", "localorg");
    let workspace = env_or("SUPERPOSITION_WORKSPACE", "dev");
    let poll_interval: u64 = env_or("POLL_INTERVAL", "10").parse().unwrap_or(10);
    let print_interval: u64 = env_or("PRINT_INTERVAL", "5").parse().unwrap_or(5);
    let config_key = env_or("CONFIG_KEY", "max_connections");

    println!("=== Superposition Polling Example ===");
    println!("Endpoint:        {}", endpoint);
    println!("Org / Workspace: {} / {}", org_id, workspace);
    println!("Poll interval:   {}s", poll_interval);
    println!("Print interval:  {}s", print_interval);
    println!("Watching key:    {}", config_key);
    println!();

    let http_source = HttpDataSource::new(SuperpositionOptions::new(
        endpoint, token, org_id, workspace,
    ));

    let provider = LocalResolutionProvider::new(
        Box::new(http_source),
        None,
        RefreshStrategy::Polling(PollingStrategy {
            interval: poll_interval,
            timeout: Some(10),
        }),
    );

    // Register with OpenFeature and create a client
    let mut api = OpenFeature::singleton_mut().await;
    api.set_provider(provider).await;
    let client = api.create_client();

    // Allow time for the provider to initialize via OpenFeature
    sleep(Duration::from_secs(2)).await;

    println!(
        "Provider ready. Printing config every {}s (Ctrl-C to stop).\n",
        print_interval
    );

    let context = EvaluationContext::default();

    loop {
        match client
            .get_int_value(&config_key, Some(&context), None)
            .await
        {
            Ok(value) => println!(
                "[{}] {} = {}",
                chrono::Utc::now().format("%H:%M:%S"),
                config_key,
                value
            ),
            Err(e) => eprintln!(
                "[{}] Error resolving {}: {:?}",
                chrono::Utc::now().format("%H:%M:%S"),
                config_key,
                e
            ),
        }

        sleep(Duration::from_secs(print_interval)).await;
    }
}
