use std::path::PathBuf;

use open_feature::{EvaluationContext, OpenFeature};
use superposition_provider::{
    data_source::file::FileDataSource, data_source::http::HttpDataSource,
    local_provider::LocalResolutionProvider, PollingStrategy, RefreshStrategy,
    SuperpositionOptions,
};
use tokio::time::{sleep, Duration};

#[tokio::main]
async fn main() {
    env_logger::init();

    let http_source = HttpDataSource::new(SuperpositionOptions::new(
        "http://localhost:8080".to_string(),
        "token".to_string(),
        "localorg".to_string(),
        "dev".to_string(),
    ));

    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let file_source = FileDataSource::new(manifest_dir.join("examples/config.toml"));

    let provider = LocalResolutionProvider::new(
        Box::new(http_source),
        Some(Box::new(file_source)),
        RefreshStrategy::Polling(PollingStrategy {
            interval: 10,
            timeout: Some(10),
        }),
    );

    // Register with OpenFeature and create a client
    let mut api = OpenFeature::singleton_mut().await;
    api.set_provider(provider).await;
    let client = api.create_client();

    // Allow time for the provider to initialize via OpenFeature
    sleep(Duration::from_secs(2)).await;

    println!("=== Superposition Fallback + Polling Example ===");
    println!("Primary: HTTP (localhost:8080), Fallback: config.toml");
    println!("Polling every 10s. Printing config every 5s (Ctrl-C to stop).\n");

    let context = EvaluationContext::default()
        .with_targeting_key("user-456")
        .with_custom_field("os", "linux")
        .with_custom_field("city", "Berlin");

    loop {
        let ts = chrono::Utc::now().format("%H:%M:%S");

        match client
            .get_string_value("currency", Some(&context), None)
            .await
        {
            Ok(value) => print!("[{}] currency = {}", ts, value),
            Err(e) => print!("[{}] currency error: {:?}", ts, e),
        }

        match client.get_int_value("timeout", Some(&context), None).await {
            Ok(value) => println!("  |  timeout = {}", value),
            Err(e) => println!("  |  timeout error: {:?}", e),
        }

        sleep(Duration::from_secs(5)).await;
    }
}
