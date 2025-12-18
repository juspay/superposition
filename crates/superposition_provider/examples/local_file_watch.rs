/// Example: LocalResolutionProvider with File Data Source and File Watching
///
/// This example demonstrates:
/// - Setting up a LocalResolutionProvider with file data source
/// - Enabling file watching for automatic reload on file changes
/// - Real-time configuration updates when the .cac.toml file is modified
/// - Long-running provider with periodic configuration checks
///
/// To test:
/// 1. Run this example
/// 2. In another terminal, edit test_data/example.cac.toml
/// 3. Observe the automatic reload and new configuration values
///
/// Prerequisites:
/// - A .cac.toml configuration file (uses test_data/example.cac.toml)

use std::path::PathBuf;
use std::sync::Arc;
use std::time::Duration;

use open_feature::EvaluationContext;
use superposition_provider::{
    AllFeatureProvider, FileDataSource, FileDataSourceOptions, LocalResolutionProvider,
    LocalResolutionProviderOptions, OnDemandStrategy, RefreshStrategy,
};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize logging
    env_logger::Builder::from_env(env_logger::Env::default().default_filter_or("info")).init();

    println!("=== LocalResolutionProvider with File Watching Example ===\n");

    // Path to the CAC TOML file
    let config_path = PathBuf::from("test_data/example.cac.toml");

    if !config_path.exists() {
        eprintln!("Error: Configuration file not found at {:?}", config_path);
        eprintln!("Please ensure test_data/example.cac.toml exists.");
        return Ok(());
    }

    println!("Configuration file: {:?}", config_path);
    println!("File watching: ENABLED");
    println!("The configuration will automatically reload when the file changes.\n");

    // Create file data source WITH file watching enabled
    println!("Creating file data source with file watching...");
    let file_data_source = Arc::new(FileDataSource::new(FileDataSourceOptions {
        config_path: config_path.clone(),
        watch_files: true, // Enable file watching
    })?);

    // Create provider options with on-demand strategy
    let provider_options = LocalResolutionProviderOptions {
        refresh_strategy: RefreshStrategy::OnDemand(OnDemandStrategy {
            ttl: 5, // Short TTL for demonstration
            timeout: None,
            use_stale_on_error: None,
        }),
        fallback_config: None,
        enable_experiments: false,
    };

    // Create and initialize the provider
    println!("Creating and initializing LocalResolutionProvider...");
    let provider = Arc::new(LocalResolutionProvider::new(
        file_data_source,
        provider_options,
    ));
    provider.init().await?;
    println!("Provider initialized successfully!\n");

    // Create test context
    let mut test_context = EvaluationContext::default();
    test_context
        .custom_fields
        .insert("country".to_string(), "US".into());
    test_context
        .custom_fields
        .insert("platform".to_string(), "web".into());

    println!("=== Starting Periodic Configuration Checks ===");
    println!("Context: country=US, platform=web");
    println!("\nTry editing {:?} and watch the configuration update!\n", config_path);

    // Periodically check and display configuration
    let mut iteration = 0;
    loop {
        iteration += 1;
        println!("--- Check #{} ---", iteration);

        // Resolve all features
        match provider.resolve_all_features(&test_context).await {
            Ok(features) => {
                // Display key features
                if let Some(value) = features.get("feature_enabled") {
                    println!("  feature_enabled: {}", value);
                }
                if let Some(value) = features.get("api_endpoint") {
                    println!("  api_endpoint: {}", value);
                }
                if let Some(value) = features.get("max_connections") {
                    println!("  max_connections: {}", value);
                }
                if let Some(value) = features.get("timeout_seconds") {
                    println!("  timeout_seconds: {}", value);
                }
                println!("  (Total keys: {})", features.len());
            }
            Err(e) => {
                println!("  Error resolving features: {}", e);
            }
        }

        println!();

        // Wait before next check
        tokio::time::sleep(Duration::from_secs(10)).await;

        // Run for a limited time in example mode
        if iteration >= 30 {
            println!("Demo completed after {} iterations.", iteration);
            break;
        }
    }

    // Cleanup
    println!("\nShutting down provider...");
    provider.shutdown().await?;
    println!("Done!");

    Ok(())
}
