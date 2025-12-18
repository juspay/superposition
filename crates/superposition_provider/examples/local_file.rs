/// Example: LocalResolutionProvider with File Data Source (No Watching)
///
/// This example demonstrates:
/// - Setting up a LocalResolutionProvider with file data source
/// - Reading configuration from a .cac.toml file
/// - Using on-demand refresh strategy
/// - Resolving feature flags based on context
///
/// Prerequisites:
/// - A .cac.toml configuration file (uses test_data/example.cac.toml)

use std::path::PathBuf;
use std::sync::Arc;

use open_feature::{EvaluationContext, OpenFeature};
use superposition_provider::{
    FileDataSource, FileDataSourceOptions, LocalResolutionProvider,
    LocalResolutionProviderOptions, OnDemandStrategy, RefreshStrategy,
};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize logging
    env_logger::Builder::from_env(env_logger::Env::default().default_filter_or("info")).init();

    println!("=== LocalResolutionProvider with File Data Source Example ===\n");

    // Path to the CAC TOML file
    let config_path = PathBuf::from("test_data/example.cac.toml");

    if !config_path.exists() {
        eprintln!("Error: Configuration file not found at {:?}", config_path);
        eprintln!("Please ensure test_data/example.cac.toml exists.");
        return Ok(());
    }

    // Create file data source (without file watching)
    println!("Creating file data source from: {:?}", config_path);
    let file_data_source = Arc::new(FileDataSource::new(FileDataSourceOptions {
        config_path,
        watch_files: false, // No file watching in this example
    })?);

    // Create provider options with on-demand strategy
    let provider_options = LocalResolutionProviderOptions {
        refresh_strategy: RefreshStrategy::OnDemand(OnDemandStrategy {
            ttl: 60, // Refresh if data is older than 60 seconds
            timeout: None,
            use_stale_on_error: None,
        }),
        fallback_config: None,
        enable_experiments: false, // File source doesn't support experiments yet
    };

    // Create and initialize the provider
    println!("Creating LocalResolutionProvider...");
    let provider = LocalResolutionProvider::new(
        file_data_source,
        provider_options,
    );

    println!("Initializing provider...");
    provider.init().await?;
    println!("Provider initialized successfully!\n");

    // Register the provider with OpenFeature
    let mut api = OpenFeature::singleton_mut().await;
    api.set_provider(provider).await;

    // Get a client
    let client = api.create_client();

    // Example 1: Default configuration (no context)
    println!("Example 1: Default configuration (no context)");
    let context = EvaluationContext::default();
    match client
        .get_bool_value("feature_enabled", Some(&context), None)
        .await
    {
        Ok(value) => println!("  feature_enabled = {}", value),
        Err(e) => println!("  Error: {:?}", e),
    }
    match client
        .get_string_value("api_endpoint", Some(&context), None)
        .await
    {
        Ok(value) => println!("  api_endpoint = {}", value),
        Err(e) => println!("  Error: {:?}", e),
    }
    match client
        .get_int_value("max_connections", Some(&context), None)
        .await
    {
        Ok(value) => println!("  max_connections = {}", value),
        Err(e) => println!("  Error: {:?}", e),
    }
    println!();

    // Example 2: US user context
    println!("Example 2: US user context");
    let mut us_context = EvaluationContext::default();
    us_context
        .custom_fields
        .insert("country".to_string(), "US".into());

    match client
        .get_bool_value("feature_enabled", Some(&us_context), None)
        .await
    {
        Ok(value) => println!("  feature_enabled (US) = {}", value),
        Err(e) => println!("  Error: {:?}", e),
    }
    match client
        .get_string_value("api_endpoint", Some(&us_context), None)
        .await
    {
        Ok(value) => println!("  api_endpoint (US) = {}", value),
        Err(e) => println!("  Error: {:?}", e),
    }
    println!();

    // Example 3: Premium user
    println!("Example 3: Premium user");
    let mut premium_context = EvaluationContext::default();
    premium_context
        .custom_fields
        .insert("user_tier".to_string(), "premium".into());

    match client
        .get_int_value("max_connections", Some(&premium_context), None)
        .await
    {
        Ok(value) => println!("  max_connections (premium) = {}", value),
        Err(e) => println!("  Error: {:?}", e),
    }
    match client
        .get_float_value("timeout_seconds", Some(&premium_context), None)
        .await
    {
        Ok(value) => println!("  timeout_seconds (premium) = {}", value),
        Err(e) => println!("  Error: {:?}", e),
    }
    println!();

    // Example 4: US web user (multiple conditions)
    println!("Example 4: US web user (multiple conditions)");
    let mut us_web_context = EvaluationContext::default();
    us_web_context
        .custom_fields
        .insert("country".to_string(), "US".into());
    us_web_context
        .custom_fields
        .insert("platform".to_string(), "web".into());

    match client
        .get_string_value("api_endpoint", Some(&us_web_context), None)
        .await
    {
        Ok(value) => println!("  api_endpoint (US web) = {}", value),
        Err(e) => println!("  Error: {:?}", e),
    }
    match client
        .get_int_value("max_connections", Some(&us_web_context), None)
        .await
    {
        Ok(value) => println!("  max_connections (US web) = {}", value),
        Err(e) => println!("  Error: {:?}", e),
    }
    println!();

    // Example 5: Enterprise mobile user
    println!("Example 5: Enterprise mobile user");
    let mut enterprise_mobile_context = EvaluationContext::default();
    enterprise_mobile_context
        .custom_fields
        .insert("user_tier".to_string(), "enterprise".into());
    enterprise_mobile_context
        .custom_fields
        .insert("platform".to_string(), "mobile".into());

    match client
        .get_int_value("max_connections", Some(&enterprise_mobile_context), None)
        .await
    {
        Ok(value) => println!("  max_connections (enterprise mobile) = {}", value),
        Err(e) => println!("  Error: {:?}", e),
    }
    match client
        .get_float_value("timeout_seconds", Some(&enterprise_mobile_context), None)
        .await
    {
        Ok(value) => println!("  timeout_seconds (enterprise mobile) = {}", value),
        Err(e) => println!("  Error: {:?}", e),
    }
    println!();

    println!("Done!");

    Ok(())
}
