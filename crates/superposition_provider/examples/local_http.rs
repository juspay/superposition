/// Example: LocalResolutionProvider with HTTP Data Source and Polling
///
/// This example demonstrates:
/// - Setting up a LocalResolutionProvider with HTTP data source
/// - Using polling refresh strategy for automatic updates
/// - Resolving feature flags using OpenFeature client
/// - Accessing configuration with different contexts
///
/// Prerequisites:
/// - A running Superposition server (e.g., http://localhost:8080)
/// - Valid org_id, workspace_id, and authentication token
/// - Some configuration data in the server
use std::sync::Arc;
use std::time::Duration;

use open_feature::{EvaluationContext, OpenFeature};
use superposition_provider::{
    HttpDataSource, LocalResolutionProvider, LocalResolutionProviderOptions,
    PollingStrategy, RefreshStrategy, SuperpositionOptions,
};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize logging
    env_logger::Builder::from_env(env_logger::Env::default().default_filter_or("info"))
        .init();

    println!("=== LocalResolutionProvider with HTTP Data Source Example ===\n");

    // Configuration - replace with your actual values
    let endpoint = std::env::var("SUPERPOSITION_ENDPOINT")
        .unwrap_or_else(|_| "http://localhost:8080".to_string());
    let token =
        std::env::var("SUPERPOSITION_TOKEN").unwrap_or_else(|_| "test-token".to_string());
    let org_id =
        std::env::var("SUPERPOSITION_ORG_ID").unwrap_or_else(|_| "localorg".to_string());
    let workspace_id =
        std::env::var("SUPERPOSITION_WORKSPACE_ID").unwrap_or_else(|_| "dev".to_string());

    // Create HTTP data source
    println!("Creating HTTP data source...");
    let superposition_options =
        SuperpositionOptions::new(endpoint.clone(), token, org_id, workspace_id);
    let http_data_source = Arc::new(HttpDataSource::new(superposition_options));

    // Create provider options with polling strategy
    let provider_options = LocalResolutionProviderOptions {
        refresh_strategy: RefreshStrategy::Polling(PollingStrategy {
            interval: 5, // Poll every 30 seconds
            timeout: None,
        }),
        fallback_config: None,
        enable_experiments: true,
    };

    // Create and initialize the provider
    println!("Creating LocalResolutionProvider with polling (30s interval)...");
    let provider = LocalResolutionProvider::new(http_data_source, provider_options);

    println!("Initializing provider...");
    provider.init().await?;
    println!("Provider initialized successfully!\n");

    // Register the provider with OpenFeature
    let mut api = OpenFeature::singleton_mut().await;
    api.set_provider(provider).await;

    // Get a client
    let client = api.create_client();

    // Example 1: Resolve a boolean feature flag
    println!("Example 1: Resolving boolean feature flag");
    let context = EvaluationContext::default();
    match client
        .get_bool_value("feature_enabled", Some(&context), None)
        .await
    {
        Ok(value) => println!("  feature_enabled = {}", value),
        Err(e) => println!("  Error: {:?}", e),
    }
    println!();

    // Example 2: Resolve with specific context (US user)
    println!("Example 2: Resolving with context (country=US)");
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
    println!();

    // Example 3: Resolve string configuration
    println!("Example 3: Resolving string configuration");
    match client
        .get_string_value("feature_mode", Some(&us_context), None)
        .await
    {
        Ok(value) => println!("  feature_mode (US) = {}", value),
        Err(e) => println!("  Error: {:?}", e),
    }
    println!();

    // Example 4: Resolve with premium user context
    println!("Example 4: Resolving with dimension as d1 and country as US");
    let mut d1_us_context = EvaluationContext::default();
    d1_us_context
        .custom_fields
        .insert("country".to_string(), "US".into());
    d1_us_context
        .custom_fields
        .insert("dimension".to_string(), "d1".into());

    match client
        .get_int_value("max_connections", Some(&d1_us_context), None)
        .await
    {
        Ok(value) => println!("  max_connections (US d1) = {}", value),
        Err(e) => println!("  Error: {:?}", e),
    }
    println!();

    // Wait to demonstrate polling
    println!("Provider is now polling for updates every 30 seconds.");
    println!("The configuration will be automatically refreshed.");
    println!("Press Ctrl+C to stop.\n");

    // Keep the program running to demonstrate polling
    tokio::time::sleep(Duration::from_secs(120)).await;

    println!("\nDone!");

    Ok(())
}
