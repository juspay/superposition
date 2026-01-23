/// Example: Using AllFeatureProvider Trait for Bulk Configuration Resolution
///
/// This example demonstrates:
/// - Resolving all features at once using AllFeatureProvider trait
/// - Using prefix filtering to get specific subsets of configuration
/// - Comparing different contexts and their resolved configurations
/// - Working with experiment metadata (when available)
///
/// This is more efficient than resolving features one by one when you need
/// multiple configuration values at once.
///
/// Prerequisites:
/// - A running Superposition server (e.g., http://localhost:8080)
/// - Valid org_id, workspace_id, and authentication token
/// - Some configuration data in the server

use std::collections::HashMap;
use std::sync::Arc;

use open_feature::EvaluationContext;
use superposition_provider::{
    AllFeatureProvider, FeatureExperimentMeta, HttpDataSource,
    LocalResolutionProvider, LocalResolutionProviderOptions, PollingStrategy, RefreshStrategy,
    SuperpositionOptions,
};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize logging
    env_logger::Builder::from_env(env_logger::Env::default().default_filter_or("info")).init();

    println!("=== AllFeatureProvider Trait Example ===\n");

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
            interval: 5, // Poll every 5 seconds
            timeout: None,
        }),
        fallback_config: None,
        enable_experiments: true,
    };

    // Create and initialize the provider
    println!("Creating LocalResolutionProvider with polling (5s interval)...");
    let provider = Arc::new(LocalResolutionProvider::new(
        http_data_source,
        provider_options,
    ));
    provider.init().await?;
    println!("Provider initialized successfully!\n");

    // Example 1: Resolve all features without filtering
    println!("=== Example 1: Resolve All Features (No Filter) ===");
    let default_context = EvaluationContext::default();
    match provider.resolve_all_features(&default_context).await {
        Ok(features) => {
            println!("Default configuration ({} keys):", features.len());
            for (key, value) in features.iter() {
                println!("  {} = {}", key, value);
            }
        }
        Err(e) => println!("Error: {}", e),
    }
    println!();

    // Example 2: Resolve with US user context
    println!("=== Example 2: Resolve All Features (US User) ===");
    let mut us_context = EvaluationContext::default();
    us_context
        .custom_fields
        .insert("country".to_string(), "US".into());

    match provider.resolve_all_features(&us_context).await {
        Ok(features) => {
            println!("US user configuration ({} keys):", features.len());
            for (key, value) in features.iter() {
                println!("  {} = {}", key, value);
            }
        }
        Err(e) => println!("Error: {}", e),
    }
    println!();

    // Example 3: Resolve with dimension context (d1)
    println!("=== Example 3: Resolve All Features (US + dimension=d1) ===");
    let mut d1_us_context = EvaluationContext::default();
    d1_us_context
        .custom_fields
        .insert("country".to_string(), "US".into());
    d1_us_context
        .custom_fields
        .insert("dimension".to_string(), "d1".into());

    match provider.resolve_all_features(&d1_us_context).await {
        Ok(features) => {
            println!("US + d1 user configuration ({} keys):", features.len());
            for (key, value) in features.iter() {
                println!("  {} = {}", key, value);
            }
        }
        Err(e) => println!("Error: {}", e),
    }
    println!();

    // Example 4: Compare configurations across different contexts
    println!("=== Example 4: Compare Configurations Across Contexts ===");

    let contexts = vec![
        ("Default", EvaluationContext::default()),
        ({
            let mut ctx = EvaluationContext::default();
            ctx.custom_fields.insert("country".to_string(), "US".into());
            ("US User", ctx)
        }),
        ({
            let mut ctx = EvaluationContext::default();
            ctx.custom_fields.insert("country".to_string(), "US".into());
            ctx.custom_fields.insert("dimension".to_string(), "d1".into());
            ("US + d1", ctx)
        }),
    ];

    let mut all_results: HashMap<&str, serde_json::Map<String, serde_json::Value>> =
        HashMap::new();

    for (name, context) in &contexts {
        if let Ok(features) = provider.resolve_all_features(context).await {
            all_results.insert(name, features);
        }
    }

    // Compare specific keys across contexts (matching local_http.rs feature flags)
    let keys_to_compare = vec!["feature_enabled", "feature_mode", "max_connections"];

    for key in &keys_to_compare {
        println!("Comparing '{}' across contexts:", key);
        for (name, features) in &all_results {
            let value = features
                .get(*key)
                .map(|v| v.to_string())
                .unwrap_or_else(|| "not found".to_string());
            println!("  {}: {}", name, value);
        }
        println!();
    }

    // Example 5: Experiment metadata (if experiments are enabled)
    println!("=== Example 5: Experiment Metadata ===");
    match provider.get_applicable_variants(&us_context).await {
        Ok(variants) => {
            if variants.is_empty() {
                println!("No experiments configured or experiments not enabled.");
            } else {
                println!("Applicable variant IDs:");
                for variant in &variants {
                    println!("  - {}", variant);
                }
            }
        }
        Err(e) => println!("Error getting variants: {}", e),
    }

    match provider.get_experiment_metadata(&us_context).await {
        Ok(metadata) => {
            if metadata.is_empty() {
                println!("No experiment metadata available.");
            } else {
                println!("\nExperiment metadata:");
                for meta in &metadata {
                    println!("  Experiment: {}", meta.experiment_id);
                    println!("    Variant: {}", meta.variant_id);
                    if let Some(exp_name) = &meta.experiment_name {
                        println!("    Name: {}", exp_name);
                    }
                }
            }
        }
        Err(e) => println!("Error getting experiment metadata: {}", e),
    }
    println!();

    // Example 6: Get provider metadata
    println!("=== Example 6: Provider Metadata ===");
    let metadata = provider.metadata();
    println!("Provider Name: {}", metadata.name);
    println!("Provider Version: {}", metadata.version);
    println!();

    // Cleanup
    println!("Shutting down provider...");
    provider.shutdown().await?;
    println!("Done!");

    Ok(())
}
