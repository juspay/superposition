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
/// - A .cac.toml configuration file (uses test_data/example.cac.toml)

use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;

use open_feature::EvaluationContext;
use superposition_provider::{
    AllFeatureProvider, FeatureExperimentMeta, FileDataSource, FileDataSourceOptions,
    LocalResolutionProvider, LocalResolutionProviderOptions, OnDemandStrategy, RefreshStrategy,
};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize logging
    env_logger::Builder::from_env(env_logger::Env::default().default_filter_or("info")).init();

    println!("=== AllFeatureProvider Trait Example ===\n");

    // Set up file data source
    let config_path = PathBuf::from("test_data/example.cac.toml");
    if !config_path.exists() {
        eprintln!("Error: Configuration file not found at {:?}", config_path);
        return Ok(());
    }

    let file_data_source = Arc::new(FileDataSource::new(FileDataSourceOptions {
        config_path,
        watch_files: false,
    })?);

    let provider_options = LocalResolutionProviderOptions {
        refresh_strategy: RefreshStrategy::OnDemand(OnDemandStrategy {
            ttl: 60,
            timeout: None,
            use_stale_on_error: None,
        }),
        fallback_config: None,
        enable_experiments: false,
    };

    let provider = Arc::new(LocalResolutionProvider::new(
        file_data_source,
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

    // Example 3: Resolve with prefix filtering
    println!("=== Example 3: Resolve Features with Prefix Filter ===");
    let prefixes = vec!["feature".to_string(), "api".to_string()];
    match provider
        .resolve_all_features_with_filter(&us_context, Some(&prefixes))
        .await
    {
        Ok(features) => {
            println!("Filtered features (prefixes: {:?}):", prefixes);
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
            ctx.custom_fields
                .insert("user_tier".to_string(), "premium".into());
            ("Premium User", ctx)
        }),
        ({
            let mut ctx = EvaluationContext::default();
            ctx.custom_fields
                .insert("user_tier".to_string(), "enterprise".into());
            ("Enterprise User", ctx)
        }),
        ({
            let mut ctx = EvaluationContext::default();
            ctx.custom_fields
                .insert("platform".to_string(), "mobile".into());
            ("Mobile User", ctx)
        }),
    ];

    let mut all_results: HashMap<&str, serde_json::Map<String, serde_json::Value>> =
        HashMap::new();

    for (name, context) in &contexts {
        if let Ok(features) = provider.resolve_all_features(context).await {
            all_results.insert(name, features);
        }
    }

    // Compare a specific key across contexts
    let key_to_compare = "max_connections";
    println!("Comparing '{}' across contexts:", key_to_compare);
    for (name, features) in &all_results {
        let value = features
            .get(key_to_compare)
            .map(|v| v.to_string())
            .unwrap_or_else(|| "not found".to_string());
        println!("  {}: {}", name, value);
    }
    println!();

    let key_to_compare = "timeout_seconds";
    println!("Comparing '{}' across contexts:", key_to_compare);
    for (name, features) in &all_results {
        let value = features
            .get(key_to_compare)
            .map(|v| v.to_string())
            .unwrap_or_else(|| "not found".to_string());
        println!("  {}: {}", name, value);
    }
    println!();

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
