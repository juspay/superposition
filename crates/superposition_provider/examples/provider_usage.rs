use open_feature::EvaluationContext;
use superposition_provider::{
    LocalRefreshStrategy, SuperpositionLocalProviderOptions, SuperpositionProvider,
    SuperpositionRemoteProviderOptions, RefreshStrategy, OnDemandStrategy,
};
use open_feature::provider::FeatureProvider;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Example 1: Using Remote Provider
    let remote_options = SuperpositionRemoteProviderOptions::new(
        "https://api.superposition.io".to_string(),
        "your-api-token".to_string(),
        "your-org-id".to_string(),
        "your-workspace-id".to_string(),
        None, // fallback_config
        None, // evaluation_cache
        RefreshStrategy::OnDemand(OnDemandStrategy::default()),
        None, // experimentation_options
    );

    let mut remote_provider = SuperpositionProvider::remote(remote_options);
    let context = EvaluationContext::default();
    
    // Initialize the remote provider
    remote_provider.initialize(&context).await;
    println!("Remote provider status: {:?}", remote_provider.status());

    // Example 2: Using Local Provider
    let local_options = SuperpositionLocalProviderOptions::with_file_path(
        "/path/to/your/config.toml".to_string()
    ).with_refresh_strategy(LocalRefreshStrategy::OnDemand);

    let mut local_provider = SuperpositionProvider::local(local_options);
    
    // Initialize the local provider
    local_provider.initialize(&context).await;
    println!("Local provider status: {:?}", local_provider.status());

    // Example 3: Backwards compatibility - old constructor still works
    #[allow(deprecated)]
    let mut legacy_provider = SuperpositionProvider::new(
        SuperpositionRemoteProviderOptions::new(
            "https://api.superposition.io".to_string(),
            "your-api-token".to_string(),
            "your-org-id".to_string(),
            "your-workspace-id".to_string(),
            None,
            None,
            RefreshStrategy::OnDemand(OnDemandStrategy::default()),
            None,
        )
    );
    
    legacy_provider.initialize(&context).await;
    println!("Legacy provider status: {:?}", legacy_provider.status());

    Ok(())
}