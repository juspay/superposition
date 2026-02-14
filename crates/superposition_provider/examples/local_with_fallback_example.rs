use std::path::PathBuf;

use open_feature::EvaluationContext;
use superposition_provider::{
    data_source::file::FileDataSource,
    data_source::http::HttpDataSource,
    local_provider::LocalResolutionProvider,
    traits::{AllFeatureProvider, FeatureExperimentMeta},
    PollingStrategy, RefreshStrategy, SuperpositionOptions,
};

#[tokio::main]
async fn main() {
    env_logger::init();

    let http_source = HttpDataSource::new(SuperpositionOptions::new(
        "http://localhost:8080".to_string(),
        "token".to_string(),
        "org1".to_string(),
        "workspace1".to_string(),
    ));

    let file_source = FileDataSource::new(PathBuf::from("./config.toml"));

    let provider = LocalResolutionProvider::new(
        Box::new(http_source),
        Some(Box::new(file_source)),
        RefreshStrategy::Polling(PollingStrategy {
            interval: 30,
            timeout: Some(10),
        }),
    );
    provider.init().await.unwrap();

    let context = EvaluationContext::default()
        .with_targeting_key("user-456")
        .with_custom_field("os", "android")
        .with_custom_field("app_version", "3.2.1");

    // Bulk resolution
    let all_config = provider.resolve_all_features(&context).await.unwrap();
    println!("All config: {:?}", all_config);

    // Filtered resolution
    let filtered = provider
        .resolve_all_features_with_filter(
            &context,
            Some(&["payment.".to_string(), "ui.".to_string()]),
        )
        .await
        .unwrap();
    println!("Filtered config: {:?}", filtered);

    // Experiment variants
    let variants = provider.get_applicable_variants(&context).await.unwrap();
    println!("Variants: {:?}", variants);

    // Manual refresh
    provider.refresh().await.unwrap();

    provider.close().await.unwrap();
}
