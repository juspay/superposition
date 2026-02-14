use open_feature::EvaluationContext;
use superposition_provider::{
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

    let provider = LocalResolutionProvider::new(
        Box::new(http_source),
        None,
        RefreshStrategy::Polling(PollingStrategy {
            interval: 30,
            timeout: Some(10),
        }),
    );
    provider.init().await.unwrap();

    let context = EvaluationContext::default()
        .with_targeting_key("user-123")
        .with_custom_field("os", "android");

    let all_config = provider.resolve_all_features(&context).await.unwrap();
    println!("All config: {:?}", all_config);

    let variants = provider.get_applicable_variants(&context).await.unwrap();
    println!("Variants: {:?}", variants);

    provider.close().await.unwrap();
}
