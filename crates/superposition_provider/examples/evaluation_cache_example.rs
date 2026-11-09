use open_feature::EvaluationContext;
use superposition_provider::{
    data_source::http::HttpDataSource,
    local_provider::LocalResolutionProvider,
    traits::{AllFeatureProvider, FeatureExperimentMeta},
    AuthMethod, EvaluationCacheOptions, PollingStrategy, RefreshStrategy,
    SuperpositionOptions,
};

/// Local provider with the evaluation result cache turned on: resolutions
/// are memoized in-process and the cache is emptied whenever the polling
/// refresh reloads config or experiment data.
///
/// Note: the remote `SuperpositionAPIProvider` has no evaluation cache —
/// it receives no invalidation signal from the server, so caching there
/// would serve stale entries until arbitrary eviction. Use the local
/// provider when memoization matters.
#[tokio::main]
async fn main() {
    env_logger::init();

    let options = SuperpositionOptions::new(
        "http://localhost:8080".to_string(),
        AuthMethod::Token("token".to_string()),
        "localorg".to_string(),
        "dev".to_string(),
    );

    // Local provider: evaluates in-process against the cached config.
    // Keep up to 5_000 memoized resolutions; `None` or `0` disables caching.
    let local_provider = LocalResolutionProvider::with_evaluation_cache(
        Box::new(HttpDataSource::new(options)),
        None,
        RefreshStrategy::Polling(PollingStrategy::new(30_000).with_timeout(10_000)),
        Some(EvaluationCacheOptions::new(5_000)),
    );
    local_provider
        .init(EvaluationContext::default())
        .await
        .unwrap();

    let context = EvaluationContext::default()
        .with_targeting_key("user-1234")
        .with_custom_field("dimension", "d2");

    // First call populates the cache...
    let local_first = local_provider
        .resolve_all_features(context.clone())
        .await
        .unwrap();
    println!("Local (miss, evaluated): {:?}", local_first);

    // ...and the identical second call is served from it without re-evaluating.
    let local_second = local_provider
        .resolve_all_features(context.clone())
        .await
        .unwrap();
    println!("Local (hit, memoized): {:?}", local_second);
    assert_eq!(local_first, local_second);

    let variants = local_provider
        .get_applicable_variants(context, None, None)
        .await
        .unwrap();
    println!("Variants: {:?}", variants);

    local_provider.close_provider().await.unwrap();
}
