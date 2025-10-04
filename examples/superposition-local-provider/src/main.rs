use open_feature::{EvaluationContext, OpenFeature};
use std::collections::HashMap;
use superposition_provider::{
    LocalRefreshStrategy, SuperpositionLocalProviderOptions, SuperpositionProvider,
};
use tokio::time::{Duration, sleep};

#[tokio::main]
async fn main() {
    env_logger::init();

    // Get the OpenFeature API singleton
    let mut api = OpenFeature::singleton_mut().await;

    let options = SuperpositionLocalProviderOptions {
        file_path: String::from("example.cac.toml"),
        evaluation_cache: None,
        refresh_strategy: LocalRefreshStrategy::Manual,
    };
    // Configure the Superposition provider
    api.set_provider(SuperpositionProvider::local(options))
        .await;

    // Create a client
    let client = api.create_client();

    // Wait for initialization
    sleep(Duration::from_secs(3)).await;

    let mut context = EvaluationContext::default();
    context.targeting_key = Some("user_123".to_string());

    // Add custom fields
    let mut custom_fields = HashMap::new();
    custom_fields.insert(
        "city".to_string(),
        open_feature::EvaluationContextFieldValue::String("Bangalore".to_string()),
    );
    custom_fields.insert(
        "vehicle_type".to_string(),
        open_feature::EvaluationContextFieldValue::String("cab".to_string()),
    );
    context.custom_fields = custom_fields;

    // Evaluate feature flags
    let surge_factor = client
        .get_float_value("surge_factor", Some(&context), None)
        .await
        .unwrap();

    let per_km_rate = client
        .get_float_value("per_km_rate", Some(&context), None)
        .await
        .unwrap();

    println!(
        "surge_factor: {:.2}, per_km_rate: {:.2}",
        surge_factor, per_km_rate
    );
}
