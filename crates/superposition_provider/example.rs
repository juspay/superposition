use std::{collections::HashMap, hash::Hash};

use open_feature::OpenFeature;
use superposition_provider::{
    ConfigurationOptions, ExperimentationOptions, PollingStrategy, RefreshStrategy,
    SuperpositionOptions, SuperpositionProvider, SuperpositionProviderOptions,
};
use tokio::time::{sleep, Duration};

#[tokio::main]
async fn main() {
    env_logger::init();

    let mut api = OpenFeature::singleton_mut().await;
    let options = SuperpositionProviderOptions {
        endpoint: "http://localhost:8080/".to_string(),
        token: "your_token_here".to_string(),
        org_id: "localorg".to_string(),
        workspace_id: "test".to_string(),
        fallback_config: None,
        evaluation_cache: None,
        refresh_strategy: RefreshStrategy::Polling(PollingStrategy {
            interval: 1,
            timeout: None,
        }),
        experimentation_options: Some(ExperimentationOptions {
            refresh_strategy: RefreshStrategy::Polling(PollingStrategy {
                interval: 1,
                timeout: None,
            }),
            evaluation_cache: None,
            default_toss: None,
        }),
    };
    api.set_provider(SuperpositionProvider::new(options)).await;
    let client = api.create_client();
    sleep(Duration::from_secs(3)).await;
    let context = open_feature::EvaluationContext {
        custom_fields: HashMap::from([(
            "d1".to_string(),
            open_feature::EvaluationContextFieldValue::String("d1".to_string()),
        )]),
        targeting_key: Some("15".to_string()),
    };
    let val = client
        .get_string_value("string", Some(&context), None)
        .await
        .unwrap();
    println!("Value: {}", val);

    println!("Hello, world!");
}
