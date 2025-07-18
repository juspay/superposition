use superposition_provider::{ConfigurationOptions, SuperpositionOptions, SuperpositionProvider, SuperpositionProviderOptions, RefreshStrategy, PollingStrategy};
use open_feature::OpenFeature;
use tokio::time::{sleep, Duration};

#[tokio::main]
async fn main() {
    env_logger::init();

    let mut api = OpenFeature::singleton_mut().await;
    let options = SuperpositionProviderOptions {
        superposition_options: SuperpositionOptions {
            endpoint: "http://localhost:8080/".to_string(),
            token: "your_token_here".to_string(),
            org_id: "localorg".to_string(),
            workspace_id: "test".to_string(),
        },
        cac_options: ConfigurationOptions {
            fallback_config: None,
            evaluation_cache: None,
            refresh_strategy: RefreshStrategy::Polling(PollingStrategy {
                interval: 1,
                timeout: None
            }),
        },
        experimentation_options: None
    };
    api.set_provider(SuperpositionProvider::new(options)).await;
    let client = api.create_client();
    sleep(Duration::from_secs(3)).await;
    let val = client.get_int_value("integer", None, None).await.unwrap();
    println!("Value: {}", val);

    println!("Hello, world!");
}
