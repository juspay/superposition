use open_feature::{EvaluationContext, OpenFeature};
use superposition_provider::{
    ExperimentationOptions, OnDemandStrategy, RefreshStrategy, SuperpositionProvider,
    SuperpositionProviderOptions,
};
use superposition_sdk::{
    types::{DimensionType, WorkspaceStatus},
    Client, Config,
};

const WORKSPACE_ID: &str = "rustprovidertest";
const ENDPOINT: &str = "http://localhost:8080";
const TOKEN: &str = "12131";

/// Helper to create SDK client with bearer token auth
fn create_sdk_client() -> Client {
    use superposition_sdk::config::Token;

    let config = Config::builder()
        .endpoint_url(ENDPOINT)
        .bearer_token(Token::new(TOKEN, None))
        .behavior_version_latest()
        .build();

    Client::from_conf(config)
}

/// Setup functions - mirrors Kotlin/JS/Python implementations
async fn create_organisation(client: &Client) -> String {
    let output = client
        .create_organisation()
        .name("rusttestorg")
        .admin_email("admin@rusttestorg.com")
        .send()
        .await
        .expect("Failed to create organisation");

    println!(
        "Organisation created: {} with ID: {}",
        output.name, output.id
    );
    output.id
}

async fn create_workspace(client: &Client, org_id: &str, workspace_name: &str) {
    client
        .create_workspace()
        .org_id(org_id)
        .workspace_name(workspace_name)
        .workspace_admin_email("test@tests.com")
        .workspace_status(WorkspaceStatus::Enabled)
        .strict_mode(true)
        .allow_experiment_self_approval(true)
        .auto_populate_control(true)
        .send()
        .await
        .expect("Failed to create workspace");

    println!("Workspace created: {}", workspace_name);
}

async fn create_dimensions(client: &Client, org_id: &str, workspace_id: &str) {
    println!("Creating dimensions:");

    use aws_smithy_types::Document;

    // Dimension 1: name (string)
    client
        .create_dimension()
        .workspace_id(workspace_id)
        .org_id(org_id)
        .dimension("name")
        .position(1)
        .schema("type", Document::from("string"))
        .description("customer name dimension")
        .change_reason("adding name dimension")
        .dimension_type(DimensionType::Regular)
        .send()
        .await
        .expect("Failed to create name dimension");
    println!("  - Created dimension: name");

    // Dimension 2: city (string)
    client
        .create_dimension()
        .workspace_id(workspace_id)
        .org_id(org_id)
        .dimension("city")
        .position(2)
        .schema("type", Document::from("string"))
        .description("city dimension")
        .change_reason("adding city dimension")
        .dimension_type(DimensionType::Regular)
        .send()
        .await
        .expect("Failed to create city dimension");
    println!("  - Created dimension: city");

    // Dimension 3: customers (LOCAL_COHORT with platinum/gold/otherwise)
    // Build enum array
    let enum_array = Document::Array(vec![
        Document::from("platinum"),
        Document::from("gold"),
        Document::from("otherwise"),
    ]);

    // Build platinum definition
    let platinum_def = Document::Object(
        [(
            "in".to_string(),
            Document::Array(vec![
                Document::Object(
                    [("var".to_string(), Document::from("name"))]
                        .into_iter()
                        .collect(),
                ),
                Document::Array(vec![Document::from("Agush"), Document::from("Sauyav")]),
            ]),
        )]
        .into_iter()
        .collect(),
    );

    // Build gold definition
    let gold_def = Document::Object(
        [(
            "in".to_string(),
            Document::Array(vec![
                Document::Object(
                    [("var".to_string(), Document::from("name"))]
                        .into_iter()
                        .collect(),
                ),
                Document::Array(vec![Document::from("Angit"), Document::from("Bhrey")]),
            ]),
        )]
        .into_iter()
        .collect(),
    );

    // Build definitions object
    let definitions = Document::Object(
        [
            ("platinum".to_string(), platinum_def),
            ("gold".to_string(), gold_def),
        ]
        .into_iter()
        .collect(),
    );

    client
        .create_dimension()
        .workspace_id(workspace_id)
        .org_id(org_id)
        .dimension("customers")
        .position(3)
        .schema("type", Document::from("string"))
        .schema("enum", enum_array)
        .schema("definitions", definitions)
        .description("customers dimension")
        .change_reason("adding customers dimension")
        .dimension_type(DimensionType::LocalCohort("name".to_string()))
        .send()
        .await
        .expect("Failed to create customers dimension");
    println!("  - Created dimension: customers");
}

async fn create_default_configs(client: &Client, org_id: &str, workspace_id: &str) {
    println!("Creating default configs:");

    use aws_smithy_types::Document;

    // Config 1: price (number, minimum 0)
    client
        .create_default_config()
        .key("price")
        .value(Document::from(10000))
        .schema("type", Document::from("number"))
        .schema("minimum", Document::from(0))
        .description("price as a positive number")
        .change_reason("adding price config")
        .workspace_id(workspace_id)
        .org_id(org_id)
        .send()
        .await
        .expect("Failed to create price config");
    println!("  - Created config: price");

    // Config 2: currency (enum: Rupee/Dollar/Euro)
    let currency_enum = Document::Array(vec![
        Document::from("Rupee"),
        Document::from("Dollar"),
        Document::from("Euro"),
    ]);

    client
        .create_default_config()
        .key("currency")
        .value(Document::from("Rupee"))
        .schema("type", Document::from("string"))
        .schema("enum", currency_enum)
        .description("currency as an enum")
        .change_reason("adding currency config")
        .workspace_id(workspace_id)
        .org_id(org_id)
        .send()
        .await
        .expect("Failed to create currency config");
    println!("  - Created config: currency");
}

async fn create_overrides(client: &Client, org_id: &str, workspace_id: &str) {
    println!("Creating overrides:");

    use aws_smithy_types::Document;

    // Override 1: Boston -> Dollar
    client
        .create_context()
        .workspace_id(workspace_id)
        .org_id(org_id)
        .context("city", Document::from("Boston"))
        .r#override("currency", Document::from("Dollar"))
        .description("Bostonian")
        .change_reason("testing")
        .send()
        .await
        .expect("Failed to create Boston override");
    println!("  - Created override: Boston -> Dollar");

    // Override 2: Berlin -> Euro
    client
        .create_context()
        .workspace_id(workspace_id)
        .org_id(org_id)
        .context("city", Document::from("Berlin"))
        .r#override("currency", Document::from("Euro"))
        .description("Berlin")
        .change_reason("testing")
        .send()
        .await
        .expect("Failed to create Berlin override");
    println!("  - Created override: Berlin -> Euro");

    // Override 3: platinum -> price 5000
    client
        .create_context()
        .workspace_id(workspace_id)
        .org_id(org_id)
        .context("customers", Document::from("platinum"))
        .r#override("price", Document::from(5000))
        .description("platinum customer")
        .change_reason("testing")
        .send()
        .await
        .expect("Failed to create platinum override");
    println!("  - Created override: platinum -> price 5000");

    // Override 4: gold -> price 8000
    client
        .create_context()
        .workspace_id(workspace_id)
        .org_id(org_id)
        .context("customers", Document::from("gold"))
        .r#override("price", Document::from(8000))
        .description("gold customers")
        .change_reason("testing")
        .send()
        .await
        .expect("Failed to create gold override");
    println!("  - Created override: gold -> price 8000");

    // Override 5: karbik (otherwise) -> price 1
    client
        .create_context()
        .workspace_id(workspace_id)
        .org_id(org_id)
        .context("name", Document::from("karbik"))
        .context("customers", Document::from("otherwise"))
        .r#override("price", Document::from(1))
        .description("edge case customer karbik")
        .change_reason("testing")
        .send()
        .await
        .expect("Failed to create karbik override");
    println!("  - Created override: karbik -> price 1");
}

async fn setup_with_sdk(org_id: &str, workspace_id: &str) {
    println!("\n=== Setting up test environment ===\n");

    let client = create_sdk_client();

    create_workspace(&client, org_id, workspace_id).await;
    create_dimensions(&client, org_id, workspace_id).await;
    create_default_configs(&client, org_id, workspace_id).await;
    create_overrides(&client, org_id, workspace_id).await;

    println!("\n=== Setup complete ===\n");
}

async fn run_provider_tests(org_id: &str, workspace_id: &str) {
    println!("\n=== Starting OpenFeature provider tests ===\n");

    // Create provider with on-demand refresh strategy
    let provider_options = SuperpositionProviderOptions {
        endpoint: ENDPOINT.to_string(),
        token: TOKEN.to_string(),
        org_id: org_id.to_string(),
        workspace_id: workspace_id.to_string(),
        refresh_strategy: RefreshStrategy::OnDemand(OnDemandStrategy::default()),
        evaluation_cache: None,
        fallback_config: None,
        experimentation_options: Some(ExperimentationOptions {
            refresh_strategy: RefreshStrategy::OnDemand(OnDemandStrategy::default()),
            evaluation_cache: None,
            default_toss: None,
        }),
    };

    let provider = SuperpositionProvider::new(provider_options);

    // Set provider as the global provider
    let mut api = OpenFeature::singleton_mut().await;
    api.set_provider(provider).await;

    let client = api.create_client();

    // Test 1: Default values (no context)
    println!("Test 1: Default values (no context)");
    {
        let ctx = EvaluationContext::default();
        let price = client
            .get_float_value("price", Some(&ctx), None)
            .await
            .unwrap();
        let currency = client
            .get_string_value("currency", Some(&ctx), None)
            .await
            .unwrap();

        assert_eq!(price, 10000.0, "Default price should be 10000");
        assert_eq!(currency, "Rupee", "Default currency should be Rupee");
        println!("  ✓ Test passed\n");
    }

    // Test 2: Platinum customer - Agush, no city
    println!("Test 2: Platinum customer - Agush (no city)");
    {
        let ctx = EvaluationContext::default().with_custom_field("name", "Agush");
        let price = client
            .get_float_value("price", Some(&ctx), None)
            .await
            .unwrap();
        let currency = client
            .get_string_value("currency", Some(&ctx), None)
            .await
            .unwrap();

        assert_eq!(price, 5000.0, "Price should be 5000 for platinum customer");
        assert_eq!(currency, "Rupee", "Currency should be default Rupee");
        println!("  ✓ Test passed\n");
    }

    // Test 3: Platinum customer - Sauyav, with city Boston
    println!("Test 3: Platinum customer - Sauyav with city Boston");
    {
        let ctx = EvaluationContext::default()
            .with_custom_field("name", "Sauyav")
            .with_custom_field("city", "Boston");
        let price = client
            .get_float_value("price", Some(&ctx), None)
            .await
            .unwrap();
        let currency = client
            .get_string_value("currency", Some(&ctx), None)
            .await
            .unwrap();

        assert_eq!(price, 5000.0, "Price should be 5000");
        assert_eq!(currency, "Dollar", "Currency should be Dollar");
        println!("  ✓ Test passed\n");
    }

    // Test 4: Regular customer - John (no city)
    println!("Test 4: Regular customer - John (no city)");
    {
        let ctx = EvaluationContext::default().with_custom_field("name", "John");
        let price = client
            .get_float_value("price", Some(&ctx), None)
            .await
            .unwrap();
        let currency = client
            .get_string_value("currency", Some(&ctx), None)
            .await
            .unwrap();

        assert_eq!(price, 10000.0, "Price should be default 10000");
        assert_eq!(currency, "Rupee", "Currency should be default Rupee");
        println!("  ✓ Test passed\n");
    }

    // Test 5: Platinum customer - Sauyav with city Berlin
    println!("Test 5: Platinum customer - Sauyav with city Berlin");
    {
        let ctx = EvaluationContext::default()
            .with_custom_field("name", "Sauyav")
            .with_custom_field("city", "Berlin");
        let price = client
            .get_float_value("price", Some(&ctx), None)
            .await
            .unwrap();
        let currency = client
            .get_string_value("currency", Some(&ctx), None)
            .await
            .unwrap();

        assert_eq!(price, 5000.0, "Price should be 5000");
        assert_eq!(currency, "Euro", "Currency should be Euro in Berlin");
        println!("  ✓ Test passed\n");
    }

    // Test 6: Regular customer - John with city Boston
    println!("Test 6: Regular customer - John with city Boston");
    {
        let ctx = EvaluationContext::default()
            .with_custom_field("name", "John")
            .with_custom_field("city", "Boston");
        let price = client
            .get_float_value("price", Some(&ctx), None)
            .await
            .unwrap();
        let currency = client
            .get_string_value("currency", Some(&ctx), None)
            .await
            .unwrap();

        assert_eq!(price, 10000.0, "Price should be default 10000");
        assert_eq!(currency, "Dollar", "Currency should be Dollar in Boston");
        println!("  ✓ Test passed\n");
    }

    // Test 7: Edge case customer - karbik (specific override)
    println!("Test 7: Edge case customer - karbik (specific override)");
    {
        let ctx = EvaluationContext::default().with_custom_field("name", "karbik");
        let price = client
            .get_float_value("price", Some(&ctx), None)
            .await
            .unwrap();
        let currency = client
            .get_string_value("currency", Some(&ctx), None)
            .await
            .unwrap();

        assert_eq!(price, 1.0, "Price should be 1 for karbik");
        assert_eq!(currency, "Rupee", "Currency should be default Rupee");
        println!("  ✓ Test passed\n");
    }

    // Test 8: Edge case customer - karbik with city Boston
    println!("Test 8: Edge case customer - karbik with city Boston");
    {
        let ctx = EvaluationContext::default()
            .with_custom_field("name", "karbik")
            .with_custom_field("city", "Boston");
        let price = client
            .get_float_value("price", Some(&ctx), None)
            .await
            .unwrap();
        let currency = client
            .get_string_value("currency", Some(&ctx), None)
            .await
            .unwrap();

        assert_eq!(price, 1.0, "Price should be 1 for karbik");
        assert_eq!(currency, "Dollar", "Currency should be Dollar in Boston");
        println!("  ✓ Test passed\n");
    }

    println!("\n=== All tests passed! ===\n");
}

#[tokio::test]
#[ignore]
async fn test_rust_provider_integration() {
    // Create organisation
    let client = create_sdk_client();
    let org_id = create_organisation(&client).await;

    // Setup test environment using SDK
    setup_with_sdk(&org_id, WORKSPACE_ID).await;

    // Run provider tests
    run_provider_tests(&org_id, WORKSPACE_ID).await;
}
