use serde_json::Map;
use superposition_core::parse_toml_config;
use superposition_core::serialize_to_toml;
use superposition_types::Config;

#[test]
#[cfg(not(feature = "jsonlogic"))]
fn test_filter_by_dimensions_debug() {
    let toml = r#"
[default-config]
timeout = { value = 30, schema = { type = "integer" } }

[dimensions]
dimension = { position = 1, schema = { type = "string" } }

[context."dimension=d1"]
timeout = 60

[context."dimension=d2"]
timeout = 90
"#;

    let config: Config = parse_toml_config(toml).unwrap();
    println!("\n=== Before filter ===");
    println!("Contexts count: {}", config.contexts.len());
    for ctx in &config.contexts {
        println!(
            "  - Context id: {}, override_key: {}",
            ctx.id,
            ctx.override_with_keys.get_key()
        );
    }
    println!(
        "Overrides keys: {:?}",
        config.overrides.keys().collect::<Vec<_>>()
    );

    // Simulate what API does - filter by empty dimension data
    let empty_dimensions: Map<String, serde_json::Value> = Map::new();
    let filtered_config = config.filter_by_dimensions(&empty_dimensions);

    println!("\n=== After filter (empty dimensions) ===");
    println!("Contexts count: {}", filtered_config.contexts.len());
    for ctx in &filtered_config.contexts {
        println!(
            "  - Context id: {}, override_key: {}",
            ctx.id,
            ctx.override_with_keys.get_key()
        );
    }
    println!(
        "Overrides keys: {:?}",
        filtered_config.overrides.keys().collect::<Vec<_>>()
    );

    println!("\n=== Serialized output ===");
    let serialized = serialize_to_toml(&filtered_config).unwrap();
    println!("{}", serialized);
}

