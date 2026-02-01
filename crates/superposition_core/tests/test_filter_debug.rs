use serde_json::{Map, Value};
use superposition_core::parse_toml_config;
use superposition_core::serialize_to_toml;
use superposition_types::{
    Config, DefaultConfigInfo, DefaultConfigWithSchema, DetailedConfig,
};

/// Helper function to convert Config to DetailedConfig by inferring schema from value.
fn config_to_detailed(config: &Config) -> DetailedConfig {
    let default_configs: std::collections::BTreeMap<String, DefaultConfigInfo> = config
        .default_configs
        .iter()
        .map(|(key, value)| {
            // Infer schema from value
            let schema = match value {
                Value::String(_) => serde_json::json!({ "type": "string" }),
                Value::Number(n) => {
                    if n.is_i64() {
                        serde_json::json!({ "type": "integer" })
                    } else {
                        serde_json::json!({ "type": "number" })
                    }
                }
                Value::Bool(_) => serde_json::json!({ "type": "boolean" }),
                Value::Array(_) => serde_json::json!({ "type": "array" }),
                Value::Object(_) => serde_json::json!({ "type": "object" }),
                Value::Null => serde_json::json!({ "type": "null" }),
            };
            (
                key.clone(),
                DefaultConfigInfo {
                    value: value.clone(),
                    schema,
                },
            )
        })
        .collect();

    DetailedConfig {
        contexts: config.contexts.clone(),
        overrides: config.overrides.clone(),
        default_configs: DefaultConfigWithSchema(default_configs),
        dimensions: config.dimensions.clone(),
    }
}

#[test]
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
    let detailed_config = config_to_detailed(&filtered_config);
    let serialized = serialize_to_toml(&detailed_config).unwrap();
    println!("{}", serialized);
}
