use serde_json::{json, Map, Value};
use std::fs;
use superposition_core::{
    eval_config, parse_json_config, parse_toml_config, MergeStrategy,
};

fn evaluate_and_print(
    config: &superposition_core::Config,
    default_configs: &Map<String, Value>,
    description: &str,
    dimensions: &Map<String, Value>,
) -> Result<(), Box<dyn std::error::Error>> {
    println!("\n--- {} ---", description);

    let result = eval_config(
        default_configs.clone(),
        &config.contexts,
        &config.overrides,
        &config.dimensions,
        dimensions,
        MergeStrategy::MERGE,
        None,
    )?;

    let dims_str: Vec<String> = dimensions
        .iter()
        .map(|(k, v)| format!("{}={}", k, v))
        .collect();
    println!("Input dimensions: {}", dims_str.join(", "));
    println!("Resolved config:");
    println!(
        "  per_km_rate: {}",
        result.get("per_km_rate").unwrap_or(&json!(null))
    );
    println!(
        "  surge_factor: {}",
        result.get("surge_factor").unwrap_or(&json!(null))
    );

    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== Superposition Config File Examples ===\n");

    // Example 1: TOML Format
    println!("═══════════════════════════════════════════════════════════════");
    println!("                    EXAMPLE 1: TOML FORMAT                     ");
    println!("═══════════════════════════════════════════════════════════════");

    run_toml_example()?;

    // Example 2: JSON Format
    println!("\n═══════════════════════════════════════════════════════════════");
    println!("                    EXAMPLE 2: JSON FORMAT                     ");
    println!("═══════════════════════════════════════════════════════════════");

    run_json_example()?;

    println!("\n=== Both examples completed successfully! ===");
    println!("\nThis example demonstrated:");
    println!("1. parse_toml_config() - Parsing TOML into a Config struct");
    println!("2. parse_json_config() - Parsing JSON into a Config struct");
    println!("3. eval_config() - Evaluating the Config with different input dimensions");
    println!("4. Both formats produce equivalent results");

    Ok(())
}

fn run_toml_example() -> Result<(), Box<dyn std::error::Error>> {
    // Read the TOML file from the same directory as the binary
    let manifest_dir = std::env!("CARGO_MANIFEST_DIR");
    let toml_path = format!("{}/example.toml", manifest_dir);
    println!("\nReading TOML file from: {}", toml_path);
    let toml_content = fs::read_to_string(&toml_path)?;

    // Parse the TOML configuration
    println!("\n--- Parsing TOML Configuration ---");
    let config = parse_toml_config(&toml_content)?;
    println!("✓ Successfully parsed TOML file");
    println!("  - Default config keys: {}", config.default_configs.len());
    println!("  - Dimensions: {}", config.dimensions.len());
    println!("  - Contexts: {}", config.contexts.len());
    println!("  - Override entries: {}", config.overrides.len());

    // Display default configuration
    println!("\n--- Default Configuration ---");
    for (key, value) in &*config.default_configs {
        println!("  {}: {}", key, value);
    }

    // Display dimensions
    println!("\n--- Available Dimensions ---");
    for (name, info) in &config.dimensions {
        println!("  {} (position: {})", name, info.position);
    }

    // Evaluate with different dimensions
    println!("\n--- Evaluating TOML Configuration ---");
    let default_configs = (*config.default_configs).clone();

    // Example 1: Basic bike ride
    let mut dims1 = Map::new();
    dims1.insert(
        "vehicle_type".to_string(),
        Value::String("bike".to_string()),
    );
    evaluate_and_print(
        &config,
        &default_configs,
        "Bike ride (no specific city)",
        &dims1,
    )?;

    // Example 2: Cab ride in Bangalore
    let mut dims2 = Map::new();
    dims2.insert("city".to_string(), Value::String("Bangalore".to_string()));
    dims2.insert("vehicle_type".to_string(), Value::String("cab".to_string()));
    evaluate_and_print(&config, &default_configs, "Cab ride in Bangalore", &dims2)?;

    // Example 3: Cab ride in Delhi at 6 AM (morning surge)
    let mut dims3 = Map::new();
    dims3.insert("city".to_string(), Value::String("Delhi".to_string()));
    dims3.insert("vehicle_type".to_string(), Value::String("cab".to_string()));
    dims3.insert("hour_of_day".to_string(), Value::Number(6.into()));
    evaluate_and_print(
        &config,
        &default_configs,
        "Cab ride in Delhi at 6 AM (morning surge)",
        &dims3,
    )?;

    // Example 4: Auto ride (uses default values)
    let mut dims4 = Map::new();
    dims4.insert(
        "vehicle_type".to_string(),
        Value::String("auto".to_string()),
    );
    evaluate_and_print(
        &config,
        &default_configs,
        "Auto ride (uses default values)",
        &dims4,
    )?;

    // Example 5: Chennai ride
    let mut dims5 = Map::new();
    dims5.insert("city".to_string(), Value::String("Chennai".to_string()));
    evaluate_and_print(
        &config,
        &default_configs,
        "Chennai ride (uses default values)",
        &dims5,
    )?;

    Ok(())
}

fn run_json_example() -> Result<(), Box<dyn std::error::Error>> {
    // Read the JSON file from the same directory as the binary
    let manifest_dir = std::env!("CARGO_MANIFEST_DIR");
    let json_path = format!("{}/example.json", manifest_dir);
    println!("\nReading JSON file from: {}", json_path);
    let json_content = fs::read_to_string(&json_path)?;

    // Parse the JSON configuration
    println!("\n--- Parsing JSON Configuration ---");
    let config = parse_json_config(&json_content)?;
    println!("✓ Successfully parsed JSON file");
    println!("  - Default config keys: {}", config.default_configs.len());
    println!("  - Dimensions: {}", config.dimensions.len());
    println!("  - Contexts: {}", config.contexts.len());
    println!("  - Override entries: {}", config.overrides.len());

    // Display default configuration
    println!("\n--- Default Configuration ---");
    for (key, value) in &*config.default_configs {
        println!("  {}: {}", key, value);
    }

    // Display dimensions
    println!("\n--- Available Dimensions ---");
    for (name, info) in &config.dimensions {
        println!("  {} (position: {})", name, info.position);
    }

    // Evaluate with different dimensions
    println!("\n--- Evaluating JSON Configuration ---");
    let default_configs = (*config.default_configs).clone();

    // Example 1: Basic bike ride
    let mut dims1 = Map::new();
    dims1.insert(
        "vehicle_type".to_string(),
        Value::String("bike".to_string()),
    );
    evaluate_and_print(
        &config,
        &default_configs,
        "Bike ride (no specific city)",
        &dims1,
    )?;

    // Example 2: Cab ride in Bangalore
    let mut dims2 = Map::new();
    dims2.insert("city".to_string(), Value::String("Bangalore".to_string()));
    dims2.insert("vehicle_type".to_string(), Value::String("cab".to_string()));
    evaluate_and_print(&config, &default_configs, "Cab ride in Bangalore", &dims2)?;

    // Example 3: Cab ride in Delhi at 6 AM (morning surge)
    let mut dims3 = Map::new();
    dims3.insert("city".to_string(), Value::String("Delhi".to_string()));
    dims3.insert("vehicle_type".to_string(), Value::String("cab".to_string()));
    dims3.insert("hour_of_day".to_string(), Value::Number(6.into()));
    evaluate_and_print(
        &config,
        &default_configs,
        "Cab ride in Delhi at 6 AM (morning surge)",
        &dims3,
    )?;

    // Example 4: Auto ride (uses default values)
    let mut dims4 = Map::new();
    dims4.insert(
        "vehicle_type".to_string(),
        Value::String("auto".to_string()),
    );
    evaluate_and_print(
        &config,
        &default_configs,
        "Auto ride (uses default values)",
        &dims4,
    )?;

    // Example 5: Chennai ride
    let mut dims5 = Map::new();
    dims5.insert("city".to_string(), Value::String("Chennai".to_string()));
    evaluate_and_print(
        &config,
        &default_configs,
        "Chennai ride (uses default values)",
        &dims5,
    )?;

    Ok(())
}
