use serde_json::{json, Map, Value};
use std::fs;
use superposition_core::{eval_config, parse_toml_config, MergeStrategy};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== Superposition TOML Parser Example ===\n");

    // Read the TOML file
    let toml_path = "example.toml";
    println!("Reading TOML file from: {}", toml_path);
    let toml_content = fs::read_to_string(toml_path)?;

    // STEP 1: Parse the TOML configuration using parse_toml_config
    println!("\n--- Step 1: Parsing TOML Configuration ---");
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

    // STEP 2: Use the parsed Config with eval_config for evaluation
    println!("\n--- Step 2: Evaluating Configuration with Different Dimensions ---");
    println!("\nNow we'll use the parsed Config struct with eval_config() to resolve");
    println!("configurations based on different input dimensions.\n");

    // Example 1: Basic bike ride
    println!("--- Example 1: Bike ride (no specific city) ---");
    let mut dims1 = Map::new();
    dims1.insert(
        "vehicle_type".to_string(),
        Value::String("bike".to_string()),
    );

    // Clone default configs once for all evaluations
    let default_configs = (*config.default_configs).clone();

    let result1 = eval_config(
        default_configs.clone(),
        &config.contexts,
        &config.overrides,
        &config.dimensions,
        &dims1,
        MergeStrategy::MERGE,
        None,
    )?;

    println!("Input dimensions: vehicle_type=bike");
    println!("Resolved config:");
    println!(
        "  per_km_rate: {}",
        result1.get("per_km_rate").unwrap_or(&json!(null))
    );
    println!(
        "  surge_factor: {}",
        result1.get("surge_factor").unwrap_or(&json!(null))
    );

    // Example 2: Cab ride in Bangalore
    println!("\n--- Example 2: Cab ride in Bangalore ---");
    let mut dims2 = Map::new();
    dims2.insert("city".to_string(), Value::String("Bangalore".to_string()));
    dims2.insert("vehicle_type".to_string(), Value::String("cab".to_string()));

    let result2 = eval_config(
        default_configs.clone(),
        &config.contexts,
        &config.overrides,
        &config.dimensions,
        &dims2,
        MergeStrategy::MERGE,
        None,
    )?;

    println!("Input dimensions: city=Bangalore, vehicle_type=cab");
    println!("Resolved config:");
    println!(
        "  per_km_rate: {}",
        result2.get("per_km_rate").unwrap_or(&json!(null))
    );
    println!(
        "  surge_factor: {}",
        result2.get("surge_factor").unwrap_or(&json!(null))
    );

    // Example 3: Cab ride in Delhi at 6 AM (morning surge)
    println!("\n--- Example 3: Cab ride in Delhi at 6 AM (morning surge) ---");
    let mut dims3 = Map::new();
    dims3.insert("city".to_string(), Value::String("Delhi".to_string()));
    dims3.insert("vehicle_type".to_string(), Value::String("cab".to_string()));
    dims3.insert("hour_of_day".to_string(), Value::Number(6.into()));

    let result3 = eval_config(
        default_configs.clone(),
        &config.contexts,
        &config.overrides,
        &config.dimensions,
        &dims3,
        MergeStrategy::MERGE,
        None,
    )?;

    println!("Input dimensions: city=Delhi, vehicle_type=cab, hour_of_day=6");
    println!("Resolved config:");
    println!(
        "  per_km_rate: {}",
        result3.get("per_km_rate").unwrap_or(&json!(null))
    );
    println!(
        "  surge_factor: {}",
        result3.get("surge_factor").unwrap_or(&json!(null))
    );

    // Example 4: Auto ride (uses default values)
    println!("\n--- Example 4: Auto ride (uses default values) ---");
    let mut dims4 = Map::new();
    dims4.insert(
        "vehicle_type".to_string(),
        Value::String("auto".to_string()),
    );

    let result4 = eval_config(
        default_configs.clone(),
        &config.contexts,
        &config.overrides,
        &config.dimensions,
        &dims4,
        MergeStrategy::MERGE,
        None,
    )?;

    println!("Input dimensions: vehicle_type=auto");
    println!("Resolved config:");
    println!(
        "  per_km_rate: {}",
        result4.get("per_km_rate").unwrap_or(&json!(null))
    );
    println!(
        "  surge_factor: {}",
        result4.get("surge_factor").unwrap_or(&json!(null))
    );

    println!("\n=== Example completed successfully! ===");
    println!("\nThis example demonstrated:");
    println!("1. parse_toml_config() - Parsing TOML into a Config struct");
    println!("2. eval_config() - Evaluating the Config with different input dimensions");
    Ok(())
}
