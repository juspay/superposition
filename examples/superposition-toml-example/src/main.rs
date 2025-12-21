use serde_json::{json, Map, Value};
use std::fs;
use superposition_core::{eval_toml_config, parse_toml_config, MergeStrategy};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== Superposition TOML Parser Example ===\n");

    // Read the TOML file
    let toml_path = "example.toml";
    println!("Reading TOML file from: {}", toml_path);
    let toml_content = fs::read_to_string(toml_path)?;

    // Parse the TOML configuration
    println!("\n--- Step 1: Parsing TOML Configuration ---");
    let parsed = parse_toml_config(&toml_content)?;
    println!("✓ Successfully parsed TOML file");
    println!("  - Default config keys: {}", parsed.default_config.len());
    println!("  - Dimensions: {}", parsed.dimensions.len());
    println!("  - Contexts: {}", parsed.contexts.len());
    println!("  - Override entries: {}", parsed.overrides.len());

    // Display default configuration
    println!("\n--- Default Configuration ---");
    for (key, value) in &parsed.default_config {
        println!("  {}: {}", key, value);
    }

    // Display dimensions
    println!("\n--- Available Dimensions ---");
    for (name, info) in &parsed.dimensions {
        println!("  {} (position: {})", name, info.position);
    }

    // Example 1: Basic bike ride
    println!("\n--- Example 1: Bike ride (no specific context) ---");
    let mut dims1 = Map::new();
    dims1.insert(
        "vehicle_type".to_string(),
        Value::String("bike".to_string()),
    );

    let config1 = eval_toml_config(&toml_content, &dims1, MergeStrategy::MERGE)?;
    println!("Input dimensions: vehicle_type=bike");
    println!("Resolved config:");
    println!(
        "  per_km_rate: {}",
        config1.get("per_km_rate").unwrap_or(&json!(null))
    );
    println!(
        "  surge_factor: {}",
        config1.get("surge_factor").unwrap_or(&json!(null))
    );

    // Example 2: Cab ride in Bangalore
    println!("\n--- Example 2: Cab ride in Bangalore ---");
    let mut dims2 = Map::new();
    dims2.insert("city".to_string(), Value::String("Bangalore".to_string()));
    dims2.insert("vehicle_type".to_string(), Value::String("cab".to_string()));

    let config2 = eval_toml_config(&toml_content, &dims2, MergeStrategy::MERGE)?;
    println!("Input dimensions: city=Bangalore, vehicle_type=cab");
    println!("Resolved config:");
    println!(
        "  per_km_rate: {}",
        config2.get("per_km_rate").unwrap_or(&json!(null))
    );
    println!(
        "  surge_factor: {}",
        config2.get("surge_factor").unwrap_or(&json!(null))
    );

    // Example 3: Cab ride in Delhi at 6 AM (morning surge)
    println!("\n--- Example 3: Cab ride in Delhi at 6 AM (morning surge) ---");
    let mut dims3 = Map::new();
    dims3.insert("city".to_string(), Value::String("Delhi".to_string()));
    dims3.insert("vehicle_type".to_string(), Value::String("cab".to_string()));
    dims3.insert("hour_of_day".to_string(), Value::Number(6.into()));

    let config3 = eval_toml_config(&toml_content, &dims3, MergeStrategy::MERGE)?;
    println!("Input dimensions: city=Delhi, vehicle_type=cab, hour_of_day=6");
    println!("Resolved config:");
    println!(
        "  per_km_rate: {}",
        config3.get("per_km_rate").unwrap_or(&json!(null))
    );
    println!(
        "  surge_factor: {}",
        config3.get("surge_factor").unwrap_or(&json!(null))
    );

    // Example 4: Cab ride in Delhi at 6 PM (evening surge)
    println!("\n--- Example 4: Cab ride in Delhi at 6 PM (evening surge) ---");
    let mut dims4 = Map::new();
    dims4.insert("city".to_string(), Value::String("Delhi".to_string()));
    dims4.insert("vehicle_type".to_string(), Value::String("cab".to_string()));
    dims4.insert("hour_of_day".to_string(), Value::Number(18.into()));

    let config4 = eval_toml_config(&toml_content, &dims4, MergeStrategy::MERGE)?;
    println!("Input dimensions: city=Delhi, vehicle_type=cab, hour_of_day=18");
    println!("Resolved config:");
    println!(
        "  per_km_rate: {}",
        config4.get("per_km_rate").unwrap_or(&json!(null))
    );
    println!(
        "  surge_factor: {}",
        config4.get("surge_factor").unwrap_or(&json!(null))
    );

    // Example 5: Auto ride (uses default values)
    println!("\n--- Example 5: Auto ride (uses default values) ---");
    let mut dims5 = Map::new();
    dims5.insert(
        "vehicle_type".to_string(),
        Value::String("auto".to_string()),
    );

    let config5 = eval_toml_config(&toml_content, &dims5, MergeStrategy::MERGE)?;
    println!("Input dimensions: vehicle_type=auto");
    println!("Resolved config:");
    println!(
        "  per_km_rate: {}",
        config5.get("per_km_rate").unwrap_or(&json!(null))
    );
    println!(
        "  surge_factor: {}",
        config5.get("surge_factor").unwrap_or(&json!(null))
    );

    println!("\n=== Example completed successfully! ===");
    Ok(())
}
