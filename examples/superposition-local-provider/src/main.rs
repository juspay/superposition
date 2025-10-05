use clap::{Parser, ValueEnum};
use open_feature::{EvaluationContext, EvaluationContextFieldValue, OpenFeature};
use std::collections::HashMap;
use std::fs;
use std::time::Duration;
use superposition_provider::{
    LocalRefreshStrategy, SuperpositionLocalProviderOptions, SuperpositionProvider,
};

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Refresh strategy to use
    #[arg(short, long, value_enum, default_value_t = RefreshStrategyArg::Manual)]
    refresh_strategy: RefreshStrategyArg,

    /// Path to the TOML configuration file
    #[arg(short, long, default_value = "example.cac.toml")]
    file_path: String,

    /// context key-value pairs (format: key=value). Can be specified multiple times.
    /// Example: --context city=Bangalore --context vehicle_type=cab
    #[arg(short, long, value_parser = parse_key_value)]
    context: Vec<(String, String)>,

    /// List of configuration keys to evaluate and print.
    /// Supports comma-separated values: --keys surge_factor,per_km_rate
    /// Or multiple specifications: --keys surge_factor --keys per_km_rate
    #[arg(short, long, value_parser = parse_keys)]
    keys: Vec<Vec<String>>,
}

/// Parse a single key-value pair from string
fn parse_key_value(s: &str) -> Result<(String, String), String> {
    let parts: Vec<&str> = s.splitn(2, '=').collect();
    if parts.len() != 2 {
        return Err(format!(
            "Invalid key-value pair '{}'. Expected format: key=value",
            s
        ));
    }
    Ok((parts[0].to_string(), parts[1].to_string()))
}

/// Parse comma-separated keys into a vector
fn parse_keys(s: &str) -> Result<Vec<String>, String> {
    Ok(s.split(',')
        .map(|key| key.trim().to_string())
        .filter(|key| !key.is_empty())
        .collect())
}

/// Discover available keys from the TOML configuration file
fn discover_available_keys(file_path: &str) -> Result<Vec<String>, String> {
    let content = fs::read_to_string(file_path)
        .map_err(|e| format!("Failed to read TOML file '{}': {}", file_path, e))?;

    let toml_value: toml::Value = content
        .parse()
        .map_err(|e| format!("Failed to parse TOML file '{}': {}", file_path, e))?;

    if let Some(default_config) = toml_value.get("default-config") {
        if let Some(table) = default_config.as_table() {
            let mut keys: Vec<String> = table.keys().cloned().collect();
            keys.sort(); // Sort for consistent ordering
            return Ok(keys);
        }
    }

    // Fallback to hardcoded keys if default-config section not found
    Ok(vec!["surge_factor".to_string(), "per_km_rate".to_string()])
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum RefreshStrategyArg {
    /// Manual reload only
    Manual,
    /// Reload file on each evaluation
    OnDemand,
    /// Watch file for changes and reload automatically
    FileWatch,
}

impl From<RefreshStrategyArg> for LocalRefreshStrategy {
    fn from(arg: RefreshStrategyArg) -> Self {
        match arg {
            RefreshStrategyArg::Manual => LocalRefreshStrategy::Manual,
            RefreshStrategyArg::OnDemand => LocalRefreshStrategy::OnDemand,
            RefreshStrategyArg::FileWatch => LocalRefreshStrategy::FileWatch,
        }
    }
}

#[tokio::main]
async fn main() {
    env_logger::init();

    let args = Args::parse();

    println!("Starting Superposition Local Provider Example");
    println!("Refresh Strategy: {:?}", args.refresh_strategy);
    println!("Configuration File: {}", args.file_path);

    if !args.context.is_empty() {
        println!("Context: {:?}", args.context);
    }

    let keys_to_evaluate = if args.keys.is_empty() {
        // Discover available keys from the TOML configuration
        match discover_available_keys(&args.file_path) {
            Ok(discovered_keys) => {
                println!(
                    "Auto-discovered keys from configuration: {:?}",
                    discovered_keys
                );
                discovered_keys
            }
            Err(e) => {
                println!("Warning: Failed to discover keys ({}), using defaults", e);
                vec!["surge_factor".to_string(), "per_km_rate".to_string()]
            }
        }
    } else {
        // Flatten the vector of vectors into a single vector and deduplicate while preserving order
        let mut seen = std::collections::HashSet::new();
        args.keys
            .into_iter()
            .flatten()
            .filter(|key| seen.insert(key.clone()))
            .collect()
    };
    println!("Keys to evaluate: {:?}", keys_to_evaluate);
    println!("---");

    // Get the OpenFeature API singleton
    let mut api = OpenFeature::singleton_mut().await;

    let options = SuperpositionLocalProviderOptions {
        file_path: args.file_path.clone(),
        evaluation_cache: None,
        refresh_strategy: args.refresh_strategy.into(),
    };

    // Configure the Superposition provider
    api.set_provider(SuperpositionProvider::local(options))
        .await;

    // Create a client
    let client = api.create_client();

    // Create evaluation context
    let mut context = EvaluationContext::default();
    context.targeting_key = Some("user_123".to_string());

    // Add custom fields from context arguments
    let mut custom_fields = HashMap::new();

    // Add context from command line arguments
    for (key, value) in &args.context {
        // Try to parse as float first, then fall back to string
        if let Ok(float_val) = value.parse::<f64>() {
            custom_fields
                .insert(key.clone(), EvaluationContextFieldValue::Float(float_val));
        } else {
            custom_fields.insert(
                key.clone(),
                EvaluationContextFieldValue::String(value.clone()),
            );
        }
    }

    context.custom_fields = custom_fields;

    match args.refresh_strategy {
        RefreshStrategyArg::FileWatch => {
            println!(
                "Running in FileWatch mode - configuration will be monitored for changes"
            );
            println!(
                "You can modify '{}' and see the values update automatically",
                args.file_path
            );
            println!("Press Ctrl+C to exit");
            println!("---");

            // Run in a loop, printing values every 10 seconds
            let mut interval = tokio::time::interval(Duration::from_secs(10));
            loop {
                interval.tick().await;

                match evaluate_flags(&client, &context, &keys_to_evaluate).await {
                    Ok(values) => {
                        let timestamp = chrono::Utc::now().format("%H:%M:%S");
                        let formatted_values: Vec<String> = values
                            .iter()
                            .map(|(key, value)| format!("{}: {}", key, value))
                            .collect();
                        println!("[{}] {}", timestamp, formatted_values.join(", "));
                    }
                    Err(e) => {
                        println!(
                            "[{}] Error evaluating flags: {}",
                            chrono::Utc::now().format("%H:%M:%S"),
                            e
                        );
                    }
                }
            }
        }
        _ => {
            // One-time evaluation for Manual and OnDemand strategies
            match evaluate_flags(&client, &context, &keys_to_evaluate).await {
                Ok(values) => {
                    let formatted_values: Vec<String> = values
                        .iter()
                        .map(|(key, value)| format!("{}: {}", key, value))
                        .collect();
                    println!("{}", formatted_values.join(", "));
                }
                Err(e) => {
                    println!("Error evaluating flags: {}", e);
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
enum ConfigValue {
    Boolean(bool),
    Float(f64),
    String(String),
}

impl std::fmt::Display for ConfigValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ConfigValue::Boolean(b) => write!(f, "{}", b),
            ConfigValue::Float(fl) => write!(f, "{:.2}", fl),
            ConfigValue::String(s) => write!(f, "{}", s),
        }
    }
}

async fn evaluate_flags(
    client: &open_feature::Client,
    context: &EvaluationContext,
    keys: &[String],
) -> Result<Vec<(String, ConfigValue)>, String> {
    let mut results = Vec::new();

    for key in keys {
        // Try different value types in order: boolean, float, string
        let value =
            if let Ok(bool_val) = client.get_bool_value(key, Some(context), None).await {
                ConfigValue::Boolean(bool_val)
            } else if let Ok(float_val) =
                client.get_float_value(key, Some(context), None).await
            {
                ConfigValue::Float(float_val)
            } else if let Ok(string_val) =
                client.get_string_value(key, Some(context), None).await
            {
                ConfigValue::String(string_val)
            } else {
                return Err(format!(
                    "Failed to get value for key '{}' as any supported type",
                    key
                ));
            };

        results.push((key.clone(), value));
    }

    Ok(results)
}
