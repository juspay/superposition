use redis_module::{redis_module, Context, RedisError, RedisResult, RedisString};
use serde_json::{json, Value, Map};
use std::collections::HashMap;
use std::fs;

mod config;

// Define the data structures needed for configuration
#[derive(Debug, Clone, serde::Deserialize)]
struct ConfigData {
    contexts: Vec<ConfigContext>,
    overrides: HashMap<String, Map<String, Value>>,
    default_configs: Map<String, Value>,
}

#[derive(Debug, Clone, serde::Deserialize)]
struct ConfigContext {
    id: String,
    condition: Map<String, Value>,
    priority: u32,
    weight: u32,
    override_with_keys: Vec<String>,
}

// Load configuration from cac_config.json
fn load_config() -> Result<ConfigData, String> {
    let config_path = "src/cac_config.json";
    let config_content = fs::read_to_string(config_path)
        .map_err(|e| format!("Failed to read config file: {}", e))?;
    
    let config: ConfigData = serde_json::from_str(&config_content)
        .map_err(|e| format!("Failed to parse config JSON: {}", e))?;
    
    Ok(config)
}

// Function to evaluate configuration using config.rs logic
fn eval_config_with_file(query_data: &Value) -> RedisResult {
    // Load the configuration file
    let config_data = match load_config() {
        Ok(config) => config,
        Err(e) => {
            let error_result = json!({
                "status": "error",
                "message": format!("Failed to load configuration: {}", e),
                "timestamp": std::time::SystemTime::now()
                    .duration_since(std::time::UNIX_EPOCH)
                    .unwrap()
                    .as_secs()
            });
            return Ok(error_result.to_string().into());
        }
    };

    // Convert query data to Map if it's an object
    let query_map = match query_data {
        Value::Object(map) => map,
        _ => {
            let error_result = json!({
                "status": "error",
                "message": "Query data must be a JSON object",
                "received_type": match query_data {
                    Value::Null => "null",
                    Value::Bool(_) => "boolean",
                    Value::Number(_) => "number",
                    Value::String(_) => "string",
                    Value::Array(_) => "array",
                    Value::Object(_) => "object",
                },
                "timestamp": std::time::SystemTime::now()
                    .duration_since(std::time::UNIX_EPOCH)
                    .unwrap()
                    .as_secs()
            });
            return Ok(error_result.to_string().into());
        }
    };
    
    // Convert ConfigContext to the format expected by config.rs
    let contexts: Vec<config::Context> = config_data.contexts.iter().map(|ctx| {
        config::Context {
            id: ctx.id.clone(),
            condition: ctx.condition.clone(),
            priority: ctx.priority,
            weight: ctx.weight,
            override_with_keys: ctx.override_with_keys.clone(),
        }
    }).collect();

    // Convert overrides to the expected format
    let overrides: HashMap<String, config::Overrides> = config_data.overrides.iter().map(|(key, value)| {
        (key.clone(), config::Overrides::from(value.clone()))
    }).collect();

    // Use the eval_config function from config.rs
    match config::eval_config(
        config_data.default_configs,
        &contexts,
        &overrides,
        query_map,
        config::MergeStrategy::MERGE,
        None, // No prefix filtering
    ) {
        Ok(result) => {
            let response = json!({
                "status": "success",
                "result": result,
                "query_data": query_data,
                "timestamp": std::time::SystemTime::now()
                    .duration_since(std::time::UNIX_EPOCH)
                    .unwrap()
                    .as_secs(),
                "eval_type": "config_evaluation_with_file"
            });
            Ok(response.to_string().into())
        },
        Err(e) => {
            let error_result = json!({
                "status": "error",
                "message": format!("Configuration evaluation failed: {}", e),
                "query_data": query_data,
                "timestamp": std::time::SystemTime::now()
                    .duration_since(std::time::UNIX_EPOCH)
                    .unwrap()
                    .as_secs()
            });
            Ok(error_result.to_string().into())
        }
    }
}

// Custom command: CAC.HELLO
fn cac_hello(_: &Context, args: Vec<RedisString>) -> RedisResult {
    let name = if args.len() > 1 {
        args[1].try_as_str().unwrap_or("World")
    } else {
        "World"
    };
    
    let response = format!("Hello from CAC module, {}!", name);
    Ok(response.into())
}

// Custom command: CAC.EVAL - Updated to use config.rs and cac_config.json
fn cac_eval(_: &Context, args: Vec<RedisString>) -> RedisResult {
    if args.len() < 2 {
        return Err(RedisError::WrongArity);
    }
    
    let json_str = args[1].try_as_str().unwrap_or("");
    
    // Parse the JSON string
    match serde_json::from_str::<Value>(json_str) {
        Ok(json_obj) => eval_config_with_file(&json_obj),
        Err(e) => {
            let error_result = json!({
                "status": "error",
                "message": format!("Invalid JSON: {}", e),
                "input": json_str,
                "timestamp": std::time::SystemTime::now()
                    .duration_since(std::time::UNIX_EPOCH)
                    .unwrap()
                    .as_secs()
            });
            Ok(error_result.to_string().into())
        }
    }
}

// Example multiplication command (from the official example)
fn hello_mul(_: &Context, args: Vec<RedisString>) -> RedisResult {
    if args.len() < 2 {
        return Err(RedisError::WrongArity);
    }

    let nums = args
        .into_iter()
        .skip(1)
        .map(|s| s.parse_integer())
        .collect::<Result<Vec<i64>, RedisError>>()?;

    let product = nums.iter().product();

    let mut response = nums;
    response.push(product);

    Ok(response.into())
}

//////////////////////////////////////////////////////

redis_module! {
    name: "cac",
    version: 1,
    allocator: (redis_module::alloc::RedisAlloc, redis_module::alloc::RedisAlloc),
    data_types: [],
    commands: [
        ["cac.hello", cac_hello, "", 0, 0, 0],
        ["cac.eval", cac_eval, "", 0, 0, 0],
        ["hello.mul", hello_mul, "", 0, 0, 0],
    ],
}
