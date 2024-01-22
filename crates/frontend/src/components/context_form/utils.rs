use crate::types::Dimension;
use crate::utils::get_host;
use reqwest::StatusCode;
use serde_json::{json, Map, Value};
use std::str::FromStr;

pub fn get_dimension_type(dimensions: Vec<Dimension>, dimension_name: &str) -> String {
    let dimension = dimensions
        .iter()
        .find(|&dimension| dimension.dimension == dimension_name.to_string());
    let schema = &dimension.unwrap().schema;
    let schema_type = schema.get("type").unwrap();
    schema_type.to_string()
}

pub fn get_condition_schema(
    var: &str,
    op: &str,
    val: &str,
    dimensions: Vec<Dimension>,
) -> Value {
    let dimension_value = |variable: &str, val: &str, dimensions: &Vec<Dimension>| {
        let dimension_type = get_dimension_type(dimensions.clone(), variable);
        match dimension_type.replace("\"", "").as_str() {
            "boolean" => match bool::from_str(val) {
                Ok(boolean) => Value::Bool(boolean),
                _ => Value::String("Invalid Boolean".to_string()),
            },
            "number" => match val.parse::<i64>() {
                Ok(number) => Value::Number(number.into()),
                Err(_) => Value::String(val.to_string()),
            },
            _ => Value::String(val.to_string()),
        }
    };
    match op {
        "<=" => {
            let mut split_value = val.split(',');

            let first_operand =
                split_value.next().unwrap().trim().parse::<i64>().unwrap();

            let dimension_val =
                dimension_value(var, split_value.next().unwrap().trim(), &dimensions);

            json!({
                op: [
                    first_operand,
                    { "var": var },
                    dimension_val
                ]
            })
        }
        _ => {
            let dimension_val = dimension_value(var, val, &dimensions);
            json!({
                op: [
                    {"var": var},
                    dimension_val
                ]
            })
        }
    }
}

pub fn construct_context(
    conditions: Vec<(String, String, String)>,
    dimensions: Vec<Dimension>,
) -> Value {
    let condition_schemas = conditions
        .iter()
        .map(|(variable, operator, value)| {
            get_condition_schema(variable, operator, value, dimensions.clone())
        })
        .collect::<Vec<Value>>();

    let context = if condition_schemas.len() == 1 {
        condition_schemas[0].clone()
    } else {
        json!({ "and": condition_schemas })
    };

    context
}

pub fn construct_request_payload(
    overrides: Map<String, Value>,
    conditions: Vec<(String, String, String)>,
    dimensions: Vec<Dimension>,
) -> Value {
    // Construct the override section
    let override_section: Map<String, Value> = overrides;

    // Construct the context section
    let context_section = construct_context(conditions, dimensions);

    // Construct the entire request payload
    let request_payload = json!({
        "override": override_section,
        "context": context_section
    });

    request_payload
}

pub async fn create_context(
    tenant: String,
    overrides: Map<String, Value>,
    conditions: Vec<(String, String, String)>,
    dimensions: Vec<Dimension>,
) -> Result<String, String> {
    let client = reqwest::Client::new();
    let host = get_host();
    let url = format!("{host}/context");
    let request_payload = construct_request_payload(overrides, conditions, dimensions);
    let response = client
        .put(url)
        .header("x-tenant", tenant)
        .json(&request_payload)
        .send()
        .await
        .map_err(|e| e.to_string())?;
    match response.status() {
        StatusCode::OK => response.text().await.map_err(|e| e.to_string()),
        StatusCode::BAD_REQUEST => Err("Schema Validation Failed".to_string()),
        _ => Err("Internal Server Error".to_string()),
    }
}
