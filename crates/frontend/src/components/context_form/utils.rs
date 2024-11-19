use crate::{
    components::condition_pills::types::{Condition, ConditionOperator},
    types::Dimension,
    utils::{
        construct_request_headers, get_config_value, get_host, parse_json_response,
        request, ConfigType,
    },
};
use anyhow::Result;
use leptos::logging;
use serde_json::{json, Map, Value};

pub fn get_condition_schema(
    condition: &Condition,
    dimensions: Vec<Dimension>,
) -> Result<Value, String> {
    let var = &condition.left_operand; // Dimension name
    let op = &condition.operator; // Operator type
    let val = &condition.right_operand; // Vec<Value>

    // Extract non-"var" elements from the right_operand
    let filtered_values: Vec<&Value> = val
        .iter()
        .filter(|v| !v.is_object() || !v.get("var").is_some()) // Ignore objects with "var"
        .collect();
    let dimensions_clone = dimensions.clone();

    match op {
        ConditionOperator::Between => {
            // Expecting three elements for "Between" condition: two operands and one "var" object
            if filtered_values.len() != 2 {
                return Err(
                    "Invalid number of operands for 'between' condition.".to_string()
                );
            }

            let first_operand = &filtered_values[0]; // The first value
            let third_operand = &filtered_values[1]; // The third value

            let first_operand_value = get_config_value(
                var,
                first_operand,
                &dimensions
                    .into_iter()
                    .map(ConfigType::Dimension)
                    .collect::<Vec<_>>(),
            )?;

            let third_operand_value = get_config_value(
                var,
                third_operand,
                &dimensions_clone
                    .into_iter()
                    .map(ConfigType::Dimension)
                    .collect::<Vec<_>>(),
            )?;

            Ok(json!({
                "<=": [
                    first_operand_value,
                    { "var": var },
                    third_operand_value
                ]
            }))
        }
        ConditionOperator::Is => {
            // Expecting two elements for "Is" condition: one "var" object and one value
            if filtered_values.len() != 1 {
                return Err("Invalid number of operands for 'is' condition.".to_string());
            }

            let value = &filtered_values[0]; // The value after "var"
            let first_operand_value = get_config_value(
                var,
                value,
                &dimensions
                    .into_iter()
                    .map(ConfigType::Dimension)
                    .collect::<Vec<_>>(),
            )?;

            Ok(json!({
                "==": [
                    { "var": var },
                    first_operand_value
                ]
            }))
        }
        ConditionOperator::In => {
            if filtered_values.len() != 1 {
                return Err("Invalid number of operands for 'in' condition.".to_string());
            }
            let value = &filtered_values[0]; // The value after "var"
            let first_operand_value = get_config_value(
                var,
                value,
                &dimensions
                    .into_iter()
                    .map(ConfigType::Dimension)
                    .collect::<Vec<_>>(),
            )?;

            Ok(json!({
                "in": [
                    { "var": var },
                    first_operand_value
                ]
            }))
        }
        ConditionOperator::Has => {
            if filtered_values.len() != 1 {
                return Err("Invalid number of operands for 'has' condition.".to_string());
            }
            let value = &filtered_values[0]; // The value after "var"
            let first_operand_value = get_config_value(
                var,
                value,
                &dimensions
                    .into_iter()
                    .map(ConfigType::Dimension)
                    .collect::<Vec<_>>(),
            )?;

            Ok(json!({
                "in": [
                    first_operand_value,
                    { "var": var }
                ]
            }))
        }
        ConditionOperator::Other(op) => {
            if filtered_values.len() == 1 {
                let value = &filtered_values[0]; // The value after "var"
                let first_operand_value = get_config_value(
                    var,
                    value,
                    &dimensions
                        .into_iter()
                        .map(ConfigType::Dimension)
                        .collect::<Vec<_>>(),
                )?;
                Ok(json!({
                    op: [
                        { "var": var },
                        first_operand_value
                    ]
                }))
            } else if filtered_values.len() == 2 {
                let first_operand = &filtered_values[0]; // The first value
                let second_operand = &filtered_values[1]; // The second value
                let first_operand_value = get_config_value(
                    var,
                    first_operand,
                    &dimensions
                        .into_iter()
                        .map(ConfigType::Dimension)
                        .collect::<Vec<_>>(),
                )?;
                let second_operand_value = get_config_value(
                    var,
                    second_operand,
                    &dimensions_clone
                        .into_iter()
                        .map(ConfigType::Dimension)
                        .collect::<Vec<_>>(),
                )?;
                Ok(json!({
                    op: [
                        first_operand_value,
                        { "var": var },
                        second_operand_value
                    ]
                }))
            } else {
                Err("Invalid number of operands for custom operator.".to_string())
            }
        }
    }
}

pub fn construct_context(
    conditions: Vec<Condition>,
    dimensions: Vec<Dimension>,
) -> Value {
    if conditions.is_empty() {
        json!({})
    } else {
        let condition_schemas = conditions
            .iter()
            .map(
                |condition| match get_condition_schema(condition, dimensions.clone()) {
                    Ok(ans) => ans,
                    Err(err) => {
                        logging::log!("Error while getting condition schema {:?}", err);
                        Value::Null
                    }
                },
            )
            .collect::<Vec<Value>>();

        if condition_schemas.len() == 1 {
            condition_schemas[0].clone()
        } else {
            json!({ "and": condition_schemas })
        }
    }
}

pub fn construct_request_payload(
    overrides: Map<String, Value>,
    conditions: Vec<Condition>,
    dimensions: Vec<Dimension>,
) -> Value {
    // Construct the override section
    let override_section: Map<String, Value> = overrides;

    // Construct the context section
    let context_section = construct_context(conditions, dimensions.clone());

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
    conditions: Vec<Condition>,
    dimensions: Vec<Dimension>,
) -> Result<serde_json::Value, String> {
    let host = get_host();
    let url = format!("{host}/context");
    let request_payload = construct_request_payload(overrides, conditions, dimensions);
    let response = request(
        url,
        reqwest::Method::PUT,
        Some(request_payload),
        construct_request_headers(&[("x-tenant", &tenant)])?,
    )
    .await?;

    parse_json_response(response).await
}

pub async fn update_context(
    tenant: String,
    overrides: Map<String, Value>,
    conditions: Vec<Condition>,
    dimensions: Vec<Dimension>,
) -> Result<serde_json::Value, String> {
    let host = get_host();
    let url = format!("{host}/context/overrides");
    let request_payload =
        construct_request_payload(overrides, conditions, dimensions.clone());
    let response = request(
        url,
        reqwest::Method::PUT,
        Some(request_payload),
        construct_request_headers(&[("x-tenant", &tenant)])?,
    )
    .await?;

    parse_json_response(response).await
}
