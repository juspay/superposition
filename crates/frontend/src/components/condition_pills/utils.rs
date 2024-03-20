use std::mem::swap;

use super::types::Condition;
use serde_json::Value;

pub fn parse_conditions(input: Vec<Condition>) -> Vec<Condition> {
    let mut conditions = Vec::new();

    // Split the string by "&&" and iterate over each condition
    for condition in input {
        let mut key = condition.left_operand;
        let mut op = condition.operator;
        let mut val = condition.right_operand;
        if op == "in" {
            swap(&mut key, &mut val)
        }
        // Add a space after key
        key.push(' ');
        match op.as_str() {
            "==" => {
                val = val.trim_matches('"').to_string();
                op = "is".to_string();
            }
            "<=" => {
                val = val.trim_matches('"').to_string();
                op = "BETWEEN".to_string();
            }
            _ => {
                val = val.trim_matches('"').to_string();
                op = "has".to_string();
            }
        }
        op.push(' ');

        conditions.push(Condition {
            left_operand: key,
            operator: op,
            right_operand: val,
        });
    }

    conditions
}

pub fn extract_and_format(condition: &Value) -> Vec<Condition> {
    let mut formatted_conditions = Vec::new();
    if condition.is_object() && condition.get("and").is_some() {
        // Handling complex "and" conditions
        let empty_vec = vec![];
        let conditions_json = condition
            .get("and")
            .and_then(|val| val.as_array())
            .unwrap_or(&empty_vec); // Default to an empty vector if not an array

        for cond in conditions_json {
            if let Some(formatted_condition) = format_condition(cond) {
                formatted_conditions.push(formatted_condition);
            }
        }
    } else if let Some(formatted_condition) = format_condition(condition) {
        // Handling single conditions
        formatted_conditions.push(formatted_condition);
    }
    formatted_conditions
}

fn format_condition(condition: &Value) -> Option<Condition> {
    if let Some(ref operator) = condition.as_object().and_then(|obj| obj.keys().next()) {
        let empty_vec = vec![];
        let operands = condition[operator].as_array().unwrap_or(&empty_vec);

        // Handling the "in" operator differently
        if operator.as_str() == "in" {
            let left_operand = &operands[0];
            let right_operand = &operands[1];

            let left_str = if left_operand.is_string() {
                format!("\"{}\"", left_operand.as_str().unwrap())
            } else {
                format!("{}", left_operand)
            };

            if right_operand.is_object() && right_operand["var"].is_string() {
                let var_str = right_operand["var"].as_str().unwrap();
                return Some(Condition {
                    left_operand: left_str,
                    operator: operator.to_string(),
                    right_operand: var_str.to_string(),
                });
            }
        }

        // Handline the "<=" operator differently
        if operator.as_str() == "<=" {
            let left_operand = &operands[0];
            let right_operand = &operands[2];
            let mid_operand = &operands[1];

            let left_str = format!("{}", left_operand).trim_matches('"').to_string();
            let right_str = format!("{}", right_operand).trim_matches('"').to_string();

            if mid_operand.is_object() && mid_operand["var"].is_string() {
                let var_str = mid_operand["var"].as_str().unwrap();
                return Some(Condition {
                    left_operand: var_str.to_string(),
                    operator: operator.to_string(),
                    right_operand: left_str + "," + &right_str,
                });
            }
        }
        // Handling regular operators
        if let Some(first_operand) = operands.get(0) {
            if first_operand.is_object() && first_operand["var"].is_string() {
                let key = first_operand["var"]
                    .as_str()
                    .unwrap_or("UnknownVar")
                    .to_string();
                if let Some(value) = operands.get(1) {
                    let val_str = match value {
                        Value::Null => "null".to_string(),
                        Value::String(v) => v.clone(),
                        _ => value.to_string(),
                    };
                    return Some(Condition {
                        left_operand: key.to_string(),
                        operator: operator.to_string(),
                        right_operand: val_str,
                    });
                }
            }
        }
    }

    None
}
