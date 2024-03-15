use std::mem::swap;

use serde_json::Value;

pub fn parse_conditions(
    input: Vec<Option<(String, String, String)>>,
) -> Vec<(String, String, String)> {
    let mut conditions = Vec::new();

    // Split the string by "&&" and iterate over each condition
    for opt_condition in input {
        if let Some(condition) = opt_condition {
            let mut key = condition.0;
            let mut op = condition.1;
            let mut val = condition.2;
            if op == "in" {
                swap(&mut key, &mut val)
            }
            // Add a space after key
            key.push(' ');
            if op == "==".to_string() {
                val = val.trim_matches('"').to_string();
                op = "is".to_string();
            } else if op == "<=".to_string() {
                val = val.trim_matches('"').to_string();
                op = "BETWEEN".to_string();
            } else {
                val = val.trim_matches('"').to_string();
                op = "has".to_string();
            }
            op.push(' ');

            conditions.push((key, op, val));
        }
    }

    conditions
}

pub fn extract_and_format(condition: &Value) -> Vec<Option<(String, String, String)>> {
    if condition.is_object() && condition.get("and").is_some() {
        // Handling complex "and" conditions
        let empty_vec = vec![];
        let conditions_json = condition
            .get("and")
            .and_then(|val| val.as_array())
            .unwrap_or(&empty_vec); // Default to an empty vector if not an array

        let mut formatted_conditions = Vec::new();
        for cond in conditions_json {
            formatted_conditions.push(format_condition(cond));
        }

        formatted_conditions
    } else {
        // Handling single conditions
        vec![format_condition(condition)]
    }
}

fn format_condition(condition: &Value) -> Option<(String, String, String)> {
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
                return Some((left_str, operator.to_string(), var_str.to_string()));
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
                return Some((
                    var_str.to_string(),
                    operator.to_string(),
                    left_str + "," + &right_str,
                ));
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
                    return Some((key.to_string(), operator.to_string(), val_str));
                }
            }
        }
    }

    None
}
