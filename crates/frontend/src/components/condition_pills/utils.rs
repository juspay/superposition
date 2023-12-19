use serde_json::Value;

pub fn extract_and_format(condition: &Value) -> String {
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

        formatted_conditions.join(" && ")
    } else {
        // Handling single conditions
        format_condition(condition)
    }
}

fn format_condition(condition: &Value) -> String {
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
                return format!("{} {} {}", left_str, operator, var_str);
            }
        }

        // Handling regular operators
        if let Some(first_operand) = operands.get(0) {
            if first_operand.is_object() && first_operand["var"].is_string() {
                let key = first_operand["var"].as_str().unwrap_or("UnknownVar");
                if let Some(value) = operands.get(1) {
                    if value.is_string() {
                        return format!(
                            "{} {} \"{}\"",
                            key,
                            operator,
                            value.as_str().unwrap()
                        );
                    } else {
                        return format!("{} {} {}", key, operator, value);
                    }
                }
            }
        }
    }

    "Invalid Condition".to_string()
}

pub fn parse_conditions(input: String) -> Vec<(String, String, String)> {
    let mut conditions = Vec::new();
    let operators = vec!["==", "in"];

    // Split the string by "and" and iterate over each condition
    for condition in input.split("&&") {
        let mut parts = Vec::new();
        let mut operator_found = "";

        // Check for each operator
        for operator in &operators {
            if condition.contains(operator) {
                operator_found = operator;
                parts = condition.split(operator).map(|s| s.trim()).collect();

                // TODO: add this when context update is enabled
                if parts.len() == 2 && operator == &"in" {
                    parts.swap(0, 1);
                }

                break;
            }
        }

        if parts.len() == 2 {
            let mut key = parts[0].to_string();
            let mut op = operator_found.to_string();
            let mut val = parts[1].to_string();
            // Add a space after key
            key.push(' ');
            if op == "==".to_string() {
                val = val.trim_matches('"').to_string();
                op = "is".to_string();
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
