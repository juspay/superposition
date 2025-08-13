use serde_json::{Map, Value};

fn apply_logic(
    condition: &Map<String, Value>,
    context: &Map<String, Value>,
    partial: bool,
) -> bool {
    for (dimension, value) in condition {
        if let Some(context_value) = context.get(dimension) {
            if dimension == "variantIds" {
                if let Value::Array(ref context_values) = context_value {
                    if !context_values.contains(value) {
                        return false;
                    }
                } else {
                    return false;
                }
            } else if *context_value != *value {
                return false;
            }
        } else if partial {
            continue;
        } else {
            return false;
        }
    }
    true
}

pub fn apply(condition: &Map<String, Value>, context: &Map<String, Value>) -> bool {
    apply_logic(condition, context, false)
}

pub fn partial_apply(
    condition: &Map<String, Value>,
    context: &Map<String, Value>,
) -> bool {
    apply_logic(condition, context, true)
}
