use super::types::Condition;
use serde_json::Value;

pub fn extract_conditions(context: &Value) -> Vec<Condition> {
    context
        .as_object()
        .and_then(|obj| {
            obj.get("and")
                .and_then(|v| v.as_array())
                .map(|arr| {
                    arr.iter()
                        .filter_map(|condition| Condition::try_from(condition).ok())
                        .collect::<Vec<Condition>>()
                })
                .or_else(|| Condition::try_from(obj).ok().map(|v| vec![v]))
        })
        .unwrap_or_default()
}
