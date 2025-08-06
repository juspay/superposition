use serde_json::Value;

pub fn string_to_vec(input: &str) -> Result<Vec<String>, String> {
    let value: Value = serde_json::from_str(input)
        .map_err(|e| format!("Failed to parse JSON: {}", e))?;
    match value {
        Value::Array(arr) => arr
            .into_iter()
            .map(|v| {
                v.as_str()
                    .map(|s| s.to_string())
                    .ok_or_else(|| "Non-string element in array".to_string())
            })
            .collect(),
        _ => Err("Input is not a JSON array".to_string()),
    }
}
