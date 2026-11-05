use std::cmp::Reverse;

use serde_json::Value;

pub const REDACTED: &str = "[REDACTED]";

pub struct SecretRedactor {
    needles: Vec<String>,
}

impl SecretRedactor {
    pub fn new(values: impl IntoIterator<Item = String>) -> Self {
        // prevent redacting short strings that are likely to be common words or substrings
        // secrets are likely to be longer
        // if a secret is shorter than or equal 4 characters, God help that person
        let mut needles: Vec<String> = values
            .into_iter()
            .filter(|value| value.len() >= 4)
            .collect();
        // redact longest string first
        needles.sort_by_key(|value| Reverse(value.len()));
        Self { needles }
    }

    pub fn redact(&self, text: &str) -> String {
        self.needles
            .iter()
            .fold(text.to_owned(), |acc, needle| acc.replace(needle, REDACTED))
    }

    pub fn redact_value(&self, value: &Value) -> Value {
        match value {
            Value::String(text) => Value::String(self.redact(text)),
            Value::Array(items) => {
                Value::Array(items.iter().map(|item| self.redact_value(item)).collect())
            }
            Value::Object(map) => Value::Object(
                map.iter()
                    .map(|(key, value)| (key.clone(), self.redact_value(value)))
                    .collect(),
            ),
            other => other.clone(),
        }
    }
}
