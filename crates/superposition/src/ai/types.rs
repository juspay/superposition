use serde::{Deserialize, Serialize};

#[derive(Debug, Deserialize, Serialize, Clone)]
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
pub enum GenerationType {
    Schema,
    ValidationFunction,
    AutocompleteFunction,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct GenerateRequest {
    pub generation_type: GenerationType,
    pub description: String,
    #[serde(default)]
    pub context: Option<String>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct GenerateResponse {
    pub generated_code: String,
}

impl GenerationType {
    pub fn get_system_prompt(&self) -> String {
        match self {
            GenerationType::Schema => {
                r#"You are a JSON Schema expert. Generate valid JSON Schema (draft-07) based on user descriptions.
- Only output valid JSON Schema, no explanations or markdown code blocks
- Include appropriate types, properties, required fields, and validation rules
- Use descriptive titles and descriptions for properties
- Follow JSON Schema best practices"#
                    .to_string()
            }
            GenerationType::ValidationFunction => {
                r#"You are a JavaScript expert. Generate validation functions with the signature: validate(key, value)
- Function must be named 'validate' and accept two parameters: key (string) and value (any)
- Return true if validation passes, false otherwise
- Only output JavaScript code, no explanations or markdown code blocks
- Use modern JavaScript (ES6+) syntax
- Include helpful comments explaining the validation logic
- Handle edge cases and null/undefined values
- Do not use external libraries or imports"#
                    .to_string()
            }
            GenerationType::AutocompleteFunction => {
                r#"You are a JavaScript expert. Generate autocomplete functions with the signature: autocomplete(name, prefix, environment)
- Function must be named 'autocomplete' and accept three parameters: name (string), prefix (string), and environment (object)
- Return an array of autocomplete suggestions (strings)
- Only output JavaScript code, no explanations or markdown code blocks
- Use modern JavaScript (ES6+) syntax
- Include helpful comments explaining the autocomplete logic
- Handle edge cases and empty prefix values
- Do not use external libraries or imports"#
                    .to_string()
            }
        }
    }
}
