use jsonschema::{Draft, JSONSchema};
use serde_json::json;

pub fn get_default_config_validation_schema() -> JSONSchema {
    let my_schema = json!({
    "type": "object",
    "properties": {
        "type": {
        "enum": ["string", "object", "enum", "number", "boolean", "array"]
        }
    },
    "required": ["type"],
    "allOf": [
//        {
//        "if": {
//            "properties": { "type": { "const": "object" } }
//        },
//        "then": {
//            "properties": { "properties": { "type": "object" } },
//            "required": ["properties"]
//        }
//        },
        {
        "if": {
            "properties": { "type": { "const": "string" } }
        },
        "then": {
            "properties": { "pattern": { "type": "string" } },
            "required": ["pattern"]
        }
        },
        // TODO: Add validations for Array types.
    ]
    });

    JSONSchema::options()
        .with_draft(Draft::Draft7)
        .compile(&my_schema)
        .expect("THE IMPOSSIBLE HAPPENED, failed to compile the schema for the schema!")
}
