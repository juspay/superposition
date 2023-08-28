use actix_web::http::header::{HeaderMap, HeaderName, HeaderValue};
use jsonschema::{Draft, JSONSchema};
use serde_json::json;
use std::collections::HashMap;

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

pub fn parse_headermap_safe(headermap: &HeaderMap) -> HashMap<String, String> {
    let mut req_headers = HashMap::new();
    let record_header = |(header_name, header_val): (&HeaderName, &HeaderValue)| {
        let header_val = match header_val.to_str() {
            Ok(s) => String::from(s),
            Err(e) => {
                log::error!(
                    "unable to parse value of header {}, error: {e}",
                    header_name
                );
                String::from("Error: non ASCII header value")
            }
        };
        req_headers.insert(header_name.to_string(), header_val);
    };
    headermap.iter().for_each(record_header);
    req_headers
}
