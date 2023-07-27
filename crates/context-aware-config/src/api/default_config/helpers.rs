use jsonschema::{JSONSchema, ValidationError};
use serde_json::Value;

/*
  This step is required because an empty object
  is also a valid JSON schema. So added required
  validations for the input.
*/
// TODO: Recursive validation.
pub fn validate_schema(
    validation_schema: &JSONSchema,
    schema: Value,
) -> Result<(), String> {
    let res = match validation_schema.validate(&schema) {
        Ok(_) => Ok(()),
        Err(e) => {
            //TODO: Try & render as json.
            let verrors = e.collect::<Vec<ValidationError>>();
            Err(String::from(format!(
                "Bad schema: {:?}",
                verrors.as_slice()
            )))
        }
    };
    res
}
