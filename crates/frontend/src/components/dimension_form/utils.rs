use serde_json::Value;
use superposition_types::{
    api::dimension::UpdateRequest,
    database::models::{cac::Position, ChangeReason, Description},
};

#[allow(clippy::too_many_arguments)]
pub fn try_update_payload(
    position: u32,
    schema: Value,
    validation_fn_name: Option<String>,
    autocomplete_fn_name: Option<String>,
    description: String,
    change_reason: String,
) -> Result<UpdateRequest, String> {
    Ok(UpdateRequest {
        position: Some(Position::from(position)),
        schema: Some(schema),
        function_name: Some(validation_fn_name),
        autocomplete_function_name: Some(autocomplete_fn_name),
        description: Some(Description::try_from(description)?),
        change_reason: ChangeReason::try_from(change_reason)?,
    })
}
