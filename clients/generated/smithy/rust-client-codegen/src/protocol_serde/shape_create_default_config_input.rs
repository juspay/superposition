// Code generated by software.amazon.smithy.rust.codegen.smithy-rs. DO NOT EDIT.
pub fn ser_create_default_config_input_input(
                         object: &mut ::aws_smithy_json::serialize::JsonObjectWriter,
                         input: &crate::operation::create_default_config::CreateDefaultConfigInput,
                    ) -> ::std::result::Result<(), ::aws_smithy_types::error::operation::SerializationError> {
    if let Some(var_1) = &input.change_reason {
        object.key("change_reason").string(var_1.as_str());
    }
    if let Some(var_2) = &input.description {
        object.key("description").string(var_2.as_str());
    }
    if let Some(var_3) = &input.function_name {
        object.key("function_name").string(var_3.as_str());
    }
    if let Some(var_4) = &input.key {
        object.key("key").string(var_4.as_str());
    }
    if let Some(var_5) = &input.schema {
        object.key("schema").document(var_5);
    }
    if let Some(var_6) = &input.value {
        object.key("value").document(var_6);
    }
    Ok(())
}

