// Code generated by software.amazon.smithy.rust.codegen.smithy-rs. DO NOT EDIT.
pub fn ser_update_dimension_input_input(
                         object: &mut ::aws_smithy_json::serialize::JsonObjectWriter,
                         input: &crate::operation::update_dimension::UpdateDimensionInput,
                    ) -> ::std::result::Result<(), ::aws_smithy_types::error::operation::SerializationError> {
    if let Some(var_1) = &input.autocomplete_function_name {
        object.key("autocomplete_function_name").string(var_1.as_str());
    }
    if let Some(var_2) = &input.change_reason {
        object.key("change_reason").string(var_2.as_str());
    }
    if let Some(var_3) = &input.dependencies {
        let mut array_4 = object.key("dependencies").start_array();
        for item_5 in var_3 {
             {
                array_4.value().string(item_5.as_str());
            }
        }
        array_4.finish();
    }
    if let Some(var_6) = &input.description {
        object.key("description").string(var_6.as_str());
    }
    if let Some(var_7) = &input.function_name {
        object.key("function_name").string(var_7.as_str());
    }
    if let Some(var_8) = &input.position {
        object.key("position").number(#[allow(clippy::useless_conversion)]::aws_smithy_types::Number::NegInt((*var_8).into()));
    }
    if let Some(var_9) = &input.schema {
        object.key("schema").document(var_9);
    }
    Ok(())
}

