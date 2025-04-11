// Code generated by software.amazon.smithy.rust.codegen.smithy-rs. DO NOT EDIT.
pub fn ser_applicable_variants_input_input(
                         object: &mut ::aws_smithy_json::serialize::JsonObjectWriter,
                         input: &crate::operation::applicable_variants::ApplicableVariantsInput,
                    ) -> ::std::result::Result<(), ::aws_smithy_types::error::operation::SerializationError> {
    if let Some(var_1) = &input.context {
        #[allow(unused_mut)]
        let mut object_2 = object.key("context").start_object();
        for (key_3, value_4) in var_1 {
             {
                object_2.key(key_3.as_str()).document(value_4);
            }
        }
        object_2.finish();
    }
    if let Some(var_5) = &input.toss {
        object.key("toss").number(#[allow(clippy::useless_conversion)]::aws_smithy_types::Number::NegInt((*var_5).into()));
    }
    Ok(())
}

