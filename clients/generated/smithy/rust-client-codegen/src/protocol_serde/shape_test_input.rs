// Code generated by software.amazon.smithy.rust.codegen.smithy-rs. DO NOT EDIT.
pub fn ser_request_http_payload(payload: &::std::option::Option<crate::types::FunctionExecutionRequest>) -> ::std::result::Result<::std::vec::Vec<u8>, ::aws_smithy_types::error::operation::BuildError> {
    let payload = match payload.as_ref() {
                                    Some(t) => t,
                                    None => return Ok(
        crate::protocol_serde::rest_json_unset_union_payload()
    )};
    Ok(
        crate::protocol_serde::shape_test_input::ser_request_payload(payload)?
    )
}

pub fn ser_request_payload(input: &crate::types::FunctionExecutionRequest) -> std::result::Result<::std::vec::Vec<u8>, ::aws_smithy_types::error::operation::SerializationError> {
    let mut out = String::new();
    let mut object = ::aws_smithy_json::serialize::JsonObjectWriter::new(&mut out);
    crate::protocol_serde::shape_function_execution_request::ser_function_execution_request(&mut object, input)?;
    object.finish();
    Ok(out.into_bytes())
}

