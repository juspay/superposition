// Code generated by software.amazon.smithy.rust.codegen.smithy-rs. DO NOT EDIT.
#[allow(clippy::unnecessary_wraps)]
pub fn de_delete_context_http_error(_response_status: u16, _response_headers: &::aws_smithy_runtime_api::http::Headers, _response_body: &[u8]) -> std::result::Result<crate::operation::delete_context::DeleteContextOutput, crate::operation::delete_context::DeleteContextError> {
    #[allow(unused_mut)]
    let mut generic_builder = crate::protocol_serde::parse_http_error_metadata(_response_status, _response_headers, _response_body).map_err(crate::operation::delete_context::DeleteContextError::unhandled)?;
    let generic = generic_builder.build();
    let error_code = match generic.code() {
                                Some(code) => code,
                                None => return Err(crate::operation::delete_context::DeleteContextError::unhandled(generic))
                            };
    
                            let _error_message = generic.message().map(|msg|msg.to_owned());
    Err(match error_code {
        "ResourceNotFound" => crate::operation::delete_context::DeleteContextError::ResourceNotFound({
            #[allow(unused_mut)]
            let mut tmp =
                 {
                    #[allow(unused_mut)]
                    let mut output = crate::types::error::builders::ResourceNotFoundBuilder::default();
                    output = crate::protocol_serde::shape_resource_not_found::de_resource_not_found_json_err(_response_body, output).map_err(crate::operation::delete_context::DeleteContextError::unhandled)?;
                    let output = output.meta(generic);
                    output.build()
                }
            ;
            if tmp.message.is_none() {
                                                            tmp.message = _error_message;
                                                        }
            tmp
        }),
        "InternalServerError" => crate::operation::delete_context::DeleteContextError::InternalServerError({
            #[allow(unused_mut)]
            let mut tmp =
                 {
                    #[allow(unused_mut)]
                    let mut output = crate::types::error::builders::InternalServerErrorBuilder::default();
                    output = crate::protocol_serde::shape_internal_server_error::de_internal_server_error_json_err(_response_body, output).map_err(crate::operation::delete_context::DeleteContextError::unhandled)?;
                    let output = output.meta(generic);
                    output.build()
                }
            ;
            if tmp.message.is_none() {
                                                            tmp.message = _error_message;
                                                        }
            tmp
        }),
        _ => crate::operation::delete_context::DeleteContextError::generic(generic)
    })
}

#[allow(clippy::unnecessary_wraps)]
pub fn de_delete_context_http_response(_response_status: u16, _response_headers: &::aws_smithy_runtime_api::http::Headers, _response_body: &[u8]) -> std::result::Result<crate::operation::delete_context::DeleteContextOutput, crate::operation::delete_context::DeleteContextError> {
    Ok({
        #[allow(unused_mut)]
        let mut output = crate::operation::delete_context::builders::DeleteContextOutputBuilder::default();
        output.build()
    })
}

pub fn ser_delete_context_headers(
                    input: &crate::operation::delete_context::DeleteContextInput,
                    mut builder: ::http::request::Builder
                ) -> std::result::Result<::http::request::Builder, ::aws_smithy_types::error::operation::BuildError> {
    if let ::std::option::Option::Some(inner_1) = &input.workspace_id {
        let formatted_2 = inner_1.as_str();
        let header_value = formatted_2;
                            let header_value: ::http::HeaderValue = header_value.parse().map_err(|err| {
                                ::aws_smithy_types::error::operation::BuildError::invalid_field("workspace_id", format!(
                                "`{}` cannot be used as a header value: {}",
                                &header_value,
                                err
                            ))
                            })?;
                            builder = builder.header("x-tenant", header_value);
    }
    if let ::std::option::Option::Some(inner_3) = &input.org_id {
        let formatted_4 = inner_3.as_str();
        let header_value = formatted_4;
                            let header_value: ::http::HeaderValue = header_value.parse().map_err(|err| {
                                ::aws_smithy_types::error::operation::BuildError::invalid_field("org_id", format!(
                                "`{}` cannot be used as a header value: {}",
                                &header_value,
                                err
                            ))
                            })?;
                            builder = builder.header("x-org-id", header_value);
    }
    if let ::std::option::Option::Some(inner_5) = &input.config_tags {
        let formatted_6 = inner_5.as_str();
        let header_value = formatted_6;
                            let header_value: ::http::HeaderValue = header_value.parse().map_err(|err| {
                                ::aws_smithy_types::error::operation::BuildError::invalid_field("config_tags", format!(
                                "`{}` cannot be used as a header value: {}",
                                &header_value,
                                err
                            ))
                            })?;
                            builder = builder.header("x-config-tags", header_value);
    }
    Ok(builder)
}

