// Code generated by software.amazon.smithy.rust.codegen.smithy-rs. DO NOT EDIT.
#[allow(clippy::unnecessary_wraps)]
pub fn de_update_type_templates_http_error(_response_status: u16, _response_headers: &::aws_smithy_runtime_api::http::Headers, _response_body: &[u8]) -> std::result::Result<crate::operation::update_type_templates::UpdateTypeTemplatesOutput, crate::operation::update_type_templates::UpdateTypeTemplatesError> {
    #[allow(unused_mut)]
    let mut generic_builder = crate::protocol_serde::parse_http_error_metadata(_response_status, _response_headers, _response_body).map_err(crate::operation::update_type_templates::UpdateTypeTemplatesError::unhandled)?;
    let generic = generic_builder.build();
    let error_code = match generic.code() {
                                Some(code) => code,
                                None => return Err(crate::operation::update_type_templates::UpdateTypeTemplatesError::unhandled(generic))
                            };
    
                            let _error_message = generic.message().map(|msg|msg.to_owned());
    Err(match error_code {
        "TypeTemplatesNotFound" => crate::operation::update_type_templates::UpdateTypeTemplatesError::TypeTemplatesNotFound({
            #[allow(unused_mut)]
            let mut tmp =
                 {
                    #[allow(unused_mut)]
                    let mut output = crate::types::error::builders::TypeTemplatesNotFoundBuilder::default();
                    output = crate::protocol_serde::shape_type_templates_not_found::de_type_templates_not_found_json_err(_response_body, output).map_err(crate::operation::update_type_templates::UpdateTypeTemplatesError::unhandled)?;
                    let output = output.meta(generic);
                    output.build()
                }
            ;
            if tmp.message.is_none() {
                                                            tmp.message = _error_message;
                                                        }
            tmp
        }),
        "InternalServerError" => crate::operation::update_type_templates::UpdateTypeTemplatesError::InternalServerError({
            #[allow(unused_mut)]
            let mut tmp =
                 {
                    #[allow(unused_mut)]
                    let mut output = crate::types::error::builders::InternalServerErrorBuilder::default();
                    output = crate::protocol_serde::shape_internal_server_error::de_internal_server_error_json_err(_response_body, output).map_err(crate::operation::update_type_templates::UpdateTypeTemplatesError::unhandled)?;
                    let output = output.meta(generic);
                    output.build()
                }
            ;
            if tmp.message.is_none() {
                                                            tmp.message = _error_message;
                                                        }
            tmp
        }),
        _ => crate::operation::update_type_templates::UpdateTypeTemplatesError::generic(generic)
    })
}

#[allow(clippy::unnecessary_wraps)]
pub fn de_update_type_templates_http_response(_response_status: u16, _response_headers: &::aws_smithy_runtime_api::http::Headers, _response_body: &[u8]) -> std::result::Result<crate::operation::update_type_templates::UpdateTypeTemplatesOutput, crate::operation::update_type_templates::UpdateTypeTemplatesError> {
    Ok({
        #[allow(unused_mut)]
        let mut output = crate::operation::update_type_templates::builders::UpdateTypeTemplatesOutputBuilder::default();
        output = crate::protocol_serde::shape_update_type_templates::de_update_type_templates(_response_body, output).map_err(crate::operation::update_type_templates::UpdateTypeTemplatesError::unhandled)?;
        crate::serde_util::update_type_templates_output_output_correct_errors(output).build().map_err(crate::operation::update_type_templates::UpdateTypeTemplatesError::unhandled)?
    })
}

pub fn ser_update_type_templates_headers(
                    input: &crate::operation::update_type_templates::UpdateTypeTemplatesInput,
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
    Ok(builder)
}

pub fn ser_update_type_templates_input(input: &crate::operation::update_type_templates::UpdateTypeTemplatesInput) -> ::std::result::Result<::aws_smithy_types::body::SdkBody, ::aws_smithy_types::error::operation::SerializationError> {
    let mut out = String::new();
    let mut object = ::aws_smithy_json::serialize::JsonObjectWriter::new(&mut out);
    crate::protocol_serde::shape_update_type_templates_input::ser_update_type_templates_input_input(&mut object, input)?;
    object.finish();
    Ok(::aws_smithy_types::body::SdkBody::from(out))
}

pub(crate) fn de_update_type_templates(value: &[u8], mut builder: crate::operation::update_type_templates::builders::UpdateTypeTemplatesOutputBuilder) -> ::std::result::Result<crate::operation::update_type_templates::builders::UpdateTypeTemplatesOutputBuilder, ::aws_smithy_json::deserialize::error::DeserializeError> {
    let mut tokens_owned = ::aws_smithy_json::deserialize::json_token_iter(crate::protocol_serde::or_empty_doc(value)).peekable();
                        let tokens = &mut tokens_owned;
                        ::aws_smithy_json::deserialize::token::expect_start_object(tokens.next())?;
    loop {
        match tokens.next().transpose()? {
            Some(::aws_smithy_json::deserialize::Token::EndObject { .. }) => break,
                                    Some(::aws_smithy_json::deserialize::Token::ObjectKey { key, .. }) => {
                match key.to_unescaped()?.as_ref() {
                    "change_reason" => {
                        builder = builder.set_change_reason(
                            ::aws_smithy_json::deserialize::token::expect_string_or_null(tokens.next())?.map(|s|
                                s.to_unescaped().map(|u|
                                    u.into_owned()
                                )
                            ).transpose()?
                        );
                    }
                    "created_at" => {
                        builder = builder.set_created_at(
                            ::aws_smithy_json::deserialize::token::expect_timestamp_or_null(tokens.next(), ::aws_smithy_types::date_time::Format::DateTimeWithOffset)?
                        );
                    }
                    "created_by" => {
                        builder = builder.set_created_by(
                            ::aws_smithy_json::deserialize::token::expect_string_or_null(tokens.next())?.map(|s|
                                s.to_unescaped().map(|u|
                                    u.into_owned()
                                )
                            ).transpose()?
                        );
                    }
                    "description" => {
                        builder = builder.set_description(
                            ::aws_smithy_json::deserialize::token::expect_string_or_null(tokens.next())?.map(|s|
                                s.to_unescaped().map(|u|
                                    u.into_owned()
                                )
                            ).transpose()?
                        );
                    }
                    "last_modified_at" => {
                        builder = builder.set_last_modified_at(
                            ::aws_smithy_json::deserialize::token::expect_timestamp_or_null(tokens.next(), ::aws_smithy_types::date_time::Format::DateTimeWithOffset)?
                        );
                    }
                    "last_modified_by" => {
                        builder = builder.set_last_modified_by(
                            ::aws_smithy_json::deserialize::token::expect_string_or_null(tokens.next())?.map(|s|
                                s.to_unescaped().map(|u|
                                    u.into_owned()
                                )
                            ).transpose()?
                        );
                    }
                    "type_name" => {
                        builder = builder.set_type_name(
                            ::aws_smithy_json::deserialize::token::expect_string_or_null(tokens.next())?.map(|s|
                                s.to_unescaped().map(|u|
                                    u.into_owned()
                                )
                            ).transpose()?
                        );
                    }
                    "type_schema" => {
                        builder = builder.set_type_schema(
                            Some(::aws_smithy_json::deserialize::token::expect_document(tokens)?)
                        );
                    }
                    _ => ::aws_smithy_json::deserialize::token::skip_value(tokens)?
                }
            }
            other => return Err(::aws_smithy_json::deserialize::error::DeserializeError::custom(format!("expected object key or end object, found: {:?}", other)))
        }
    }
    if tokens.next().is_some() {
        return Err(::aws_smithy_json::deserialize::error::DeserializeError::custom("found more JSON tokens after completing parsing"));
    }
    Ok(builder)
}

