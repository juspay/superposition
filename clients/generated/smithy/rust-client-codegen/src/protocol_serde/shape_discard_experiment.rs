// Code generated by software.amazon.smithy.rust.codegen.smithy-rs. DO NOT EDIT.
#[allow(clippy::unnecessary_wraps)]
pub fn de_discard_experiment_http_error(_response_status: u16, _response_headers: &::aws_smithy_runtime_api::http::Headers, _response_body: &[u8]) -> std::result::Result<crate::operation::discard_experiment::DiscardExperimentOutput, crate::operation::discard_experiment::DiscardExperimentError> {
    #[allow(unused_mut)]
    let mut generic_builder = crate::protocol_serde::parse_http_error_metadata(_response_status, _response_headers, _response_body).map_err(crate::operation::discard_experiment::DiscardExperimentError::unhandled)?;
    let generic = generic_builder.build();
    let error_code = match generic.code() {
                                Some(code) => code,
                                None => return Err(crate::operation::discard_experiment::DiscardExperimentError::unhandled(generic))
                            };
    
                            let _error_message = generic.message().map(|msg|msg.to_owned());
    Err(match error_code {
        "InternalServerError" => crate::operation::discard_experiment::DiscardExperimentError::InternalServerError({
            #[allow(unused_mut)]
            let mut tmp =
                 {
                    #[allow(unused_mut)]
                    let mut output = crate::types::error::builders::InternalServerErrorBuilder::default();
                    output = crate::protocol_serde::shape_internal_server_error::de_internal_server_error_json_err(_response_body, output).map_err(crate::operation::discard_experiment::DiscardExperimentError::unhandled)?;
                    let output = output.meta(generic);
                    output.build()
                }
            ;
            if tmp.message.is_none() {
                                                            tmp.message = _error_message;
                                                        }
            tmp
        }),
        _ => crate::operation::discard_experiment::DiscardExperimentError::generic(generic)
    })
}

#[allow(clippy::unnecessary_wraps)]
pub fn de_discard_experiment_http_response(_response_status: u16, _response_headers: &::aws_smithy_runtime_api::http::Headers, _response_body: &[u8]) -> std::result::Result<crate::operation::discard_experiment::DiscardExperimentOutput, crate::operation::discard_experiment::DiscardExperimentError> {
    Ok({
        #[allow(unused_mut)]
        let mut output = crate::operation::discard_experiment::builders::DiscardExperimentOutputBuilder::default();
        output = crate::protocol_serde::shape_discard_experiment::de_discard_experiment(_response_body, output).map_err(crate::operation::discard_experiment::DiscardExperimentError::unhandled)?;
        crate::serde_util::discard_experiment_output_output_correct_errors(output).build().map_err(crate::operation::discard_experiment::DiscardExperimentError::unhandled)?
    })
}

pub fn ser_discard_experiment_headers(
                    input: &crate::operation::discard_experiment::DiscardExperimentInput,
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

pub fn ser_discard_experiment_input(input: &crate::operation::discard_experiment::DiscardExperimentInput) -> ::std::result::Result<::aws_smithy_types::body::SdkBody, ::aws_smithy_types::error::operation::SerializationError> {
    let mut out = String::new();
    let mut object = ::aws_smithy_json::serialize::JsonObjectWriter::new(&mut out);
    crate::protocol_serde::shape_discard_experiment_input::ser_discard_experiment_input_input(&mut object, input)?;
    object.finish();
    Ok(::aws_smithy_types::body::SdkBody::from(out))
}

pub(crate) fn de_discard_experiment(value: &[u8], mut builder: crate::operation::discard_experiment::builders::DiscardExperimentOutputBuilder) -> ::std::result::Result<crate::operation::discard_experiment::builders::DiscardExperimentOutputBuilder, ::aws_smithy_json::deserialize::error::DeserializeError> {
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
                    "chosen_variant" => {
                        builder = builder.set_chosen_variant(
                            ::aws_smithy_json::deserialize::token::expect_string_or_null(tokens.next())?.map(|s|
                                s.to_unescaped().map(|u|
                                    u.into_owned()
                                )
                            ).transpose()?
                        );
                    }
                    "context" => {
                        builder = builder.set_context(
                            crate::protocol_serde::shape_condition::de_condition(tokens)?
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
                    "id" => {
                        builder = builder.set_id(
                            ::aws_smithy_json::deserialize::token::expect_string_or_null(tokens.next())?.map(|s|
                                s.to_unescaped().map(|u|
                                    u.into_owned()
                                )
                            ).transpose()?
                        );
                    }
                    "last_modified" => {
                        builder = builder.set_last_modified(
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
                    "name" => {
                        builder = builder.set_name(
                            ::aws_smithy_json::deserialize::token::expect_string_or_null(tokens.next())?.map(|s|
                                s.to_unescaped().map(|u|
                                    u.into_owned()
                                )
                            ).transpose()?
                        );
                    }
                    "override_keys" => {
                        builder = builder.set_override_keys(
                            crate::protocol_serde::shape_list_override_keys::de_list_override_keys(tokens)?
                        );
                    }
                    "status" => {
                        builder = builder.set_status(
                            ::aws_smithy_json::deserialize::token::expect_string_or_null(tokens.next())?.map(|s|
                                s.to_unescaped().map(|u|
                                    crate::types::ExperimentStatusType::from(u.as_ref())
                                )
                            ).transpose()?
                        );
                    }
                    "traffic_percentage" => {
                        builder = builder.set_traffic_percentage(
                            ::aws_smithy_json::deserialize::token::expect_number_or_null(tokens.next())?
                                                .map(i32::try_from)
                                                .transpose()?
                        );
                    }
                    "variants" => {
                        builder = builder.set_variants(
                            crate::protocol_serde::shape_list_variant::de_list_variant(tokens)?
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

