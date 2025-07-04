// Code generated by software.amazon.smithy.rust.codegen.smithy-rs. DO NOT EDIT.
pub(crate) fn de_workspace_response<'a, I>(tokens: &mut ::std::iter::Peekable<I>) -> ::std::result::Result<Option<crate::types::WorkspaceResponse>, ::aws_smithy_json::deserialize::error::DeserializeError>
                        where I: Iterator<Item = Result<::aws_smithy_json::deserialize::Token<'a>, ::aws_smithy_json::deserialize::error::DeserializeError>> {
    match tokens.next().transpose()? {
        Some(::aws_smithy_json::deserialize::Token::ValueNull { .. }) => Ok(None),
                        Some(::aws_smithy_json::deserialize::Token::StartObject { .. }) => {
            #[allow(unused_mut)]
            let mut builder = crate::types::builders::WorkspaceResponseBuilder::default();
            loop {
                match tokens.next().transpose()? {
                    Some(::aws_smithy_json::deserialize::Token::EndObject { .. }) => break,
                                            Some(::aws_smithy_json::deserialize::Token::ObjectKey { key, .. }) => {
                        match key.to_unescaped()?.as_ref() {
                            "workspace_name" => {
                                builder = builder.set_workspace_name(
                                    ::aws_smithy_json::deserialize::token::expect_string_or_null(tokens.next())?.map(|s|
                                        s.to_unescaped().map(|u|
                                            u.into_owned()
                                        )
                                    ).transpose()?
                                );
                            }
                            "organisation_id" => {
                                builder = builder.set_organisation_id(
                                    ::aws_smithy_json::deserialize::token::expect_string_or_null(tokens.next())?.map(|s|
                                        s.to_unescaped().map(|u|
                                            u.into_owned()
                                        )
                                    ).transpose()?
                                );
                            }
                            "organisation_name" => {
                                builder = builder.set_organisation_name(
                                    ::aws_smithy_json::deserialize::token::expect_string_or_null(tokens.next())?.map(|s|
                                        s.to_unescaped().map(|u|
                                            u.into_owned()
                                        )
                                    ).transpose()?
                                );
                            }
                            "workspace_schema_name" => {
                                builder = builder.set_workspace_schema_name(
                                    ::aws_smithy_json::deserialize::token::expect_string_or_null(tokens.next())?.map(|s|
                                        s.to_unescaped().map(|u|
                                            u.into_owned()
                                        )
                                    ).transpose()?
                                );
                            }
                            "workspace_status" => {
                                builder = builder.set_workspace_status(
                                    ::aws_smithy_json::deserialize::token::expect_string_or_null(tokens.next())?.map(|s|
                                        s.to_unescaped().map(|u|
                                            crate::types::WorkspaceStatus::from(u.as_ref())
                                        )
                                    ).transpose()?
                                );
                            }
                            "workspace_admin_email" => {
                                builder = builder.set_workspace_admin_email(
                                    ::aws_smithy_json::deserialize::token::expect_string_or_null(tokens.next())?.map(|s|
                                        s.to_unescaped().map(|u|
                                            u.into_owned()
                                        )
                                    ).transpose()?
                                );
                            }
                            "config_version" => {
                                builder = builder.set_config_version(
                                    ::aws_smithy_json::deserialize::token::expect_string_or_null(tokens.next())?.map(|s|
                                        s.to_unescaped().map(|u|
                                            u.into_owned()
                                        )
                                    ).transpose()?
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
                            "last_modified_by" => {
                                builder = builder.set_last_modified_by(
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
                            "created_at" => {
                                builder = builder.set_created_at(
                                    ::aws_smithy_json::deserialize::token::expect_timestamp_or_null(tokens.next(), ::aws_smithy_types::date_time::Format::DateTimeWithOffset)?
                                );
                            }
                            "mandatory_dimensions" => {
                                builder = builder.set_mandatory_dimensions(
                                    crate::protocol_serde::shape_list_mandatory_dimensions::de_list_mandatory_dimensions(tokens)?
                                );
                            }
                            "strict_mode" => {
                                builder = builder.set_strict_mode(
                                    ::aws_smithy_json::deserialize::token::expect_bool_or_null(tokens.next())?
                                );
                            }
                            "metrics" => {
                                builder = builder.set_metrics(
                                    Some(::aws_smithy_json::deserialize::token::expect_document(tokens)?)
                                );
                            }
                            "allow_experiment_self_approval" => {
                                builder = builder.set_allow_experiment_self_approval(
                                    ::aws_smithy_json::deserialize::token::expect_bool_or_null(tokens.next())?
                                );
                            }
                            _ => ::aws_smithy_json::deserialize::token::skip_value(tokens)?
                        }
                    }
                    other => return Err(::aws_smithy_json::deserialize::error::DeserializeError::custom(format!("expected object key or end object, found: {:?}", other)))
                }
            }
            Ok(Some(crate::serde_util::workspace_response_correct_errors(builder).build().map_err(|err|::aws_smithy_json::deserialize::error::DeserializeError::custom_source("Response was invalid", err))?))
        }
        _ => {
            Err(::aws_smithy_json::deserialize::error::DeserializeError::custom("expected start object or null"))
        }
    }
}

