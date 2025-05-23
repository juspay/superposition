// Code generated by software.amazon.smithy.rust.codegen.smithy-rs. DO NOT EDIT.
pub(crate) fn de_audit_log_full<'a, I>(tokens: &mut ::std::iter::Peekable<I>) -> ::std::result::Result<Option<crate::types::AuditLogFull>, ::aws_smithy_json::deserialize::error::DeserializeError>
                        where I: Iterator<Item = Result<::aws_smithy_json::deserialize::Token<'a>, ::aws_smithy_json::deserialize::error::DeserializeError>> {
    match tokens.next().transpose()? {
        Some(::aws_smithy_json::deserialize::Token::ValueNull { .. }) => Ok(None),
                        Some(::aws_smithy_json::deserialize::Token::StartObject { .. }) => {
            #[allow(unused_mut)]
            let mut builder = crate::types::builders::AuditLogFullBuilder::default();
            loop {
                match tokens.next().transpose()? {
                    Some(::aws_smithy_json::deserialize::Token::EndObject { .. }) => break,
                                            Some(::aws_smithy_json::deserialize::Token::ObjectKey { key, .. }) => {
                        match key.to_unescaped()?.as_ref() {
                            "table_name" => {
                                builder = builder.set_table_name(
                                    ::aws_smithy_json::deserialize::token::expect_string_or_null(tokens.next())?.map(|s|
                                        s.to_unescaped().map(|u|
                                            u.into_owned()
                                        )
                                    ).transpose()?
                                );
                            }
                            "user_name" => {
                                builder = builder.set_user_name(
                                    ::aws_smithy_json::deserialize::token::expect_string_or_null(tokens.next())?.map(|s|
                                        s.to_unescaped().map(|u|
                                            u.into_owned()
                                        )
                                    ).transpose()?
                                );
                            }
                            "timestamp" => {
                                builder = builder.set_timestamp(
                                    ::aws_smithy_json::deserialize::token::expect_timestamp_or_null(tokens.next(), ::aws_smithy_types::date_time::Format::DateTimeWithOffset)?
                                );
                            }
                            "action" => {
                                builder = builder.set_action(
                                    ::aws_smithy_json::deserialize::token::expect_string_or_null(tokens.next())?.map(|s|
                                        s.to_unescaped().map(|u|
                                            u.into_owned()
                                        )
                                    ).transpose()?
                                );
                            }
                            "original_data" => {
                                builder = builder.set_original_data(
                                    Some(::aws_smithy_json::deserialize::token::expect_document(tokens)?)
                                );
                            }
                            "new_data" => {
                                builder = builder.set_new_data(
                                    Some(::aws_smithy_json::deserialize::token::expect_document(tokens)?)
                                );
                            }
                            "query" => {
                                builder = builder.set_query(
                                    ::aws_smithy_json::deserialize::token::expect_string_or_null(tokens.next())?.map(|s|
                                        s.to_unescaped().map(|u|
                                            u.into_owned()
                                        )
                                    ).transpose()?
                                );
                            }
                            _ => ::aws_smithy_json::deserialize::token::skip_value(tokens)?
                        }
                    }
                    other => return Err(::aws_smithy_json::deserialize::error::DeserializeError::custom(format!("expected object key or end object, found: {:?}", other)))
                }
            }
            Ok(Some(builder.build()))
        }
        _ => {
            Err(::aws_smithy_json::deserialize::error::DeserializeError::custom("expected start object or null"))
        }
    }
}

