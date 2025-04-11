// Code generated by software.amazon.smithy.rust.codegen.smithy-rs. DO NOT EDIT.
pub(crate) fn de_weight_recompute_response<'a, I>(tokens: &mut ::std::iter::Peekable<I>) -> ::std::result::Result<Option<crate::types::WeightRecomputeResponse>, ::aws_smithy_json::deserialize::error::DeserializeError>
                        where I: Iterator<Item = Result<::aws_smithy_json::deserialize::Token<'a>, ::aws_smithy_json::deserialize::error::DeserializeError>> {
    match tokens.next().transpose()? {
        Some(::aws_smithy_json::deserialize::Token::ValueNull { .. }) => Ok(None),
                        Some(::aws_smithy_json::deserialize::Token::StartObject { .. }) => {
            #[allow(unused_mut)]
            let mut builder = crate::types::builders::WeightRecomputeResponseBuilder::default();
            loop {
                match tokens.next().transpose()? {
                    Some(::aws_smithy_json::deserialize::Token::EndObject { .. }) => break,
                                            Some(::aws_smithy_json::deserialize::Token::ObjectKey { key, .. }) => {
                        match key.to_unescaped()?.as_ref() {
                            "id" => {
                                builder = builder.set_id(
                                    ::aws_smithy_json::deserialize::token::expect_string_or_null(tokens.next())?.map(|s|
                                        s.to_unescaped().map(|u|
                                            u.into_owned()
                                        )
                                    ).transpose()?
                                );
                            }
                            "condition" => {
                                builder = builder.set_condition(
                                    crate::protocol_serde::shape_condition::de_condition(tokens)?
                                );
                            }
                            "old_weight" => {
                                builder = builder.set_old_weight(
                                    ::aws_smithy_json::deserialize::token::expect_string_or_null(tokens.next())?.map(|s|
                                        s.to_unescaped().map(|u|
                                            u.into_owned()
                                        )
                                    ).transpose()?
                                );
                            }
                            "new_weight" => {
                                builder = builder.set_new_weight(
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

