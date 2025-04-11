// Code generated by software.amazon.smithy.rust.codegen.smithy-rs. DO NOT EDIT.
pub fn ser_context_action(object_4: &mut ::aws_smithy_json::serialize::JsonObjectWriter, input: &crate::types::ContextAction) -> ::std::result::Result<(), ::aws_smithy_types::error::operation::SerializationError> {
    match input {
        crate::types::ContextAction::Put(inner) => {
             {
                #[allow(unused_mut)]
                let mut object_1 = object_4.key("PUT").start_object();
                crate::protocol_serde::shape_context_put::ser_context_put(&mut object_1, inner)?;
                object_1.finish();
            }
        },
        crate::types::ContextAction::Replace(inner) => {
             {
                #[allow(unused_mut)]
                let mut object_2 = object_4.key("REPLACE").start_object();
                crate::protocol_serde::shape_context_put::ser_context_put(&mut object_2, inner)?;
                object_2.finish();
            }
        },
        crate::types::ContextAction::Delete(inner) => {
             {
                object_4.key("DELETE").string(inner.as_str());
            }
        },
        crate::types::ContextAction::Move(inner) => {
             {
                #[allow(unused_mut)]
                let mut object_3 = object_4.key("MOVE").start_object();
                crate::protocol_serde::shape_context_move::ser_context_move(&mut object_3, inner)?;
                object_3.finish();
            }
        },
        crate::types::ContextAction::Unknown => return Err(::aws_smithy_types::error::operation::SerializationError::unknown_variant("ContextAction"))
    }
    Ok(())
}

