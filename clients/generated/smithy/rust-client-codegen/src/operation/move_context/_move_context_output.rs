// Code generated by software.amazon.smithy.rust.codegen.smithy-rs. DO NOT EDIT.
#[allow(missing_docs)] // documentation missing in model
#[non_exhaustive]
#[derive(::std::clone::Clone, ::std::cmp::PartialEq, ::std::fmt::Debug)]
pub struct MoveContextOutput  {
    #[allow(missing_docs)] // documentation missing in model
    pub context_id: ::std::string::String,
    #[allow(missing_docs)] // documentation missing in model
    pub override_id: ::std::string::String,
    #[allow(missing_docs)] // documentation missing in model
    pub weight: ::std::string::String,
    #[allow(missing_docs)] // documentation missing in model
    pub description: ::std::string::String,
    #[allow(missing_docs)] // documentation missing in model
    pub change_reason: ::std::string::String,
}
impl  MoveContextOutput  {
    #[allow(missing_docs)] // documentation missing in model
    pub fn context_id(&self) -> &str {
        use std::ops::Deref; self.context_id.deref()
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn override_id(&self) -> &str {
        use std::ops::Deref; self.override_id.deref()
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn weight(&self) -> &str {
        use std::ops::Deref; self.weight.deref()
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn description(&self) -> &str {
        use std::ops::Deref; self.description.deref()
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn change_reason(&self) -> &str {
        use std::ops::Deref; self.change_reason.deref()
    }
}
impl MoveContextOutput {
    /// Creates a new builder-style object to manufacture [`MoveContextOutput`](crate::operation::move_context::MoveContextOutput).
    pub fn builder() -> crate::operation::move_context::builders::MoveContextOutputBuilder {
        crate::operation::move_context::builders::MoveContextOutputBuilder::default()
    }
}

/// A builder for [`MoveContextOutput`](crate::operation::move_context::MoveContextOutput).
#[derive(::std::clone::Clone, ::std::cmp::PartialEq, ::std::default::Default, ::std::fmt::Debug)]
#[non_exhaustive]
pub struct MoveContextOutputBuilder {
    pub(crate) context_id: ::std::option::Option<::std::string::String>,
    pub(crate) override_id: ::std::option::Option<::std::string::String>,
    pub(crate) weight: ::std::option::Option<::std::string::String>,
    pub(crate) description: ::std::option::Option<::std::string::String>,
    pub(crate) change_reason: ::std::option::Option<::std::string::String>,
}
impl MoveContextOutputBuilder {
    #[allow(missing_docs)] // documentation missing in model
    /// This field is required.
    pub fn context_id(mut self, input: impl ::std::convert::Into<::std::string::String>) -> Self {
        self.context_id = ::std::option::Option::Some(input.into());
        self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_context_id(mut self, input: ::std::option::Option<::std::string::String>) -> Self {
        self.context_id = input; self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_context_id(&self) -> &::std::option::Option<::std::string::String> {
        &self.context_id
    }
    #[allow(missing_docs)] // documentation missing in model
    /// This field is required.
    pub fn override_id(mut self, input: impl ::std::convert::Into<::std::string::String>) -> Self {
        self.override_id = ::std::option::Option::Some(input.into());
        self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_override_id(mut self, input: ::std::option::Option<::std::string::String>) -> Self {
        self.override_id = input; self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_override_id(&self) -> &::std::option::Option<::std::string::String> {
        &self.override_id
    }
    #[allow(missing_docs)] // documentation missing in model
    /// This field is required.
    pub fn weight(mut self, input: impl ::std::convert::Into<::std::string::String>) -> Self {
        self.weight = ::std::option::Option::Some(input.into());
        self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_weight(mut self, input: ::std::option::Option<::std::string::String>) -> Self {
        self.weight = input; self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_weight(&self) -> &::std::option::Option<::std::string::String> {
        &self.weight
    }
    #[allow(missing_docs)] // documentation missing in model
    /// This field is required.
    pub fn description(mut self, input: impl ::std::convert::Into<::std::string::String>) -> Self {
        self.description = ::std::option::Option::Some(input.into());
        self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_description(mut self, input: ::std::option::Option<::std::string::String>) -> Self {
        self.description = input; self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_description(&self) -> &::std::option::Option<::std::string::String> {
        &self.description
    }
    #[allow(missing_docs)] // documentation missing in model
    /// This field is required.
    pub fn change_reason(mut self, input: impl ::std::convert::Into<::std::string::String>) -> Self {
        self.change_reason = ::std::option::Option::Some(input.into());
        self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_change_reason(mut self, input: ::std::option::Option<::std::string::String>) -> Self {
        self.change_reason = input; self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_change_reason(&self) -> &::std::option::Option<::std::string::String> {
        &self.change_reason
    }
    /// Consumes the builder and constructs a [`MoveContextOutput`](crate::operation::move_context::MoveContextOutput).
    /// This method will fail if any of the following fields are not set:
    /// - [`context_id`](crate::operation::move_context::builders::MoveContextOutputBuilder::context_id)
    /// - [`override_id`](crate::operation::move_context::builders::MoveContextOutputBuilder::override_id)
    /// - [`weight`](crate::operation::move_context::builders::MoveContextOutputBuilder::weight)
    /// - [`description`](crate::operation::move_context::builders::MoveContextOutputBuilder::description)
    /// - [`change_reason`](crate::operation::move_context::builders::MoveContextOutputBuilder::change_reason)
    pub fn build(self) -> ::std::result::Result<crate::operation::move_context::MoveContextOutput, ::aws_smithy_types::error::operation::BuildError> {
        ::std::result::Result::Ok(
            crate::operation::move_context::MoveContextOutput {
                context_id: self.context_id
                    .ok_or_else(||
                        ::aws_smithy_types::error::operation::BuildError::missing_field("context_id", "context_id was not specified but it is required when building MoveContextOutput")
                    )?
                ,
                override_id: self.override_id
                    .ok_or_else(||
                        ::aws_smithy_types::error::operation::BuildError::missing_field("override_id", "override_id was not specified but it is required when building MoveContextOutput")
                    )?
                ,
                weight: self.weight
                    .ok_or_else(||
                        ::aws_smithy_types::error::operation::BuildError::missing_field("weight", "weight was not specified but it is required when building MoveContextOutput")
                    )?
                ,
                description: self.description
                    .ok_or_else(||
                        ::aws_smithy_types::error::operation::BuildError::missing_field("description", "description was not specified but it is required when building MoveContextOutput")
                    )?
                ,
                change_reason: self.change_reason
                    .ok_or_else(||
                        ::aws_smithy_types::error::operation::BuildError::missing_field("change_reason", "change_reason was not specified but it is required when building MoveContextOutput")
                    )?
                ,
            }
        )
    }
}

