// Code generated by software.amazon.smithy.rust.codegen.smithy-rs. DO NOT EDIT.
#[allow(missing_docs)] // documentation missing in model
#[non_exhaustive]
#[derive(::std::clone::Clone, ::std::cmp::PartialEq, ::std::fmt::Debug)]
pub struct PublishOutput  {
    #[allow(missing_docs)] // documentation missing in model
    pub function_name: ::std::string::String,
    #[allow(missing_docs)] // documentation missing in model
    pub published_code: ::std::option::Option<::std::string::String>,
    #[allow(missing_docs)] // documentation missing in model
    pub draft_code: ::std::string::String,
    #[allow(missing_docs)] // documentation missing in model
    pub published_runtime_version: ::std::option::Option<::std::string::String>,
    #[allow(missing_docs)] // documentation missing in model
    pub draft_runtime_version: ::std::string::String,
    #[allow(missing_docs)] // documentation missing in model
    pub published_at: ::std::option::Option<::aws_smithy_types::DateTime>,
    #[allow(missing_docs)] // documentation missing in model
    pub draft_edited_at: ::aws_smithy_types::DateTime,
    #[allow(missing_docs)] // documentation missing in model
    pub published_by: ::std::option::Option<::std::string::String>,
    #[allow(missing_docs)] // documentation missing in model
    pub draft_edited_by: ::std::string::String,
    #[allow(missing_docs)] // documentation missing in model
    pub last_modified_at: ::aws_smithy_types::DateTime,
    #[allow(missing_docs)] // documentation missing in model
    pub last_modified_by: ::std::string::String,
    #[allow(missing_docs)] // documentation missing in model
    pub change_reason: ::std::string::String,
    #[allow(missing_docs)] // documentation missing in model
    pub description: ::std::string::String,
    #[allow(missing_docs)] // documentation missing in model
    pub function_type: crate::types::FunctionTypes,
}
impl  PublishOutput  {
    #[allow(missing_docs)] // documentation missing in model
    pub fn function_name(&self) -> &str {
        use std::ops::Deref; self.function_name.deref()
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn published_code(&self) -> ::std::option::Option<&str> {
        self.published_code.as_deref()
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn draft_code(&self) -> &str {
        use std::ops::Deref; self.draft_code.deref()
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn published_runtime_version(&self) -> ::std::option::Option<&str> {
        self.published_runtime_version.as_deref()
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn draft_runtime_version(&self) -> &str {
        use std::ops::Deref; self.draft_runtime_version.deref()
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn published_at(&self) -> ::std::option::Option<&::aws_smithy_types::DateTime> {
        self.published_at.as_ref()
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn draft_edited_at(&self) -> &::aws_smithy_types::DateTime {
        &self.draft_edited_at
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn published_by(&self) -> ::std::option::Option<&str> {
        self.published_by.as_deref()
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn draft_edited_by(&self) -> &str {
        use std::ops::Deref; self.draft_edited_by.deref()
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn last_modified_at(&self) -> &::aws_smithy_types::DateTime {
        &self.last_modified_at
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn last_modified_by(&self) -> &str {
        use std::ops::Deref; self.last_modified_by.deref()
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn change_reason(&self) -> &str {
        use std::ops::Deref; self.change_reason.deref()
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn description(&self) -> &str {
        use std::ops::Deref; self.description.deref()
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn function_type(&self) -> &crate::types::FunctionTypes {
        &self.function_type
    }
}
impl PublishOutput {
    /// Creates a new builder-style object to manufacture [`PublishOutput`](crate::operation::publish::PublishOutput).
    pub fn builder() -> crate::operation::publish::builders::PublishOutputBuilder {
        crate::operation::publish::builders::PublishOutputBuilder::default()
    }
}

/// A builder for [`PublishOutput`](crate::operation::publish::PublishOutput).
#[derive(::std::clone::Clone, ::std::cmp::PartialEq, ::std::default::Default, ::std::fmt::Debug)]
#[non_exhaustive]
pub struct PublishOutputBuilder {
    pub(crate) function_name: ::std::option::Option<::std::string::String>,
    pub(crate) published_code: ::std::option::Option<::std::string::String>,
    pub(crate) draft_code: ::std::option::Option<::std::string::String>,
    pub(crate) published_runtime_version: ::std::option::Option<::std::string::String>,
    pub(crate) draft_runtime_version: ::std::option::Option<::std::string::String>,
    pub(crate) published_at: ::std::option::Option<::aws_smithy_types::DateTime>,
    pub(crate) draft_edited_at: ::std::option::Option<::aws_smithy_types::DateTime>,
    pub(crate) published_by: ::std::option::Option<::std::string::String>,
    pub(crate) draft_edited_by: ::std::option::Option<::std::string::String>,
    pub(crate) last_modified_at: ::std::option::Option<::aws_smithy_types::DateTime>,
    pub(crate) last_modified_by: ::std::option::Option<::std::string::String>,
    pub(crate) change_reason: ::std::option::Option<::std::string::String>,
    pub(crate) description: ::std::option::Option<::std::string::String>,
    pub(crate) function_type: ::std::option::Option<crate::types::FunctionTypes>,
}
impl PublishOutputBuilder {
    #[allow(missing_docs)] // documentation missing in model
    /// This field is required.
    pub fn function_name(mut self, input: impl ::std::convert::Into<::std::string::String>) -> Self {
        self.function_name = ::std::option::Option::Some(input.into());
        self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_function_name(mut self, input: ::std::option::Option<::std::string::String>) -> Self {
        self.function_name = input; self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_function_name(&self) -> &::std::option::Option<::std::string::String> {
        &self.function_name
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn published_code(mut self, input: impl ::std::convert::Into<::std::string::String>) -> Self {
        self.published_code = ::std::option::Option::Some(input.into());
        self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_published_code(mut self, input: ::std::option::Option<::std::string::String>) -> Self {
        self.published_code = input; self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_published_code(&self) -> &::std::option::Option<::std::string::String> {
        &self.published_code
    }
    #[allow(missing_docs)] // documentation missing in model
    /// This field is required.
    pub fn draft_code(mut self, input: impl ::std::convert::Into<::std::string::String>) -> Self {
        self.draft_code = ::std::option::Option::Some(input.into());
        self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_draft_code(mut self, input: ::std::option::Option<::std::string::String>) -> Self {
        self.draft_code = input; self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_draft_code(&self) -> &::std::option::Option<::std::string::String> {
        &self.draft_code
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn published_runtime_version(mut self, input: impl ::std::convert::Into<::std::string::String>) -> Self {
        self.published_runtime_version = ::std::option::Option::Some(input.into());
        self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_published_runtime_version(mut self, input: ::std::option::Option<::std::string::String>) -> Self {
        self.published_runtime_version = input; self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_published_runtime_version(&self) -> &::std::option::Option<::std::string::String> {
        &self.published_runtime_version
    }
    #[allow(missing_docs)] // documentation missing in model
    /// This field is required.
    pub fn draft_runtime_version(mut self, input: impl ::std::convert::Into<::std::string::String>) -> Self {
        self.draft_runtime_version = ::std::option::Option::Some(input.into());
        self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_draft_runtime_version(mut self, input: ::std::option::Option<::std::string::String>) -> Self {
        self.draft_runtime_version = input; self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_draft_runtime_version(&self) -> &::std::option::Option<::std::string::String> {
        &self.draft_runtime_version
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn published_at(mut self, input: ::aws_smithy_types::DateTime) -> Self {
        self.published_at = ::std::option::Option::Some(input);
        self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_published_at(mut self, input: ::std::option::Option<::aws_smithy_types::DateTime>) -> Self {
        self.published_at = input; self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_published_at(&self) -> &::std::option::Option<::aws_smithy_types::DateTime> {
        &self.published_at
    }
    #[allow(missing_docs)] // documentation missing in model
    /// This field is required.
    pub fn draft_edited_at(mut self, input: ::aws_smithy_types::DateTime) -> Self {
        self.draft_edited_at = ::std::option::Option::Some(input);
        self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_draft_edited_at(mut self, input: ::std::option::Option<::aws_smithy_types::DateTime>) -> Self {
        self.draft_edited_at = input; self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_draft_edited_at(&self) -> &::std::option::Option<::aws_smithy_types::DateTime> {
        &self.draft_edited_at
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn published_by(mut self, input: impl ::std::convert::Into<::std::string::String>) -> Self {
        self.published_by = ::std::option::Option::Some(input.into());
        self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_published_by(mut self, input: ::std::option::Option<::std::string::String>) -> Self {
        self.published_by = input; self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_published_by(&self) -> &::std::option::Option<::std::string::String> {
        &self.published_by
    }
    #[allow(missing_docs)] // documentation missing in model
    /// This field is required.
    pub fn draft_edited_by(mut self, input: impl ::std::convert::Into<::std::string::String>) -> Self {
        self.draft_edited_by = ::std::option::Option::Some(input.into());
        self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_draft_edited_by(mut self, input: ::std::option::Option<::std::string::String>) -> Self {
        self.draft_edited_by = input; self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_draft_edited_by(&self) -> &::std::option::Option<::std::string::String> {
        &self.draft_edited_by
    }
    #[allow(missing_docs)] // documentation missing in model
    /// This field is required.
    pub fn last_modified_at(mut self, input: ::aws_smithy_types::DateTime) -> Self {
        self.last_modified_at = ::std::option::Option::Some(input);
        self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_last_modified_at(mut self, input: ::std::option::Option<::aws_smithy_types::DateTime>) -> Self {
        self.last_modified_at = input; self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_last_modified_at(&self) -> &::std::option::Option<::aws_smithy_types::DateTime> {
        &self.last_modified_at
    }
    #[allow(missing_docs)] // documentation missing in model
    /// This field is required.
    pub fn last_modified_by(mut self, input: impl ::std::convert::Into<::std::string::String>) -> Self {
        self.last_modified_by = ::std::option::Option::Some(input.into());
        self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_last_modified_by(mut self, input: ::std::option::Option<::std::string::String>) -> Self {
        self.last_modified_by = input; self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_last_modified_by(&self) -> &::std::option::Option<::std::string::String> {
        &self.last_modified_by
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
    pub fn function_type(mut self, input: crate::types::FunctionTypes) -> Self {
        self.function_type = ::std::option::Option::Some(input);
        self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_function_type(mut self, input: ::std::option::Option<crate::types::FunctionTypes>) -> Self {
        self.function_type = input; self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_function_type(&self) -> &::std::option::Option<crate::types::FunctionTypes> {
        &self.function_type
    }
    /// Consumes the builder and constructs a [`PublishOutput`](crate::operation::publish::PublishOutput).
    /// This method will fail if any of the following fields are not set:
    /// - [`function_name`](crate::operation::publish::builders::PublishOutputBuilder::function_name)
    /// - [`draft_code`](crate::operation::publish::builders::PublishOutputBuilder::draft_code)
    /// - [`draft_runtime_version`](crate::operation::publish::builders::PublishOutputBuilder::draft_runtime_version)
    /// - [`draft_edited_at`](crate::operation::publish::builders::PublishOutputBuilder::draft_edited_at)
    /// - [`draft_edited_by`](crate::operation::publish::builders::PublishOutputBuilder::draft_edited_by)
    /// - [`last_modified_at`](crate::operation::publish::builders::PublishOutputBuilder::last_modified_at)
    /// - [`last_modified_by`](crate::operation::publish::builders::PublishOutputBuilder::last_modified_by)
    /// - [`change_reason`](crate::operation::publish::builders::PublishOutputBuilder::change_reason)
    /// - [`description`](crate::operation::publish::builders::PublishOutputBuilder::description)
    /// - [`function_type`](crate::operation::publish::builders::PublishOutputBuilder::function_type)
    pub fn build(self) -> ::std::result::Result<crate::operation::publish::PublishOutput, ::aws_smithy_types::error::operation::BuildError> {
        ::std::result::Result::Ok(
            crate::operation::publish::PublishOutput {
                function_name: self.function_name
                    .ok_or_else(||
                        ::aws_smithy_types::error::operation::BuildError::missing_field("function_name", "function_name was not specified but it is required when building PublishOutput")
                    )?
                ,
                published_code: self.published_code
                ,
                draft_code: self.draft_code
                    .ok_or_else(||
                        ::aws_smithy_types::error::operation::BuildError::missing_field("draft_code", "draft_code was not specified but it is required when building PublishOutput")
                    )?
                ,
                published_runtime_version: self.published_runtime_version
                ,
                draft_runtime_version: self.draft_runtime_version
                    .ok_or_else(||
                        ::aws_smithy_types::error::operation::BuildError::missing_field("draft_runtime_version", "draft_runtime_version was not specified but it is required when building PublishOutput")
                    )?
                ,
                published_at: self.published_at
                ,
                draft_edited_at: self.draft_edited_at
                    .ok_or_else(||
                        ::aws_smithy_types::error::operation::BuildError::missing_field("draft_edited_at", "draft_edited_at was not specified but it is required when building PublishOutput")
                    )?
                ,
                published_by: self.published_by
                ,
                draft_edited_by: self.draft_edited_by
                    .ok_or_else(||
                        ::aws_smithy_types::error::operation::BuildError::missing_field("draft_edited_by", "draft_edited_by was not specified but it is required when building PublishOutput")
                    )?
                ,
                last_modified_at: self.last_modified_at
                    .ok_or_else(||
                        ::aws_smithy_types::error::operation::BuildError::missing_field("last_modified_at", "last_modified_at was not specified but it is required when building PublishOutput")
                    )?
                ,
                last_modified_by: self.last_modified_by
                    .ok_or_else(||
                        ::aws_smithy_types::error::operation::BuildError::missing_field("last_modified_by", "last_modified_by was not specified but it is required when building PublishOutput")
                    )?
                ,
                change_reason: self.change_reason
                    .ok_or_else(||
                        ::aws_smithy_types::error::operation::BuildError::missing_field("change_reason", "change_reason was not specified but it is required when building PublishOutput")
                    )?
                ,
                description: self.description
                    .ok_or_else(||
                        ::aws_smithy_types::error::operation::BuildError::missing_field("description", "description was not specified but it is required when building PublishOutput")
                    )?
                ,
                function_type: self.function_type
                    .ok_or_else(||
                        ::aws_smithy_types::error::operation::BuildError::missing_field("function_type", "function_type was not specified but it is required when building PublishOutput")
                    )?
                ,
            }
        )
    }
}

