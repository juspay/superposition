// Code generated by software.amazon.smithy.rust.codegen.smithy-rs. DO NOT EDIT.
#[allow(missing_docs)] // documentation missing in model
#[non_exhaustive]
#[derive(::std::clone::Clone, ::std::cmp::PartialEq, ::std::fmt::Debug)]
pub struct CreateDefaultConfigOutput  {
    #[allow(missing_docs)] // documentation missing in model
    pub key: ::std::string::String,
    #[allow(missing_docs)] // documentation missing in model
    pub value: ::aws_smithy_types::Document,
    #[allow(missing_docs)] // documentation missing in model
    pub schema: ::aws_smithy_types::Document,
    #[allow(missing_docs)] // documentation missing in model
    pub description: ::std::string::String,
    #[allow(missing_docs)] // documentation missing in model
    pub change_reason: ::std::string::String,
    /// Optional
    pub function_name: ::std::option::Option<::std::string::String>,
    #[allow(missing_docs)] // documentation missing in model
    pub created_at: ::aws_smithy_types::DateTime,
    #[allow(missing_docs)] // documentation missing in model
    pub created_by: ::std::string::String,
    #[allow(missing_docs)] // documentation missing in model
    pub last_modified_at: ::aws_smithy_types::DateTime,
    #[allow(missing_docs)] // documentation missing in model
    pub last_modified_by: ::std::string::String,
}
impl  CreateDefaultConfigOutput  {
    #[allow(missing_docs)] // documentation missing in model
    pub fn key(&self) -> &str {
        use std::ops::Deref; self.key.deref()
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn value(&self) -> &::aws_smithy_types::Document {
        &self.value
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn schema(&self) -> &::aws_smithy_types::Document {
        &self.schema
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn description(&self) -> &str {
        use std::ops::Deref; self.description.deref()
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn change_reason(&self) -> &str {
        use std::ops::Deref; self.change_reason.deref()
    }
    /// Optional
    pub fn function_name(&self) -> ::std::option::Option<&str> {
        self.function_name.as_deref()
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn created_at(&self) -> &::aws_smithy_types::DateTime {
        &self.created_at
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn created_by(&self) -> &str {
        use std::ops::Deref; self.created_by.deref()
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn last_modified_at(&self) -> &::aws_smithy_types::DateTime {
        &self.last_modified_at
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn last_modified_by(&self) -> &str {
        use std::ops::Deref; self.last_modified_by.deref()
    }
}
impl CreateDefaultConfigOutput {
    /// Creates a new builder-style object to manufacture [`CreateDefaultConfigOutput`](crate::operation::create_default_config::CreateDefaultConfigOutput).
    pub fn builder() -> crate::operation::create_default_config::builders::CreateDefaultConfigOutputBuilder {
        crate::operation::create_default_config::builders::CreateDefaultConfigOutputBuilder::default()
    }
}

/// A builder for [`CreateDefaultConfigOutput`](crate::operation::create_default_config::CreateDefaultConfigOutput).
#[derive(::std::clone::Clone, ::std::cmp::PartialEq, ::std::default::Default, ::std::fmt::Debug)]
#[non_exhaustive]
pub struct CreateDefaultConfigOutputBuilder {
    pub(crate) key: ::std::option::Option<::std::string::String>,
    pub(crate) value: ::std::option::Option<::aws_smithy_types::Document>,
    pub(crate) schema: ::std::option::Option<::aws_smithy_types::Document>,
    pub(crate) description: ::std::option::Option<::std::string::String>,
    pub(crate) change_reason: ::std::option::Option<::std::string::String>,
    pub(crate) function_name: ::std::option::Option<::std::string::String>,
    pub(crate) created_at: ::std::option::Option<::aws_smithy_types::DateTime>,
    pub(crate) created_by: ::std::option::Option<::std::string::String>,
    pub(crate) last_modified_at: ::std::option::Option<::aws_smithy_types::DateTime>,
    pub(crate) last_modified_by: ::std::option::Option<::std::string::String>,
}
impl CreateDefaultConfigOutputBuilder {
    #[allow(missing_docs)] // documentation missing in model
    /// This field is required.
    pub fn key(mut self, input: impl ::std::convert::Into<::std::string::String>) -> Self {
        self.key = ::std::option::Option::Some(input.into());
        self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_key(mut self, input: ::std::option::Option<::std::string::String>) -> Self {
        self.key = input; self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_key(&self) -> &::std::option::Option<::std::string::String> {
        &self.key
    }
    #[allow(missing_docs)] // documentation missing in model
    /// This field is required.
    pub fn value(mut self, input: ::aws_smithy_types::Document) -> Self {
        self.value = ::std::option::Option::Some(input);
        self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_value(mut self, input: ::std::option::Option<::aws_smithy_types::Document>) -> Self {
        self.value = input; self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_value(&self) -> &::std::option::Option<::aws_smithy_types::Document> {
        &self.value
    }
    #[allow(missing_docs)] // documentation missing in model
    /// This field is required.
    pub fn schema(mut self, input: ::aws_smithy_types::Document) -> Self {
        self.schema = ::std::option::Option::Some(input);
        self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_schema(mut self, input: ::std::option::Option<::aws_smithy_types::Document>) -> Self {
        self.schema = input; self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_schema(&self) -> &::std::option::Option<::aws_smithy_types::Document> {
        &self.schema
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
    /// Optional
    pub fn function_name(mut self, input: impl ::std::convert::Into<::std::string::String>) -> Self {
        self.function_name = ::std::option::Option::Some(input.into());
        self
    }
    /// Optional
    pub fn set_function_name(mut self, input: ::std::option::Option<::std::string::String>) -> Self {
        self.function_name = input; self
    }
    /// Optional
    pub fn get_function_name(&self) -> &::std::option::Option<::std::string::String> {
        &self.function_name
    }
    #[allow(missing_docs)] // documentation missing in model
    /// This field is required.
    pub fn created_at(mut self, input: ::aws_smithy_types::DateTime) -> Self {
        self.created_at = ::std::option::Option::Some(input);
        self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_created_at(mut self, input: ::std::option::Option<::aws_smithy_types::DateTime>) -> Self {
        self.created_at = input; self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_created_at(&self) -> &::std::option::Option<::aws_smithy_types::DateTime> {
        &self.created_at
    }
    #[allow(missing_docs)] // documentation missing in model
    /// This field is required.
    pub fn created_by(mut self, input: impl ::std::convert::Into<::std::string::String>) -> Self {
        self.created_by = ::std::option::Option::Some(input.into());
        self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_created_by(mut self, input: ::std::option::Option<::std::string::String>) -> Self {
        self.created_by = input; self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_created_by(&self) -> &::std::option::Option<::std::string::String> {
        &self.created_by
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
    /// Consumes the builder and constructs a [`CreateDefaultConfigOutput`](crate::operation::create_default_config::CreateDefaultConfigOutput).
    /// This method will fail if any of the following fields are not set:
    /// - [`key`](crate::operation::create_default_config::builders::CreateDefaultConfigOutputBuilder::key)
    /// - [`value`](crate::operation::create_default_config::builders::CreateDefaultConfigOutputBuilder::value)
    /// - [`schema`](crate::operation::create_default_config::builders::CreateDefaultConfigOutputBuilder::schema)
    /// - [`description`](crate::operation::create_default_config::builders::CreateDefaultConfigOutputBuilder::description)
    /// - [`change_reason`](crate::operation::create_default_config::builders::CreateDefaultConfigOutputBuilder::change_reason)
    /// - [`created_at`](crate::operation::create_default_config::builders::CreateDefaultConfigOutputBuilder::created_at)
    /// - [`created_by`](crate::operation::create_default_config::builders::CreateDefaultConfigOutputBuilder::created_by)
    /// - [`last_modified_at`](crate::operation::create_default_config::builders::CreateDefaultConfigOutputBuilder::last_modified_at)
    /// - [`last_modified_by`](crate::operation::create_default_config::builders::CreateDefaultConfigOutputBuilder::last_modified_by)
    pub fn build(self) -> ::std::result::Result<crate::operation::create_default_config::CreateDefaultConfigOutput, ::aws_smithy_types::error::operation::BuildError> {
        ::std::result::Result::Ok(
            crate::operation::create_default_config::CreateDefaultConfigOutput {
                key: self.key
                    .ok_or_else(||
                        ::aws_smithy_types::error::operation::BuildError::missing_field("key", "key was not specified but it is required when building CreateDefaultConfigOutput")
                    )?
                ,
                value: self.value
                    .ok_or_else(||
                        ::aws_smithy_types::error::operation::BuildError::missing_field("value", "value was not specified but it is required when building CreateDefaultConfigOutput")
                    )?
                ,
                schema: self.schema
                    .ok_or_else(||
                        ::aws_smithy_types::error::operation::BuildError::missing_field("schema", "schema was not specified but it is required when building CreateDefaultConfigOutput")
                    )?
                ,
                description: self.description
                    .ok_or_else(||
                        ::aws_smithy_types::error::operation::BuildError::missing_field("description", "description was not specified but it is required when building CreateDefaultConfigOutput")
                    )?
                ,
                change_reason: self.change_reason
                    .ok_or_else(||
                        ::aws_smithy_types::error::operation::BuildError::missing_field("change_reason", "change_reason was not specified but it is required when building CreateDefaultConfigOutput")
                    )?
                ,
                function_name: self.function_name
                ,
                created_at: self.created_at
                    .ok_or_else(||
                        ::aws_smithy_types::error::operation::BuildError::missing_field("created_at", "created_at was not specified but it is required when building CreateDefaultConfigOutput")
                    )?
                ,
                created_by: self.created_by
                    .ok_or_else(||
                        ::aws_smithy_types::error::operation::BuildError::missing_field("created_by", "created_by was not specified but it is required when building CreateDefaultConfigOutput")
                    )?
                ,
                last_modified_at: self.last_modified_at
                    .ok_or_else(||
                        ::aws_smithy_types::error::operation::BuildError::missing_field("last_modified_at", "last_modified_at was not specified but it is required when building CreateDefaultConfigOutput")
                    )?
                ,
                last_modified_by: self.last_modified_by
                    .ok_or_else(||
                        ::aws_smithy_types::error::operation::BuildError::missing_field("last_modified_by", "last_modified_by was not specified but it is required when building CreateDefaultConfigOutput")
                    )?
                ,
            }
        )
    }
}

