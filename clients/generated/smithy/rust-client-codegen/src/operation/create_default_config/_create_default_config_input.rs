// Code generated by software.amazon.smithy.rust.codegen.smithy-rs. DO NOT EDIT.
#[allow(missing_docs)] // documentation missing in model
#[non_exhaustive]
#[derive(::std::clone::Clone, ::std::cmp::PartialEq, ::std::fmt::Debug)]
pub struct CreateDefaultConfigInput  {
    #[allow(missing_docs)] // documentation missing in model
    pub key: ::std::option::Option<::std::string::String>,
    #[allow(missing_docs)] // documentation missing in model
    pub value: ::std::option::Option<::aws_smithy_types::Document>,
    #[allow(missing_docs)] // documentation missing in model
    pub schema: ::std::option::Option<::aws_smithy_types::Document>,
    #[allow(missing_docs)] // documentation missing in model
    pub description: ::std::option::Option<::std::string::String>,
    #[allow(missing_docs)] // documentation missing in model
    pub change_reason: ::std::option::Option<::std::string::String>,
    /// Optional
    pub function_name: ::std::option::Option<::std::string::String>,
    #[allow(missing_docs)] // documentation missing in model
    pub workspace_id: ::std::option::Option<::std::string::String>,
    #[allow(missing_docs)] // documentation missing in model
    pub org_id: ::std::option::Option<::std::string::String>,
}
impl  CreateDefaultConfigInput  {
    #[allow(missing_docs)] // documentation missing in model
    pub fn key(&self) -> ::std::option::Option<&str> {
        self.key.as_deref()
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn value(&self) -> ::std::option::Option<&::aws_smithy_types::Document> {
        self.value.as_ref()
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn schema(&self) -> ::std::option::Option<&::aws_smithy_types::Document> {
        self.schema.as_ref()
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn description(&self) -> ::std::option::Option<&str> {
        self.description.as_deref()
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn change_reason(&self) -> ::std::option::Option<&str> {
        self.change_reason.as_deref()
    }
    /// Optional
    pub fn function_name(&self) -> ::std::option::Option<&str> {
        self.function_name.as_deref()
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn workspace_id(&self) -> ::std::option::Option<&str> {
        self.workspace_id.as_deref()
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn org_id(&self) -> ::std::option::Option<&str> {
        self.org_id.as_deref()
    }
}
impl CreateDefaultConfigInput {
    /// Creates a new builder-style object to manufacture [`CreateDefaultConfigInput`](crate::operation::create_default_config::CreateDefaultConfigInput).
    pub fn builder() -> crate::operation::create_default_config::builders::CreateDefaultConfigInputBuilder {
        crate::operation::create_default_config::builders::CreateDefaultConfigInputBuilder::default()
    }
}

/// A builder for [`CreateDefaultConfigInput`](crate::operation::create_default_config::CreateDefaultConfigInput).
#[derive(::std::clone::Clone, ::std::cmp::PartialEq, ::std::default::Default, ::std::fmt::Debug)]
#[non_exhaustive]
pub struct CreateDefaultConfigInputBuilder {
    pub(crate) key: ::std::option::Option<::std::string::String>,
    pub(crate) value: ::std::option::Option<::aws_smithy_types::Document>,
    pub(crate) schema: ::std::option::Option<::aws_smithy_types::Document>,
    pub(crate) description: ::std::option::Option<::std::string::String>,
    pub(crate) change_reason: ::std::option::Option<::std::string::String>,
    pub(crate) function_name: ::std::option::Option<::std::string::String>,
    pub(crate) workspace_id: ::std::option::Option<::std::string::String>,
    pub(crate) org_id: ::std::option::Option<::std::string::String>,
}
impl CreateDefaultConfigInputBuilder {
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
    pub fn workspace_id(mut self, input: impl ::std::convert::Into<::std::string::String>) -> Self {
        self.workspace_id = ::std::option::Option::Some(input.into());
        self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_workspace_id(mut self, input: ::std::option::Option<::std::string::String>) -> Self {
        self.workspace_id = input; self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_workspace_id(&self) -> &::std::option::Option<::std::string::String> {
        &self.workspace_id
    }
    #[allow(missing_docs)] // documentation missing in model
    /// This field is required.
    pub fn org_id(mut self, input: impl ::std::convert::Into<::std::string::String>) -> Self {
        self.org_id = ::std::option::Option::Some(input.into());
        self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_org_id(mut self, input: ::std::option::Option<::std::string::String>) -> Self {
        self.org_id = input; self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_org_id(&self) -> &::std::option::Option<::std::string::String> {
        &self.org_id
    }
    /// Consumes the builder and constructs a [`CreateDefaultConfigInput`](crate::operation::create_default_config::CreateDefaultConfigInput).
    pub fn build(self) -> ::std::result::Result<crate::operation::create_default_config::CreateDefaultConfigInput, ::aws_smithy_types::error::operation::BuildError> {
        ::std::result::Result::Ok(
            crate::operation::create_default_config::CreateDefaultConfigInput {
                key: self.key
                ,
                value: self.value
                ,
                schema: self.schema
                ,
                description: self.description
                ,
                change_reason: self.change_reason
                ,
                function_name: self.function_name
                ,
                workspace_id: self.workspace_id
                ,
                org_id: self.org_id
                ,
            }
        )
    }
}

