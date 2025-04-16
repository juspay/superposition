// Code generated by software.amazon.smithy.rust.codegen.smithy-rs. DO NOT EDIT.
#[allow(missing_docs)] // documentation missing in model
#[non_exhaustive]
#[derive(::std::clone::Clone, ::std::cmp::PartialEq, ::std::fmt::Debug)]
pub struct CreateContextInput  {
    #[allow(missing_docs)] // documentation missing in model
    pub workspace_id: ::std::option::Option<::std::string::String>,
    #[allow(missing_docs)] // documentation missing in model
    pub org_id: ::std::option::Option<::std::string::String>,
    #[allow(missing_docs)] // documentation missing in model
    pub context: ::std::option::Option<::std::collections::HashMap::<::std::string::String, ::aws_smithy_types::Document>>,
    #[allow(missing_docs)] // documentation missing in model
    pub config_tags: ::std::option::Option<::std::string::String>,
    #[allow(missing_docs)] // documentation missing in model
    pub r#override: ::std::option::Option<::std::collections::HashMap::<::std::string::String, ::aws_smithy_types::Document>>,
    #[allow(missing_docs)] // documentation missing in model
    pub description: ::std::option::Option<::std::string::String>,
    #[allow(missing_docs)] // documentation missing in model
    pub change_reason: ::std::option::Option<::std::string::String>,
}
impl  CreateContextInput  {
    #[allow(missing_docs)] // documentation missing in model
    pub fn workspace_id(&self) -> ::std::option::Option<&str> {
        self.workspace_id.as_deref()
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn org_id(&self) -> ::std::option::Option<&str> {
        self.org_id.as_deref()
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn context(&self) -> ::std::option::Option<&::std::collections::HashMap::<::std::string::String, ::aws_smithy_types::Document>> {
        self.context.as_ref()
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn config_tags(&self) -> ::std::option::Option<&str> {
        self.config_tags.as_deref()
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn r#override(&self) -> ::std::option::Option<&::std::collections::HashMap::<::std::string::String, ::aws_smithy_types::Document>> {
        self.r#override.as_ref()
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn description(&self) -> ::std::option::Option<&str> {
        self.description.as_deref()
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn change_reason(&self) -> ::std::option::Option<&str> {
        self.change_reason.as_deref()
    }
}
impl CreateContextInput {
    /// Creates a new builder-style object to manufacture [`CreateContextInput`](crate::operation::create_context::CreateContextInput).
    pub fn builder() -> crate::operation::create_context::builders::CreateContextInputBuilder {
        crate::operation::create_context::builders::CreateContextInputBuilder::default()
    }
}

/// A builder for [`CreateContextInput`](crate::operation::create_context::CreateContextInput).
#[derive(::std::clone::Clone, ::std::cmp::PartialEq, ::std::default::Default, ::std::fmt::Debug)]
#[non_exhaustive]
pub struct CreateContextInputBuilder {
    pub(crate) workspace_id: ::std::option::Option<::std::string::String>,
    pub(crate) org_id: ::std::option::Option<::std::string::String>,
    pub(crate) context: ::std::option::Option<::std::collections::HashMap::<::std::string::String, ::aws_smithy_types::Document>>,
    pub(crate) config_tags: ::std::option::Option<::std::string::String>,
    pub(crate) r#override: ::std::option::Option<::std::collections::HashMap::<::std::string::String, ::aws_smithy_types::Document>>,
    pub(crate) description: ::std::option::Option<::std::string::String>,
    pub(crate) change_reason: ::std::option::Option<::std::string::String>,
}
impl CreateContextInputBuilder {
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
    /// Adds a key-value pair to `context`.
    ///
    /// To override the contents of this collection use [`set_context`](Self::set_context).
    ///
    pub fn context(mut self, k: impl ::std::convert::Into<::std::string::String>, v: ::aws_smithy_types::Document) -> Self {
        let mut hash_map = self.context.unwrap_or_default();
                        hash_map.insert(k.into(), v);
                        self.context = ::std::option::Option::Some(hash_map);
                        self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_context(mut self, input: ::std::option::Option<::std::collections::HashMap::<::std::string::String, ::aws_smithy_types::Document>>) -> Self {
        self.context = input; self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_context(&self) -> &::std::option::Option<::std::collections::HashMap::<::std::string::String, ::aws_smithy_types::Document>> {
        &self.context
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn config_tags(mut self, input: impl ::std::convert::Into<::std::string::String>) -> Self {
        self.config_tags = ::std::option::Option::Some(input.into());
        self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_config_tags(mut self, input: ::std::option::Option<::std::string::String>) -> Self {
        self.config_tags = input; self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_config_tags(&self) -> &::std::option::Option<::std::string::String> {
        &self.config_tags
    }
    /// Adds a key-value pair to `r#override`.
    ///
    /// To override the contents of this collection use [`set_override`](Self::set_override).
    ///
    pub fn r#override(mut self, k: impl ::std::convert::Into<::std::string::String>, v: ::aws_smithy_types::Document) -> Self {
        let mut hash_map = self.r#override.unwrap_or_default();
                        hash_map.insert(k.into(), v);
                        self.r#override = ::std::option::Option::Some(hash_map);
                        self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_override(mut self, input: ::std::option::Option<::std::collections::HashMap::<::std::string::String, ::aws_smithy_types::Document>>) -> Self {
        self.r#override = input; self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_override(&self) -> &::std::option::Option<::std::collections::HashMap::<::std::string::String, ::aws_smithy_types::Document>> {
        &self.r#override
    }
    #[allow(missing_docs)] // documentation missing in model
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
    /// Consumes the builder and constructs a [`CreateContextInput`](crate::operation::create_context::CreateContextInput).
    pub fn build(self) -> ::std::result::Result<crate::operation::create_context::CreateContextInput, ::aws_smithy_types::error::operation::BuildError> {
        ::std::result::Result::Ok(
            crate::operation::create_context::CreateContextInput {
                workspace_id: self.workspace_id
                ,
                org_id: self.org_id
                ,
                context: self.context
                ,
                config_tags: self.config_tags
                ,
                r#override: self.r#override
                ,
                description: self.description
                ,
                change_reason: self.change_reason
                ,
            }
        )
    }
}

