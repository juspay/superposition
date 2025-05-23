// Code generated by software.amazon.smithy.rust.codegen.smithy-rs. DO NOT EDIT.
#[allow(missing_docs)] // documentation missing in model
#[non_exhaustive]
#[derive(::std::clone::Clone, ::std::cmp::PartialEq, ::std::fmt::Debug)]
pub struct GetConfigFastOutput  {
    #[allow(missing_docs)] // documentation missing in model
    pub config: ::std::option::Option<::aws_smithy_types::Document>,
    #[allow(missing_docs)] // documentation missing in model
    pub version: ::std::option::Option<::std::string::String>,
    #[allow(missing_docs)] // documentation missing in model
    pub last_modified: ::std::option::Option<::aws_smithy_types::DateTime>,
    #[allow(missing_docs)] // documentation missing in model
    pub audit_id: ::std::option::Option<::std::string::String>,
}
impl  GetConfigFastOutput  {
    #[allow(missing_docs)] // documentation missing in model
    pub fn config(&self) -> ::std::option::Option<&::aws_smithy_types::Document> {
        self.config.as_ref()
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn version(&self) -> ::std::option::Option<&str> {
        self.version.as_deref()
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn last_modified(&self) -> ::std::option::Option<&::aws_smithy_types::DateTime> {
        self.last_modified.as_ref()
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn audit_id(&self) -> ::std::option::Option<&str> {
        self.audit_id.as_deref()
    }
}
impl GetConfigFastOutput {
    /// Creates a new builder-style object to manufacture [`GetConfigFastOutput`](crate::operation::get_config_fast::GetConfigFastOutput).
    pub fn builder() -> crate::operation::get_config_fast::builders::GetConfigFastOutputBuilder {
        crate::operation::get_config_fast::builders::GetConfigFastOutputBuilder::default()
    }
}

/// A builder for [`GetConfigFastOutput`](crate::operation::get_config_fast::GetConfigFastOutput).
#[derive(::std::clone::Clone, ::std::cmp::PartialEq, ::std::default::Default, ::std::fmt::Debug)]
#[non_exhaustive]
pub struct GetConfigFastOutputBuilder {
    pub(crate) config: ::std::option::Option<::aws_smithy_types::Document>,
    pub(crate) version: ::std::option::Option<::std::string::String>,
    pub(crate) last_modified: ::std::option::Option<::aws_smithy_types::DateTime>,
    pub(crate) audit_id: ::std::option::Option<::std::string::String>,
}
impl GetConfigFastOutputBuilder {
    #[allow(missing_docs)] // documentation missing in model
    pub fn config(mut self, input: ::aws_smithy_types::Document) -> Self {
        self.config = ::std::option::Option::Some(input);
        self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_config(mut self, input: ::std::option::Option<::aws_smithy_types::Document>) -> Self {
        self.config = input; self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_config(&self) -> &::std::option::Option<::aws_smithy_types::Document> {
        &self.config
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn version(mut self, input: impl ::std::convert::Into<::std::string::String>) -> Self {
        self.version = ::std::option::Option::Some(input.into());
        self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_version(mut self, input: ::std::option::Option<::std::string::String>) -> Self {
        self.version = input; self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_version(&self) -> &::std::option::Option<::std::string::String> {
        &self.version
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn last_modified(mut self, input: ::aws_smithy_types::DateTime) -> Self {
        self.last_modified = ::std::option::Option::Some(input);
        self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_last_modified(mut self, input: ::std::option::Option<::aws_smithy_types::DateTime>) -> Self {
        self.last_modified = input; self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_last_modified(&self) -> &::std::option::Option<::aws_smithy_types::DateTime> {
        &self.last_modified
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn audit_id(mut self, input: impl ::std::convert::Into<::std::string::String>) -> Self {
        self.audit_id = ::std::option::Option::Some(input.into());
        self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_audit_id(mut self, input: ::std::option::Option<::std::string::String>) -> Self {
        self.audit_id = input; self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_audit_id(&self) -> &::std::option::Option<::std::string::String> {
        &self.audit_id
    }
    /// Consumes the builder and constructs a [`GetConfigFastOutput`](crate::operation::get_config_fast::GetConfigFastOutput).
    pub fn build(self) -> crate::operation::get_config_fast::GetConfigFastOutput {
        crate::operation::get_config_fast::GetConfigFastOutput {
            config: self.config
            ,
            version: self.version
            ,
            last_modified: self.last_modified
            ,
            audit_id: self.audit_id
            ,
        }
    }
}

