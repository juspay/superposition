// Code generated by software.amazon.smithy.rust.codegen.smithy-rs. DO NOT EDIT.
#[allow(missing_docs)] // documentation missing in model
#[non_exhaustive]
#[derive(::std::clone::Clone, ::std::cmp::PartialEq, ::std::fmt::Debug)]
pub struct ListFunctionInput  {
    #[allow(missing_docs)] // documentation missing in model
    pub count: ::std::option::Option<i32>,
    #[allow(missing_docs)] // documentation missing in model
    pub page: ::std::option::Option<i32>,
    #[allow(missing_docs)] // documentation missing in model
    pub workspace_id: ::std::option::Option<::std::string::String>,
    #[allow(missing_docs)] // documentation missing in model
    pub org_id: ::std::option::Option<::std::string::String>,
}
impl  ListFunctionInput  {
    #[allow(missing_docs)] // documentation missing in model
    pub fn count(&self) -> ::std::option::Option<i32> {
        self.count
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn page(&self) -> ::std::option::Option<i32> {
        self.page
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
impl ListFunctionInput {
    /// Creates a new builder-style object to manufacture [`ListFunctionInput`](crate::operation::list_function::ListFunctionInput).
    pub fn builder() -> crate::operation::list_function::builders::ListFunctionInputBuilder {
        crate::operation::list_function::builders::ListFunctionInputBuilder::default()
    }
}

/// A builder for [`ListFunctionInput`](crate::operation::list_function::ListFunctionInput).
#[derive(::std::clone::Clone, ::std::cmp::PartialEq, ::std::default::Default, ::std::fmt::Debug)]
#[non_exhaustive]
pub struct ListFunctionInputBuilder {
    pub(crate) count: ::std::option::Option<i32>,
    pub(crate) page: ::std::option::Option<i32>,
    pub(crate) workspace_id: ::std::option::Option<::std::string::String>,
    pub(crate) org_id: ::std::option::Option<::std::string::String>,
}
impl ListFunctionInputBuilder {
    #[allow(missing_docs)] // documentation missing in model
    pub fn count(mut self, input: i32) -> Self {
        self.count = ::std::option::Option::Some(input);
        self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_count(mut self, input: ::std::option::Option<i32>) -> Self {
        self.count = input; self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_count(&self) -> &::std::option::Option<i32> {
        &self.count
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn page(mut self, input: i32) -> Self {
        self.page = ::std::option::Option::Some(input);
        self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_page(mut self, input: ::std::option::Option<i32>) -> Self {
        self.page = input; self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_page(&self) -> &::std::option::Option<i32> {
        &self.page
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
    /// Consumes the builder and constructs a [`ListFunctionInput`](crate::operation::list_function::ListFunctionInput).
    pub fn build(self) -> ::std::result::Result<crate::operation::list_function::ListFunctionInput, ::aws_smithy_types::error::operation::BuildError> {
        ::std::result::Result::Ok(
            crate::operation::list_function::ListFunctionInput {
                count: self.count
                ,
                page: self.page
                ,
                workspace_id: self.workspace_id
                ,
                org_id: self.org_id
                ,
            }
        )
    }
}

