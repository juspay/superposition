// Code generated by software.amazon.smithy.rust.codegen.smithy-rs. DO NOT EDIT.
#[allow(missing_docs)] // documentation missing in model
#[non_exhaustive]
#[derive(::std::clone::Clone, ::std::cmp::PartialEq, ::std::fmt::Debug)]
pub struct UpdateDimensionInput  {
    #[allow(missing_docs)] // documentation missing in model
    pub workspace_id: ::std::option::Option<::std::string::String>,
    #[allow(missing_docs)] // documentation missing in model
    pub org_id: ::std::option::Option<::std::string::String>,
    #[allow(missing_docs)] // documentation missing in model
    pub dimension: ::std::option::Option<::std::string::String>,
    #[allow(missing_docs)] // documentation missing in model
    pub schema: ::std::option::Option<::aws_smithy_types::Document>,
    #[allow(missing_docs)] // documentation missing in model
    pub function_name: ::std::option::Option<::std::string::String>,
    #[allow(missing_docs)] // documentation missing in model
    pub description: ::std::option::Option<::std::string::String>,
    #[allow(missing_docs)] // documentation missing in model
    pub dependencies: ::std::option::Option<::std::vec::Vec::<::std::string::String>>,
    #[allow(missing_docs)] // documentation missing in model
    pub change_reason: ::std::option::Option<::std::string::String>,
}
impl  UpdateDimensionInput  {
    #[allow(missing_docs)] // documentation missing in model
    pub fn workspace_id(&self) -> ::std::option::Option<&str> {
        self.workspace_id.as_deref()
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn org_id(&self) -> ::std::option::Option<&str> {
        self.org_id.as_deref()
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn dimension(&self) -> ::std::option::Option<&str> {
        self.dimension.as_deref()
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn schema(&self) -> ::std::option::Option<&::aws_smithy_types::Document> {
        self.schema.as_ref()
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn function_name(&self) -> ::std::option::Option<&str> {
        self.function_name.as_deref()
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn description(&self) -> ::std::option::Option<&str> {
        self.description.as_deref()
    }
    #[allow(missing_docs)] // documentation missing in model
    /// 
    /// If no value was sent for this field, a default will be set. If you want to determine if no value was sent, use `.dependencies.is_none()`.
    pub fn dependencies(&self) -> &[::std::string::String] {
        self.dependencies.as_deref()
        .unwrap_or_default()
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn change_reason(&self) -> ::std::option::Option<&str> {
        self.change_reason.as_deref()
    }
}
impl UpdateDimensionInput {
    /// Creates a new builder-style object to manufacture [`UpdateDimensionInput`](crate::operation::update_dimension::UpdateDimensionInput).
    pub fn builder() -> crate::operation::update_dimension::builders::UpdateDimensionInputBuilder {
        crate::operation::update_dimension::builders::UpdateDimensionInputBuilder::default()
    }
}

/// A builder for [`UpdateDimensionInput`](crate::operation::update_dimension::UpdateDimensionInput).
#[derive(::std::clone::Clone, ::std::cmp::PartialEq, ::std::default::Default, ::std::fmt::Debug)]
#[non_exhaustive]
pub struct UpdateDimensionInputBuilder {
    pub(crate) workspace_id: ::std::option::Option<::std::string::String>,
    pub(crate) org_id: ::std::option::Option<::std::string::String>,
    pub(crate) dimension: ::std::option::Option<::std::string::String>,
    pub(crate) schema: ::std::option::Option<::aws_smithy_types::Document>,
    pub(crate) function_name: ::std::option::Option<::std::string::String>,
    pub(crate) description: ::std::option::Option<::std::string::String>,
    pub(crate) dependencies: ::std::option::Option<::std::vec::Vec::<::std::string::String>>,
    pub(crate) change_reason: ::std::option::Option<::std::string::String>,
}
impl UpdateDimensionInputBuilder {
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
    #[allow(missing_docs)] // documentation missing in model
    /// This field is required.
    pub fn dimension(mut self, input: impl ::std::convert::Into<::std::string::String>) -> Self {
        self.dimension = ::std::option::Option::Some(input.into());
        self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_dimension(mut self, input: ::std::option::Option<::std::string::String>) -> Self {
        self.dimension = input; self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_dimension(&self) -> &::std::option::Option<::std::string::String> {
        &self.dimension
    }
    #[allow(missing_docs)] // documentation missing in model
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
    /// Appends an item to `dependencies`.
    ///
    /// To override the contents of this collection use [`set_dependencies`](Self::set_dependencies).
    ///
    pub fn dependencies(mut self, input: impl ::std::convert::Into<::std::string::String>) -> Self {
        let mut v = self.dependencies.unwrap_or_default();
                        v.push(input.into());
                        self.dependencies = ::std::option::Option::Some(v);
                        self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_dependencies(mut self, input: ::std::option::Option<::std::vec::Vec::<::std::string::String>>) -> Self {
        self.dependencies = input; self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_dependencies(&self) -> &::std::option::Option<::std::vec::Vec::<::std::string::String>> {
        &self.dependencies
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
    /// Consumes the builder and constructs a [`UpdateDimensionInput`](crate::operation::update_dimension::UpdateDimensionInput).
    pub fn build(self) -> ::std::result::Result<crate::operation::update_dimension::UpdateDimensionInput, ::aws_smithy_types::error::operation::BuildError> {
        ::std::result::Result::Ok(
            crate::operation::update_dimension::UpdateDimensionInput {
                workspace_id: self.workspace_id
                ,
                org_id: self.org_id
                ,
                dimension: self.dimension
                ,
                schema: self.schema
                ,
                function_name: self.function_name
                ,
                description: self.description
                ,
                dependencies: self.dependencies
                ,
                change_reason: self.change_reason
                ,
            }
        )
    }
}

