// Code generated by software.amazon.smithy.rust.codegen.smithy-rs. DO NOT EDIT.
#[allow(missing_docs)] // documentation missing in model
#[non_exhaustive]
#[derive(::std::clone::Clone, ::std::cmp::PartialEq, ::std::fmt::Debug)]
pub struct UpdateWorkspaceInput  {
    #[allow(missing_docs)] // documentation missing in model
    pub org_id: ::std::option::Option<::std::string::String>,
    #[allow(missing_docs)] // documentation missing in model
    pub workspace_name: ::std::option::Option<::std::string::String>,
    #[allow(missing_docs)] // documentation missing in model
    pub workspace_admin_email: ::std::option::Option<::std::string::String>,
    #[allow(missing_docs)] // documentation missing in model
    pub mandatory_dimensions: ::std::option::Option<::std::vec::Vec::<::std::string::String>>,
    #[allow(missing_docs)] // documentation missing in model
    pub workspace_status: ::std::option::Option<crate::types::WorkspaceStatus>,
}
impl  UpdateWorkspaceInput  {
    #[allow(missing_docs)] // documentation missing in model
    pub fn org_id(&self) -> ::std::option::Option<&str> {
        self.org_id.as_deref()
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn workspace_name(&self) -> ::std::option::Option<&str> {
        self.workspace_name.as_deref()
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn workspace_admin_email(&self) -> ::std::option::Option<&str> {
        self.workspace_admin_email.as_deref()
    }
    #[allow(missing_docs)] // documentation missing in model
    /// 
    /// If no value was sent for this field, a default will be set. If you want to determine if no value was sent, use `.mandatory_dimensions.is_none()`.
    pub fn mandatory_dimensions(&self) -> &[::std::string::String] {
        self.mandatory_dimensions.as_deref()
        .unwrap_or_default()
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn workspace_status(&self) -> ::std::option::Option<&crate::types::WorkspaceStatus> {
        self.workspace_status.as_ref()
    }
}
impl UpdateWorkspaceInput {
    /// Creates a new builder-style object to manufacture [`UpdateWorkspaceInput`](crate::operation::update_workspace::UpdateWorkspaceInput).
    pub fn builder() -> crate::operation::update_workspace::builders::UpdateWorkspaceInputBuilder {
        crate::operation::update_workspace::builders::UpdateWorkspaceInputBuilder::default()
    }
}

/// A builder for [`UpdateWorkspaceInput`](crate::operation::update_workspace::UpdateWorkspaceInput).
#[derive(::std::clone::Clone, ::std::cmp::PartialEq, ::std::default::Default, ::std::fmt::Debug)]
#[non_exhaustive]
pub struct UpdateWorkspaceInputBuilder {
    pub(crate) org_id: ::std::option::Option<::std::string::String>,
    pub(crate) workspace_name: ::std::option::Option<::std::string::String>,
    pub(crate) workspace_admin_email: ::std::option::Option<::std::string::String>,
    pub(crate) mandatory_dimensions: ::std::option::Option<::std::vec::Vec::<::std::string::String>>,
    pub(crate) workspace_status: ::std::option::Option<crate::types::WorkspaceStatus>,
}
impl UpdateWorkspaceInputBuilder {
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
    pub fn workspace_name(mut self, input: impl ::std::convert::Into<::std::string::String>) -> Self {
        self.workspace_name = ::std::option::Option::Some(input.into());
        self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_workspace_name(mut self, input: ::std::option::Option<::std::string::String>) -> Self {
        self.workspace_name = input; self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_workspace_name(&self) -> &::std::option::Option<::std::string::String> {
        &self.workspace_name
    }
    #[allow(missing_docs)] // documentation missing in model
    /// This field is required.
    pub fn workspace_admin_email(mut self, input: impl ::std::convert::Into<::std::string::String>) -> Self {
        self.workspace_admin_email = ::std::option::Option::Some(input.into());
        self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_workspace_admin_email(mut self, input: ::std::option::Option<::std::string::String>) -> Self {
        self.workspace_admin_email = input; self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_workspace_admin_email(&self) -> &::std::option::Option<::std::string::String> {
        &self.workspace_admin_email
    }
    /// Appends an item to `mandatory_dimensions`.
    ///
    /// To override the contents of this collection use [`set_mandatory_dimensions`](Self::set_mandatory_dimensions).
    ///
    pub fn mandatory_dimensions(mut self, input: impl ::std::convert::Into<::std::string::String>) -> Self {
        let mut v = self.mandatory_dimensions.unwrap_or_default();
                        v.push(input.into());
                        self.mandatory_dimensions = ::std::option::Option::Some(v);
                        self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_mandatory_dimensions(mut self, input: ::std::option::Option<::std::vec::Vec::<::std::string::String>>) -> Self {
        self.mandatory_dimensions = input; self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_mandatory_dimensions(&self) -> &::std::option::Option<::std::vec::Vec::<::std::string::String>> {
        &self.mandatory_dimensions
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn workspace_status(mut self, input: crate::types::WorkspaceStatus) -> Self {
        self.workspace_status = ::std::option::Option::Some(input);
        self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_workspace_status(mut self, input: ::std::option::Option<crate::types::WorkspaceStatus>) -> Self {
        self.workspace_status = input; self
    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_workspace_status(&self) -> &::std::option::Option<crate::types::WorkspaceStatus> {
        &self.workspace_status
    }
    /// Consumes the builder and constructs a [`UpdateWorkspaceInput`](crate::operation::update_workspace::UpdateWorkspaceInput).
    pub fn build(self) -> ::std::result::Result<crate::operation::update_workspace::UpdateWorkspaceInput, ::aws_smithy_types::error::operation::BuildError> {
        ::std::result::Result::Ok(
            crate::operation::update_workspace::UpdateWorkspaceInput {
                org_id: self.org_id
                ,
                workspace_name: self.workspace_name
                ,
                workspace_admin_email: self.workspace_admin_email
                ,
                mandatory_dimensions: self.mandatory_dimensions
                ,
                workspace_status: self.workspace_status
                ,
            }
        )
    }
}

