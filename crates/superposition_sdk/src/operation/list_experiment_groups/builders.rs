// Code generated by software.amazon.smithy.rust.codegen.smithy-rs. DO NOT EDIT.
pub use crate::operation::list_experiment_groups::_list_experiment_groups_output::ListExperimentGroupsOutputBuilder;

pub use crate::operation::list_experiment_groups::_list_experiment_groups_input::ListExperimentGroupsInputBuilder;

impl crate::operation::list_experiment_groups::builders::ListExperimentGroupsInputBuilder {
                    /// Sends a request with this input using the given client.
                    pub async fn send_with(self, client: &crate::Client) -> ::std::result::Result<
                        crate::operation::list_experiment_groups::ListExperimentGroupsOutput,
                        ::aws_smithy_runtime_api::client::result::SdkError<
                            crate::operation::list_experiment_groups::ListExperimentGroupsError,
                            ::aws_smithy_runtime_api::client::orchestrator::HttpResponse
                        >
                    > {
                        let mut fluent_builder = client.list_experiment_groups();
                        fluent_builder.inner = self;
                        fluent_builder.send().await
                    }
                }
/// Fluent builder constructing a request to `ListExperimentGroups`.
/// 
/// Lists experiment groups, with support for filtering and pagination.
#[derive(::std::clone::Clone, ::std::fmt::Debug)]
pub struct ListExperimentGroupsFluentBuilder {
                handle: ::std::sync::Arc<crate::client::Handle>,
                inner: crate::operation::list_experiment_groups::builders::ListExperimentGroupsInputBuilder,
config_override: ::std::option::Option<crate::config::Builder>,
            }
impl
                crate::client::customize::internal::CustomizableSend<
                    crate::operation::list_experiment_groups::ListExperimentGroupsOutput,
                    crate::operation::list_experiment_groups::ListExperimentGroupsError,
                > for ListExperimentGroupsFluentBuilder
            {
                fn send(
                    self,
                    config_override: crate::config::Builder,
                ) -> crate::client::customize::internal::BoxFuture<
                    crate::client::customize::internal::SendResult<
                        crate::operation::list_experiment_groups::ListExperimentGroupsOutput,
                        crate::operation::list_experiment_groups::ListExperimentGroupsError,
                    >,
                > {
                    ::std::boxed::Box::pin(async move { self.config_override(config_override).send().await })
                }
            }
impl ListExperimentGroupsFluentBuilder {
    /// Creates a new `ListExperimentGroupsFluentBuilder`.
                    pub(crate) fn new(handle: ::std::sync::Arc<crate::client::Handle>) -> Self {
                        Self {
                            handle,
                            inner: ::std::default::Default::default(),
    config_override: ::std::option::Option::None,
                        }
                    }
    /// Access the ListExperimentGroups as a reference.
                    pub fn as_input(&self) -> &crate::operation::list_experiment_groups::builders::ListExperimentGroupsInputBuilder {
                        &self.inner
                    }
    /// Sends the request and returns the response.
                    ///
                    /// If an error occurs, an `SdkError` will be returned with additional details that
                    /// can be matched against.
                    ///
                    /// By default, any retryable failures will be retried twice. Retry behavior
                    /// is configurable with the [RetryConfig](aws_smithy_types::retry::RetryConfig), which can be
                    /// set when configuring the client.
                    pub async fn send(self) -> ::std::result::Result<crate::operation::list_experiment_groups::ListExperimentGroupsOutput, ::aws_smithy_runtime_api::client::result::SdkError<crate::operation::list_experiment_groups::ListExperimentGroupsError, ::aws_smithy_runtime_api::client::orchestrator::HttpResponse>> {
                        let input = self.inner.build().map_err(::aws_smithy_runtime_api::client::result::SdkError::construction_failure)?;
                        let runtime_plugins = crate::operation::list_experiment_groups::ListExperimentGroups::operation_runtime_plugins(
                            self.handle.runtime_plugins.clone(),
                            &self.handle.conf,
                            self.config_override,
                        );
                        crate::operation::list_experiment_groups::ListExperimentGroups::orchestrate(&runtime_plugins, input).await
                    }
    
                    /// Consumes this builder, creating a customizable operation that can be modified before being sent.
                    pub fn customize(
                        self,
                    ) -> crate::client::customize::CustomizableOperation<crate::operation::list_experiment_groups::ListExperimentGroupsOutput, crate::operation::list_experiment_groups::ListExperimentGroupsError, Self> {
                        crate::client::customize::CustomizableOperation::new(self)
                    }
    pub(crate) fn config_override(
                            mut self,
                            config_override: impl ::std::convert::Into<crate::config::Builder>,
                        ) -> Self {
                            self.set_config_override(::std::option::Option::Some(config_override.into()));
                            self
                        }
    
                        pub(crate) fn set_config_override(
                            &mut self,
                            config_override: ::std::option::Option<crate::config::Builder>,
                        ) -> &mut Self {
                            self.config_override = config_override;
                            self
                        }
    #[allow(missing_docs)] // documentation missing in model
    pub fn workspace_id(mut self, input: impl ::std::convert::Into<::std::string::String>) -> Self {
                    self.inner = self.inner.workspace_id(input.into());
                    self
                }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_workspace_id(mut self, input: ::std::option::Option<::std::string::String>) -> Self {
                    self.inner = self.inner.set_workspace_id(input);
                    self
                }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_workspace_id(&self) -> &::std::option::Option<::std::string::String> {
                    self.inner.get_workspace_id()
                }
    #[allow(missing_docs)] // documentation missing in model
    pub fn org_id(mut self, input: impl ::std::convert::Into<::std::string::String>) -> Self {
                    self.inner = self.inner.org_id(input.into());
                    self
                }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_org_id(mut self, input: ::std::option::Option<::std::string::String>) -> Self {
                    self.inner = self.inner.set_org_id(input);
                    self
                }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_org_id(&self) -> &::std::option::Option<::std::string::String> {
                    self.inner.get_org_id()
                }
    #[allow(missing_docs)] // documentation missing in model
    pub fn page(mut self, input: i64) -> Self {
                    self.inner = self.inner.page(input);
                    self
                }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_page(mut self, input: ::std::option::Option<i64>) -> Self {
                    self.inner = self.inner.set_page(input);
                    self
                }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_page(&self) -> &::std::option::Option<i64> {
                    self.inner.get_page()
                }
    #[allow(missing_docs)] // documentation missing in model
    pub fn count(mut self, input: i64) -> Self {
                    self.inner = self.inner.count(input);
                    self
                }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_count(mut self, input: ::std::option::Option<i64>) -> Self {
                    self.inner = self.inner.set_count(input);
                    self
                }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_count(&self) -> &::std::option::Option<i64> {
                    self.inner.get_count()
                }
    /// Filter by experiment group name (exact match or substring, depending on backend implementation).
    pub fn name(mut self, input: impl ::std::convert::Into<::std::string::String>) -> Self {
                    self.inner = self.inner.name(input.into());
                    self
                }
    /// Filter by experiment group name (exact match or substring, depending on backend implementation).
    pub fn set_name(mut self, input: ::std::option::Option<::std::string::String>) -> Self {
                    self.inner = self.inner.set_name(input);
                    self
                }
    /// Filter by experiment group name (exact match or substring, depending on backend implementation).
    pub fn get_name(&self) -> &::std::option::Option<::std::string::String> {
                    self.inner.get_name()
                }
    /// Filter by the user who created the experiment group.
    pub fn created_by(mut self, input: impl ::std::convert::Into<::std::string::String>) -> Self {
                    self.inner = self.inner.created_by(input.into());
                    self
                }
    /// Filter by the user who created the experiment group.
    pub fn set_created_by(mut self, input: ::std::option::Option<::std::string::String>) -> Self {
                    self.inner = self.inner.set_created_by(input);
                    self
                }
    /// Filter by the user who created the experiment group.
    pub fn get_created_by(&self) -> &::std::option::Option<::std::string::String> {
                    self.inner.get_created_by()
                }
    /// Filter by the user who last modified the experiment group.
    pub fn last_modified_by(mut self, input: impl ::std::convert::Into<::std::string::String>) -> Self {
                    self.inner = self.inner.last_modified_by(input.into());
                    self
                }
    /// Filter by the user who last modified the experiment group.
    pub fn set_last_modified_by(mut self, input: ::std::option::Option<::std::string::String>) -> Self {
                    self.inner = self.inner.set_last_modified_by(input);
                    self
                }
    /// Filter by the user who last modified the experiment group.
    pub fn get_last_modified_by(&self) -> &::std::option::Option<::std::string::String> {
                    self.inner.get_last_modified_by()
                }
    /// Field to sort the results by.
    pub fn sort_on(mut self, input: crate::types::ExperimentGroupSortOn) -> Self {
                    self.inner = self.inner.sort_on(input);
                    self
                }
    /// Field to sort the results by.
    pub fn set_sort_on(mut self, input: ::std::option::Option<crate::types::ExperimentGroupSortOn>) -> Self {
                    self.inner = self.inner.set_sort_on(input);
                    self
                }
    /// Field to sort the results by.
    pub fn get_sort_on(&self) -> &::std::option::Option<crate::types::ExperimentGroupSortOn> {
                    self.inner.get_sort_on()
                }
    /// Sort order (ascending or descending).
    pub fn sort_by(mut self, input: crate::types::SortBy) -> Self {
                    self.inner = self.inner.sort_by(input);
                    self
                }
    /// Sort order (ascending or descending).
    pub fn set_sort_by(mut self, input: ::std::option::Option<crate::types::SortBy>) -> Self {
                    self.inner = self.inner.set_sort_by(input);
                    self
                }
    /// Sort order (ascending or descending).
    pub fn get_sort_by(&self) -> &::std::option::Option<crate::types::SortBy> {
                    self.inner.get_sort_by()
                }
    /// If true, returns all experiment groups, ignoring pagination parameters page and count.
    pub fn all(mut self, input: bool) -> Self {
                    self.inner = self.inner.all(input);
                    self
                }
    /// If true, returns all experiment groups, ignoring pagination parameters page and count.
    pub fn set_all(mut self, input: ::std::option::Option<bool>) -> Self {
                    self.inner = self.inner.set_all(input);
                    self
                }
    /// If true, returns all experiment groups, ignoring pagination parameters page and count.
    pub fn get_all(&self) -> &::std::option::Option<bool> {
                    self.inner.get_all()
                }
    /// Filter by the type of group (USER_CREATED or SYSTEM_GENERATED).
    pub fn group_type(mut self, input: crate::types::GroupType) -> Self {
                    self.inner = self.inner.group_type(input);
                    self
                }
    /// Filter by the type of group (USER_CREATED or SYSTEM_GENERATED).
    pub fn set_group_type(mut self, input: ::std::option::Option<crate::types::GroupType>) -> Self {
                    self.inner = self.inner.set_group_type(input);
                    self
                }
    /// Filter by the type of group (USER_CREATED or SYSTEM_GENERATED).
    pub fn get_group_type(&self) -> &::std::option::Option<crate::types::GroupType> {
                    self.inner.get_group_type()
                }
}

