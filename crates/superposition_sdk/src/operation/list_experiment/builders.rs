// Code generated by software.amazon.smithy.rust.codegen.smithy-rs. DO NOT EDIT.
pub use crate::operation::list_experiment::_list_experiment_output::ListExperimentOutputBuilder;

pub use crate::operation::list_experiment::_list_experiment_input::ListExperimentInputBuilder;

impl crate::operation::list_experiment::builders::ListExperimentInputBuilder {
                    /// Sends a request with this input using the given client.
                    pub async fn send_with(self, client: &crate::Client) -> ::std::result::Result<
                        crate::operation::list_experiment::ListExperimentOutput,
                        ::aws_smithy_runtime_api::client::result::SdkError<
                            crate::operation::list_experiment::ListExperimentError,
                            ::aws_smithy_runtime_api::client::orchestrator::HttpResponse
                        >
                    > {
                        let mut fluent_builder = client.list_experiment();
                        fluent_builder.inner = self;
                        fluent_builder.send().await
                    }
                }
/// Fluent builder constructing a request to `ListExperiment`.
/// 
#[derive(::std::clone::Clone, ::std::fmt::Debug)]
pub struct ListExperimentFluentBuilder {
                handle: ::std::sync::Arc<crate::client::Handle>,
                inner: crate::operation::list_experiment::builders::ListExperimentInputBuilder,
config_override: ::std::option::Option<crate::config::Builder>,
            }
impl
                crate::client::customize::internal::CustomizableSend<
                    crate::operation::list_experiment::ListExperimentOutput,
                    crate::operation::list_experiment::ListExperimentError,
                > for ListExperimentFluentBuilder
            {
                fn send(
                    self,
                    config_override: crate::config::Builder,
                ) -> crate::client::customize::internal::BoxFuture<
                    crate::client::customize::internal::SendResult<
                        crate::operation::list_experiment::ListExperimentOutput,
                        crate::operation::list_experiment::ListExperimentError,
                    >,
                > {
                    ::std::boxed::Box::pin(async move { self.config_override(config_override).send().await })
                }
            }
impl ListExperimentFluentBuilder {
    /// Creates a new `ListExperimentFluentBuilder`.
                    pub(crate) fn new(handle: ::std::sync::Arc<crate::client::Handle>) -> Self {
                        Self {
                            handle,
                            inner: ::std::default::Default::default(),
    config_override: ::std::option::Option::None,
                        }
                    }
    /// Access the ListExperiment as a reference.
                    pub fn as_input(&self) -> &crate::operation::list_experiment::builders::ListExperimentInputBuilder {
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
                    pub async fn send(self) -> ::std::result::Result<crate::operation::list_experiment::ListExperimentOutput, ::aws_smithy_runtime_api::client::result::SdkError<crate::operation::list_experiment::ListExperimentError, ::aws_smithy_runtime_api::client::orchestrator::HttpResponse>> {
                        let input = self.inner.build().map_err(::aws_smithy_runtime_api::client::result::SdkError::construction_failure)?;
                        let runtime_plugins = crate::operation::list_experiment::ListExperiment::operation_runtime_plugins(
                            self.handle.runtime_plugins.clone(),
                            &self.handle.conf,
                            self.config_override,
                        );
                        crate::operation::list_experiment::ListExperiment::orchestrate(&runtime_plugins, input).await
                    }
    
                    /// Consumes this builder, creating a customizable operation that can be modified before being sent.
                    pub fn customize(
                        self,
                    ) -> crate::client::customize::CustomizableOperation<crate::operation::list_experiment::ListExperimentOutput, crate::operation::list_experiment::ListExperimentError, Self> {
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
    #[allow(missing_docs)] // documentation missing in model
    pub fn all(mut self, input: bool) -> Self {
                    self.inner = self.inner.all(input);
                    self
                }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_all(mut self, input: ::std::option::Option<bool>) -> Self {
                    self.inner = self.inner.set_all(input);
                    self
                }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_all(&self) -> &::std::option::Option<bool> {
                    self.inner.get_all()
                }
    #[allow(missing_docs)] // documentation missing in model
    pub fn status(mut self, input: crate::types::ExperimentStatusType) -> Self {
                    self.inner = self.inner.status(input);
                    self
                }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_status(mut self, input: ::std::option::Option<crate::types::ExperimentStatusType>) -> Self {
                    self.inner = self.inner.set_status(input);
                    self
                }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_status(&self) -> &::std::option::Option<crate::types::ExperimentStatusType> {
                    self.inner.get_status()
                }
    #[allow(missing_docs)] // documentation missing in model
    pub fn from_date(mut self, input: ::aws_smithy_types::DateTime) -> Self {
                    self.inner = self.inner.from_date(input);
                    self
                }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_from_date(mut self, input: ::std::option::Option<::aws_smithy_types::DateTime>) -> Self {
                    self.inner = self.inner.set_from_date(input);
                    self
                }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_from_date(&self) -> &::std::option::Option<::aws_smithy_types::DateTime> {
                    self.inner.get_from_date()
                }
    #[allow(missing_docs)] // documentation missing in model
    pub fn to_date(mut self, input: ::aws_smithy_types::DateTime) -> Self {
                    self.inner = self.inner.to_date(input);
                    self
                }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_to_date(mut self, input: ::std::option::Option<::aws_smithy_types::DateTime>) -> Self {
                    self.inner = self.inner.set_to_date(input);
                    self
                }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_to_date(&self) -> &::std::option::Option<::aws_smithy_types::DateTime> {
                    self.inner.get_to_date()
                }
    #[allow(missing_docs)] // documentation missing in model
    pub fn experiment_name(mut self, input: impl ::std::convert::Into<::std::string::String>) -> Self {
                    self.inner = self.inner.experiment_name(input.into());
                    self
                }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_experiment_name(mut self, input: ::std::option::Option<::std::string::String>) -> Self {
                    self.inner = self.inner.set_experiment_name(input);
                    self
                }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_experiment_name(&self) -> &::std::option::Option<::std::string::String> {
                    self.inner.get_experiment_name()
                }
    #[allow(missing_docs)] // documentation missing in model
    pub fn experiment_ids(mut self, input: impl ::std::convert::Into<::std::string::String>) -> Self {
                    self.inner = self.inner.experiment_ids(input.into());
                    self
                }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_experiment_ids(mut self, input: ::std::option::Option<::std::string::String>) -> Self {
                    self.inner = self.inner.set_experiment_ids(input);
                    self
                }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_experiment_ids(&self) -> &::std::option::Option<::std::string::String> {
                    self.inner.get_experiment_ids()
                }
    #[allow(missing_docs)] // documentation missing in model
    pub fn experiment_group_ids(mut self, input: impl ::std::convert::Into<::std::string::String>) -> Self {
                    self.inner = self.inner.experiment_group_ids(input.into());
                    self
                }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_experiment_group_ids(mut self, input: ::std::option::Option<::std::string::String>) -> Self {
                    self.inner = self.inner.set_experiment_group_ids(input);
                    self
                }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_experiment_group_ids(&self) -> &::std::option::Option<::std::string::String> {
                    self.inner.get_experiment_group_ids()
                }
    #[allow(missing_docs)] // documentation missing in model
    pub fn created_by(mut self, input: impl ::std::convert::Into<::std::string::String>) -> Self {
                    self.inner = self.inner.created_by(input.into());
                    self
                }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_created_by(mut self, input: ::std::option::Option<::std::string::String>) -> Self {
                    self.inner = self.inner.set_created_by(input);
                    self
                }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_created_by(&self) -> &::std::option::Option<::std::string::String> {
                    self.inner.get_created_by()
                }
    #[allow(missing_docs)] // documentation missing in model
    pub fn sort_on(mut self, input: crate::types::ExperimentSortOn) -> Self {
                    self.inner = self.inner.sort_on(input);
                    self
                }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_sort_on(mut self, input: ::std::option::Option<crate::types::ExperimentSortOn>) -> Self {
                    self.inner = self.inner.set_sort_on(input);
                    self
                }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_sort_on(&self) -> &::std::option::Option<crate::types::ExperimentSortOn> {
                    self.inner.get_sort_on()
                }
    #[allow(missing_docs)] // documentation missing in model
    pub fn sort_by(mut self, input: crate::types::SortBy) -> Self {
                    self.inner = self.inner.sort_by(input);
                    self
                }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_sort_by(mut self, input: ::std::option::Option<crate::types::SortBy>) -> Self {
                    self.inner = self.inner.set_sort_by(input);
                    self
                }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_sort_by(&self) -> &::std::option::Option<crate::types::SortBy> {
                    self.inner.get_sort_by()
                }
}

