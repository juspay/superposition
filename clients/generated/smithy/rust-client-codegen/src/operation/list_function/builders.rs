// Code generated by software.amazon.smithy.rust.codegen.smithy-rs. DO NOT EDIT.
pub use crate::operation::list_function::_list_function_output::ListFunctionOutputBuilder;

pub use crate::operation::list_function::_list_function_input::ListFunctionInputBuilder;

impl crate::operation::list_function::builders::ListFunctionInputBuilder {
                    /// Sends a request with this input using the given client.
                    pub async fn send_with(self, client: &crate::Client) -> ::std::result::Result<
                        crate::operation::list_function::ListFunctionOutput,
                        ::aws_smithy_runtime_api::client::result::SdkError<
                            crate::operation::list_function::ListFunctionError,
                            ::aws_smithy_runtime_api::client::orchestrator::HttpResponse
                        >
                    > {
                        let mut fluent_builder = client.list_function();
                        fluent_builder.inner = self;
                        fluent_builder.send().await
                    }
                }
/// Fluent builder constructing a request to `ListFunction`.
/// 
#[derive(::std::clone::Clone, ::std::fmt::Debug)]
pub struct ListFunctionFluentBuilder {
                handle: ::std::sync::Arc<crate::client::Handle>,
                inner: crate::operation::list_function::builders::ListFunctionInputBuilder,
config_override: ::std::option::Option<crate::config::Builder>,
            }
impl
                crate::client::customize::internal::CustomizableSend<
                    crate::operation::list_function::ListFunctionOutput,
                    crate::operation::list_function::ListFunctionError,
                > for ListFunctionFluentBuilder
            {
                fn send(
                    self,
                    config_override: crate::config::Builder,
                ) -> crate::client::customize::internal::BoxFuture<
                    crate::client::customize::internal::SendResult<
                        crate::operation::list_function::ListFunctionOutput,
                        crate::operation::list_function::ListFunctionError,
                    >,
                > {
                    ::std::boxed::Box::pin(async move { self.config_override(config_override).send().await })
                }
            }
impl ListFunctionFluentBuilder {
    /// Creates a new `ListFunctionFluentBuilder`.
                    pub(crate) fn new(handle: ::std::sync::Arc<crate::client::Handle>) -> Self {
                        Self {
                            handle,
                            inner: ::std::default::Default::default(),
    config_override: ::std::option::Option::None,
                        }
                    }
    /// Access the ListFunction as a reference.
                    pub fn as_input(&self) -> &crate::operation::list_function::builders::ListFunctionInputBuilder {
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
                    pub async fn send(self) -> ::std::result::Result<crate::operation::list_function::ListFunctionOutput, ::aws_smithy_runtime_api::client::result::SdkError<crate::operation::list_function::ListFunctionError, ::aws_smithy_runtime_api::client::orchestrator::HttpResponse>> {
                        let input = self.inner.build().map_err(::aws_smithy_runtime_api::client::result::SdkError::construction_failure)?;
                        let runtime_plugins = crate::operation::list_function::ListFunction::operation_runtime_plugins(
                            self.handle.runtime_plugins.clone(),
                            &self.handle.conf,
                            self.config_override,
                        );
                        crate::operation::list_function::ListFunction::orchestrate(&runtime_plugins, input).await
                    }
    
                    /// Consumes this builder, creating a customizable operation that can be modified before being sent.
                    pub fn customize(
                        self,
                    ) -> crate::client::customize::CustomizableOperation<crate::operation::list_function::ListFunctionOutput, crate::operation::list_function::ListFunctionError, Self> {
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
    pub fn count(mut self, input: i32) -> Self {
                    self.inner = self.inner.count(input);
                    self
                }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_count(mut self, input: ::std::option::Option<i32>) -> Self {
                    self.inner = self.inner.set_count(input);
                    self
                }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_count(&self) -> &::std::option::Option<i32> {
                    self.inner.get_count()
                }
    #[allow(missing_docs)] // documentation missing in model
    pub fn page(mut self, input: i32) -> Self {
                    self.inner = self.inner.page(input);
                    self
                }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_page(mut self, input: ::std::option::Option<i32>) -> Self {
                    self.inner = self.inner.set_page(input);
                    self
                }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_page(&self) -> &::std::option::Option<i32> {
                    self.inner.get_page()
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
}

