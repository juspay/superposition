// Code generated by software.amazon.smithy.rust.codegen.smithy-rs. DO NOT EDIT.
pub use crate::operation::delete_function::_delete_function_output::DeleteFunctionOutputBuilder;

pub use crate::operation::delete_function::_delete_function_input::DeleteFunctionInputBuilder;

impl crate::operation::delete_function::builders::DeleteFunctionInputBuilder {
                    /// Sends a request with this input using the given client.
                    pub async fn send_with(self, client: &crate::Client) -> ::std::result::Result<
                        crate::operation::delete_function::DeleteFunctionOutput,
                        ::aws_smithy_runtime_api::client::result::SdkError<
                            crate::operation::delete_function::DeleteFunctionError,
                            ::aws_smithy_runtime_api::client::orchestrator::HttpResponse
                        >
                    > {
                        let mut fluent_builder = client.delete_function();
                        fluent_builder.inner = self;
                        fluent_builder.send().await
                    }
                }
/// Fluent builder constructing a request to `DeleteFunction`.
/// 
#[derive(::std::clone::Clone, ::std::fmt::Debug)]
pub struct DeleteFunctionFluentBuilder {
                handle: ::std::sync::Arc<crate::client::Handle>,
                inner: crate::operation::delete_function::builders::DeleteFunctionInputBuilder,
config_override: ::std::option::Option<crate::config::Builder>,
            }
impl
                crate::client::customize::internal::CustomizableSend<
                    crate::operation::delete_function::DeleteFunctionOutput,
                    crate::operation::delete_function::DeleteFunctionError,
                > for DeleteFunctionFluentBuilder
            {
                fn send(
                    self,
                    config_override: crate::config::Builder,
                ) -> crate::client::customize::internal::BoxFuture<
                    crate::client::customize::internal::SendResult<
                        crate::operation::delete_function::DeleteFunctionOutput,
                        crate::operation::delete_function::DeleteFunctionError,
                    >,
                > {
                    ::std::boxed::Box::pin(async move { self.config_override(config_override).send().await })
                }
            }
impl DeleteFunctionFluentBuilder {
    /// Creates a new `DeleteFunctionFluentBuilder`.
                    pub(crate) fn new(handle: ::std::sync::Arc<crate::client::Handle>) -> Self {
                        Self {
                            handle,
                            inner: ::std::default::Default::default(),
    config_override: ::std::option::Option::None,
                        }
                    }
    /// Access the DeleteFunction as a reference.
                    pub fn as_input(&self) -> &crate::operation::delete_function::builders::DeleteFunctionInputBuilder {
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
                    pub async fn send(self) -> ::std::result::Result<crate::operation::delete_function::DeleteFunctionOutput, ::aws_smithy_runtime_api::client::result::SdkError<crate::operation::delete_function::DeleteFunctionError, ::aws_smithy_runtime_api::client::orchestrator::HttpResponse>> {
                        let input = self.inner.build().map_err(::aws_smithy_runtime_api::client::result::SdkError::construction_failure)?;
                        let runtime_plugins = crate::operation::delete_function::DeleteFunction::operation_runtime_plugins(
                            self.handle.runtime_plugins.clone(),
                            &self.handle.conf,
                            self.config_override,
                        );
                        crate::operation::delete_function::DeleteFunction::orchestrate(&runtime_plugins, input).await
                    }
    
                    /// Consumes this builder, creating a customizable operation that can be modified before being sent.
                    pub fn customize(
                        self,
                    ) -> crate::client::customize::CustomizableOperation<crate::operation::delete_function::DeleteFunctionOutput, crate::operation::delete_function::DeleteFunctionError, Self> {
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
    pub fn function_name(mut self, input: impl ::std::convert::Into<::std::string::String>) -> Self {
                    self.inner = self.inner.function_name(input.into());
                    self
                }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_function_name(mut self, input: ::std::option::Option<::std::string::String>) -> Self {
                    self.inner = self.inner.set_function_name(input);
                    self
                }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_function_name(&self) -> &::std::option::Option<::std::string::String> {
                    self.inner.get_function_name()
                }
}

