// Code generated by software.amazon.smithy.rust.codegen.smithy-rs. DO NOT EDIT.
pub use crate::operation::create_dimension::_create_dimension_output::CreateDimensionOutputBuilder;

pub use crate::operation::create_dimension::_create_dimension_input::CreateDimensionInputBuilder;

impl crate::operation::create_dimension::builders::CreateDimensionInputBuilder {
                    /// Sends a request with this input using the given client.
                    pub async fn send_with(self, client: &crate::Client) -> ::std::result::Result<
                        crate::operation::create_dimension::CreateDimensionOutput,
                        ::aws_smithy_runtime_api::client::result::SdkError<
                            crate::operation::create_dimension::CreateDimensionError,
                            ::aws_smithy_runtime_api::client::orchestrator::HttpResponse
                        >
                    > {
                        let mut fluent_builder = client.create_dimension();
                        fluent_builder.inner = self;
                        fluent_builder.send().await
                    }
                }
/// Fluent builder constructing a request to `CreateDimension`.
/// 
#[derive(::std::clone::Clone, ::std::fmt::Debug)]
pub struct CreateDimensionFluentBuilder {
                handle: ::std::sync::Arc<crate::client::Handle>,
                inner: crate::operation::create_dimension::builders::CreateDimensionInputBuilder,
config_override: ::std::option::Option<crate::config::Builder>,
            }
impl
                crate::client::customize::internal::CustomizableSend<
                    crate::operation::create_dimension::CreateDimensionOutput,
                    crate::operation::create_dimension::CreateDimensionError,
                > for CreateDimensionFluentBuilder
            {
                fn send(
                    self,
                    config_override: crate::config::Builder,
                ) -> crate::client::customize::internal::BoxFuture<
                    crate::client::customize::internal::SendResult<
                        crate::operation::create_dimension::CreateDimensionOutput,
                        crate::operation::create_dimension::CreateDimensionError,
                    >,
                > {
                    ::std::boxed::Box::pin(async move { self.config_override(config_override).send().await })
                }
            }
impl CreateDimensionFluentBuilder {
    /// Creates a new `CreateDimensionFluentBuilder`.
                    pub(crate) fn new(handle: ::std::sync::Arc<crate::client::Handle>) -> Self {
                        Self {
                            handle,
                            inner: ::std::default::Default::default(),
    config_override: ::std::option::Option::None,
                        }
                    }
    /// Access the CreateDimension as a reference.
                    pub fn as_input(&self) -> &crate::operation::create_dimension::builders::CreateDimensionInputBuilder {
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
                    pub async fn send(self) -> ::std::result::Result<crate::operation::create_dimension::CreateDimensionOutput, ::aws_smithy_runtime_api::client::result::SdkError<crate::operation::create_dimension::CreateDimensionError, ::aws_smithy_runtime_api::client::orchestrator::HttpResponse>> {
                        let input = self.inner.build().map_err(::aws_smithy_runtime_api::client::result::SdkError::construction_failure)?;
                        let runtime_plugins = crate::operation::create_dimension::CreateDimension::operation_runtime_plugins(
                            self.handle.runtime_plugins.clone(),
                            &self.handle.conf,
                            self.config_override,
                        );
                        crate::operation::create_dimension::CreateDimension::orchestrate(&runtime_plugins, input).await
                    }
    
                    /// Consumes this builder, creating a customizable operation that can be modified before being sent.
                    pub fn customize(
                        self,
                    ) -> crate::client::customize::CustomizableOperation<crate::operation::create_dimension::CreateDimensionOutput, crate::operation::create_dimension::CreateDimensionError, Self> {
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
    pub fn dimension(mut self, input: impl ::std::convert::Into<::std::string::String>) -> Self {
                    self.inner = self.inner.dimension(input.into());
                    self
                }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_dimension(mut self, input: ::std::option::Option<::std::string::String>) -> Self {
                    self.inner = self.inner.set_dimension(input);
                    self
                }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_dimension(&self) -> &::std::option::Option<::std::string::String> {
                    self.inner.get_dimension()
                }
    #[allow(missing_docs)] // documentation missing in model
    pub fn position(mut self, input: i32) -> Self {
                    self.inner = self.inner.position(input);
                    self
                }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_position(mut self, input: ::std::option::Option<i32>) -> Self {
                    self.inner = self.inner.set_position(input);
                    self
                }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_position(&self) -> &::std::option::Option<i32> {
                    self.inner.get_position()
                }
    #[allow(missing_docs)] // documentation missing in model
    pub fn schema(mut self, input: ::aws_smithy_types::Document) -> Self {
                    self.inner = self.inner.schema(input);
                    self
                }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_schema(mut self, input: ::std::option::Option<::aws_smithy_types::Document>) -> Self {
                    self.inner = self.inner.set_schema(input);
                    self
                }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_schema(&self) -> &::std::option::Option<::aws_smithy_types::Document> {
                    self.inner.get_schema()
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
    /// 
    /// Appends an item to `dependencies`.
    /// 
    /// To override the contents of this collection use [`set_dependencies`](Self::set_dependencies).
    /// 
    #[allow(missing_docs)] // documentation missing in model
    pub fn dependencies(mut self, input: impl ::std::convert::Into<::std::string::String>) -> Self {
                        self.inner = self.inner.dependencies(input.into());
                        self
                    }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_dependencies(mut self, input: ::std::option::Option<::std::vec::Vec::<::std::string::String>>) -> Self {
                    self.inner = self.inner.set_dependencies(input);
                    self
                }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_dependencies(&self) -> &::std::option::Option<::std::vec::Vec::<::std::string::String>> {
                    self.inner.get_dependencies()
                }
    #[allow(missing_docs)] // documentation missing in model
    pub fn description(mut self, input: impl ::std::convert::Into<::std::string::String>) -> Self {
                    self.inner = self.inner.description(input.into());
                    self
                }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_description(mut self, input: ::std::option::Option<::std::string::String>) -> Self {
                    self.inner = self.inner.set_description(input);
                    self
                }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_description(&self) -> &::std::option::Option<::std::string::String> {
                    self.inner.get_description()
                }
    #[allow(missing_docs)] // documentation missing in model
    pub fn change_reason(mut self, input: impl ::std::convert::Into<::std::string::String>) -> Self {
                    self.inner = self.inner.change_reason(input.into());
                    self
                }
    #[allow(missing_docs)] // documentation missing in model
    pub fn set_change_reason(mut self, input: ::std::option::Option<::std::string::String>) -> Self {
                    self.inner = self.inner.set_change_reason(input);
                    self
                }
    #[allow(missing_docs)] // documentation missing in model
    pub fn get_change_reason(&self) -> &::std::option::Option<::std::string::String> {
                    self.inner.get_change_reason()
                }
}

