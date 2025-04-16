// Code generated by software.amazon.smithy.rust.codegen.smithy-rs. DO NOT EDIT.
/// Orchestration and serialization glue logic for `GetTypeTemplatesList`.
#[derive(::std::clone::Clone, ::std::default::Default, ::std::fmt::Debug)]
#[non_exhaustive]
pub struct GetTypeTemplatesList;
impl GetTypeTemplatesList {
    /// Creates a new `GetTypeTemplatesList`
    pub fn new() -> Self {
        Self
    }
    pub(crate) async fn orchestrate(
                        runtime_plugins: &::aws_smithy_runtime_api::client::runtime_plugin::RuntimePlugins,
                        input: crate::operation::get_type_templates_list::GetTypeTemplatesListInput,
                    ) -> ::std::result::Result<crate::operation::get_type_templates_list::GetTypeTemplatesListOutput, ::aws_smithy_runtime_api::client::result::SdkError<crate::operation::get_type_templates_list::GetTypeTemplatesListError, ::aws_smithy_runtime_api::client::orchestrator::HttpResponse>> {
                        let map_err = |err: ::aws_smithy_runtime_api::client::result::SdkError<::aws_smithy_runtime_api::client::interceptors::context::Error, ::aws_smithy_runtime_api::client::orchestrator::HttpResponse>| {
                            err.map_service_error(|err| {
                                err.downcast::<crate::operation::get_type_templates_list::GetTypeTemplatesListError>().expect("correct error type")
                            })
                        };
                        let context = Self::orchestrate_with_stop_point(runtime_plugins, input, ::aws_smithy_runtime::client::orchestrator::StopPoint::None)
                            .await
                            .map_err(map_err)?;
                        let output = context.finalize().map_err(map_err)?;
                        ::std::result::Result::Ok(output.downcast::<crate::operation::get_type_templates_list::GetTypeTemplatesListOutput>().expect("correct output type"))
                    }
    
                    pub(crate) async fn orchestrate_with_stop_point(
                        runtime_plugins: &::aws_smithy_runtime_api::client::runtime_plugin::RuntimePlugins,
                        input: crate::operation::get_type_templates_list::GetTypeTemplatesListInput,
                        stop_point: ::aws_smithy_runtime::client::orchestrator::StopPoint,
                    ) -> ::std::result::Result<::aws_smithy_runtime_api::client::interceptors::context::InterceptorContext, ::aws_smithy_runtime_api::client::result::SdkError<::aws_smithy_runtime_api::client::interceptors::context::Error, ::aws_smithy_runtime_api::client::orchestrator::HttpResponse>> {
                        let input = ::aws_smithy_runtime_api::client::interceptors::context::Input::erase(input);
                        use ::tracing::Instrument;
                        ::aws_smithy_runtime::client::orchestrator::invoke_with_stop_point(
                            "Superposition",
                            "GetTypeTemplatesList",
                            input,
                            runtime_plugins,
                            stop_point
                        )
                        // Create a parent span for the entire operation. Includes a random, internal-only,
                        // seven-digit ID for the operation orchestration so that it can be correlated in the logs.
                        .instrument(::tracing::debug_span!(
                                "Superposition.GetTypeTemplatesList",
                                "rpc.service" = "Superposition",
                                "rpc.method" = "GetTypeTemplatesList",
                                "sdk_invocation_id" = ::fastrand::u32(1_000_000..10_000_000),
                                
                            ))
                        .await
                    }
    
                    pub(crate) fn operation_runtime_plugins(
                        client_runtime_plugins: ::aws_smithy_runtime_api::client::runtime_plugin::RuntimePlugins,
                        client_config: &crate::config::Config,
                        config_override: ::std::option::Option<crate::config::Builder>,
                    ) -> ::aws_smithy_runtime_api::client::runtime_plugin::RuntimePlugins {
                        let mut runtime_plugins = client_runtime_plugins.with_operation_plugin(Self::new());
                        runtime_plugins = runtime_plugins
                                        .with_client_plugin(crate::auth_plugin::DefaultAuthOptionsPlugin::new(vec![::aws_smithy_runtime_api::client::auth::http::HTTP_BEARER_AUTH_SCHEME_ID]));
                        if let ::std::option::Option::Some(config_override) = config_override {
                            for plugin in config_override.runtime_plugins.iter().cloned() {
                                runtime_plugins = runtime_plugins.with_operation_plugin(plugin);
                            }
                            runtime_plugins = runtime_plugins.with_operation_plugin(
                                crate::config::ConfigOverrideRuntimePlugin::new(config_override, client_config.config.clone(), &client_config.runtime_components)
                            );
                        }
                        runtime_plugins
                    }
}
impl ::aws_smithy_runtime_api::client::runtime_plugin::RuntimePlugin for GetTypeTemplatesList {
                fn config(&self) -> ::std::option::Option<::aws_smithy_types::config_bag::FrozenLayer> {
                    let mut cfg = ::aws_smithy_types::config_bag::Layer::new("GetTypeTemplatesList");

                    cfg.store_put(::aws_smithy_runtime_api::client::ser_de::SharedRequestSerializer::new(GetTypeTemplatesListRequestSerializer));
                    cfg.store_put(::aws_smithy_runtime_api::client::ser_de::SharedResponseDeserializer::new(GetTypeTemplatesListResponseDeserializer));

                    
                    cfg.store_put(::aws_smithy_runtime_api::client::auth::AuthSchemeOptionResolverParams::new(::aws_smithy_runtime_api::client::auth::static_resolver::StaticAuthSchemeOptionResolverParams::new()));

                    cfg.store_put(::aws_smithy_runtime_api::client::orchestrator::Metadata::new(
                            "GetTypeTemplatesList",
                            "Superposition",
                        ));

                    ::std::option::Option::Some(cfg.freeze())
                }

                fn runtime_components(&self, _: &::aws_smithy_runtime_api::client::runtime_components::RuntimeComponentsBuilder) -> ::std::borrow::Cow<'_, ::aws_smithy_runtime_api::client::runtime_components::RuntimeComponentsBuilder> {
                    #[allow(unused_mut)]
                    let mut rcb = ::aws_smithy_runtime_api::client::runtime_components::RuntimeComponentsBuilder::new("GetTypeTemplatesList")
                            .with_interceptor(::aws_smithy_runtime::client::stalled_stream_protection::StalledStreamProtectionInterceptor::default())
.with_interceptor(GetTypeTemplatesListEndpointParamsInterceptor)
                            .with_retry_classifier(::aws_smithy_runtime::client::retries::classifiers::TransientErrorClassifier::<crate::operation::get_type_templates_list::GetTypeTemplatesListError>::new())
.with_retry_classifier(::aws_smithy_runtime::client::retries::classifiers::ModeledAsRetryableClassifier::<crate::operation::get_type_templates_list::GetTypeTemplatesListError>::new());

                    ::std::borrow::Cow::Owned(rcb)
                }
            }

            
#[derive(Debug)]
            struct GetTypeTemplatesListResponseDeserializer;
            impl ::aws_smithy_runtime_api::client::ser_de::DeserializeResponse for GetTypeTemplatesListResponseDeserializer {
                

                fn deserialize_nonstreaming(&self, response: &::aws_smithy_runtime_api::client::orchestrator::HttpResponse) -> ::aws_smithy_runtime_api::client::interceptors::context::OutputOrError {
                    let (success, status) = (response.status().is_success(), response.status().as_u16());
            let headers = response.headers();
            let body = response.body().bytes().expect("body loaded");
            #[allow(unused_mut)]
            let mut force_error = false;
            
            let parse_result = if !success && status != 200 || force_error {
                crate::protocol_serde::shape_get_type_templates_list::de_get_type_templates_list_http_error(status, headers, body)
            } else {
                crate::protocol_serde::shape_get_type_templates_list::de_get_type_templates_list_http_response(status, headers, body)
            };
            crate::protocol_serde::type_erase_result(parse_result)
                }
            }
#[derive(Debug)]
            struct GetTypeTemplatesListRequestSerializer;
            impl ::aws_smithy_runtime_api::client::ser_de::SerializeRequest for GetTypeTemplatesListRequestSerializer {
                #[allow(unused_mut, clippy::let_and_return, clippy::needless_borrow, clippy::useless_conversion)]
                fn serialize_input(&self, input: ::aws_smithy_runtime_api::client::interceptors::context::Input, _cfg: &mut ::aws_smithy_types::config_bag::ConfigBag) -> ::std::result::Result<::aws_smithy_runtime_api::client::orchestrator::HttpRequest, ::aws_smithy_runtime_api::box_error::BoxError> {
                    let input = input.downcast::<crate::operation::get_type_templates_list::GetTypeTemplatesListInput>().expect("correct type");
                    let _header_serialization_settings = _cfg.load::<crate::serialization_settings::HeaderSerializationSettings>().cloned().unwrap_or_default();
                    let mut request_builder = {
                        fn uri_base(_input: &crate::operation::get_type_templates_list::GetTypeTemplatesListInput, output: &mut ::std::string::String) -> ::std::result::Result<(), ::aws_smithy_types::error::operation::BuildError> {
    use ::std::fmt::Write as _;
    ::std::write!(output, "/types").expect("formatting should succeed");
    ::std::result::Result::Ok(())
}
fn uri_query(_input: &crate::operation::get_type_templates_list::GetTypeTemplatesListInput, mut output: &mut ::std::string::String) -> ::std::result::Result<(), ::aws_smithy_types::error::operation::BuildError> {
    let mut query = ::aws_smithy_http::query::Writer::new(output);
    if let ::std::option::Option::Some(inner_1) = &_input.count {
         {
            query.push_kv("count", ::aws_smithy_types::primitive::Encoder::from(*inner_1).encode());
        }
    }
    if let ::std::option::Option::Some(inner_2) = &_input.page {
         {
            query.push_kv("page", ::aws_smithy_types::primitive::Encoder::from(*inner_2).encode());
        }
    }
    ::std::result::Result::Ok(())
}
#[allow(clippy::unnecessary_wraps)]
fn update_http_builder(
                input: &crate::operation::get_type_templates_list::GetTypeTemplatesListInput,
                builder: ::http::request::Builder
            ) -> ::std::result::Result<::http::request::Builder, ::aws_smithy_types::error::operation::BuildError> {
    let mut uri = ::std::string::String::new();
    uri_base(input, &mut uri)?;
    uri_query(input, &mut uri)?;
    let builder = crate::protocol_serde::shape_get_type_templates_list::ser_get_type_templates_list_headers(input, builder)?;
    ::std::result::Result::Ok(builder.method("GET").uri(uri))
}
let mut builder = update_http_builder(&input, ::http::request::Builder::new())?;
builder
                    };
                    let body = ::aws_smithy_types::body::SdkBody::from("");
                    
                    ::std::result::Result::Ok(request_builder.body(body).expect("valid request").try_into().unwrap())
                }
            }
#[derive(Debug)]
            struct GetTypeTemplatesListEndpointParamsInterceptor;

            impl ::aws_smithy_runtime_api::client::interceptors::Intercept for GetTypeTemplatesListEndpointParamsInterceptor {
                fn name(&self) -> &'static str {
                    "GetTypeTemplatesListEndpointParamsInterceptor"
                }

                fn read_before_execution(
                    &self,
                    context: &::aws_smithy_runtime_api::client::interceptors::context::BeforeSerializationInterceptorContextRef<'_, ::aws_smithy_runtime_api::client::interceptors::context::Input, ::aws_smithy_runtime_api::client::interceptors::context::Output, ::aws_smithy_runtime_api::client::interceptors::context::Error>,
                    cfg: &mut ::aws_smithy_types::config_bag::ConfigBag,
                ) -> ::std::result::Result<(), ::aws_smithy_runtime_api::box_error::BoxError> {
                    let _input = context.input()
                        .downcast_ref::<GetTypeTemplatesListInput>()
                        .ok_or("failed to downcast to GetTypeTemplatesListInput")?;

                    

                    let params = crate::config::endpoint::Params::builder()
                        
                        .build()
                        .map_err(|err| ::aws_smithy_runtime_api::client::interceptors::error::ContextAttachedError::new("endpoint params could not be built", err))?;
                    cfg.interceptor_state().store_put(::aws_smithy_runtime_api::client::endpoint::EndpointResolverParams::new(params));
                    ::std::result::Result::Ok(())
                }
            }

            // The get_* functions below are generated from JMESPath expressions in the
            // operationContextParams trait. They target the operation's input shape.

            

/// Error type for the `GetTypeTemplatesListError` operation.
#[non_exhaustive]
#[derive(::std::fmt::Debug)]
pub enum GetTypeTemplatesListError {
    #[allow(missing_docs)] // documentation missing in model
    InternalServerError(crate::types::error::InternalServerError),
    /// An unexpected error occurred (e.g., invalid JSON returned by the service or an unknown error code).
                    #[deprecated(note = "Matching `Unhandled` directly is not forwards compatible. Instead, match using a \
    variable wildcard pattern and check `.code()`:
     \
    &nbsp;&nbsp;&nbsp;`err if err.code() == Some(\"SpecificExceptionCode\") => { /* handle the error */ }`
     \
    See [`ProvideErrorMetadata`](#impl-ProvideErrorMetadata-for-GetTypeTemplatesListError) for what information is available for the error.")]
                    Unhandled(crate::error::sealed_unhandled::Unhandled),
}
impl GetTypeTemplatesListError {
    /// Creates the `GetTypeTemplatesListError::Unhandled` variant from any error type.
                    pub fn unhandled(err: impl ::std::convert::Into<::std::boxed::Box<dyn ::std::error::Error + ::std::marker::Send + ::std::marker::Sync + 'static>>) -> Self {
                        Self::Unhandled(crate::error::sealed_unhandled::Unhandled { source: err.into(), meta: ::std::default::Default::default() })
                    }
    
                    /// Creates the `GetTypeTemplatesListError::Unhandled` variant from an [`ErrorMetadata`](::aws_smithy_types::error::ErrorMetadata).
                    pub fn generic(err: ::aws_smithy_types::error::ErrorMetadata) -> Self {
                        Self::Unhandled(crate::error::sealed_unhandled::Unhandled { source: err.clone().into(), meta: err })
                    }
    /// 
    /// Returns error metadata, which includes the error code, message,
    /// request ID, and potentially additional information.
    /// 
    pub fn meta(&self) -> &::aws_smithy_types::error::ErrorMetadata {
        match self {
            Self::InternalServerError(e) => ::aws_smithy_types::error::metadata::ProvideErrorMetadata::meta(e),
            Self::Unhandled(e) => &e.meta,
        }
    }
    /// Returns `true` if the error kind is `GetTypeTemplatesListError::InternalServerError`.
    pub fn is_internal_server_error(&self) -> bool {
        matches!(self, Self::InternalServerError(_))
    }
}
impl ::std::error::Error for GetTypeTemplatesListError {
    fn source(&self) -> ::std::option::Option<&(dyn ::std::error::Error + 'static)> {
        match self {
            Self::InternalServerError(_inner) =>
            ::std::option::Option::Some(_inner)
            ,
            Self::Unhandled(_inner) => {
                ::std::option::Option::Some(&*_inner.source)
            }
        }
    }
}
impl ::std::fmt::Display for GetTypeTemplatesListError {
    fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
        match self {
            Self::InternalServerError(_inner) =>
            _inner.fmt(f)
            ,
            Self::Unhandled(_inner) => {
                if let ::std::option::Option::Some(code) = ::aws_smithy_types::error::metadata::ProvideErrorMetadata::code(self) {
                                                        write!(f, "unhandled error ({code})")
                                                    } else {
                                                        f.write_str("unhandled error")
                                                    }
            }
        }
    }
}
impl ::aws_smithy_types::retry::ProvideErrorKind for GetTypeTemplatesListError {
    fn code(&self) -> ::std::option::Option<&str> {
        ::aws_smithy_types::error::metadata::ProvideErrorMetadata::code(self)
    }
    fn retryable_error_kind(&self) -> ::std::option::Option<::aws_smithy_types::retry::ErrorKind> {
        ::std::option::Option::None
    }
}
impl ::aws_smithy_types::error::metadata::ProvideErrorMetadata for GetTypeTemplatesListError {
    fn meta(&self) -> &::aws_smithy_types::error::ErrorMetadata {
        match self {
            Self::InternalServerError(_inner) =>
            ::aws_smithy_types::error::metadata::ProvideErrorMetadata::meta(_inner)
            ,
            Self::Unhandled(_inner) => {
                &_inner.meta
            }
        }
    }
}
impl ::aws_smithy_runtime_api::client::result::CreateUnhandledError for GetTypeTemplatesListError {
    fn create_unhandled_error(
                        source: ::std::boxed::Box<dyn ::std::error::Error + ::std::marker::Send + ::std::marker::Sync + 'static>,
                        meta: ::std::option::Option<::aws_smithy_types::error::ErrorMetadata>
                    ) -> Self {
        Self::Unhandled(crate::error::sealed_unhandled::Unhandled { source, meta: meta.unwrap_or_default() })
    }
}

pub use crate::operation::get_type_templates_list::_get_type_templates_list_output::GetTypeTemplatesListOutput;

pub use crate::operation::get_type_templates_list::_get_type_templates_list_input::GetTypeTemplatesListInput;

mod _get_type_templates_list_input;

mod _get_type_templates_list_output;

/// Builders
pub mod builders;

