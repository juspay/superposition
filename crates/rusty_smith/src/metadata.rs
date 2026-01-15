// Metadata structures representing Smithy IDL concepts

use std::collections::HashMap;
use syn::{Field, Ident, Type};

#[derive(Debug, Clone)]
pub struct ServiceMeta {
    pub name: Ident,
    pub namespace: String,
    pub version: String,
    pub protocol: String,
    pub resources: Vec<String>,
    pub operations: Vec<String>,
    pub auth: Vec<AuthScheme>,
    pub title: Option<String>,
    pub documentation: Option<String>,
}

#[derive(Debug, Clone)]
pub enum AuthScheme {
    HttpBearerAuth,
    HttpBasicAuth,
    Custom(String),
}

#[derive(Debug, Clone)]
pub struct ResourceMeta {
    pub name: Ident,
    pub identifiers: HashMap<String, String>,
    pub properties: Vec<PropertyMeta>,
    pub operations: Vec<String>,
    pub read_operation: Option<String>,
    pub list_operation: Option<String>,
    pub create_operation: Option<String>,
    pub update_operation: Option<String>,
    pub delete_operation: Option<String>,
    pub documentation: Option<String>,
}

#[derive(Debug, Clone)]
pub struct PropertyMeta {
    pub name: String,
    pub ty: Type,
    pub required: bool,
    pub documentation: Option<String>,
}

#[derive(Debug, Clone)]
pub struct OperationMeta {
    pub name: Ident,
    pub http_method: HttpMethod,
    pub uri: String,
    pub input: Option<StructMeta>,
    pub output: Option<StructMeta>,
    pub errors: Vec<String>,
    pub tags: Vec<String>,
    pub readonly: bool,
    pub idempotent: bool,
    pub documentation: Option<String>,
}

#[derive(Debug, Clone)]
pub enum HttpMethod {
    GET,
    POST,
    PUT,
    PATCH,
    DELETE,
}

impl std::fmt::Display for HttpMethod {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            HttpMethod::GET => write!(f, "GET"),
            HttpMethod::POST => write!(f, "POST"),
            HttpMethod::PUT => write!(f, "PUT"),
            HttpMethod::PATCH => write!(f, "PATCH"),
            HttpMethod::DELETE => write!(f, "DELETE"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct StructMeta {
    pub name: Ident,
    pub fields: Vec<FieldMeta>,
    pub namespace: Option<String>,
    pub mixins: Vec<String>,
    pub documentation: Option<String>,
}

#[derive(Debug, Clone)]
pub struct FieldMeta {
    pub name: String,
    pub ty: Type,
    pub required: bool,
    pub http_binding: Option<HttpBinding>,
    pub documentation: Option<String>,
    pub default_value: Option<String>,
    pub not_property: bool,
}

#[derive(Debug, Clone)]
pub enum HttpBinding {
    Header(String),
    Query(String),
    Label,
    Payload,
}

#[derive(Debug, Clone)]
pub struct EnumMeta {
    pub name: Ident,
    pub variants: Vec<EnumVariantMeta>,
    pub namespace: Option<String>,
    pub documentation: Option<String>,
}

#[derive(Debug, Clone)]
pub struct EnumVariantMeta {
    pub name: String,
    pub value: Option<String>,
    pub documentation: Option<String>,
}

#[derive(Debug, Clone)]
pub struct ErrorMeta {
    pub name: Ident,
    pub http_code: u16,
    pub error_type: ErrorType,
    pub retryable: bool,
    pub documentation: Option<String>,
}

#[derive(Debug, Clone)]
pub enum ErrorType {
    Client,
    Server,
}

/// Registry to collect all Smithy definitions for transpilation
pub struct SmithyRegistry {
    pub services: Vec<ServiceMeta>,
    pub resources: Vec<ResourceMeta>,
    pub operations: Vec<OperationMeta>,
    pub shapes: Vec<StructMeta>,
    pub enums: Vec<EnumMeta>,
    pub errors: Vec<ErrorMeta>,
}

impl SmithyRegistry {
    pub fn new() -> Self {
        Self {
            services: Vec::new(),
            resources: Vec::new(),
            operations: Vec::new(),
            shapes: Vec::new(),
            enums: Vec::new(),
            errors: Vec::new(),
        }
    }

    pub fn register_service(&mut self, service: ServiceMeta) {
        self.services.push(service);
    }

    pub fn register_resource(&mut self, resource: ResourceMeta) {
        self.resources.push(resource);
    }

    pub fn register_operation(&mut self, operation: OperationMeta) {
        self.operations.push(operation);
    }

    pub fn register_shape(&mut self, shape: StructMeta) {
        self.shapes.push(shape);
    }

    pub fn register_enum(&mut self, enum_meta: EnumMeta) {
        self.enums.push(enum_meta);
    }

    pub fn register_error(&mut self, error: ErrorMeta) {
        self.errors.push(error);
    }
}
