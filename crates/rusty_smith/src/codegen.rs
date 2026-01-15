// Code generation for both Rust and Smithy IDL

use crate::metadata::*;
use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote};
use syn::{DeriveInput, ItemStruct, Type};

#[cfg(feature = "proc-macro")]
use proc_macro::TokenStream;

#[cfg(feature = "proc-macro")]
pub fn generate_service_code(meta: ServiceMeta, original: ItemStruct) -> TokenStream {
    let name = &original.ident;
    let namespace = &meta.namespace;
    let version = &meta.version;
    let protocol = &meta.protocol;

    let registration = quote! {
        #original

        impl #name {
            pub fn register_smithy_service(registry: &mut rusty_smith::SmithyRegistry) {
                registry.register_service(rusty_smith::ServiceMeta {
                    name: stringify!(#name).to_string(),
                    namespace: #namespace.to_string(),
                    version: #version.to_string(),
                    protocol: #protocol.to_string(),
                    resources: vec![],
                    operations: vec![],
                    auth: vec![],
                    title: None,
                    documentation: None,
                });
            }
        }
    };

    registration.into()
}

pub fn generate_resource_code(meta: ResourceMeta, original: ItemStruct) -> TokenStream {
    let name = &original.ident;

    let registration = quote! {
        #original

        impl #name {
            pub fn register_smithy_resource(registry: &mut smithy_transpiler::SmithyRegistry) {
                // Resource registration code
            }
        }
    };

    registration.into()
}

pub fn generate_operation_code(meta: OperationMeta, original: ItemStruct) -> TokenStream {
    let name = &original.ident;

    let registration = quote! {
        #original

        impl #name {
            pub fn register_smithy_operation(registry: &mut smithy_transpiler::SmithyRegistry) {
                // Operation registration code
            }
        }
    };

    registration.into()
}

pub fn generate_shape_code(meta: StructMeta) -> TokenStream {
    let name = &meta.name;

    let registration = quote! {
        impl #name {
            pub fn register_smithy_shape(registry: &mut smithy_transpiler::SmithyRegistry) {
                // Shape registration code
            }
        }
    };

    registration.into()
}

pub fn generate_enum_code(meta: EnumMeta) -> TokenStream {
    let name = &meta.name;

    let registration = quote! {
        impl #name {
            pub fn register_smithy_enum(registry: &mut smithy_transpiler::SmithyRegistry) {
                // Enum registration code
            }
        }
    };

    registration.into()
}

/// Generate Smithy IDL file from registry
pub fn generate_smithy_idl(registry: &SmithyRegistry) -> String {
    let mut output = String::new();

    output.push_str("$version: \"2.0\"\n\n");

    // Generate services
    for service in &registry.services {
        output.push_str(&format!("namespace {}\n\n", service.namespace));

        if !service.auth.is_empty() {
            for auth in &service.auth {
                match auth {
                    AuthScheme::HttpBearerAuth => output.push_str("@httpBearerAuth\n"),
                    AuthScheme::HttpBasicAuth => output.push_str("@httpBasicAuth\n"),
                    AuthScheme::Custom(name) => output.push_str(&format!("@{}\n", name)),
                }
            }
        }

        if let Some(title) = &service.title {
            output.push_str(&format!("@title(\"{}\")\n", title));
        }

        output.push_str(&format!("@{}\n", service.protocol));

        if let Some(doc) = &service.documentation {
            output.push_str(&format!("@documentation(\"{}\")\n", escape_string(doc)));
        }

        output.push_str(&format!("service {} {{\n", service.name));
        output.push_str(&format!("    version: \"{}\"\n", service.version));

        if !service.resources.is_empty() {
            output.push_str("    resources: [\n");
            for resource in &service.resources {
                output.push_str(&format!("        {}\n", resource));
            }
            output.push_str("    ]\n");
        }

        if !service.operations.is_empty() {
            output.push_str("    operations: [\n");
            for operation in &service.operations {
                output.push_str(&format!("        {}\n", operation));
            }
            output.push_str("    ]\n");
        }

        output.push_str("}\n\n");
    }

    // Generate resources
    for resource in &registry.resources {
        if let Some(doc) = &resource.documentation {
            output.push_str(&format!("@documentation(\"{}\")\n", escape_string(doc)));
        }

        output.push_str(&format!("resource {} {{\n", resource.name));

        if !resource.identifiers.is_empty() {
            output.push_str("    identifiers: {\n");
            for (key, ty) in &resource.identifiers {
                output.push_str(&format!("        {}: {}\n", key, ty));
            }
            output.push_str("    }\n");
        }

        if !resource.properties.is_empty() {
            output.push_str("    properties: {\n");
            for prop in &resource.properties {
                let ty_str = type_to_smithy(&prop.ty);
                output.push_str(&format!("        {}: {}\n", prop.name, ty_str));
            }
            output.push_str("    }\n");
        }

        if let Some(read) = &resource.read_operation {
            output.push_str(&format!("    read: {}\n", read));
        }

        if let Some(list) = &resource.list_operation {
            output.push_str(&format!("    list: {}\n", list));
        }

        if let Some(create) = &resource.create_operation {
            output.push_str(&format!("    create: {}\n", create));
        }

        if let Some(update) = &resource.update_operation {
            output.push_str(&format!("    update: {}\n", update));
        }

        if let Some(delete) = &resource.delete_operation {
            output.push_str(&format!("    delete: {}\n", delete));
        }

        if !resource.operations.is_empty() {
            output.push_str("    operations: [\n");
            for op in &resource.operations {
                output.push_str(&format!("        {}\n", op));
            }
            output.push_str("    ]\n");
        }

        output.push_str("}\n\n");
    }

    // Generate operations
    for operation in &registry.operations {
        if let Some(doc) = &operation.documentation {
            output.push_str(&format!("@documentation(\"{}\")\n", escape_string(doc)));
        }

        output.push_str(&format!(
            "@http(method: \"{}\", uri: \"{}\")\n",
            operation.http_method, operation.uri
        ));

        if !operation.tags.is_empty() {
            let tags = operation
                .tags
                .iter()
                .map(|t| format!("\"{}\"", t))
                .collect::<Vec<_>>()
                .join(", ");
            output.push_str(&format!("@tags([{}])\n", tags));
        }

        if operation.readonly {
            output.push_str("@readonly\n");
        }

        if operation.idempotent {
            output.push_str("@idempotent\n");
        }

        output.push_str(&format!("operation {} {{\n", operation.name));

        if let Some(input) = &operation.input {
            output.push_str("    input := {\n");
            for field in &input.fields {
                if let Some(binding) = &field.http_binding {
                    match binding {
                        HttpBinding::Header(name) => {
                            output.push_str(&format!("        @httpHeader(\"{}\")\n", name));
                        }
                        HttpBinding::Query(name) => {
                            output.push_str(&format!("        @httpQuery(\"{}\")\n", name));
                        }
                        HttpBinding::Label => {
                            output.push_str("        @httpLabel\n");
                        }
                        HttpBinding::Payload => {
                            output.push_str("        @httpPayload\n");
                        }
                    }
                }

                if field.required {
                    output.push_str("        @required\n");
                }

                if field.not_property {
                    output.push_str("        @notProperty\n");
                }

                if let Some(doc) = &field.documentation {
                    output.push_str(&format!("        @documentation(\"{}\")\n", escape_string(doc)));
                }

                let ty_str = type_to_smithy(&field.ty);
                output.push_str(&format!("        {}: {}\n", field.name, ty_str));
            }
            output.push_str("    }\n");
        }

        if let Some(output_struct) = &operation.output {
            output.push_str("    output := {\n");
            for field in &output_struct.fields {
                if let Some(binding) = &field.http_binding {
                    match binding {
                        HttpBinding::Header(name) => {
                            output.push_str(&format!("        @httpHeader(\"{}\")\n", name));
                        }
                        HttpBinding::Payload => {
                            output.push_str("        @httpPayload\n");
                        }
                        _ => {}
                    }
                }

                if field.required {
                    output.push_str("        @required\n");
                }

                if field.not_property {
                    output.push_str("        @notProperty\n");
                }

                let ty_str = type_to_smithy(&field.ty);
                output.push_str(&format!("        {}: {}\n", field.name, ty_str));
            }
            output.push_str("    }\n");
        }

        if !operation.errors.is_empty() {
            output.push_str("    errors: [\n");
            for error in &operation.errors {
                output.push_str(&format!("        {}\n", error));
            }
            output.push_str("    ]\n");
        }

        output.push_str("}\n\n");
    }

    // Generate shapes/structures
    for shape in &registry.shapes {
        if let Some(doc) = &shape.documentation {
            output.push_str(&format!("@documentation(\"{}\")\n", escape_string(doc)));
        }

        output.push_str(&format!("structure {} {{\n", shape.name));

        for field in &shape.fields {
            if field.required {
                output.push_str("    @required\n");
            }

            if let Some(doc) = &field.documentation {
                output.push_str(&format!("    @documentation(\"{}\")\n", escape_string(doc)));
            }

            let ty_str = type_to_smithy(&field.ty);
            output.push_str(&format!("    {}: {}\n", field.name, ty_str));
        }

        output.push_str("}\n\n");
    }

    // Generate enums
    for enum_meta in &registry.enums {
        if let Some(doc) = &enum_meta.documentation {
            output.push_str(&format!("@documentation(\"{}\")\n", escape_string(doc)));
        }

        output.push_str(&format!("enum {} {{\n", enum_meta.name));

        for variant in &enum_meta.variants {
            if let Some(doc) = &variant.documentation {
                output.push_str(&format!("    @documentation(\"{}\")\n", escape_string(doc)));
            }

            if let Some(value) = &variant.value {
                output.push_str(&format!("    {} = \"{}\"\n", variant.name, value));
            } else {
                output.push_str(&format!("    {}\n", variant.name));
            }
        }

        output.push_str("}\n\n");
    }

    // Generate errors
    for error in &registry.errors {
        output.push_str(&format!("@httpError({})\n", error.http_code));

        let error_type_str = match error.error_type {
            ErrorType::Client => "client",
            ErrorType::Server => "server",
        };
        output.push_str(&format!("@error(\"{}\")\n", error_type_str));

        if error.retryable {
            output.push_str("@retryable\n");
        }

        if let Some(doc) = &error.documentation {
            output.push_str(&format!("@documentation(\"{}\")\n", escape_string(doc)));
        }

        output.push_str(&format!("structure {} {{\n", error.name));
        output.push_str("    message: String\n");
        output.push_str("}\n\n");
    }

    output
}

fn type_to_smithy(ty: &Type) -> String {
    match ty {
        Type::Path(type_path) => {
            let segments = &type_path.path.segments;
            if let Some(last) = segments.last() {
                let ident = &last.ident;
                let ident_str = ident.to_string();

                // Handle common Rust types
                match ident_str.as_str() {
                    "String" => "String".to_string(),
                    "i32" | "i64" => "Integer".to_string(),
                    "f32" | "f64" => "Float".to_string(),
                    "bool" => "Boolean".to_string(),
                    "Vec" => {
                        // Extract inner type
                        if let syn::PathArguments::AngleBracketed(args) = &last.arguments {
                            if let Some(syn::GenericArgument::Type(inner_ty)) = args.args.first() {
                                return format!("{}List", type_to_smithy(inner_ty));
                            }
                        }
                        "List".to_string()
                    }
                    "HashMap" | "BTreeMap" => {
                        // Extract key and value types
                        if let syn::PathArguments::AngleBracketed(args) = &last.arguments {
                            let args_vec: Vec<_> = args.args.iter().collect();
                            if args_vec.len() == 2 {
                                if let (
                                    syn::GenericArgument::Type(key_ty),
                                    syn::GenericArgument::Type(val_ty),
                                ) = (args_vec[0], args_vec[1])
                                {
                                    return format!("Map<{}, {}>", type_to_smithy(key_ty), type_to_smithy(val_ty));
                                }
                            }
                        }
                        "Map".to_string()
                    }
                    "Option" => {
                        // Extract inner type - in Smithy, optional is implicit
                        if let syn::PathArguments::AngleBracketed(args) = &last.arguments {
                            if let Some(syn::GenericArgument::Type(inner_ty)) = args.args.first() {
                                return type_to_smithy(inner_ty);
                            }
                        }
                        "Document".to_string()
                    }
                    _ => ident_str,
                }
            } else {
                "Document".to_string()
            }
        }
        _ => "Document".to_string(),
    }
}

fn escape_string(s: &str) -> String {
    s.replace('\"', "\\\"")
        .replace('\n', "\\n")
        .replace('\r', "\\r")
        .replace('\t', "\\t")
}
