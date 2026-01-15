// Parser for Smithy attributes and metadata extraction

use crate::metadata::*;
use proc_macro2::Span;
use quote::ToTokens;
use std::collections::HashMap;
use syn::{
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    token::Comma,
    Attribute, DeriveInput, Error, Expr, Field, Fields, Ident, ItemStruct, Lit, Meta,
    MetaNameValue, NestedMeta, Result, Token, Type,
};

pub type AttributeArgs = Punctuated<NestedMeta, Token![,]>;

pub fn parse_service_attrs(attrs: &AttributeArgs, item: &ItemStruct) -> Result<ServiceMeta> {
    let mut namespace = None;
    let mut version = None;
    let mut protocol = String::from("restJson1");
    let mut auth = Vec::new();
    let mut title = None;

    for attr in attrs {
        match attr {
            NestedMeta::Meta(Meta::NameValue(nv)) => {
                let name = nv
                    .path
                    .get_ident()
                    .ok_or_else(|| Error::new_spanned(&nv.path, "expected identifier"))?
                    .to_string();

                match name.as_str() {
                    "namespace" => {
                        namespace = Some(parse_string_lit(&nv.lit)?);
                    }
                    "version" => {
                        version = Some(parse_string_lit(&nv.lit)?);
                    }
                    "protocol" => {
                        protocol = parse_string_lit(&nv.lit)?;
                    }
                    "title" => {
                        title = Some(parse_string_lit(&nv.lit)?);
                    }
                    _ => {
                        return Err(Error::new_spanned(
                            nv,
                            format!("unknown service attribute: {}", name),
                        ))
                    }
                }
            }
            NestedMeta::Meta(Meta::Path(path)) => {
                let name = path
                    .get_ident()
                    .ok_or_else(|| Error::new_spanned(path, "expected identifier"))?
                    .to_string();

                match name.as_str() {
                    "http_bearer_auth" => auth.push(AuthScheme::HttpBearerAuth),
                    "http_basic_auth" => auth.push(AuthScheme::HttpBasicAuth),
                    _ => {
                        return Err(Error::new_spanned(
                            path,
                            format!("unknown auth scheme: {}", name),
                        ))
                    }
                }
            }
            _ => return Err(Error::new_spanned(attr, "unexpected attribute format")),
        }
    }

    let namespace =
        namespace.ok_or_else(|| Error::new(Span::call_site(), "namespace is required"))?;
    let version = version.ok_or_else(|| Error::new(Span::call_site(), "version is required"))?;

    let documentation = extract_doc_comments(&item.attrs);

    Ok(ServiceMeta {
        name: item.ident.clone(),
        namespace,
        version,
        protocol,
        resources: Vec::new(),
        operations: Vec::new(),
        auth,
        title,
        documentation,
    })
}

pub fn parse_resource_attrs(attrs: &AttributeArgs, item: &ItemStruct) -> Result<ResourceMeta> {
    let mut identifiers = HashMap::new();
    let mut operations = Vec::new();
    let mut read_operation = None;
    let mut list_operation = None;
    let mut create_operation = None;
    let mut update_operation = None;
    let mut delete_operation = None;

    for attr in attrs {
        match attr {
            NestedMeta::Meta(Meta::NameValue(nv)) => {
                let name = nv
                    .path
                    .get_ident()
                    .ok_or_else(|| Error::new_spanned(&nv.path, "expected identifier"))?
                    .to_string();

                match name.as_str() {
                    "identifiers" => {
                        // Parse array of "key: Type" strings
                        if let Lit::Str(s) = &nv.lit {
                            for ident_str in s.value().split(',') {
                                let parts: Vec<&str> = ident_str.trim().split(':').collect();
                                if parts.len() == 2 {
                                    identifiers
                                        .insert(parts[0].trim().to_string(), parts[1].trim().to_string());
                                }
                            }
                        }
                    }
                    "read" => {
                        read_operation = Some(parse_string_lit(&nv.lit)?);
                    }
                    "list" => {
                        list_operation = Some(parse_string_lit(&nv.lit)?);
                    }
                    "create" => {
                        create_operation = Some(parse_string_lit(&nv.lit)?);
                    }
                    "update" => {
                        update_operation = Some(parse_string_lit(&nv.lit)?);
                    }
                    "delete" => {
                        delete_operation = Some(parse_string_lit(&nv.lit)?);
                    }
                    _ => {
                        return Err(Error::new_spanned(
                            nv,
                            format!("unknown resource attribute: {}", name),
                        ))
                    }
                }
            }
            _ => {}
        }
    }

    let properties = parse_struct_fields(&item.fields)?;
    let documentation = extract_doc_comments(&item.attrs);

    Ok(ResourceMeta {
        name: item.ident.clone(),
        identifiers,
        properties,
        operations,
        read_operation,
        list_operation,
        create_operation,
        update_operation,
        delete_operation,
        documentation,
    })
}

pub fn parse_operation_attrs(attrs: &AttributeArgs, item: &ItemStruct) -> Result<OperationMeta> {
    let mut http_method = None;
    let mut uri = None;
    let mut tags = Vec::new();
    let mut readonly = false;
    let mut idempotent = false;

    for attr in attrs {
        match attr {
            NestedMeta::Meta(Meta::NameValue(nv)) => {
                let name = nv
                    .path
                    .get_ident()
                    .ok_or_else(|| Error::new_spanned(&nv.path, "expected identifier"))?
                    .to_string();

                match name.as_str() {
                    "http_method" => {
                        let method_str = parse_string_lit(&nv.lit)?;
                        http_method = Some(parse_http_method(&method_str)?);
                    }
                    "uri" => {
                        uri = Some(parse_string_lit(&nv.lit)?);
                    }
                    "tags" => {
                        if let Lit::Str(s) = &nv.lit {
                            tags = s
                                .value()
                                .split(',')
                                .map(|t| t.trim().to_string())
                                .collect();
                        }
                    }
                    _ => {
                        return Err(Error::new_spanned(
                            nv,
                            format!("unknown operation attribute: {}", name),
                        ))
                    }
                }
            }
            NestedMeta::Meta(Meta::Path(path)) => {
                let name = path
                    .get_ident()
                    .ok_or_else(|| Error::new_spanned(path, "expected identifier"))?
                    .to_string();

                match name.as_str() {
                    "readonly" => readonly = true,
                    "idempotent" => idempotent = true,
                    _ => {
                        return Err(Error::new_spanned(
                            path,
                            format!("unknown operation flag: {}", name),
                        ))
                    }
                }
            }
            _ => {}
        }
    }

    let http_method =
        http_method.ok_or_else(|| Error::new(Span::call_site(), "http_method is required"))?;
    let uri = uri.ok_or_else(|| Error::new(Span::call_site(), "uri is required"))?;

    let documentation = extract_doc_comments(&item.attrs);

    Ok(OperationMeta {
        name: item.ident.clone(),
        http_method,
        uri,
        input: None,
        output: None,
        errors: Vec::new(),
        tags,
        readonly,
        idempotent,
        documentation,
    })
}

pub fn parse_shape(input: &DeriveInput) -> Result<StructMeta> {
    let fields = match &input.data {
        syn::Data::Struct(data) => parse_struct_fields(&data.fields)?,
        _ => return Err(Error::new_spanned(input, "expected struct")),
    };

    let namespace = extract_smithy_attr(&input.attrs, "namespace")?;
    let documentation = extract_doc_comments(&input.attrs);

    let field_metas = fields
        .into_iter()
        .map(|p| FieldMeta {
            name: p.name,
            ty: p.ty,
            required: p.required,
            http_binding: None,
            documentation: p.documentation,
            default_value: None,
            not_property: false,
        })
        .collect();

    Ok(StructMeta {
        name: input.ident.clone(),
        fields: field_metas,
        namespace,
        mixins: Vec::new(),
        documentation,
    })
}

pub fn parse_enum(input: &DeriveInput) -> Result<EnumMeta> {
    let variants = match &input.data {
        syn::Data::Enum(data) => {
            let mut result = Vec::new();
            for variant in &data.variants {
                let value = extract_smithy_attr(&variant.attrs, "value")?;
                let documentation = extract_doc_comments(&variant.attrs);

                result.push(EnumVariantMeta {
                    name: variant.ident.to_string(),
                    value,
                    documentation,
                });
            }
            result
        }
        _ => return Err(Error::new_spanned(input, "expected enum")),
    };

    let namespace = extract_smithy_attr(&input.attrs, "namespace")?;
    let documentation = extract_doc_comments(&input.attrs);

    Ok(EnumMeta {
        name: input.ident.clone(),
        variants,
        namespace,
        documentation,
    })
}

fn parse_struct_fields(fields: &Fields) -> Result<Vec<PropertyMeta>> {
    let mut properties = Vec::new();

    match fields {
        Fields::Named(named) => {
            for field in &named.named {
                let name = field
                    .ident
                    .as_ref()
                    .ok_or_else(|| Error::new_spanned(field, "field must have a name"))?
                    .to_string();

                let required = has_smithy_attr(&field.attrs, "required");
                let documentation = extract_doc_comments(&field.attrs);

                properties.push(PropertyMeta {
                    name,
                    ty: field.ty.clone(),
                    required,
                    documentation,
                });
            }
        }
        _ => {}
    }

    Ok(properties)
}

fn parse_string_lit(lit: &Lit) -> Result<String> {
    match lit {
        Lit::Str(s) => Ok(s.value()),
        _ => Err(Error::new_spanned(lit, "expected string literal")),
    }
}

fn parse_http_method(s: &str) -> Result<HttpMethod> {
    match s.to_uppercase().as_str() {
        "GET" => Ok(HttpMethod::GET),
        "POST" => Ok(HttpMethod::POST),
        "PUT" => Ok(HttpMethod::PUT),
        "PATCH" => Ok(HttpMethod::PATCH),
        "DELETE" => Ok(HttpMethod::DELETE),
        _ => Err(Error::new(
            Span::call_site(),
            format!("unknown HTTP method: {}", s),
        )),
    }
}

fn extract_doc_comments(attrs: &[Attribute]) -> Option<String> {
    let mut docs = Vec::new();

    for attr in attrs {
        if attr.path.is_ident("doc") {
            if let Ok(Meta::NameValue(MetaNameValue {
                lit: Lit::Str(s), ..
            })) = attr.parse_meta()
            {
                docs.push(s.value().trim().to_string());
            }
        }
    }

    if docs.is_empty() {
        None
    } else {
        Some(docs.join("\n"))
    }
}

fn extract_smithy_attr(attrs: &[Attribute], name: &str) -> Result<Option<String>> {
    for attr in attrs {
        if attr.path.is_ident("smithy") {
            if let Ok(Meta::List(list)) = attr.parse_meta() {
                for nested in list.nested {
                    if let NestedMeta::Meta(Meta::NameValue(nv)) = nested {
                        if nv.path.is_ident(name) {
                            return Ok(Some(parse_string_lit(&nv.lit)?));
                        }
                    }
                }
            }
        }
    }
    Ok(None)
}

fn has_smithy_attr(attrs: &[Attribute], name: &str) -> bool {
    for attr in attrs {
        if attr.path.is_ident("smithy") {
            if let Ok(Meta::List(list)) = attr.parse_meta() {
                for nested in list.nested {
                    if let NestedMeta::Meta(Meta::Path(path)) = nested {
                        if path.is_ident(name) {
                            return true;
                        }
                    }
                }
            }
        }
    }
    false
}
