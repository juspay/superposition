// Smithy Transpiler - A Rust-based DSL for writing API interfaces that transpile to Smithy IDL
//
// This crate provides procedural macros and types to define APIs in Rust and generate
// Smithy specification files.

use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse_macro_input, punctuated::Punctuated, token::Comma, DeriveInput, ItemStruct, NestedMeta,
};

pub mod codegen;
pub mod metadata;
mod parser;

pub use metadata::*;
pub use codegen::generate_smithy_idl;

/// Marks a struct as a Smithy service definition
///
/// # Example
/// ```ignore
/// #[smithy_service(
///     namespace = "io.example",
///     version = "2024-01-01",
///     protocol = "restJson1"
/// )]
/// pub struct MyService;
/// ```
#[proc_macro_attribute]
pub fn smithy_service(attr: TokenStream, item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as ItemStruct);
    let attrs = parse_macro_input!(attr with Punctuated::<NestedMeta, Comma>::parse_terminated);

    match parser::parse_service_attrs(&attrs, &input) {
        Ok(service_meta) => {
            codegen::generate_service_code(service_meta, input)
        }
        Err(e) => e.to_compile_error().into(),
    }
}

/// Marks a struct as a Smithy resource
///
/// # Example
/// ```ignore
/// #[smithy_resource(
///     identifiers = ["id: String"],
///     operations = [GetUser, UpdateUser]
/// )]
/// pub struct User {
///     pub id: String,
///     pub name: String,
///     pub email: String,
/// }
/// ```
#[proc_macro_attribute]
pub fn smithy_resource(attr: TokenStream, item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as ItemStruct);
    let attrs = parse_macro_input!(attr with Punctuated::<NestedMeta, Comma>::parse_terminated);

    match parser::parse_resource_attrs(&attrs, &input) {
        Ok(resource_meta) => {
            codegen::generate_resource_code(resource_meta, input)
        }
        Err(e) => e.to_compile_error().into(),
    }
}

/// Marks a function or struct as a Smithy operation
///
/// # Example
/// ```ignore
/// #[smithy_operation(
///     http_method = "POST",
///     uri = "/users",
///     tags = ["User Management"]
/// )]
/// pub struct CreateUser {
///     #[smithy(http_payload)]
///     pub name: String,
///     pub email: String,
/// }
/// ```
#[proc_macro_attribute]
pub fn smithy_operation(attr: TokenStream, item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as ItemStruct);
    let attrs = parse_macro_input!(attr with Punctuated::<NestedMeta, Comma>::parse_terminated);

    match parser::parse_operation_attrs(&attrs, &input) {
        Ok(op_meta) => {
            codegen::generate_operation_code(op_meta, input)
        }
        Err(e) => e.to_compile_error().into(),
    }
}

/// Field-level attribute for Smithy-specific metadata
///
/// # Example
/// ```ignore
/// pub struct CreateUser {
///     #[smithy(http_header = "x-api-key")]
///     pub api_key: String,
///
///     #[smithy(http_query = "version")]
///     pub version: Option<String>,
///
///     #[smithy(http_payload, required)]
///     pub data: UserData,
/// }
/// ```
#[proc_macro_attribute]
pub fn smithy(attr: TokenStream, item: TokenStream) -> TokenStream {
    // This is handled during parsing of parent structures
    item
}

/// Derive macro to automatically generate Smithy shape definitions
///
/// # Example
/// ```ignore
/// #[derive(SmithyShape)]
/// #[smithy(namespace = "io.example")]
/// pub struct UserData {
///     #[smithy(required)]
///     pub name: String,
///
///     #[smithy(documentation = "User's email address")]
///     pub email: String,
///
///     pub age: Option<i32>,
/// }
/// ```
#[proc_macro_derive(SmithyShape, attributes(smithy))]
pub fn derive_smithy_shape(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    match parser::parse_shape(&input) {
        Ok(shape_meta) => {
            codegen::generate_shape_code(shape_meta)
        }
        Err(e) => e.to_compile_error().into(),
    }
}

/// Derive macro for Smithy enum shapes
///
/// # Example
/// ```ignore
/// #[derive(SmithyEnum)]
/// #[smithy(namespace = "io.example")]
/// pub enum UserStatus {
///     #[smithy(value = "active")]
///     Active,
///
///     #[smithy(value = "inactive")]
///     Inactive,
///
///     #[smithy(value = "suspended")]
///     Suspended,
/// }
/// ```
#[proc_macro_derive(SmithyEnum, attributes(smithy))]
pub fn derive_smithy_enum(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    match parser::parse_enum(&input) {
        Ok(enum_meta) => {
            codegen::generate_enum_code(enum_meta)
        }
        Err(e) => e.to_compile_error().into(),
    }
}
