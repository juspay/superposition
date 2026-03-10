#![deny(unused_crate_dependencies)]
extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro_crate::{FoundCrate, crate_name};
use quote::{format_ident, quote};
use syn::{
    Data, DeriveInput, Fields, ItemFn, Token, Type,
    parse::{Parse, ParseStream},
    parse_macro_input,
};

/// Implements `FromSql` trait for converting `Json` type to the type for `Pg` backend
///
#[cfg(feature = "diesel_derives")]
#[proc_macro_derive(JsonFromSql)]
pub fn json_from_sql_derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;

    let expanded = quote! {
        impl diesel::deserialize::FromSql<diesel::sql_types::Json, diesel::pg::Pg> for #name {
            fn from_sql(bytes: diesel::pg::PgValue<'_>) -> diesel::deserialize::Result<Self> {
                let value = <serde_json::Value as diesel::deserialize::FromSql<diesel::sql_types::Json, diesel::pg::Pg>>::from_sql(bytes)?;
                Ok(serde_json::from_value(value)?)
            }
        }
    };

    TokenStream::from(expanded)
}

/// Implements `ToSql` trait for converting the typed data to `Json` type for `Pg` backend
///
#[cfg(feature = "diesel_derives")]
#[proc_macro_derive(JsonToSql)]
pub fn json_to_sql_derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;

    let expanded = quote! {
        impl diesel::serialize::ToSql<diesel::sql_types::Json, diesel::pg::Pg> for #name {
            fn to_sql<'b>(
                &'b self,
                out: &mut diesel::serialize::Output<'b, '_, diesel::pg::Pg>,
            ) -> diesel::serialize::Result {
                let value = serde_json::to_value(self)?;
                <serde_json::Value as diesel::serialize::ToSql<diesel::sql_types::Json, diesel::pg::Pg>>::to_sql(
                    &value,
                    &mut out.reborrow(),
                )
            }
        }
    };

    TokenStream::from(expanded)
}

/// Implements `FromSql` trait for converting `Text` type to the type for `Pg` backend
///
#[cfg(feature = "diesel_derives")]
#[proc_macro_derive(TextFromSql)]
pub fn text_from_sql_derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;

    quote! {
        impl diesel::deserialize::FromSql<diesel::sql_types::Text, diesel::pg::Pg> for #name {
            fn from_sql(bytes: diesel::pg::PgValue<'_>) -> diesel::deserialize::Result<Self> {
                let text = <String as diesel::deserialize::FromSql<diesel::sql_types::Text, diesel::pg::Pg>>::from_sql(bytes)?;
                text.try_into().map_err(|e: String| e.into())
            }
        }
    }.into()
}

/// Implements `ToSql` trait for converting the typed data to `Json` type for `Pg` backend
///
#[cfg(feature = "diesel_derives")]
#[proc_macro_derive(TextToSql)]
pub fn text_to_sql_derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;

    quote! {
        impl diesel::serialize::ToSql<diesel::sql_types::Text, diesel::pg::Pg> for #name {
            fn to_sql<'b>(
                &'b self,
                out: &mut diesel::serialize::Output<'b, '_, diesel::pg::Pg>,
            ) -> diesel::serialize::Result {
                let text: String = self.into();
                <String as diesel::serialize::ToSql<
                    diesel::sql_types::Text,
                    diesel::pg::Pg,
                >>::to_sql(&text, &mut out.reborrow())
            }
        }
    }
    .into()
}

/// Implements `FromSql` trait for converting `Text` type to the type for `Pg` backend without running validations on it
///
#[cfg(feature = "diesel_derives")]
#[proc_macro_derive(TextFromSqlNoValidation)]
pub fn text_from_sql_derive_no_validation(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;

    quote! {
        impl diesel::deserialize::FromSql<diesel::sql_types::Text, diesel::pg::Pg> for #name {
            fn from_sql(bytes: diesel::pg::PgValue<'_>) -> diesel::deserialize::Result<Self> {
                let text = <String as diesel::deserialize::FromSql<diesel::sql_types::Text, diesel::pg::Pg>>::from_sql(bytes)?;
                Ok(<#name as DisableDBValidation>::from_db_unvalidated(text))
            }
        }
    }.into()
}

/// Implements `IsEmpty` trait for structs
///
/// This trait is used to check if a struct is empty based on its fields.
/// If a struct has any non-Option fields, it is considered non-empty.
/// If all fields are Option types, it is considered empty if all Options are None.
/// The macro generates an implementation of the `IsEmpty` trait for the struct.
/// The macro checks the fields of the struct and generates the appropriate implementation.
/// The macro can only be used on structs with named fields.
/// If the struct has no fields or has unnamed fields, an error is returned.
///
#[proc_macro_derive(IsEmpty)]
pub fn derive_is_empty(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let struct_name = input.ident;

    let fields = if let Data::Struct(data) = input.data {
        match data.fields {
            Fields::Named(fields) => fields.named,
            Fields::Unnamed(_) | Fields::Unit => {
                return syn::Error::new_spanned(
                    struct_name,
                    "IsEmpty can only be derived for structs with named fields",
                )
                .to_compile_error()
                .into();
            }
        }
    } else {
        return syn::Error::new_spanned(
            struct_name,
            "IsEmpty can only be derived for structs",
        )
        .to_compile_error()
        .into();
    };

    let mut checks = Vec::new();
    let mut has_mandatory = false;

    for field in fields {
        let field_name = field.ident.unwrap();
        let ty = field.ty;

        if let Type::Path(type_path) = &ty {
            if type_path
                .path
                .segments
                .first()
                .is_some_and(|seg| seg.ident == "Option")
            {
                checks.push(quote! {
                    if self.#field_name.is_some() {
                        return false;
                    }
                });
                continue;
            }
        }

        has_mandatory = true;
        break;
    }

    let expanded = if has_mandatory {
        quote! {
            impl IsEmpty for #struct_name {
                fn is_empty(&self) -> bool {
                    false
                }
            }
        }
    } else {
        quote! {
            impl IsEmpty for #struct_name {
                fn is_empty(&self) -> bool {
                    #(#checks)*

                    true
                }
            }
        }
    };

    TokenStream::from(expanded)
}

/// Implements `QueryParam` trait for the struct, allowing it to be used as a query string
///
/// Supports the following attributes on fields:
/// - `#[query_param(skip_if_empty)]`: Skip the field if it's empty (requires `IsEmpty` trait)
/// - `#[query_param(iterable)]`: Iterate over the field and create multiple query parameters with the same key
/// - `#[query_param(skip_if_empty, iterable)]`: Combine both behaviors
///
#[proc_macro_derive(QueryParam, attributes(query_param))]
pub fn derive_query_param(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let struct_name = input.ident;

    let fields = if let Data::Struct(data) = input.data {
        match data.fields {
            Fields::Named(fields) => fields.named,
            Fields::Unnamed(_) | Fields::Unit => {
                return syn::Error::new_spanned(
                    struct_name,
                    "QueryParam can only be derived for structs with named fields",
                )
                .to_compile_error()
                .into();
            }
        }
    } else {
        return syn::Error::new_spanned(
            struct_name,
            "QueryParam can only be derived for structs",
        )
        .to_compile_error()
        .into();
    };

    let mut query_parts = Vec::new();

    for field in fields {
        let field_name = field.ident.unwrap();
        let field_str = field_name.to_string();

        // detect if field has #[query_param(skip_if_empty, iterable)]
        let mut skip_if_empty = false;
        let mut iter = false;
        for attr in &field.attrs {
            if attr.path().is_ident("query_param") {
                // Parse comma-separated arguments
                let args: syn::punctuated::Punctuated<syn::Ident, syn::Token![,]> =
                    match attr
                        .parse_args_with(syn::punctuated::Punctuated::parse_terminated)
                    {
                        Ok(v) => v,
                        Err(_) => continue,
                    };

                for arg in args {
                    match arg.to_string().as_str() {
                        "skip_if_empty" => skip_if_empty = true,
                        "iterable" => iter = true,
                        _ => {}
                    }
                }
            }
        }

        // check if the type is Option<_>
        let is_option = matches!(&field.ty, Type::Path(type_path) if type_path.path.segments.first().is_some_and(|seg| seg.ident == "Option"));

        // Build the query generation logic dynamically
        let query_generation = match (is_option, skip_if_empty, iter) {
            // For Option types
            (true, skip_empty, use_iter) => {
                let inner_logic = if use_iter {
                    quote! {
                        for item in value.iter() {
                            query_params.push(format!("{}={}", #field_str, item));
                        }
                    }
                } else {
                    quote! {
                        query_params.push(format!("{}={}", #field_str, value));
                    }
                };

                if skip_empty {
                    quote! {
                        if let Some(value) = &self.#field_name {
                            if !value.is_empty() {
                                #inner_logic
                            }
                        }
                    }
                } else {
                    quote! {
                        if let Some(value) = &self.#field_name {
                            #inner_logic
                        }
                    }
                }
            }
            // For non-Option types
            (false, skip_empty, use_iter) => {
                let inner_logic = if use_iter {
                    quote! {
                        for item in self.#field_name.iter() {
                            query_params.push(format!("{}={}", #field_str, item));
                        }
                    }
                } else {
                    quote! {
                        query_params.push(format!("{}={}", #field_str, self.#field_name));
                    }
                };
                if skip_empty {
                    quote! {
                        if !self.#field_name.is_empty() {
                            #inner_logic
                        }
                    }
                } else {
                    inner_logic
                }
            }
        };

        query_parts.push(query_generation);
    }

    let expanded = quote! {
        impl QueryParam for #struct_name {
            fn to_query_param(&self) -> String {
                let mut query_params = Vec::new();
                #(#query_parts)*

                query_params.join("&")
            }
        }
    };

    TokenStream::from(expanded)
}

/// Parsed attribute arguments for `#[authorized]`
#[derive(Default)]
struct AuthorizedArgs {
    action: Option<String>,
    resource: Option<String>,
}

impl Parse for AuthorizedArgs {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut args = AuthorizedArgs::default();

        if input.is_empty() {
            return Ok(args);
        }

        let parsed =
            syn::punctuated::Punctuated::<syn::Meta, Token![,]>::parse_terminated(input)?;

        for meta in parsed {
            let syn::Meta::NameValue(nv) = meta else {
                return Err(syn::Error::new_spanned(
                    meta,
                    "Expected key = value syntax (e.g., action = \"create\", resource = Secret)",
                ));
            };

            let Some(key) = nv.path.get_ident() else {
                return Err(syn::Error::new_spanned(
                    &nv.path,
                    "Expected simple identifier",
                ));
            };

            match key.to_string().as_str() {
                "action" => {
                    let syn::Expr::Lit(syn::ExprLit {
                        lit: syn::Lit::Str(lit_str),
                        ..
                    }) = nv.value
                    else {
                        return Err(syn::Error::new_spanned(
                            nv.value,
                            "action must be a string literal",
                        ));
                    };
                    args.action = Some(lit_str.value());
                }
                "resource" => {
                    let syn::Expr::Path(expr_path) = nv.value else {
                        return Err(syn::Error::new_spanned(
                            nv.value,
                            "resource must be an identifier (e.g., Secret, Config)",
                        ));
                    };
                    let Some(ident) = expr_path.path.get_ident() else {
                        return Err(syn::Error::new_spanned(
                            expr_path,
                            "resource must be a simple identifier",
                        ));
                    };
                    args.resource = Some(ident.to_string());
                }
                unknown => {
                    return Err(syn::Error::new_spanned(
                        &nv.path,
                        format!(
                            "Unknown attribute parameter: '{}'. Valid parameters are 'action' and 'resource'",
                            unknown
                        ),
                    ));
                }
            }
        }

        Ok(args)
    }
}

/// For an action `act` on a resource, on a function `act_handler`, this macro generates a struct
/// `AuthZActionAct` that implements the `Action` trait from `service_utils::middlewares::auth_z`.
/// It injects an additional parameter `_auth_z: AuthZ<AuthZActionAct>` into the function signature.
/// `AuthZ` struct implements `FromRequest` trait to handle authorization checks.
///
/// The struct is used to represent the action and is generated based on the action name and function name.
/// The handler function name must end with `_handler` to derive the struct name correctly and should not have
/// the resource part in its name, the name should represent the action only.
///
/// The resource is determined by one of two mechanisms (in order of precedence):
/// 1. An explicit Resource override in the macro: `#[authorized(resource = Secret)]`
/// 2. A file-level declaration via `declare_resource!(Config)` which sets `__DECLARED_RESOURCE__`
///
/// Examples:
/// - `#[authorized]` - action derived from fn_name, resource from __DECLARED_RESOURCE__
/// - `#[authorized(action = "custom_action")]` - explicit action, resource from __DECLARED_RESOURCE__
/// - `#[authorized(resource = Secret)]` - action from fn_name, explicit resource override
/// - `#[authorized(action = "custom_action", resource = Secret)]` - both explicit
#[proc_macro_attribute]
pub fn authorized(attr: TokenStream, item: TokenStream) -> TokenStream {
    // Parse the function
    let mut input_fn = parse_macro_input!(item as ItemFn);
    let fn_name = input_fn.sig.ident.to_string();

    let service_utils_path: syn::Path = match crate_name("service_utils") {
        Ok(FoundCrate::Itself) => syn::parse_quote!(crate),
        Ok(FoundCrate::Name(name)) => {
            let ident = format_ident!("{}", name);
            syn::parse_quote!(::#ident)
        }
        Err(_) => syn::parse_quote!(::service_utils),
    };

    let superposition_types_path: syn::Path = match crate_name("superposition_types") {
        Ok(FoundCrate::Itself) => syn::parse_quote!(crate),
        Ok(FoundCrate::Name(name)) => {
            let ident = format_ident!("{}", name);
            syn::parse_quote!(::#ident)
        }
        Err(_) => syn::parse_quote!(::superposition_types),
    };

    // Parse attribute arguments using proper syn parsing
    let args = parse_macro_input!(attr as AuthorizedArgs);

    // Determine action name: use explicit action if provided, otherwise derive from function name
    let action_name = if let Some(explicit_action) = args.action {
        explicit_action
    } else {
        // Derive action from function name
        let Some(name) = fn_name.strip_suffix("_handler") else {
            return syn::Error::new_spanned(
                input_fn.sig.ident,
                "Function name must end with '_handler' to derive action name automatically. \
                 Alternatively, specify action explicitly: #[authorized(action = \"your_action\")]",
            )
            .to_compile_error()
            .into();
        };
        name.to_string()
    };

    // Generate mangled struct name: AuthZActionAct
    let struct_name = format_ident!("AuthZAction{}", pascal_case(&action_name));

    // Inject parameter: _auth_z: AuthZ<AuthZActionAct>
    input_fn.sig.inputs.insert(
        0,
        syn::parse_quote!(_auth_z: #service_utils_path::middlewares::auth_z::AuthZ<#struct_name>),
    );

    // Generate the resource() method implementation and resource value for registry
    let (resource_impl, resource_value) = if let Some(override_resource) = &args.resource
    {
        // Use explicit override
        let resource_ident = format_ident!("{}", override_resource);
        (
            quote! {
                fn resource() -> #superposition_types_path::Resource {
                    #superposition_types_path::Resource::#resource_ident
                }
            },
            quote! { #superposition_types_path::Resource::#resource_ident },
        )
    } else {
        // Use __DECLARED_RESOURCE__ from file-level declare_resource! macro
        (
            quote! {
                fn resource() -> #superposition_types_path::Resource {
                    __DECLARED_RESOURCE__
                }
            },
            quote! { __DECLARED_RESOURCE__ },
        )
    };

    quote! {
        struct #struct_name;

        impl #service_utils_path::middlewares::auth_z::Action for #struct_name {
            fn get() -> String {
                #action_name.to_string()
            }

            #resource_impl
        }

        // Register action in inventory for runtime introspection
        inventory::submit! {
            #service_utils_path::registry::ActionRegistry::new(
                file!(),
                #fn_name,
                #action_name,
                #resource_value,
            )
        }

        #input_fn
    }
    .into()
}

fn pascal_case(s: &str) -> String {
    s.split(&['-', '_'])
        .map(|word| {
            let mut chars = word.chars();
            match chars.next() {
                Some(first) => first.to_uppercase().collect::<String>() + chars.as_str(),
                None => String::new(),
            }
        })
        .collect()
}

/// Declares the default resource for all #[authorized] handlers in this module.
/// Must be used once at the top of any handlers file that uses #[authorized].
///
/// # Example
/// ```ignore
/// declare_resource!(Config);
/// ```
#[proc_macro]
pub fn declare_resource(input: TokenStream) -> TokenStream {
    let resource = parse_macro_input!(input as syn::Ident);

    let superposition_types_path: syn::Path = match crate_name("superposition_types") {
        Ok(FoundCrate::Itself) => syn::parse_quote!(crate),
        Ok(FoundCrate::Name(name)) => {
            let ident = format_ident!("{}", name);
            syn::parse_quote!(::#ident)
        }
        Err(_) => syn::parse_quote!(::superposition_types),
    };

    quote! {
        #[doc(hidden)]
        const __DECLARED_RESOURCE__: #superposition_types_path::Resource =
            #superposition_types_path::Resource::#resource;
    }
    .into()
}
