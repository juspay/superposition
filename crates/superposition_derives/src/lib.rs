extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, Data, DeriveInput, Fields, Type};

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
                .into()
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
                .into()
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

        // detect if field has #[query_param(skip_if_empty)]
        let mut skip_if_empty = false;
        for attr in &field.attrs {
            if attr.path().is_ident("query_param") {
                let ident: syn::Ident = match attr.parse_args() {
                    Ok(v) => v,
                    Err(_) => continue,
                };
                if ident == "skip_if_empty" {
                    skip_if_empty = true;
                }
            }
        }

        // check if the type is Option<_>
        let is_option = matches!(&field.ty, Type::Path(type_path) if type_path.path.segments.first().is_some_and(|seg| seg.ident == "Option"));

        if is_option && skip_if_empty {
            query_parts.push(quote! {
                if let Some(value) = &self.#field_name {
                    if !value.is_empty() {
                        query_params.push(format!("{}={}", #field_str, value));
                    }
                }
            });
        } else if is_option && !skip_if_empty {
            query_parts.push(quote! {
                if let Some(value) = &self.#field_name {
                    query_params.push(format!("{}={}", #field_str, value));
                }
            });
        } else if skip_if_empty {
            query_parts.push(quote! {
                if !self.#field_name.is_empty() {
                    query_params.push(format!("{}={}", #field_str, self.#field_name));
                }
            });
        } else {
            query_parts.push(quote! {
                query_params.push(format!("{}={}", #field_str, self.#field_name));
            });
        }
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
