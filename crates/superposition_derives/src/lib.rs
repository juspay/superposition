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

/// Implements `Display` trait for the struct, allowing it to be used as a query string
///
#[proc_macro_derive(DisplayQuery)]
pub fn derive_display_query(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let struct_name = input.ident;

    let fields = match input.data {
        Data::Struct(data) => match data.fields {
            Fields::Named(fields) => fields.named,
            _ => {
                return syn::Error::new_spanned(
                    struct_name,
                    "DisplayQuery can only be derived for structs with named fields",
                )
                .to_compile_error()
                .into();
            }
        },
        _ => {
            return syn::Error::new_spanned(
                struct_name,
                "DisplayQuery can only be derived for structs",
            )
            .to_compile_error()
            .into();
        }
    };

    let mut pushes = Vec::new();

    for field in fields {
        let field_name = field.ident.unwrap();
        let field_str = field_name.to_string();

        // check if the type is Option<_>
        let is_option = match &field.ty {
            Type::Path(type_path) => type_path
                .path
                .segments
                .first()
                .map_or(false, |seg| seg.ident == "Option"),
            _ => false,
        };

        if is_option {
            pushes.push(quote! {
                if let Some(value) = &self.#field_name {
                    query_params.push(format!("{}={}", #field_str, value));
                }
            });
        } else {
            pushes.push(quote! {
                query_params.push(format!("{}={}", #field_str, self.#field_name));
            });
        }
    }

    let expanded = quote! {
        impl std::fmt::Display for #struct_name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                let mut query_params = Vec::new();
                #(#pushes)*

                write!(f, "{}", query_params.join("&"))
            }
        }
    };

    TokenStream::from(expanded)
}
