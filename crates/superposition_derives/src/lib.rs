extern crate proc_macro;
use std::fmt::Error;

use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::meta::ParseNestedMeta;
use syn::{braced, parse::Parse, punctuated::Punctuated, Attribute, Field, Ident, Token};
use syn::{parenthesized, parse_macro_input, DeriveInput, LitStr, Visibility};

/// Implements `FromSql` trait for converting `Json` type to the type for `Pg` backend
///
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

#[proc_macro_derive(Fetch)]
pub fn derive(ts: TokenStream) -> TokenStream {
    let input = parse_macro_input!(ts as DBStruct);
    let DBStruct {
        attrs,
        ident,
        table_name,
        fields,
        primary_key,
    } = input;
    let http = quote! { actix_web::HttpResponse };
    let handler = format_ident!("fetch_{}_handler", table_name);
    let path = format!("/{{{}}}", primary_key.ident.to_string());
    let ptype = primary_key.field.ty;
    let exprs = quote! {
        #[actix_web::get(#path)]
        pub async fn #handler(id: actix_web::web::Path<#ptype>, db_conn: DbConnection) -> crate::result::Result<#ident> {
            use crate::database::schema::#table_name::dsl::*;
            let service_utils::service::types::DbConnection(ref conn) = db_conn;
            Ok(actix_web::types::Json(#table_name.find(id.into_inner()).get_result::<#ident>(conn)?));
        }
    };
    let tokens = TokenStream::from(exprs);
    eprintln!("Tokens: {}", tokens);
    tokens
}

#[derive(Debug)]
struct DBStruct {
    attrs: Vec<Attribute>,
    ident: Ident,
    table_name: Ident,
    fields: Punctuated<Field, Token![,]>,
    primary_key: PrimaryKey,
}

#[derive(Debug)]
struct PrimaryKey {
    ident: Ident,
    field: Field,
}

impl Parse for DBStruct {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut attrs = input.call(Attribute::parse_outer)?;
        let pk_id = match infer_primary_key(attrs.as_mut_slice()) {
            Some(pk) => pk,
            _ => return Err(syn::Error::new(
                input.span(),
                "Could not infer primary key, has the diesel-attribute been declared?",
            )),
        };
        // Parsing to skip some tokens.
        input.parse::<Visibility>()?;
        input.parse::<Token![struct]>()?;
        let ident: Ident = input.parse()?;
        let lowered_ident = syn::Ident::new(&ident.to_string().to_lowercase(), input.span());
        // TODO Add check attribute check for table name.
        let table_name = format_ident!("{}s", lowered_ident);
        let content;
        braced!(content in input);
        let fields = content.parse_terminated(Field::parse_named, Token![,])?;
        let pk_field =
            match fields.iter().find(|f| f.ident.as_ref().map(|id| *id == pk_id) == Some(true)) {
                Some(p) => p.clone(),
                _ => return Err(syn::Error::new(input.span(), format!("Could not find primary-key w/ name {}, is the primary-key attribute correct?", pk_id)))
            };
        Ok(DBStruct {
            attrs,
            ident,
            table_name,
            fields,
            primary_key: PrimaryKey {
                ident: pk_id,
                field: pk_field,
            },
        })
    }
}

fn infer_primary_key(attrs: &mut [Attribute]) -> Option<Ident> {
    let mut pkey = None;
    for attr in attrs {
        if attr.path().is_ident("diesel") {
            // `parse_nested_meta` does not allow returning values, so have to use
            // some kind of externel state to set the primary key.
            attr.parse_nested_meta(|m| {
                if m.path.is_ident("primary_key") {
                    let content;
                    parenthesized!(content in m.input);
                    pkey = Some(content.parse::<Ident>()?);
                }
                Ok(())
            });
        }
    }
    pkey
}
