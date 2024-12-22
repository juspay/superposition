use proc_macro as pm;
use syn::{braced, parse::Parse, parse_macro_input, punctuated::Punctuated, Attribute, Field, Ident, Token};
use quote::quote;

#[proc_macro_derive(Fetch)]
pub fn derive(ts: pm::TokenStream) -> pm::TokenStream {
    let input = parse_macro_input!(ts as DBStruct);
    eprintln!("DBStruct: {:#?}", input);
    let DBStruct { attrs, ident, fields } = input;
    let http = quote! { actix_web::HttpResponse };
    let exprs = quote! {
        impl #ident {
            #[actix_web::web::get("/")]
            async pub fn fetch() -> #http {
                #http::Ok().finish()
            }
        }
    };
    pm::TokenStream::from(exprs)
}

#[derive(Debug)]
struct DBStruct {
    attrs: Vec<Attribute>,
    ident: Ident,
    fields: Punctuated<Field, Token![,]>,
}

impl Parse for DBStruct {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let attrs = input.call(Attribute::parse_outer)?;
        input.parse::<Token![struct]>()?;
        let ident = input.parse()?;
        let content;
        braced!(content in input);
        Ok(DBStruct {
            attrs,
            ident,
            fields: content.parse_terminated(Field::parse_named, Token![,])?
        })
    }
}
