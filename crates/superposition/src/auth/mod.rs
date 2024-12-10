use samael::{
    metadata::{ContactPerson, ContactType, EntityDescriptor},
    service_provider::ServiceProviderBuilder,
};
use std::{env, fs};
use url::Url;

mod saml2;
mod oidc;

pub fn init_auth() -> saml2::SAMLAuthProvider {
    let var = env::var("AUTH_PROVIDER")
        .ok()
        .expect("Env 'AUTH_PROVIDER' not declared, unable to initalize auth provider.");
    let mut auth = var.split('+');
    assert_eq!(auth.next(), Some("SAML2"));
    let idp_url = auth
        .next()
        .ok_or(String::from("URL not set in auth env."))
        .and_then(|u| Url::parse(u).map_err(|e| e.to_string()))
        .unwrap();
    let md_xml = fs::read_to_string("saml-idp-meta.xml").unwrap();
    let md: EntityDescriptor = samael::metadata::de::from_str(md_xml.as_str()).unwrap();
    let domain = env::var("SAML_HOST")
        .ok()
        .expect("Env 'SAML_HOST' not declared.");
    let sp = ServiceProviderBuilder::default()
        .entity_id("test-saml-sso-app".to_string())
        .idp_metadata(md)
        .allow_idp_initiated(true)
        .contact_person(ContactPerson {
            contact_type: Some(ContactType::Technical.value().to_string()),
            sur_name: Some("Doe".to_string()),
            ..ContactPerson::default()
        })
        .acs_url(format!("https://{}/saml/acs", domain))
        .slo_url(format!("https://{}/saml/metadata", domain))
        .build()
        .unwrap();
    saml2::SAMLAuthProvider {
        metadata: sp.metadata().unwrap(),
        idp_url: idp_url,
        service_provider: sp,
        jwt_key_pair: (
            jsonwebtoken::EncodingKey::from_secret("secret".as_ref()),
            jsonwebtoken::DecodingKey::from_secret("secret".as_ref()),
        ),
    }
}
