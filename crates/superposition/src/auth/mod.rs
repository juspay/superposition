use actix_web::{
    dev::{ServiceRequest, Transform, Service},
    Scope,
};
use samael::{
    metadata::{ContactPerson, ContactType, EntityDescriptor},
    service_provider::ServiceProviderBuilder,
};
use std::{env, fs};
use url::Url;

use self::saml2::SAMLAuthProvider;

mod oidc;
mod saml2;

trait AuthProvider
{
    type ActixMiddleware<S: Service<ServiceRequest>>: Transform<S, ServiceRequest>;
    fn middleware<S: Service<ServiceRequest>>(&self) -> Self::ActixMiddleware<S>;
    fn routes(&self) -> Scope;
}

impl AuthProvider for saml2::SAMLAuthProvider
where
    S: Service<ServiceRequest>
{
    type _Service = S;
    type ActixMiddleware = Self;

    fn middleware<S>(&self) -> Self::ActixMiddleware {
        self.clone()
    }

    fn routes(&self) -> Scope {
        self.routes()
    }
}

pub fn init_auth() -> saml2::SAMLAuthProvider {
    let var = env::var("AUTH_PROVIDER")
        .ok()
        .expect("Env 'AUTH_PROVIDER' not declared, unable to initalize auth provider.");
    let mut auth = var.split('+');
    assert_eq!(auth.next(), Some("SAML2"));
    return init_saml2_auth(auth.next().expect("Url not provided in env."));
}

fn init_saml2_auth(url: &str) -> saml2::SAMLAuthProvider {
    let idp_url = Url::parse(url).map_err(|e| e.to_string()).unwrap();
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
