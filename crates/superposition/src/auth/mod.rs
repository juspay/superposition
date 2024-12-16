use actix_web::{
    body::{BoxBody, EitherBody},
    dev::{forward_ready, Service, ServiceRequest, ServiceResponse, Transform},
    Error, Scope,
};
use futures_util::future::LocalBoxFuture;
use samael::{
    metadata::{ContactPerson, ContactType, EntityDescriptor},
    service_provider::ServiceProviderBuilder,
};
use std::{
    env, fs,
    future::{ready, Ready},
    sync::Arc,
};
use url::Url;

use self::authenticator::Authenticator;

mod authenticator;
mod oidc;
mod saml2;

pub struct AuthMiddleware<S> {
    service: S,
    auth_handler: AuthHandler,
}

impl<S, B> Service<ServiceRequest> for AuthMiddleware<S>
where
    S: Service<ServiceRequest, Response = ServiceResponse<B>, Error = Error>,
    S::Future: 'static,
{
    type Response = ServiceResponse<EitherBody<B, BoxBody>>;
    type Error = Error;
    type Future = LocalBoxFuture<'static, Result<Self::Response, Self::Error>>;

    // Generate polling fn.
    forward_ready!(service);

    fn call(&self, request: ServiceRequest) -> Self::Future {
        match self.auth_handler.0.authenticate(&request) {
            Ok(()) => {
                let fut = self.service.call(request);
                Box::pin(async move { fut.await.map(|sr| sr.map_into_left_body()) })
            }
            Err(resp) => Box::pin(async move {
                Ok(ServiceResponse::new(
                    request.request().clone(),
                    resp.map_into_right_body(),
                ))
            }),
        }
    }
}

#[derive(Clone)]
pub struct AuthHandler(Arc<dyn Authenticator>);

impl AuthHandler {
    pub fn routes(&self) -> Scope {
        self.0.routes()
    }
}

impl<S, B> Transform<S, ServiceRequest> for AuthHandler
where
    S: Service<ServiceRequest, Response = ServiceResponse<B>, Error = Error>,
    S::Future: 'static,
{
    type Response = ServiceResponse<EitherBody<B>>;
    type Error = Error;
    type Transform = AuthMiddleware<S>;
    type InitError = ();
    type Future = Ready<Result<Self::Transform, Self::InitError>>;

    fn new_transform(&self, service: S) -> Self::Future {
        ready(Ok(AuthMiddleware {
            service,
            auth_handler: self.clone(),
        }))
    }
}

pub fn init_auth() -> AuthHandler {
    let var = env::var("AUTH_PROVIDER")
        .ok()
        .expect("Env 'AUTH_PROVIDER' not declared, unable to initalize auth provider.");
    let mut auth = var.split('+');
    assert_eq!(auth.next(), Some("SAML2"));
    let ap: Arc<dyn Authenticator> = match auth.next() {
        Some("SAML2") => Arc::new(init_saml2_auth(auth.next().expect("Url not provided in env."))),
        Some("OIDC") => {
            let url = Url::parse(auth.next().unwrap()).map_err(|e| e.to_string()).unwrap();
            Arc::new(oidc::OIDCAuthenticator::new(url, "https://superposition.devspaceworks.net".to_string()).unwrap())
        }
        _ => panic!("Missing/Unknown authenticator.")
    };
    AuthHandler(ap)
}

fn init_saml2_auth(url: &str) -> saml2::SAMLAuthenticator {
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
    saml2::SAMLAuthenticator {
        metadata: sp.metadata().unwrap(),
        idp_url: idp_url,
        service_provider: sp,
        jwt_key_pair: (
            jsonwebtoken::EncodingKey::from_secret("secret".as_ref()),
            jsonwebtoken::DecodingKey::from_secret("secret".as_ref()),
        ),
    }
}
