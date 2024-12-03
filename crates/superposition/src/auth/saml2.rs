use actix_web::{
    body::{BoxBody, EitherBody},
    cookie::{time::Duration, Cookie},
    dev::{forward_ready, Service, ServiceRequest, ServiceResponse, Transform},
    error::InternalError,
    get,
    http::{
        header::{self, ContentType},
        StatusCode,
    },
    post,
    web::{self, Data},
    Error, HttpResponse, Responder, Scope,
};
use futures_util::future::LocalBoxFuture;
use jsonwebtoken::{
    decode, Algorithm, DecodingKey, EncodingKey, Header, TokenData, Validation,
};
use samael::{
    attribute::Attribute, metadata::EntityDescriptor, schema::AttributeStatement,
    service_provider::ServiceProvider, traits::ToXml,
};
use serde::{Deserialize, Serialize};
use std::{
    collections::{HashMap, HashSet},
    future::{ready, Ready},
};
use url::Url;

#[derive(Clone)]
pub struct SAMLAuthProvider {
    pub service_provider: ServiceProvider,
    pub idp_url: Url,
    pub metadata: EntityDescriptor,
    pub jwt_key_pair: (EncodingKey, DecodingKey),
}

#[derive(Clone, Serialize, Deserialize)]
struct User {
    first_name: String,
    last_name: String,
    email: String,
}

impl SAMLAuthProvider {
    fn authentication_request(&self, relay: &str) -> Result<Url, String> {
        let areq = self
            .service_provider
            .make_authentication_request(self.idp_url.as_str());
        match areq.and_then(|ar| ar.redirect(relay)) {
            Ok(url) => url.ok_or(String::from("DUDE WHERE IS MY REDIRECT???")),
            Err(e) => Err(e.to_string()),
        }
    }

    pub fn routes(&self) -> Scope {
        web::scope("saml")
            .app_data(Data::new(self.to_owned()))
            .service(assertion_consumer_service)
            .service(metadata)
    }

    fn decode_jwt(&self, cookie: &str) -> Option<TokenData<User>> {
        let dkey = &self.jwt_key_pair.1;
        let mut vds = Validation::new(Algorithm::HS256);
        vds.validate_exp = false;
        vds.required_spec_claims = HashSet::new();
        let result = decode::<User>(cookie, dkey, &vds);
        match result {
            Ok(td) => Some(td),
            Err(e) => {
                eprintln!("Failed to decode jwt: {}", e);
                None
            }
        }
    }
}

pub struct SAMLMiddleware<S> {
    service: S,
    auth_provider: SAMLAuthProvider,
}

impl<S, B> Service<ServiceRequest> for SAMLMiddleware<S>
where
    S: Service<ServiceRequest, Response = ServiceResponse<B>, Error = Error>,
    S::Future: 'static,
{
    type Response = ServiceResponse<EitherBody<B, BoxBody>>;
    type Error = Error;
    type Future = LocalBoxFuture<'static, Result<Self::Response, Self::Error>>;

    // Generate polling fn.
    forward_ready!(service);

    fn call(&self, req: ServiceRequest) -> Self::Future {
        let td = req
            .cookie("user")
            .and_then(|c| self.auth_provider.decode_jwt(c.value()));
        let exp = req.path().matches("saml/acs").count() > 0
            || req.path().matches("health").count() > 0
            || req.path().matches("ready").count() > 0;
        if td.is_some() || exp {
            let fut = self.service.call(req);
            Box::pin(async move { fut.await.map(|sr| sr.map_into_left_body()) })
        } else {
            // TODO Add query params to relay.
            match self.auth_provider.authentication_request(req.path()) {
                Ok(redirect) => Box::pin(async move {
                    let resp = HttpResponse::Found()
                        .insert_header((header::LOCATION, redirect.to_string()))
                        .finish()
                        .map_into_right_body();
                    Ok(ServiceResponse::new(req.request().clone(), resp))
                }),
                Err(e) => Box::pin(async move {
                    Err(InternalError::new(e, StatusCode::INTERNAL_SERVER_ERROR).into())
                }),
            }
        }
    }
}

impl<S, B> Transform<S, ServiceRequest> for SAMLAuthProvider
where
    S: Service<ServiceRequest, Response = ServiceResponse<B>, Error = Error>,
    S::Future: 'static,
{
    type Response = ServiceResponse<EitherBody<B>>;
    type Error = Error;
    type InitError = ();
    type Transform = SAMLMiddleware<S>;
    type Future = Ready<Result<Self::Transform, Self::InitError>>;

    fn new_transform(&self, service: S) -> Self::Future {
        ready(Ok(SAMLMiddleware {
            service,
            auth_provider: self.to_owned(),
        }))
    }
}

#[get("/metadata")]
async fn metadata(ctx: web::Data<SAMLAuthProvider>) -> impl Responder {
    HttpResponse::Ok()
        .content_type(ContentType::xml())
        .body(ctx.metadata.to_string().unwrap())
}

fn try_into_pair(attribute: Attribute) -> Option<(String, String)> {
    match (
        attribute.name,
        attribute.values.first().and_then(|v| v.value.clone()),
    ) {
        (Some(n), Some(v)) => Some((n, v)),
        _ => None,
    }
}

fn simplify_attribute_statments(
    attribute_statements: Vec<AttributeStatement>,
) -> HashMap<String, String> {
    attribute_statements
        .into_iter()
        .map(|ast| ast.attributes.into_iter())
        .flatten()
        .map(try_into_pair)
        .filter_map(|x| x)
        .collect()
}

#[post("/acs")]
async fn assertion_consumer_service(
    auth_provider: web::Data<SAMLAuthProvider>,
    form: web::Form<HashMap<String, String>>,
) -> impl Responder {
    let sr_b64 = form
        .get("SAMLResponse")
        .expect("No SAMLResponse found in body.");
    let asrn = auth_provider
        .service_provider
        .parse_base64_response(sr_b64, None)
        .unwrap();
    let attrs = asrn
        .attribute_statements
        .map(simplify_attribute_statments)
        .unwrap_or_default();
    let data = User {
        first_name: attrs.get("first_name").unwrap().to_string(),
        last_name: attrs.get("last_name").unwrap().to_string(),
        email: attrs.get("email").unwrap().to_string(),
    };
    let jwt =
        jsonwebtoken::encode(&Header::default(), &data, &auth_provider.jwt_key_pair.0)
            .unwrap();
    let cookie = Cookie::build("user", jwt)
        .path("/")
        .http_only(true)
        .max_age(Duration::days(1))
        .finish();
    let redirect = form.get("RelayState").map(|r| r.as_str()).unwrap_or("/");
    HttpResponse::Found()
        .cookie(cookie)
        .insert_header(("Location", redirect))
        .finish()
}
