use actix_web::{
    cookie::{time::Duration, Cookie},
    dev::ServiceRequest,
    get,
    http::header::{self, ContentType},
    post,
    web::{self, Data},
    HttpResponse, Responder, Scope,
};

use jsonwebtoken::{
    decode, Algorithm, DecodingKey, EncodingKey, Header, TokenData, Validation,
};
use samael::{
    attribute::Attribute, metadata::EntityDescriptor, schema::AttributeStatement,
    service_provider::ServiceProvider, traits::ToXml,
};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use url::Url;

use super::authenticator::Authenticator;

#[derive(Clone)]
pub struct SAMLAuthenticator {
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

impl SAMLAuthenticator {
    fn new_auth_request(&self, relay: &str) -> Result<Url, String> {
        let areq = self
            .service_provider
            .make_authentication_request(self.idp_url.as_str());
        match areq.and_then(|ar| ar.redirect(relay)) {
            Ok(url) => url.ok_or(String::from("DUDE WHERE IS MY REDIRECT???")),
            Err(e) => Err(e.to_string()),
        }
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

impl Authenticator for SAMLAuthenticator {
    fn authenticate(&self, req: &ServiceRequest) -> Result<(), HttpResponse> {
        let td = req.cookie("user").and_then(|c| self.decode_jwt(c.value()));
        let exp = req.path().matches("saml/acs").count() > 0
            // Implies it's a local/un-forwarded request.
            || !req.headers().contains_key(header::X_FORWARDED_HOST)
            || req.path().matches("health").count() > 0
            || req.path().matches("ready").count() > 0;
        if td.is_some() || exp {
            Ok(())
        } else {
            match self.new_auth_request(req.path()) {
                Ok(redirect) => {
                    let resp = HttpResponse::Found()
                        .insert_header((header::LOCATION, redirect.to_string()))
                        .finish();
                    Err(resp)
                }
                Err(e) => {
                    eprintln!("SAML2: Error while constructing redirect: {}", e);
                    Err(HttpResponse::InternalServerError().finish())
                }
            }
        }
    }

    fn routes(&self) -> Scope {
        web::scope("saml")
            .app_data(Data::new(self.to_owned()))
            .service(assertion_consumer_service)
            .service(metadata)
    }
}

#[get("/metadata")]
async fn metadata(ctx: web::Data<SAMLAuthenticator>) -> impl Responder {
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
    auth_provider: web::Data<SAMLAuthenticator>,
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
