use std::future::{ready, Ready};

use actix_web::{
    body::{BoxBody, EitherBody},
    cookie::{time::Duration, Cookie},
    dev::{forward_ready, Service, ServiceRequest, ServiceResponse, Transform},
    get,
    http::header,
    web::{Data, Query},
    Error, HttpResponse, Responder,
};
use futures_util::future::LocalBoxFuture;
use oidc::{
    core::{CoreClient, CoreErrorResponseType, CoreTokenResponse},
    AuthorizationCode, CsrfToken, Nonce, RequestTokenError, StandardErrorResponse,
    TokenResponse,
};
use openidconnect as oidc;
use reqwest as http;
use serde::Deserialize;

#[derive(Clone)]
struct OIDCAuthProvider {
    oidc_client: CoreClient,
    http_client: http::Client,
    idp_url: url::Url,
    csrf: CsrfToken,
    nonce: Nonce,
}

#[derive(Deserialize)]
struct LoginParams {
    code: AuthorizationCode,
    state: CsrfToken,
}

fn verify_presence(n: Option<&Nonce>) -> Result<(), String> {
    if n.is_some() {
        Ok(())
    } else {
        Err("missing nonce claim".to_string())
    }
}

type OIDCError =
    RequestTokenError<http::Error, StandardErrorResponse<CoreErrorResponseType>>;

fn to_oidc_error(error: http::Error) -> OIDCError {
    if error.is_connect() {
        RequestTokenError::Request(error)
    } else if error.is_status() {
        let se = StandardErrorResponse::new(
            CoreErrorResponseType::Extension(error.to_string()),
            None,
            error.url().map(|u| u.to_string()),
        );
        RequestTokenError::ServerResponse(se)
    } else {
        RequestTokenError::Other(error.to_string())
    }
}

impl OIDCAuthProvider {
    async fn request_token(
        &self,
        oidc_req: oidc::HttpRequest,
    ) -> Result<oidc::HttpResponse, OIDCError> {
        let res = self
            .http_client
            .request(oidc_req.method, oidc_req.url)
            .headers(oidc_req.headers)
            .send()
            .await
            .map_err(to_oidc_error)?;
        Ok(oidc::HttpResponse {
            status_code: res.status(),
            headers: res.headers().clone(),
            body: res.bytes().await.map_err(to_oidc_error)?.to_vec(),
        })
    }

    fn decode_token(&self, cookie: &str) -> Option<CoreTokenResponse> {
        let ctr = serde_json::from_str::<CoreTokenResponse>(cookie)
            .map_err(|e| eprintln!("error while decoding token: {}", e))
            .ok()?;
        ctr.id_token()?
            .claims(&self.oidc_client.id_token_verifier(), verify_presence)
            .map_err(|e| eprintln!("error while getting claims: {}", e))
            .ok()?;
        Some(ctr)
    }
}

pub struct OIDCMiddleware<S> {
    service: S,
    auth_provider: OIDCAuthProvider,
}

impl<S, B> Service<ServiceRequest> for OIDCMiddleware<S>
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
        let td: Option<CoreTokenResponse> = req
            .cookie("user")
            .and_then(|c| self.auth_provider.decode_token(c.value()));
        let p = &req.path();
        let exc = p.matches("login").count() > 0
            // Implies it's a local/un-forwarded request.
            || !req.headers().contains_key(header::X_FORWARDED_HOST)
            || p.matches("health").count() > 0
            || p.matches("ready").count() > 0;
        if td.is_some() || exc {
            let fut = self.service.call(req);
            Box::pin(async move { fut.await.map(|sr| sr.map_into_left_body()) })
        } else {
            let url = self.auth_provider.idp_url.to_string();
            // TODO Generate redirect.
            Box::pin(async move {
                let r = HttpResponse::Found()
                    .insert_header((header::LOCATION, url))
                    .finish()
                    .map_into_right_body();
                Ok(ServiceResponse::new(req.request().clone(), r))
            })
        }
    }
}

impl<S, B> Transform<S, ServiceRequest> for OIDCAuthProvider
where
    S: Service<ServiceRequest, Response = ServiceResponse<B>, Error = Error>,
    S::Future: 'static,
{
    type Response = ServiceResponse<EitherBody<B>>;
    type Error = Error;
    type InitError = ();
    type Transform = OIDCMiddleware<S>;
    type Future = Ready<Result<Self::Transform, Self::InitError>>;

    fn new_transform(&self, service: S) -> Self::Future {
        ready(Ok(OIDCMiddleware {
            service,
            auth_provider: self.to_owned(),
        }))
    }
}

#[get("/login")]
async fn oidc_login(
    auth_p: Data<OIDCAuthProvider>,
    params: Query<LoginParams>,
) -> impl Responder {
    if params.state.secret() != auth_p.csrf.secret() {
        eprintln!(
            "Bad csrf state, given: {:?} expected: {:?}",
            params.state, auth_p.csrf
        );
        return HttpResponse::Unauthorized().finish();
    }

    // Exchange the code with a token.
    let token_response = auth_p
        .oidc_client
        .exchange_code(params.code.clone())
        .request_async(|rq| auth_p.request_token(rq))
        .await
        .map_err(|e| eprintln!("Failed to exchange auth-code for token: {}", e))
        .and_then(|tr| {
            tr.id_token()
                .ok_or_else(|| eprintln!("No identity-token!"))
                .and_then(|t| {
                    t.claims(&auth_p.oidc_client.id_token_verifier(), &auth_p.nonce)
                        .map_err(|e| eprintln!("Couldn't verify claims: {}", e))
                })?;
            Ok(tr)
        });

    match token_response {
        Ok(tr) => {
            let token = serde_json::to_string(&tr).unwrap();
            let cookie = Cookie::build("user", token)
                .path("/")
                .http_only(true)
                .max_age(Duration::days(1))
                .finish();
            HttpResponse::Found()
                .cookie(cookie)
                .insert_header(("Location", "/"))
                .finish()
        }
        _ => HttpResponse::InternalServerError().finish(),
    }
}
