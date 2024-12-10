use actix_web::{
    body::{BoxBody, EitherBody},
    dev::{forward_ready, Service, ServiceRequest, ServiceResponse, Url},
    http::header,
    Error,
};
use futures_util::future::LocalBoxFuture;
use openidconnect::{
    core::{CoreClient, CoreTokenResponse},
    Nonce, OAuth2TokenResponse, TokenResponse,
};
use reqwest as http;

#[derive(Clone)]
struct OIDCAuthProvider {
    oidc_client: CoreClient,
    http_client: http::Client,
    idp_url: Url,
    nonce: Nonce,
}

impl OIDCAuthProvider {
    fn decode_jwt(&self, cookie: &str) -> Option<CoreTokenResponse> {
        let ctr = serde_json::from_str::<CoreTokenResponse>(cookie)
            // .and_then(|td| td.claims(&occ.id_token_verifier(), no_verify))
            .map_err(|e| eprintln!("error while decoding token: {}", e))
            .ok()?;
        ctr.id_token()?
            .claims(&self.oidc_client.id_token_verifier(), |nonce: Option<&Nonce>| {
                if nonce.is_some() {
                    Ok(())
                } else {
                    Err("missing nonce claim".to_string())
                }
            })
            .map_err(|e| eprintln!("error while getting claims: {}", e))
            .ok()?;
        Some(ctr)
    }
}

pub struct OIDCMiddleware<S> {
    service: S,
    auth_provider: OIDCAuthProvider,
}

fn no_verify(n: Option<&Nonce>) -> Result<(), String> {
    if n.is_some() {
        Ok(())
    } else {
        Err("missing nonce claim".to_string())
    }
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
        // TODO Check validity.
        let td: Option<CoreTokenResponse> = req
            .cookie("user")
            .and_then(|c| self.auth_provider.decode_jwt(c.value()));
        let exp = req.path().matches("login").count() > 0
            // Implies it's a local/un-forwarded request.
            || !req.headers().contains_key(header::X_FORWARDED_HOST)
            || req.path().matches("health").count() > 0
            || req.path().matches("ready").count() > 0;
        if td.is_some() || exp {
            let fut = self.service.call(req);
            Box::pin(async move { fut.await.map(|sr| sr.map_into_left_body()) })
        }
    }
}
