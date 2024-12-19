mod authenticator;
mod oidc;

use std::{
    future::{ready, Ready},
    sync::Arc,
};

use actix_web::{
    body::{BoxBody, EitherBody},
    dev::{forward_ready, Service, ServiceRequest, ServiceResponse, Transform},
    http::header,
    web::Data,
    Error, HttpMessage, Scope,
};
use aws_sdk_kms::Client;
use futures_util::future::LocalBoxFuture;
use service_utils::{
    db::utils::get_oidc_client_secret,
    helpers::get_from_env_unsafe,
    service::types::{AppEnv, AppState},
};
use superposition_types::User;
use url::Url;

use self::authenticator::Authenticator;

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
        let state = request.app_data::<Data<AppState>>().unwrap();

        let result = request
            .headers()
            .get(header::AUTHORIZATION)
            .and_then(|auth| auth.to_str().ok())
            .and_then(|auth| {
                let mut token = auth.split(' ').into_iter();
                match (token.next(), token.next()) {
                    (Some("Internal"), Some(token))
                        if token == state.superposition_token =>
                    {
                        request
                            .headers()
                            .get("x-user")
                            .and_then(|auth| auth.to_str().ok())
                            .and_then(|user_str| {
                                serde_json::from_str::<User>(user_str).ok()
                            })
                            .map(Ok)
                    }
                    (_, _) => None,
                }
            })
            .unwrap_or_else(|| self.auth_handler.0.authenticate(&request));

        match result {
            Ok(user) => {
                request.extensions_mut().insert::<User>(user);
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

struct DisabledAuthenticator;

impl Authenticator for DisabledAuthenticator {
    fn authenticate(&self, _: &ServiceRequest) -> Result<User, actix_web::HttpResponse> {
        Ok(User::default())
    }

    fn routes(&self) -> Scope {
        Scope::new("no_auth")
    }
}

pub async fn init_auth(kms_client: &Option<Client>, app_env: &AppEnv) -> AuthHandler {
    let auth_provider: String = get_from_env_unsafe("AUTH_PROVIDER").unwrap();
    let mut auth = auth_provider.split('+');

    let ap: Arc<dyn Authenticator> = match auth.next() {
        Some("DISABLED") => Arc::new(DisabledAuthenticator),
        Some("OIDC") => {
            let url = Url::parse(auth.next().unwrap())
                .map_err(|e| e.to_string())
                .unwrap();
            let base_url = get_from_env_unsafe("CAC_HOST").unwrap();
            let cid = get_from_env_unsafe("OIDC_CLIENT_ID").unwrap();
            let csecret = get_oidc_client_secret(kms_client, app_env).await;
            Arc::new(
                oidc::OIDCAuthenticator::new(url, base_url, cid, csecret)
                    .await
                    .unwrap(),
            )
        }
        _ => panic!("Missing/Unknown authenticator."),
    };
    AuthHandler(ap)
}
