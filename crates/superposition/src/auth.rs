mod authenticator;
mod no_auth;
mod oidc;

use std::{
    future::{ready, Ready},
    sync::Arc,
};

use actix_web::{
    body::{BoxBody, EitherBody},
    dev::{forward_ready, Service, ServiceRequest, ServiceResponse, Transform},
    get,
    http::header,
    web::{self, Data, Path},
    Error, HttpMessage, HttpRequest, HttpResponse, Scope,
};
use authenticator::{Authenticator, SwitchOrgParams};
use futures_util::future::LocalBoxFuture;
use no_auth::DisabledAuthenticator;
use service_utils::{
    db::utils::get_oidc_client_secret, helpers::get_from_env_unsafe,
    service::types::AppState,
};
use superposition_types::User;
use url::Url;

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

    pub fn org_routes(&self) -> Scope {
        routes(self.clone())
    }

    pub async fn init() -> Self {
        let auth_provider: String = get_from_env_unsafe("AUTH_PROVIDER").unwrap();
        let mut auth = auth_provider.split('+');

        let ap: Arc<dyn Authenticator> = match auth.next() {
            Some("DISABLED") => Arc::new(DisabledAuthenticator::new(
                get_from_env_unsafe::<String>("LOCAL_ORGS")
                    .unwrap()
                    .split(",")
                    .map(String::from)
                    .collect(),
            )),
            Some("OIDC") => {
                let url = Url::parse(auth.next().unwrap())
                    .map_err(|e| e.to_string())
                    .unwrap();
                let base_url = get_from_env_unsafe("CAC_HOST").unwrap();
                let cid = get_from_env_unsafe("OIDC_CLIENT_ID").unwrap();
                let csecret = get_oidc_client_secret().await;
                Arc::new(
                    oidc::OIDCAuthenticator::new(url, base_url, cid, csecret)
                        .await
                        .unwrap(),
                )
            }
            _ => panic!("Missing/Unknown authenticator."),
        };
        Self(ap)
    }
}

pub fn routes(auth: AuthHandler) -> Scope {
    web::scope("organisations")
        .app_data(Data::new(auth))
        .service(get_organisations)
        .service(switch_organisation)
}

#[get("")]
async fn get_organisations(data: Data<AuthHandler>, req: HttpRequest) -> HttpResponse {
    data.0.get_organisations(&req)
}

#[get("/switch/{organisation_id}")]
async fn switch_organisation(
    data: Data<AuthHandler>,
    req: HttpRequest,
    path: Path<SwitchOrgParams>,
) -> actix_web::Result<HttpResponse> {
    data.0.switch_organisation(&req, &path).await
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
