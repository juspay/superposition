mod authentication;
mod no_auth;
mod oidc;

use std::{
    collections::HashSet,
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
use authentication::{Authenticator, Login, SwitchOrgParams};
use aws_sdk_kms::Client;
use futures_util::future::LocalBoxFuture;
use no_auth::DisabledAuthenticator;
use oidc::OIDCAuthenticator;
use superposition_types::User;
use url::Url;

use crate::{
    db::utils::get_oidc_client_secret,
    helpers::get_from_env_unsafe,
    service::types::{AppEnv, AppState},
};

pub struct AuthNMiddleware<S> {
    service: S,
    auth_n_handler: AuthNHandler,
}

impl<S> AuthNMiddleware<S> {
    fn get_login_type(
        &self,
        request: &ServiceRequest,
        exception: &HashSet<String>,
    ) -> Login {
        let path_prefix = self.auth_n_handler.0.get_path_prefix();
        let request_pattern = request
            .match_pattern()
            .map(|a| a.replace(&path_prefix, ""))
            .unwrap_or_else(|| request.uri().path().replace(&path_prefix, ""));

        let excep = exception.contains(&request_pattern)
                        // Implies it's a local/un-forwarded request.
                        || !request.headers().contains_key(header::USER_AGENT);
        let org_request = request.path().matches("/organisations").count() > 0;

        match (excep, org_request) {
            (true, false) => Login::None,
            (_, true) => Login::Global,
            (false, false) => Login::Org,
        }
    }
}

impl<S, B> Service<ServiceRequest> for AuthNMiddleware<S>
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
                let mut token = auth.split(' ');
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
            .unwrap_or_else(|| {
                let login_type = self
                    .get_login_type(&request, &state.tenant_middleware_exclusion_list);

                self.auth_n_handler.0.authenticate(&request, &login_type)
            });

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
pub struct AuthNHandler(Arc<dyn Authenticator>);

impl AuthNHandler {
    pub fn routes(&self) -> Scope {
        self.0.routes()
    }

    pub fn org_routes(&self) -> Scope {
        routes(self.clone())
    }

    pub async fn init(
        kms_client: &Option<Client>,
        app_env: &AppEnv,
        path_prefix: String,
    ) -> Self {
        let auth_provider: String = get_from_env_unsafe("AUTH_PROVIDER").unwrap();
        let mut auth = auth_provider.split('+');

        let ap: Arc<dyn Authenticator> = match auth.next() {
            Some("DISABLED") => Arc::new(DisabledAuthenticator::new(
                get_from_env_unsafe::<String>("LOCAL_ORGS")
                    .unwrap()
                    .split(',')
                    .map(String::from)
                    .collect(),
                path_prefix,
            )),
            Some("OIDC") => {
                let url = Url::parse(auth.next().unwrap())
                    .map_err(|e| e.to_string())
                    .unwrap();
                let base_url = get_from_env_unsafe("CAC_HOST").unwrap();
                let cid = get_from_env_unsafe("OIDC_CLIENT_ID").unwrap();
                let csecret = get_oidc_client_secret(kms_client, app_env).await;
                Arc::new(
                    OIDCAuthenticator::new(url, base_url, path_prefix, cid, csecret)
                        .await
                        .unwrap(),
                )
            }
            _ => panic!("Missing/Unknown authenticator."),
        };
        Self(ap)
    }
}

pub fn routes(auth: AuthNHandler) -> Scope {
    web::scope("organisations")
        .app_data(Data::new(auth))
        .service(get_organisations)
        .service(switch_organisation)
}

#[get("")]
async fn get_organisations(data: Data<AuthNHandler>, req: HttpRequest) -> HttpResponse {
    data.0.get_organisations(&req)
}

#[get("/switch/{organisation_id}")]
async fn switch_organisation(
    data: Data<AuthNHandler>,
    req: HttpRequest,
    path: Path<SwitchOrgParams>,
) -> actix_web::Result<HttpResponse> {
    data.0.switch_organisation(&req, &path).await
}

impl<S, B> Transform<S, ServiceRequest> for AuthNHandler
where
    S: Service<ServiceRequest, Response = ServiceResponse<B>, Error = Error>,
    S::Future: 'static,
{
    type Response = ServiceResponse<EitherBody<B>>;
    type Error = Error;
    type Transform = AuthNMiddleware<S>;
    type InitError = ();
    type Future = Ready<Result<Self::Transform, Self::InitError>>;

    fn new_transform(&self, service: S) -> Self::Future {
        ready(Ok(AuthNMiddleware {
            service,
            auth_n_handler: self.clone(),
        }))
    }
}
