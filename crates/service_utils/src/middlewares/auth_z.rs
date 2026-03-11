mod authorization;
mod casbin;
mod no_auth;

use std::{
    fmt::Display,
    future::{Ready, ready},
    sync::Arc,
};

use actix_web::{
    Error, FromRequest, HttpMessage, HttpRequest, Scope,
    dev::{Payload, Service, ServiceRequest, ServiceResponse, Transform, forward_ready},
};
use authorization::Authorizer;
use aws_sdk_kms::Client;
use casbin::CasbinPolicyEngine;
use derive_more::Deref;
use futures_util::future::LocalBoxFuture;
use no_auth::NoAuth;
use superposition_macros::{forbidden, unexpected_error};
use superposition_types::{InternalUser, Resource, User, result as superposition};

use crate::{helpers::get_from_env_unsafe, service::types::AppEnv};

pub trait Action: Send + Sync + 'static {
    fn get() -> String;
    fn resource() -> Resource;
}

pub struct AuthZ<A: Action> {
    action: std::marker::PhantomData<A>,
    authz_handler: AuthZHandler,
    domain: AuthZDomain,
    user: User,
    internal_user: bool,
}

impl<A: Action> AuthZ<A> {
    fn new(
        authz_handler: AuthZHandler,
        domain: AuthZDomain,
        user: User,
        internal_user: bool,
    ) -> Self {
        Self {
            action: std::marker::PhantomData,
            authz_handler,
            domain,
            user,
            internal_user,
        }
    }

    pub async fn authorize_action(
        &self,
        action: &String,
        attributes: &[&String],
    ) -> superposition::Result<()> {
        if self.internal_user {
            return Ok(());
        }

        let resp = self
            .authz_handler
            .0
            .is_allowed(
                &self.domain,
                &self.user,
                &A::resource(),
                action,
                Some(attributes),
            )
            .await
            .map_err(|e| unexpected_error!("Error checking authorization: {}", e))?;

        if !resp {
            return Err(forbidden!("You are not authorized to perform this action."));
        }
        Ok(())
    }

    pub async fn authorize(&self, attributes: &[&String]) -> superposition::Result<()> {
        self.authorize_action(&A::get(), attributes).await
    }
}

impl<A: Action> FromRequest for AuthZ<A> {
    type Error = superposition::AppError;

    type Future = LocalBoxFuture<'static, Result<Self, Self::Error>>;

    fn from_request(req: &HttpRequest, _: &mut Payload) -> Self::Future {
        let auth_z_handler = match req.extensions().get::<AuthZHandler>() {
            Some(handler) => handler.clone(),
            None => {
                return Box::pin(async {
                    Err(unexpected_error!(
                        "AuthZHandler not found in request extensions."
                    ))
                });
            }
        };

        let domain = match req.extensions().get::<AuthZDomain>() {
            Some(org_id) => org_id.clone(),
            None => {
                return Box::pin(async {
                    Err(unexpected_error!(
                        "Organisation Id not found in request extensions."
                    ))
                });
            }
        };

        let user = match req.extensions().get::<User>() {
            Some(user) => user.clone(),
            None => {
                return Box::pin(async { Err(forbidden!("User not authenticated.")) });
            }
        };

        if req.extensions().get::<InternalUser>().is_some() {
            return Box::pin(async {
                Ok(AuthZ::new(auth_z_handler, domain, user, true))
            });
        }

        Box::pin(async move {
            let is_allowed = auth_z_handler
                .is_allowed(&domain, &user, &A::resource(), &A::get(), None)
                .await;

            match is_allowed {
                Err(e) => Err(unexpected_error!("Error checking authorization: {}", e)),
                Ok(is_allowed) => {
                    if is_allowed {
                        Ok(AuthZ::new(auth_z_handler, domain, user, false))
                    } else {
                        Err(forbidden!("You are not authorized to perform this action."))
                    }
                }
            }
        })
    }
}

pub struct AuthZMiddleware<S> {
    service: S,
    auth_z_handler: AuthZHandler,
}

impl<S, B> Service<ServiceRequest> for AuthZMiddleware<S>
where
    S: Service<ServiceRequest, Response = ServiceResponse<B>, Error = Error>,
    S::Future: 'static,
{
    type Response = ServiceResponse<B>;
    type Error = Error;
    type Future = LocalBoxFuture<'static, Result<Self::Response, Self::Error>>;

    forward_ready!(service);

    fn call(&self, req: ServiceRequest) -> Self::Future {
        req.extensions_mut().insert(self.auth_z_handler.clone());
        Box::pin(self.service.call(req))
    }
}

#[derive(Clone, Deref)]
pub struct AuthZHandler(Arc<dyn Authorizer>);

fn get_auth_z_provider() -> String {
    get_from_env_unsafe("AUTH_Z_PROVIDER").unwrap()
}

pub fn is_auth_z_enabled() -> bool {
    get_auth_z_provider().as_str() != "DISABLED"
}

impl AuthZHandler {
    pub async fn init(kms_client: &Option<Client>, app_env: &AppEnv) -> Self {
        let ap: Arc<dyn Authorizer> = match get_auth_z_provider().as_str() {
            "CASBIN" => Arc::new(
                CasbinPolicyEngine::new(kms_client, app_env, None)
                    .await
                    .unwrap(),
            ),
            "DISABLED" => Arc::new(NoAuth),
            _ => panic!("Missing/Unknown authorizer."),
        };
        Self(ap)
    }
}

impl FromRequest for AuthZHandler {
    type Error = superposition::AppError;
    type Future = Ready<Result<Self, Self::Error>>;

    fn from_request(
        req: &actix_web::HttpRequest,
        _: &mut actix_web::dev::Payload,
    ) -> Self::Future {
        let result = req
            .extensions()
            .get::<Self>()
            .cloned()
            .ok_or_else(|| unexpected_error!("AuthZHandler not found"));

        ready(result)
    }
}

impl<S, B> Transform<S, ServiceRequest> for AuthZHandler
where
    S: Service<ServiceRequest, Response = ServiceResponse<B>, Error = Error>,
    S::Future: 'static,
{
    type Response = ServiceResponse<B>;
    type Error = Error;
    type Transform = AuthZMiddleware<S>;
    type InitError = ();
    type Future = Ready<Result<Self::Transform, Self::InitError>>;

    fn new_transform(&self, service: S) -> Self::Future {
        ready(Ok(AuthZMiddleware {
            service,
            auth_z_handler: self.clone(),
        }))
    }
}

#[derive(Clone, Debug)]
pub struct AuthZDomain(String);

impl AuthZDomain {
    pub fn new(schema: String) -> Self {
        Self(schema)
    }
}

impl FromRequest for AuthZDomain {
    type Error = Error;
    type Future = Ready<Result<Self, Self::Error>>;

    fn from_request(
        req: &actix_web::HttpRequest,
        _: &mut actix_web::dev::Payload,
    ) -> Self::Future {
        let result = req.extensions().get::<Self>().cloned().ok_or_else(|| {
            log::error!("Please check that the organisation id and workspace id are being properly sent");
            actix_web::error::ErrorInternalServerError("Please check that the organisation id and workspace id are being properly sent")
        });

        ready(result)
    }
}

impl Display for AuthZDomain {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Clone)]
pub enum AuthZManager {
    NoAuth,
    Casbin(Arc<CasbinPolicyEngine>),
}

impl AuthZManager {
    pub async fn init(kms_client: &Option<Client>, app_env: &AppEnv) -> Self {
        match get_auth_z_provider().as_str() {
            "CASBIN" => Self::Casbin(Arc::new(
                CasbinPolicyEngine::management(kms_client, app_env)
                    .await
                    .expect("Failed to initialize Casbin policy engine"),
            )),
            "DISABLED" => Self::NoAuth,
            _ => panic!("Missing/Unknown authorizer."),
        }
    }

    pub fn workspace_endpoints(&self) -> actix_web::Scope {
        match self {
            AuthZManager::Casbin(_) => casbin::workspace_endpoints(),
            AuthZManager::NoAuth => Scope::new(""),
        }
    }

    pub fn admin_endpoints(&self) -> actix_web::Scope {
        match self {
            AuthZManager::Casbin(_) => casbin::admin_endpoints(),
            AuthZManager::NoAuth => Scope::new(""),
        }
    }

    pub fn org_endpoints(&self) -> actix_web::Scope {
        match self {
            AuthZManager::Casbin(_) => casbin::org_endpoints(),
            AuthZManager::NoAuth => Scope::new(""),
        }
    }

    fn try_get_casbin_policy_engine(
        &self,
    ) -> superposition::Result<Arc<CasbinPolicyEngine>> {
        match self {
            AuthZManager::Casbin(engine) => Ok(engine.clone()),
            AuthZManager::NoAuth => {
                Err(unexpected_error!("CasbinPolicyEngine not found."))
            }
        }
    }
}
