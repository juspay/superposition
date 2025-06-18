mod authorization;
// mod casbin;
mod no_auth;

use std::{
    future::{ready, Ready},
    sync::Arc,
};

use actix_web::{
    dev::{forward_ready, Payload, Service, ServiceRequest, ServiceResponse, Transform},
    Error, FromRequest, HttpMessage, HttpRequest, Scope,
};
use authorization::Authorizer;
use aws_sdk_kms::Client;
// use casbin::CasbinPolicyEngine;
use futures_util::future::LocalBoxFuture;
use no_auth::NoAuth;
use superposition_macros::{forbidden, unexpected_error};
use superposition_types::{result as superposition, User};

use crate::{
    helpers::get_from_env_unsafe,
    service::types::{AppEnv, Resource, WorkspaceContext},
};

pub trait Action: Send + Sync + 'static {
    fn get() -> String;
}

pub struct AuthZ<A: Action> {
    action: std::marker::PhantomData<A>,
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
                })
            }
        };

        let resource = match req.extensions().get::<Resource>() {
            Some(resource) => *resource,
            None => {
                return Box::pin(async {
                    Err(unexpected_error!(
                        "Resource not found in request extensions."
                    ))
                });
            }
        };

        let workspace_context = match req.extensions().get::<WorkspaceContext>() {
            Some(context) => context.clone(),
            None => {
                return Box::pin(async {
                    Err(unexpected_error!(
                        "Workspace Context not found in request extensions."
                    ))
                });
            }
        };

        let user = match req.extensions().get::<User>() {
            Some(user) => user.clone(),
            None => {
                return Box::pin(async { Err(forbidden!("User not authenticated.")) })
            }
        };

        Box::pin(async move {
            let is_allowed = auth_z_handler
                .0
                .is_allowed(&workspace_context, &user, &resource, &A::get(), None)
                .await;

            match is_allowed {
                Err(e) => Err(unexpected_error!("Error checking authorization: {}", e)),
                Ok(is_allowed) => {
                    if is_allowed {
                        Ok(AuthZ::<A> {
                            action: std::marker::PhantomData,
                        })
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

#[derive(Clone)]
pub struct AuthZHandler(Arc<dyn Authorizer>);

fn get_auth_z_provider() -> String {
    get_from_env_unsafe("AUTH_Z_PROVIDER").unwrap()
}

impl AuthZHandler {
    pub async fn init(_kms_client: &Option<Client>, _app_env: &AppEnv) -> Self {
        let ap: Arc<dyn Authorizer> = match get_auth_z_provider().as_str() {
            // "CASBIN" => Arc::new(
            //     CasbinPolicyEngine::new(kms_client, app_env, None)
            //         .await
            //         .unwrap(),
            // ),
            "DISABLED" => Arc::new(NoAuth),
            _ => panic!("Missing/Unknown authorizer."),
        };
        Self(ap)
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

#[derive(Clone)]
pub enum AuthZManager {
    NoAuth,
    // Casbin(Arc<CasbinPolicyEngine>),
}

impl AuthZManager {
    pub async fn init(_kms_client: &Option<Client>, _app_env: &AppEnv) -> Self {
        match get_auth_z_provider().as_str() {
            // "CASBIN" => Self::Casbin(Arc::new(
            //     CasbinPolicyEngine::management(&kms_client, &app_env)
            //         .await
            //         .expect("Failed to initialize Casbin policy engine"),
            // )),
            "DISABLED" => Self::NoAuth,
            _ => panic!("Missing/Unknown authorizer."),
        }
    }

    pub fn endpoints(&self) -> actix_web::Scope {
        match self {
            // AuthZManager::Casbin(_) => casbin::endpoints(),
            AuthZManager::NoAuth => Scope::new(""),
        }
    }

    // pub(self) fn try_get_casbin_policy_engine(
    //     &self,
    // ) -> superposition::Result<Arc<CasbinPolicyEngine>> {
    //     match self {
    //         AuthZManager::Casbin(engine) => Ok(engine.clone()),
    //         AuthZManager::NoAuth => {
    //             Err(unexpected_error!("CasbinPolicyEngine not found."))
    //         }
    //     }
    // }
}
