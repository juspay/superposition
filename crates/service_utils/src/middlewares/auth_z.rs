mod authorization;
// mod casbin;
mod no_auth;

use std::{
    future::{ready, Ready},
    sync::Arc,
};

use actix_web::{
    dev::{forward_ready, Payload, Service, ServiceRequest, ServiceResponse, Transform},
    error::{ErrorForbidden, ErrorInternalServerError},
    Error, FromRequest, HttpMessage, HttpRequest, Scope,
};
use authorization::Authorizer;
use aws_sdk_kms::Client;
// use casbin::CasbinPolicyEngine;
use futures_util::future::LocalBoxFuture;
use superposition_types::User;

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
    type Error = actix_web::error::Error;

    type Future = LocalBoxFuture<'static, Result<Self, Self::Error>>;

    fn from_request(req: &HttpRequest, _: &mut Payload) -> Self::Future {
        let auth_z_handler = req
            .extensions()
            .get::<AuthZHandler>()
            .expect("AuthZHandler not found")
            .clone();

        let resource = *req
            .extensions()
            .get::<Resource>()
            .expect("Resource not found");

        let workspace_context = req
            .extensions()
            .get::<WorkspaceContext>()
            .expect("Workspace Context not found")
            .clone();

        let user = match req.extensions().get::<User>() {
            Some(user) => user.clone(),
            None => {
                return Box::pin(async {
                    Err(actix_web::error::ErrorUnauthorized(
                        "User not authenticated.",
                    ))
                })
            }
        };

        Box::pin(async move {
            let is_allowed = auth_z_handler
                .0
                .is_allowed(&workspace_context, &user, &resource, &A::get(), None)
                .await;

            match is_allowed {
                Err(e) => Err(ErrorInternalServerError(format!(
                    "Error checking authorization: {e}",
                ))),
                Ok(is_allowed) => {
                    if is_allowed {
                        Ok(AuthZ::<A> {
                            action: std::marker::PhantomData,
                        })
                    } else {
                        Err(ErrorForbidden(
                            "You are not authorized to perform this action.",
                        ))
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

impl AuthZHandler {
    pub fn endpoints(&self) -> Scope {
        self.0.management_routes()
    }

    pub async fn init(_kms_client: &Option<Client>, _app_env: &AppEnv) -> Self {
        let auth_provider: String = get_from_env_unsafe("AUTH_Z_PROVIDER").unwrap();

        let ap: Arc<dyn Authorizer> = match auth_provider.as_str() {
            // "CASBIN" => {
            //     Arc::new(CasbinPolicyEngine::new(kms_client, app_env).await.unwrap())
            // }
            "DISABLED" => Arc::new(no_auth::NoAuth),
            _ => panic!("Missing/Unknown authorizer."),
        };
        Self(ap)
    }

    pub fn new(auth_z_handler: Arc<dyn Authorizer>) -> Self {
        Self(auth_z_handler)
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
