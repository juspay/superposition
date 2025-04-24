use std::future::{ready, Ready};
use superposition_types::result as superposition;

use actix_web::{
    dev::{forward_ready, Service, ServiceRequest, ServiceResponse, Transform},
    error,
    web::Data,
    Error, HttpMessage,
};
use diesel::Connection;
use futures_util::future::LocalBoxFuture;
use service_utils::service::types::{AppState, DbConnection};
use std::rc::Rc;

pub struct ConfigVersionMiddlewareFactory;
impl<S, B> Transform<S, ServiceRequest> for ConfigVersionMiddlewareFactory
where
    S: Service<ServiceRequest, Response = ServiceResponse<B>, Error = Error> + 'static,
    S::Future: 'static,
    B: 'static,
{
    type Response = ServiceResponse<B>;
    type Error = Error;
    type InitError = ();
    type Transform = ConfigVersionMiddleware<S>;
    type Future = Ready<Result<Self::Transform, Self::InitError>>;

    fn new_transform(&self, service: S) -> Self::Future {
        ready(Ok(ConfigVersionMiddleware {
            service: Rc::new(service),
        }))
    }
}

pub struct ConfigVersionMiddleware<S> {
    service: Rc<S>,
}

impl<S, B> Service<ServiceRequest> for ConfigVersionMiddleware<S>
where
    S: Service<ServiceRequest, Response = ServiceResponse<B>, Error = Error> + 'static,
    S::Future: 'static,
    B: 'static,
{
    type Response = ServiceResponse<B>;
    type Error = Error;
    type Future = LocalBoxFuture<'static, Result<Self::Response, Self::Error>>;

    forward_ready!(service);

    fn call(&self, req: ServiceRequest) -> Self::Future {
        let srv = self.service.clone();

        Box::pin(async move {
            let app_state = match req.app_data::<Data<AppState>>() {
                Some(val) => val,
                None => {
                    log::error!("app state not set");
                    return Err(error::ErrorInternalServerError(""));
                }
            };
            let DbConnection(mut db_conn) =
                DbConnection::from_request_sync(req.request())?;

            let rt = actix_rt::Runtime::new().map_err(|err| {
                log::error!("failed to create new runtime: {err}");
                error::ErrorInternalServerError("")
            })?;
            db_conn.transaction::<ServiceResponse<B>, _, _>(
                |transaction_conn| {
                    req.extensions_mut().insert(transaction_conn);
                    let result = rt.block_on(srv.call(req));
                    result
                    // let version_id = add_config_version(&app_state, tags, transaction_conn)?;
                },
            );

            let res = srv.call(req).await?;

            Ok(res)
        })
    }
}
