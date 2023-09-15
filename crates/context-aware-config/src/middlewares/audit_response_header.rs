use std::future::{ready, Ready};

use actix_web::{
    dev::{forward_ready, Service, ServiceRequest, ServiceResponse, Transform},
    http::header::{HeaderName, HeaderValue},
    web::Data,
    Error,
};
use diesel::{ExpressionMethods, QueryDsl, RunQueryDsl};
use futures_util::future::LocalBoxFuture;
use service_utils::service::types::AppState;

use crate::db::schema::event_log::dsl as event_log;
use uuid::Uuid;

#[derive(Clone, Copy, Debug, strum_macros::Display)]
#[strum(serialize_all = "snake_case")]
#[allow(dead_code)]
pub enum TableName {
    Contexts,
    DefaultConfigs,
    Dimensions,
    Experiments,
}

pub struct AuditHeader {
    table_name: TableName,
}

impl AuditHeader {
    pub fn new(table_name: TableName) -> Self {
        AuditHeader { table_name }
    }
}

impl<S, B> Transform<S, ServiceRequest> for AuditHeader
where
    S: Service<ServiceRequest, Response = ServiceResponse<B>, Error = Error>,
    S::Future: 'static,
    B: 'static,
{
    type Response = ServiceResponse<B>;
    type Error = Error;
    type InitError = ();
    type Transform = AuditHeaderMiddleware<S>;
    type Future = Ready<Result<Self::Transform, Self::InitError>>;

    fn new_transform(&self, service: S) -> Self::Future {
        ready(Ok(AuditHeaderMiddleware {
            service,
            table_name: self.table_name,
        }))
    }
}

pub struct AuditHeaderMiddleware<S> {
    service: S,
    table_name: TableName,
}

impl<S, B> Service<ServiceRequest> for AuditHeaderMiddleware<S>
where
    S: Service<ServiceRequest, Response = ServiceResponse<B>, Error = Error>,
    S::Future: 'static,
    B: 'static,
{
    type Response = ServiceResponse<B>;
    type Error = Error;
    type Future = LocalBoxFuture<'static, Result<Self::Response, Self::Error>>;

    forward_ready!(service);

    fn call(&self, req: ServiceRequest) -> Self::Future {
        let app_state = req.app_data::<Data<AppState>>();
        let mut db_conn_option = None;

        if let Some(app_data) = app_state {
            match app_data.db_pool.get() {
                Ok(conn) => db_conn_option = Some(conn),
                Err(_) => log::error!("Failed to get connection"),
            }
        } else {
            log::error!("App State not available");
        }

        let fut = self.service.call(req);
        let table_name = self.table_name;

        Box::pin(async move {
            let mut res = fut.await?;

            if let Some(mut conn) = db_conn_option {
                let uuid = event_log::event_log
                    .select(event_log::id)
                    .filter(event_log::table_name.eq(table_name.to_string()))
                    .order_by(event_log::timestamp.desc())
                    .first::<Uuid>(&mut conn);

                if let Ok(uuid) = uuid {
                    res.headers_mut().insert(
                        HeaderName::from_static("x-audit-id"),
                        HeaderValue::from_str(&uuid.to_string())
                            .unwrap_or_else(|_| HeaderValue::from_static("invalid")),
                    );
                } else {
                    log::error!("Unable to fetch uuid");
                }
            }
            Ok(res)
        })
    }
}