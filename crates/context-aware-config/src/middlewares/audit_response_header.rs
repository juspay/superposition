use std::future::{ready, Ready};

use actix_web::{
    dev::{forward_ready, Service, ServiceRequest, ServiceResponse, Transform},
    http::header::{HeaderName, HeaderValue},
    Error,
};
use diesel::{ExpressionMethods, QueryDsl, RunQueryDsl};
use futures_util::future::LocalBoxFuture;
use service_utils::service::types::DbConnection;

use crate::db::schema::event_log::dsl as event_log;
use std::rc::Rc;
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
    S: Service<ServiceRequest, Response = ServiceResponse<B>, Error = Error> + 'static,
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
            service: Rc::new(service),
            table_name: self.table_name,
        }))
    }
}

pub struct AuditHeaderMiddleware<S> {
    service: Rc<S>,
    table_name: TableName,
}

impl<S, B> Service<ServiceRequest> for AuditHeaderMiddleware<S>
where
    S: Service<ServiceRequest, Response = ServiceResponse<B>, Error = Error> + 'static,
    S::Future: 'static,
    B: 'static,
{
    type Response = ServiceResponse<B>;
    type Error = Error;
    type Future = LocalBoxFuture<'static, Result<Self::Response, Self::Error>>;

    forward_ready!(service);

    fn call(&self, mut req: ServiceRequest) -> Self::Future {
        let srv = self.service.clone();
        let table_name = self.table_name;

        Box::pin(async move {
            let db_conn = req.extract::<DbConnection>().await?;
            let DbConnection(mut conn) = db_conn;

            let mut res = srv.call(req).await?;

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
            Ok(res)
        })
    }
}
