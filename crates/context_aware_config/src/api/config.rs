mod handlers;
use std::{future, ops::Deref};

use actix_web::FromRequest;
pub use handlers::endpoints;
mod helpers;

#[derive(Debug, Clone, Copy)]
struct XConfigVersion(i64);

impl FromRequest for XConfigVersion {
    type Error = actix_web::Error;
    type Future = future::Ready<Result<Self, Self::Error>>;

    fn from_request(
        req: &actix_web::HttpRequest,
        _payload: &mut actix_web::dev::Payload,
    ) -> Self::Future {
        if let Some(version) = req
            .headers()
            .get("x-config-version")
            .and_then(|v| v.to_str().ok())
            // TODO Debug/Error log should be added here.
            .and_then(|s| s.parse::<i64>().ok())
        {
            return future::ready(Ok(XConfigVersion(version)));
        }
        future::ready(Err(actix_web::error::ErrorBadRequest(
            "Missing/Invalid x-config-version header",
        )))
    }
}

#[derive(Debug, Clone)]
struct XConfigPrefix(String);

impl FromRequest for XConfigPrefix {
    type Error = actix_web::Error;
    type Future = future::Ready<Result<Self, Self::Error>>;

    fn from_request(
        req: &actix_web::HttpRequest,
        _payload: &mut actix_web::dev::Payload,
    ) -> Self::Future {
        if let Some(prefix) = req
            .headers()
            .get("x-config-prefix")
            .and_then(|v| v.to_str().ok())
        {
            return future::ready(Ok(XConfigPrefix(prefix.to_owned())));
        }
        future::ready(Err(actix_web::error::ErrorBadRequest(
            "Missing x-config-prefix header",
        )))
    }
}
