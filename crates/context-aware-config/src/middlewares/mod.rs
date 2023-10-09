pub mod audit_response_header;

use actix_web::{dev::RequestHead, http::header::HeaderValue};
pub fn cors() -> actix_cors::Cors {
    let origins_env_name = "MJOS_ALLOWED_ORIGINS";
    let allowed_origins: Vec<String> = std::env::var(origins_env_name)
        .expect(&format!("{origins_env_name} env"))
        .split(",")
        .map(String::from)
        .collect();
    let validate_origin = move |req_origin: &HeaderValue, _req: &RequestHead| {
        if let Ok(req_origin) = req_origin.to_str() {
            allowed_origins.contains(&req_origin.into())
        } else {
            log::error!("string parsing of req_origin {req_origin:?} failed");
            false
        }
    };
    actix_cors::Cors::default()
        //TODO move this to allowed_origin_fn once middlewares which put tenant
        //in request extension are attached
        .allowed_origin_fn(validate_origin)
        .allow_any_method()
        .allow_any_header()
}
