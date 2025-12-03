mod handlers;
mod key_rotation;

use actix_web::Scope;

pub fn endpoints() -> Scope {
    actix_web::web::scope("")
        .service(handlers::endpoints())
        .service(key_rotation::endpoints())
}
