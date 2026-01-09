mod handlers;
mod key_rotation;

use actix_web::Scope;

pub fn endpoints() -> Scope {
    handlers::endpoints()
}
