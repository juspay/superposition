pub mod handlers;
pub mod types;

use actix_web::Scope;

pub fn endpoints() -> Scope {
    actix_web::web::scope("/ai").configure(handlers::configure)
}
