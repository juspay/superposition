//! Stub — real implementation in Task 13.
use std::{net::SocketAddr, sync::Arc};
use prometheus::Registry;
pub fn spawn_metrics_server(
    _registry: Arc<Registry>,
    _bind: SocketAddr,
) -> std::io::Result<actix_web::dev::Server> {
    unimplemented!("Task 13")
}
