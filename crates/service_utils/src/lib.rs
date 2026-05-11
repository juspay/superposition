#![deny(unused_crate_dependencies)]
// tokio_metrics is only referenced inside cfg(tokio_unstable) code; suppress
// the lint when building without that flag.
#[cfg(not(tokio_unstable))]
use tokio_metrics as _;
pub mod aws;
pub mod db;
pub mod encryption;
pub mod extensions;
pub mod helpers;
pub mod middlewares;
pub mod observability;
pub mod redis;
pub mod registry;
pub mod service;
pub mod workspace_lock;
