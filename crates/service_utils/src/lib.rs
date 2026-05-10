#![deny(unused_crate_dependencies)]
// Re-exported so T4+ modules can use them without triggering
// the `unused_crate_dependencies` lint while no source yet imports them.
use humantime as _;
use opentelemetry as _;
use opentelemetry_otlp as _;
use opentelemetry_prometheus as _;
use opentelemetry_sdk as _;
use opentelemetry_semantic_conventions as _;
use prometheus as _;
use tokio_metrics as _;
pub mod aws;
pub mod db;
pub mod encryption;
pub mod extensions;
pub mod helpers;
pub mod middlewares;
pub mod redis;
pub mod registry;
pub mod service;
pub mod workspace_lock;
