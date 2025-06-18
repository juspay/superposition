// This crate provides efficient , cached resolution of configurations based on context conditions and overrides
// , with FFI bindings fir use in non-Rust applications
uniffi::setup_scaffolding!("superposition_core");

pub mod config;
pub mod experiment;
pub mod ffi;

pub use config::{eval_config, eval_config_with_reasoning, merge, MergeStrategy};

// pub use ffi::{
//     core_free_string, core_get_resolved_config, core_get_resolved_config_with_reasoning,
//     core_last_error_length, core_last_error_message,
// };

pub const VERSION: &str = env!("CARGO_PKG_VERSION");
