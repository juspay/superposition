#![deny(unused_crate_dependencies)]
uniffi::setup_scaffolding!("superposition_client");
pub mod config;
pub mod experiment;
pub mod ffi;
pub mod ffi_legacy;
pub mod format;
pub mod helpers;
pub mod validations;

// Re-export main config functions
pub use config::{eval_config, eval_config_with_reasoning, merge, MergeStrategy};

// Re-export experiment functions
pub use experiment::{
    get_applicable_variants, get_satisfied_experiments, Experiments, FfiExperiment,
};

// Re-export legacy FFI functions
pub use ffi_legacy::{
    core_free_string, core_get_resolved_config, core_get_resolved_config_with_reasoning,
    core_parse_json_config, core_parse_toml_config,
};

// Re-export format module and types
pub use format::{json::JsonFormat, toml::TomlFormat, ConfigFormat, FormatError};

// Re-export Config type
pub use superposition_types::Config;
