#![deny(unused_crate_dependencies)]
#[cfg(test)]
use criterion as _;

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
    core_parse_config_file_with_filters, core_provider_cache_filter_config,
    core_provider_cache_filter_experiment, core_provider_cache_get_applicable_variants,
};

// Re-export format module and types
pub use format::{
    json::JsonFormat, parse_config_file_with_filters, toml::TomlFormat, ConfigFormat,
    FormatError,
};

// Re-export Config type
pub use superposition_types::Config;
