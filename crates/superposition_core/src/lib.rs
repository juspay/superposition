uniffi::setup_scaffolding!("superposition_client");

pub mod config;
pub mod experiment;
pub mod ffi;
pub mod ffi_legacy;
pub mod format;
pub mod helpers;
pub mod toml;
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
};

// Re-export format module and types
pub use format::{
    json_impl::{parse_json_config, serialize_to_json},
    toml_impl::{
        parse_toml_config, serialize_to_toml, DetailedConfigToml, DimensionInfoToml,
    },
    ConfigFormat, FormatError,
};

// Re-export TOML-specific error type for backward compatibility
pub use format::FormatError as TomlError;

// Re-export Config type
pub use superposition_types::Config;
