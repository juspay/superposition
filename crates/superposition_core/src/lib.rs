uniffi::setup_scaffolding!("superposition_client");

pub mod config;
pub mod experiment;
pub mod ffi;
pub mod ffi_legacy;
pub mod format;
pub mod helpers;
pub mod toml;
pub mod validations;

pub use config::{eval_config, eval_config_with_reasoning, merge, MergeStrategy};
pub use experiment::{
    get_applicable_variants, get_satisfied_experiments, Experiments, FfiExperiment,
};
pub use ffi_legacy::{
    core_free_string, core_get_resolved_config, core_get_resolved_config_with_reasoning,
};
pub use superposition_types::Config;
pub use toml::{parse_toml_config, serialize_to_toml, TomlError};
