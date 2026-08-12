// src/ffi.rs
use std::collections::HashMap;
use std::ffi::{c_char, CStr, CString};
use std::ptr;

use serde_json::{Map, Value};
use superposition_types::{Context, DimensionInfo, Overrides, PrefixList};

use crate::config::{self, MergeStrategy};
use crate::experiment::{ExperimentConfig, ExperimentGroups, ExperimentationArgs};
use crate::ffi::ProviderCache;
use crate::{get_applicable_variants, Experiments, FfiExperiment};

#[no_mangle]
pub extern "C" fn core_provider_cache_new() -> *mut ProviderCache {
    Box::into_raw(Box::new(ProviderCache::default()))
}

/// # Safety
///
/// `handle` must be a pointer previously returned by `core_provider_cache_new` and
/// must not be used after this call.
#[no_mangle]
pub unsafe extern "C" fn core_provider_cache_free(handle: *mut ProviderCache) {
    if !handle.is_null() {
        drop(Box::from_raw(handle));
    }
}

/// # Safety
///
/// `handle` must be a valid pointer from `core_provider_cache_new`. All JSON pointer
/// arguments must be valid null-terminated C strings. `ebuf` must be a sufficiently
/// large buffer for error messages.
#[no_mangle]
pub unsafe extern "C" fn core_provider_cache_init_config(
    handle: *mut ProviderCache,
    default_config_json: *const c_char,
    contexts_json: *const c_char,
    overrides_json: *const c_char,
    dimensions_json: *const c_char,
    ebuf: *mut c_char,
) {
    if handle.is_null() {
        copy_string(ebuf, "handle is null");
        return;
    }

    let default_config = match parse_json::<Map<String, Value>>(default_config_json) {
        Ok(v) => v,
        Err(e) => {
            copy_string(ebuf, format!("Failed to parse default_config: {}", e));
            return;
        }
    };
    let contexts = match parse_json::<Vec<Context>>(contexts_json) {
        Ok(v) => v,
        Err(e) => {
            copy_string(ebuf, format!("Failed to parse contexts: {}", e));
            return;
        }
    };
    let overrides = match parse_json::<HashMap<String, Overrides>>(overrides_json) {
        Ok(v) => v,
        Err(e) => {
            copy_string(ebuf, format!("Failed to parse overrides: {}", e));
            return;
        }
    };
    let dimensions = match parse_json::<HashMap<String, DimensionInfo>>(dimensions_json) {
        Ok(v) => v,
        Err(e) => {
            copy_string(ebuf, format!("Failed to parse dimensions: {}", e));
            return;
        }
    };

    let cache = &*handle;
    match cache.data.lock() {
        Ok(mut d) => {
            d.config.default_configs = default_config.into();
            d.config.contexts = contexts;
            d.config.overrides = overrides;
            d.config.dimensions = dimensions;
        }
        Err(e) => copy_string(ebuf, format!("Failed to acquire cache lock: {}", e)),
    }
}

/// # Safety
///
/// `handle` must be a valid pointer from `core_provider_cache_new`. All JSON pointer
/// arguments must be valid null-terminated C strings. `ebuf` must be a sufficiently
/// large buffer for error messages.
#[no_mangle]
pub unsafe extern "C" fn core_provider_cache_init_experiments(
    handle: *mut ProviderCache,
    experiments_json: *const c_char,
    experiment_groups_json: *const c_char,
    ebuf: *mut c_char,
) {
    if handle.is_null() {
        copy_string(ebuf, "handle is null");
        return;
    }

    let experiments = match parse_json::<Vec<FfiExperiment>>(experiments_json) {
        Ok(v) => v,
        Err(e) => {
            copy_string(ebuf, format!("Failed to parse experiments: {}", e));
            return;
        }
    };
    let experiment_groups = match parse_json::<ExperimentGroups>(experiment_groups_json) {
        Ok(v) => v,
        Err(e) => {
            copy_string(ebuf, format!("Failed to parse experiment_groups: {}", e));
            return;
        }
    };

    let cache = &*handle;
    match cache.data.lock() {
        Ok(mut d) => {
            d.experiment = Some(ExperimentConfig {
                experiments,
                experiment_groups,
            });
        }
        Err(e) => copy_string(ebuf, format!("Failed to acquire cache lock: {}", e)),
    }
}

/// # Safety
///
/// `handle` must be a valid pointer from `core_provider_cache_new`. All non-null pointer
/// arguments must be valid null-terminated C strings. `ebuf` must be a sufficiently
/// large buffer for error messages. The returned pointer must be freed with `core_free_string`.
#[no_mangle]
pub unsafe extern "C" fn core_provider_cache_eval_config(
    handle: *mut ProviderCache,
    query_data_json: *const c_char,
    merge_strategy_str: *const c_char,
    filter_prefixes_json: *const c_char,
    filter_exclude_prefixes_json: *const c_char,
    targeting_key: *const c_char,
    ebuf: *mut c_char,
) -> *mut c_char {
    if handle.is_null() {
        copy_string(ebuf, "handle is null");
        return ptr::null_mut();
    }

    let mut query_data = match parse_json::<Map<String, Value>>(query_data_json) {
        Ok(v) => v,
        Err(e) => {
            copy_string(ebuf, format!("Failed to parse query_data: {}", e));
            return ptr::null_mut();
        }
    };
    let merge_strategy = match c_str_to_string(merge_strategy_str) {
        Ok(s) => match s.to_lowercase().as_str() {
            "replace" => MergeStrategy::REPLACE,
            _ => MergeStrategy::MERGE,
        },
        Err(e) => {
            copy_string(ebuf, format!("Failed to parse merge_strategy: {}", e));
            return ptr::null_mut();
        }
    };
    let filter_prefixes: Option<Vec<String>> = if filter_prefixes_json.is_null() {
        None
    } else {
        match parse_json::<Vec<String>>(filter_prefixes_json) {
            Ok(v) => Some(v),
            Err(e) => {
                copy_string(ebuf, format!("Failed to parse filter_prefixes: {}", e));
                return ptr::null_mut();
            }
        }
    };

    let filter_exclude_prefixes: Option<Vec<String>> =
        if filter_exclude_prefixes_json.is_null() {
            None
        } else {
            match parse_json::<Vec<String>>(filter_exclude_prefixes_json) {
                Ok(v) => Some(v),
                Err(e) => {
                    copy_string(
                        ebuf,
                        format!("Failed to parse filter_exclude_prefixes: {}", e),
                    );
                    return ptr::null_mut();
                }
            }
        };

    let tkey: Option<String> = if targeting_key.is_null() {
        None
    } else {
        match c_str_to_string(targeting_key) {
            Ok(s) if !s.is_empty() => Some(s),
            _ => None,
        }
    };

    let cache = &*handle;
    let data = match cache.data.lock() {
        Ok(d) => d,
        Err(e) => {
            copy_string(ebuf, format!("Failed to acquire cache lock: {}", e));
            return ptr::null_mut();
        }
    };

    if let Some(ref experiment_config) = data.experiment {
        if (!experiment_config.experiments.is_empty()
            || !experiment_config.experiment_groups.is_empty())
            && tkey.as_ref().is_some_and(|key| !key.is_empty())
        {
            let variants = get_applicable_variants(
                &data.config.dimensions,
                experiment_config.experiments.clone(),
                &experiment_config.experiment_groups,
                &query_data,
                tkey.as_deref().unwrap_or(""),
                filter_prefixes.clone(),
                filter_exclude_prefixes.clone(),
            );
            query_data.insert("variantIds".to_string(), variants.into());
        }
    }

    match config::eval_config(
        data.config.default_configs.inner().clone(),
        &data.config.contexts,
        &data.config.overrides,
        &data.config.dimensions,
        &query_data,
        merge_strategy,
        filter_prefixes,
        filter_exclude_prefixes,
    ) {
        Ok(result) => match serde_json::to_string(&result) {
            Ok(json_str) => string_to_c_str(json_str),
            Err(e) => {
                copy_string(ebuf, format!("Failed to serialize result: {}", e));
                ptr::null_mut()
            }
        },
        Err(e) => {
            copy_string(ebuf, e);
            ptr::null_mut()
        }
    }
}

fn c_str_to_string(s: *const c_char) -> Result<String, String> {
    if s.is_null() {
        return Err("Null pointer encountered while converting".into());
    }

    unsafe {
        CStr::from_ptr(s)
            .to_str()
            .map(String::from)
            .map_err(|e| format!("Invalid UTF-8: {}", e))
    }
}

fn parse_json<T: serde::de::DeserializeOwned>(s: *const c_char) -> Result<T, String> {
    let json_str = c_str_to_string(s)?;
    serde_json::from_str(&json_str).map_err(|e| format!("Invalid JSON: {}", e))
}

/// Parse an optional JSON argument: a null pointer means `None`, otherwise parse as `T`.
fn parse_optional_json<T: serde::de::DeserializeOwned>(
    s: *const c_char,
) -> Result<Option<T>, String> {
    if s.is_null() {
        Ok(None)
    } else {
        parse_json::<T>(s).map(Some)
    }
}

fn string_to_c_str(s: String) -> *mut c_char {
    CString::new(s).unwrap().into_raw()
}

unsafe fn copy_string(to: *mut c_char, from: impl AsRef<str>) {
    let from = from.as_ref();
    let cstr = CString::new(from).unwrap();
    let src = cstr.as_ptr();
    // REVIEW Truncate to 256 chars?
    ptr::copy_nonoverlapping(src, to, from.len() + 1 /*+1 for null byte.*/);
}

/// Filter the cache's stored config by dimension data / prefixes, returning the filtered Config as
/// JSON. Mirrors the UniFFI `ProviderCache::filter_config`.
///
/// # Safety
///
/// `handle` must be a valid pointer from `core_provider_cache_new`. `dimension_data_json`,
/// `prefix_json` and `exclude_prefix_json` are each nullable (null means "no filter"). `ebuf` must be
/// a sufficiently large buffer for error messages.
///
/// # Memory Management
/// Caller must free the returned string using `core_free_string`.
#[no_mangle]
pub unsafe extern "C" fn core_provider_cache_filter_config(
    handle: *mut ProviderCache,
    dimension_data_json: *const c_char,
    prefix_json: *const c_char,
    exclude_prefix_json: *const c_char,
    ebuf: *mut c_char,
) -> *mut c_char {
    if handle.is_null() {
        copy_string(ebuf, "handle is null");
        return ptr::null_mut();
    }

    let dimension_data = match parse_optional_json::<Map<String, Value>>(dimension_data_json) {
        Ok(v) => v,
        Err(e) => {
            copy_string(ebuf, format!("Failed to parse dimension_data: {}", e));
            return ptr::null_mut();
        }
    };
    let prefix = match parse_optional_json::<Vec<String>>(prefix_json) {
        Ok(v) => v,
        Err(e) => {
            copy_string(ebuf, format!("Failed to parse prefix: {}", e));
            return ptr::null_mut();
        }
    };
    let exclude_prefix = match parse_optional_json::<Vec<String>>(exclude_prefix_json) {
        Ok(v) => v,
        Err(e) => {
            copy_string(ebuf, format!("Failed to parse exclude_prefix: {}", e));
            return ptr::null_mut();
        }
    };

    let cache = &*handle;
    match cache.filter_config_inner(dimension_data.as_ref(), prefix, exclude_prefix) {
        Ok(config) => match serde_json::to_string(&config) {
            Ok(json_str) => string_to_c_str(json_str),
            Err(e) => {
                copy_string(ebuf, format!("Failed to serialize result: {}", e));
                ptr::null_mut()
            }
        },
        Err(e) => {
            copy_string(ebuf, e);
            ptr::null_mut()
        }
    }
}

/// Filter the cache's stored experiments by dimension data / prefixes, returning the filtered
/// ExperimentConfig as JSON. `partial_apply` selects candidate (true) vs matching (false) semantics.
/// Mirrors the UniFFI `ProviderCache::filter_experiment`.
///
/// # Safety
///
/// See `core_provider_cache_filter_config`. Additionally requires experiments to have been loaded via
/// `core_provider_cache_init_experiments`.
///
/// # Memory Management
/// Caller must free the returned string using `core_free_string`.
#[no_mangle]
pub unsafe extern "C" fn core_provider_cache_filter_experiment(
    handle: *mut ProviderCache,
    dimension_data_json: *const c_char,
    prefix_json: *const c_char,
    exclude_prefix_json: *const c_char,
    partial_apply: bool,
    ebuf: *mut c_char,
) -> *mut c_char {
    if handle.is_null() {
        copy_string(ebuf, "handle is null");
        return ptr::null_mut();
    }

    let dimension_data = match parse_optional_json::<Map<String, Value>>(dimension_data_json) {
        Ok(v) => v.unwrap_or_default(),
        Err(e) => {
            copy_string(ebuf, format!("Failed to parse dimension_data: {}", e));
            return ptr::null_mut();
        }
    };
    let prefix = match parse_optional_json::<Vec<String>>(prefix_json) {
        Ok(v) => v,
        Err(e) => {
            copy_string(ebuf, format!("Failed to parse prefix: {}", e));
            return ptr::null_mut();
        }
    };
    let exclude_prefix = match parse_optional_json::<Vec<String>>(exclude_prefix_json) {
        Ok(v) => v,
        Err(e) => {
            copy_string(ebuf, format!("Failed to parse exclude_prefix: {}", e));
            return ptr::null_mut();
        }
    };

    let cache = &*handle;
    match cache.filter_experiment_inner(&dimension_data, prefix, exclude_prefix, partial_apply) {
        Ok(exp_config) => match serde_json::to_string(&exp_config) {
            Ok(json_str) => string_to_c_str(json_str),
            Err(e) => {
                copy_string(ebuf, format!("Failed to serialize result: {}", e));
                ptr::null_mut()
            }
        },
        Err(e) => {
            copy_string(ebuf, e);
            ptr::null_mut()
        }
    }
}

/// Get the applicable experiment variant IDs from the cache's stored experiments, returning a JSON
/// array of strings. Mirrors the UniFFI `ProviderCache::get_applicable_variants`.
///
/// # Safety
///
/// See `core_provider_cache_filter_experiment`. `targeting_key` is nullable (null means an empty
/// identifier).
///
/// # Memory Management
/// Caller must free the returned string using `core_free_string`.
#[no_mangle]
pub unsafe extern "C" fn core_provider_cache_get_applicable_variants(
    handle: *mut ProviderCache,
    dimension_data_json: *const c_char,
    prefix_json: *const c_char,
    exclude_prefix_json: *const c_char,
    targeting_key: *const c_char,
    ebuf: *mut c_char,
) -> *mut c_char {
    if handle.is_null() {
        copy_string(ebuf, "handle is null");
        return ptr::null_mut();
    }

    let dimension_data = match parse_optional_json::<Map<String, Value>>(dimension_data_json) {
        Ok(v) => v.unwrap_or_default(),
        Err(e) => {
            copy_string(ebuf, format!("Failed to parse dimension_data: {}", e));
            return ptr::null_mut();
        }
    };
    let prefix = match parse_optional_json::<Vec<String>>(prefix_json) {
        Ok(v) => v,
        Err(e) => {
            copy_string(ebuf, format!("Failed to parse prefix: {}", e));
            return ptr::null_mut();
        }
    };
    let exclude_prefix = match parse_optional_json::<Vec<String>>(exclude_prefix_json) {
        Ok(v) => v,
        Err(e) => {
            copy_string(ebuf, format!("Failed to parse exclude_prefix: {}", e));
            return ptr::null_mut();
        }
    };
    let targeting_key = if targeting_key.is_null() {
        String::new()
    } else {
        match c_str_to_string(targeting_key) {
            Ok(s) => s,
            Err(e) => {
                copy_string(ebuf, format!("Invalid UTF-8 in targeting_key: {}", e));
                return ptr::null_mut();
            }
        }
    };

    let cache = &*handle;
    match cache.get_applicable_variants_inner(
        &dimension_data,
        prefix,
        exclude_prefix,
        &targeting_key,
    ) {
        Ok(variants) => match serde_json::to_string(&variants) {
            Ok(json_str) => string_to_c_str(json_str),
            Err(e) => {
                copy_string(ebuf, format!("Failed to serialize result: {}", e));
                ptr::null_mut()
            }
        },
        Err(e) => {
            copy_string(ebuf, e);
            ptr::null_mut()
        }
    }
}

/// # Safety
///
/// Caller ensures that `ebuf` is a sufficiently long buffer to store the
/// error message.
#[no_mangle]
pub unsafe extern "C" fn core_get_resolved_config(
    default_config_json: *const c_char,
    contexts_json: *const c_char,
    overrides_json: *const c_char,
    dimensions: *const c_char,
    query_data_json: *const c_char,
    merge_strategy_str: *const c_char,
    filter_prefixes_json: *const c_char,
    filter_exclude_prefixes_json: *const c_char,
    experimentation_json: *const c_char,
    ebuf: *mut c_char,
) -> *mut c_char {
    // Parameter validation
    if default_config_json.is_null()
        || contexts_json.is_null()
        || overrides_json.is_null()
        || dimensions.is_null()
        || query_data_json.is_null()
        || merge_strategy_str.is_null()
    {
        copy_string(ebuf, "Null pointer provided in required value");
        return ptr::null_mut();
    }

    // Parse all parameters
    let default_config = match parse_json::<Map<String, Value>>(default_config_json) {
        Ok(config) => config,
        Err(e) => {
            copy_string(ebuf, format!("Failed to parse default_config: {}", e));
            return ptr::null_mut();
        }
    };

    let contexts = match parse_json::<Vec<Context>>(contexts_json) {
        Ok(contexts) => contexts,
        Err(e) => {
            copy_string(ebuf, format!("Failed to parse contexts: {}", e));
            return ptr::null_mut();
        }
    };

    let overrides = match parse_json::<HashMap<String, Overrides>>(overrides_json) {
        Ok(overrides) => overrides,
        Err(e) => {
            copy_string(ebuf, format!("Failed to parse overrides: {}", e));
            return ptr::null_mut();
        }
    };

    let mut query_data = match parse_json::<Map<String, Value>>(query_data_json) {
        Ok(data) => data,
        Err(e) => {
            copy_string(ebuf, format!("Failed to parse query_data: {}", e));
            return ptr::null_mut();
        }
    };

    let merge_strategy = match c_str_to_string(merge_strategy_str) {
        Ok(strategy) => match strategy.to_lowercase().as_str() {
            "merge" => MergeStrategy::MERGE,
            "replace" => MergeStrategy::REPLACE,
            _ => MergeStrategy::default(),
        },
        Err(e) => {
            copy_string(ebuf, format!("Failed to parse merge_strategy: {}", e));
            return ptr::null_mut();
        }
    };
    let filter_prefixes: Option<Vec<String>> = if filter_prefixes_json.is_null() {
        None
    } else {
        match parse_json::<Vec<String>>(filter_prefixes_json) {
            Ok(prefixes) => Some(prefixes),
            Err(e) => {
                copy_string(ebuf, format!("Failed to parse filter_prefixes: {}", e));
                return ptr::null_mut();
            }
        }
    };

    let filter_exclude_prefixes: Option<Vec<String>> =
        if filter_exclude_prefixes_json.is_null() {
            None
        } else {
            match parse_json::<Vec<String>>(filter_exclude_prefixes_json) {
                Ok(v) => Some(v),
                Err(e) => {
                    copy_string(
                        ebuf,
                        format!("Failed to parse filter_exclude_prefixes: {}", e),
                    );
                    return ptr::null_mut();
                }
            }
        };

    let experimentation: Option<ExperimentationArgs> = if experimentation_json.is_null() {
        None
    } else {
        match parse_json::<ExperimentationArgs>(experimentation_json) {
            Ok(exp_args) => Some(exp_args),
            Err(e) => {
                copy_string(ebuf, format!("Failed to parse experimentation: {}", e));
                return ptr::null_mut();
            }
        }
    };

    let dimensions = match parse_json::<HashMap<String, DimensionInfo>>(dimensions) {
        Ok(dimensions) => dimensions,
        Err(e) => {
            copy_string(ebuf, format!("Failed to parse dimensions: {}", e));
            return ptr::null_mut();
        }
    };

    if let Some(e_args) = experimentation {
        let identifier = e_args.targeting_key;

        let variants = get_applicable_variants(
            &dimensions,
            e_args.experiments,
            &e_args.experiment_groups,
            &query_data,
            &identifier,
            filter_prefixes.clone(),
            filter_exclude_prefixes.clone(),
        );

        query_data.insert("variantIds".to_string(), variants.into());
    }

    // Call pure config resolution logic
    match config::eval_config(
        default_config,
        &contexts,
        &overrides,
        &dimensions,
        &query_data,
        merge_strategy,
        filter_prefixes,
        filter_exclude_prefixes,
    ) {
        Ok(result) => match serde_json::to_string(&result) {
            Ok(json_str) => string_to_c_str(json_str),
            Err(e) => {
                copy_string(ebuf, format!("Failed to serialize result: {}", e));
                ptr::null_mut()
            }
        },
        Err(e) => {
            copy_string(ebuf, e);
            ptr::null_mut()
        }
    }
}

/// # Safety
///
/// Caller ensures that `ebuf` is a sufficiently long buffer to store the
/// error message.
#[no_mangle]
pub unsafe extern "C" fn core_get_resolved_config_with_reasoning(
    default_config_json: *const c_char,
    contexts_json: *const c_char,
    overrides_json: *const c_char,
    dimensions: *const c_char,
    query_data_json: *const c_char,
    merge_strategy_str: *const c_char,
    filter_prefixes_json: *const c_char,
    filter_exclude_prefixes_json: *const c_char,
    experimentation_json: *const c_char,
    ebuf: *mut c_char,
) -> *mut c_char {
    // Same parameter validation as above...
    if default_config_json.is_null()
        || contexts_json.is_null()
        || overrides_json.is_null()
        || dimensions.is_null()
        || query_data_json.is_null()
        || merge_strategy_str.is_null()
    {
        copy_string(ebuf, "Null pointer provided");
        return ptr::null_mut();
    }

    // Parse parameters (same logic as above)
    let default_config = match parse_json::<Map<String, Value>>(default_config_json) {
        Ok(config) => config,
        Err(e) => {
            copy_string(ebuf, format!("Failed to parse default_config: {}", e));
            return ptr::null_mut();
        }
    };

    let contexts = match parse_json::<Vec<Context>>(contexts_json) {
        Ok(contexts) => contexts,
        Err(e) => {
            copy_string(ebuf, format!("Failed to parse contexts: {}", e));
            return ptr::null_mut();
        }
    };

    let overrides = match parse_json::<HashMap<String, Overrides>>(overrides_json) {
        Ok(overrides) => overrides,
        Err(e) => {
            copy_string(ebuf, format!("Failed to parse overrides: {}", e));
            return ptr::null_mut();
        }
    };

    let mut query_data = match parse_json::<Map<String, Value>>(query_data_json) {
        Ok(data) => data,
        Err(e) => {
            copy_string(ebuf, format!("Failed to parse query_data: {}", e));
            return ptr::null_mut();
        }
    };

    let merge_strategy = match c_str_to_string(merge_strategy_str) {
        Ok(strategy) => match strategy.to_lowercase().as_str() {
            "merge" => MergeStrategy::MERGE,
            "replace" => MergeStrategy::REPLACE,
            _ => MergeStrategy::default(),
        },
        Err(e) => {
            copy_string(ebuf, format!("Failed to parse merge_strategy: {}", e));
            return ptr::null_mut();
        }
    };

    let filter_prefixes: Option<Vec<String>> = if filter_prefixes_json.is_null() {
        None
    } else {
        match parse_json::<Vec<String>>(filter_prefixes_json) {
            Ok(prefixes) => Some(prefixes),
            Err(e) => {
                copy_string(ebuf, format!("Failed to parse filter_prefixes: {}", e));
                return ptr::null_mut();
            }
        }
    };

    let filter_exclude_prefixes: Option<Vec<String>> =
        if filter_exclude_prefixes_json.is_null() {
            None
        } else {
            match parse_json::<Vec<String>>(filter_exclude_prefixes_json) {
                Ok(v) => Some(v),
                Err(e) => {
                    copy_string(
                        ebuf,
                        format!("Failed to parse filter_exclude_prefixes: {}", e),
                    );
                    return ptr::null_mut();
                }
            }
        };

    let experimentation: Option<ExperimentationArgs> = if experimentation_json.is_null() {
        None
    } else {
        match parse_json::<ExperimentationArgs>(experimentation_json) {
            Ok(exp_args) => Some(exp_args),
            Err(e) => {
                copy_string(ebuf, format!("Failed to parse experimentation: {}", e));
                return ptr::null_mut();
            }
        }
    };

    let dimensions = match parse_json::<HashMap<String, DimensionInfo>>(dimensions) {
        Ok(dimensions) => dimensions,
        Err(e) => {
            copy_string(ebuf, format!("Failed to parse dimensions: {}", e));
            return ptr::null_mut();
        }
    };

    if let Some(e_args) = experimentation {
        let identifier = e_args.targeting_key;

        let variants = get_applicable_variants(
            &dimensions,
            e_args.experiments,
            &e_args.experiment_groups,
            &query_data,
            &identifier,
            filter_prefixes.clone(),
            filter_exclude_prefixes.clone(),
        );

        query_data.insert("variantIds".to_string(), variants.into());
    }

    // Call config resolution with reasoning
    match config::eval_config_with_reasoning(
        default_config,
        &contexts,
        &overrides,
        &dimensions,
        &query_data,
        merge_strategy,
        filter_prefixes,
        filter_exclude_prefixes,
    ) {
        Ok(result) => match serde_json::to_string(&result) {
            Ok(json_str) => string_to_c_str(json_str),
            Err(e) => {
                copy_string(ebuf, format!("Failed to serialize result: {}", e));
                ptr::null_mut()
            }
        },
        Err(e) => {
            copy_string(ebuf, e);
            ptr::null_mut()
        }
    }
}

// Add helper functions following existing pattern
#[no_mangle]
pub extern "C" fn core_test_connection() -> i32 {
    1 // Return 1 for success
}

/// # Safety
///
/// This function is unsafe because:
/// - `s` must be a valid pointer to a C string previously allocated by this library
/// - `s` must not be null
/// - The caller must ensure `s` is not used after this function is called
/// - Double-free will cause undefined behavior
#[no_mangle]
pub unsafe extern "C" fn core_free_string(s: *mut c_char) {
    if !s.is_null() {
        drop(CString::from_raw(s));
    }
}

/// # Safety
///
/// Caller ensures that `ebuf` is a sufficiently long buffer to store the
/// error message.
#[no_mangle]
pub unsafe extern "C" fn core_get_applicable_variants(
    experiments_json: *const c_char,
    experiment_groups_json: *const c_char,
    dimensions: *const c_char,
    query_data_json: *const c_char,
    identifier: *const c_char,
    filter_prefixes_json: *const c_char,
    filter_exclude_prefixes_json: *const c_char,
    ebuf: *mut c_char,
) -> *mut c_char {
    if experiments_json.is_null() || query_data_json.is_null() || dimensions.is_null() {
        copy_string(ebuf, "Null pointer provided");
        return ptr::null_mut();
    }

    let experiments = match parse_json::<Experiments>(experiments_json) {
        Ok(experiments) => experiments,
        Err(e) => {
            copy_string(ebuf, format!("Failed to parse experiments: {}", e));
            return ptr::null_mut();
        }
    };

    let experiment_groups = match parse_json::<ExperimentGroups>(experiment_groups_json) {
        Ok(groups) => groups,
        Err(e) => {
            copy_string(ebuf, format!("Failed to parse experiment_groups: {}", e));
            return ptr::null_mut();
        }
    };

    let query_data = match parse_json::<Map<String, Value>>(query_data_json) {
        Ok(data) => data,
        Err(e) => {
            copy_string(ebuf, format!("Failed to parse query_data: {}", e));
            return ptr::null_mut();
        }
    };

    let dimensions = match parse_json::<HashMap<String, DimensionInfo>>(dimensions) {
        Ok(dimensions) => dimensions,
        Err(e) => {
            copy_string(ebuf, format!("Failed to parse dimensions: {}", e));
            return ptr::null_mut();
        }
    };

    let filter_prefixes: Option<Vec<String>> = if filter_prefixes_json.is_null() {
        None
    } else {
        match parse_json::<Vec<String>>(filter_prefixes_json) {
            Ok(prefixes) => Some(prefixes),
            Err(e) => {
                copy_string(ebuf, format!("Failed to parse filter_prefixes: {}", e));
                return ptr::null_mut();
            }
        }
    };

    let filter_exclude_prefixes: Option<Vec<String>> =
        if filter_exclude_prefixes_json.is_null() {
            None
        } else {
            match parse_json::<Vec<String>>(filter_exclude_prefixes_json) {
                Ok(v) => Some(v),
                Err(e) => {
                    copy_string(
                        ebuf,
                        format!("Failed to parse filter_exclude_prefixes: {}", e),
                    );
                    return ptr::null_mut();
                }
            }
        };

    let identifier = match c_str_to_string(identifier) {
        Ok(id) => id,
        Err(e) => {
            copy_string(ebuf, format!("Failed to parse identifier: {}", e));
            return ptr::null_mut();
        }
    };

    // Call the experimentation logic
    let result = get_applicable_variants(
        &dimensions,
        experiments,
        &experiment_groups,
        &query_data,
        &identifier,
        filter_prefixes,
        filter_exclude_prefixes,
    );
    match serde_json::to_string(&result) {
        Ok(json_str) => string_to_c_str(json_str),
        Err(e) => {
            copy_string(ebuf, format!("Failed to serialize result: {}", e));
            ptr::null_mut()
        }
    }
}

/// Parse a config file (JSON or TOML) and filter it by dimension data / prefixes in one step,
/// returning the filtered Config as JSON.
///
/// Mirrors the UniFFI `ffi_parse_config_file_with_filters`; the JS file data source uses this to
/// honour context and prefix filtering at parse time (matching the Python/Java file sources).
///
/// # Safety
///
/// `file_content` and `format` must be valid null-terminated C strings. `dimension_data_json`,
/// `prefix_json` and `exclude_prefix_json` are each nullable (a null pointer means "no filter").
/// `ebuf` must be a sufficiently large buffer for error messages.
///
/// # Memory Management
/// Caller must free the returned string using `core_free_string`.
#[no_mangle]
pub unsafe extern "C" fn core_parse_config_file_with_filters(
    file_content: *const c_char,
    format: *const c_char,
    dimension_data_json: *const c_char,
    prefix_json: *const c_char,
    exclude_prefix_json: *const c_char,
    ebuf: *mut c_char,
) -> *mut c_char {
    let file_content = match c_str_to_string(file_content) {
        Ok(s) => s,
        Err(e) => {
            copy_string(ebuf, format!("Invalid UTF-8 in file_content: {}", e));
            return ptr::null_mut();
        }
    };
    let format = match c_str_to_string(format) {
        Ok(s) => s,
        Err(e) => {
            copy_string(ebuf, format!("Invalid UTF-8 in format: {}", e));
            return ptr::null_mut();
        }
    };

    // Dimension data comes through as a JSON object of real values (like query data elsewhere).
    let dimension_data = match parse_optional_json::<Map<String, Value>>(dimension_data_json) {
        Ok(v) => v,
        Err(e) => {
            copy_string(ebuf, format!("Failed to parse dimension_data: {}", e));
            return ptr::null_mut();
        }
    };
    let prefix = match parse_optional_json::<Vec<String>>(prefix_json) {
        Ok(v) => v,
        Err(e) => {
            copy_string(ebuf, format!("Failed to parse prefix: {}", e));
            return ptr::null_mut();
        }
    };
    let exclude_prefix = match parse_optional_json::<Vec<String>>(exclude_prefix_json) {
        Ok(v) => v,
        Err(e) => {
            copy_string(ebuf, format!("Failed to parse exclude_prefix: {}", e));
            return ptr::null_mut();
        }
    };

    let prefix_list = prefix.map(PrefixList::from_iter);
    let exclude_prefix_list = exclude_prefix.map(PrefixList::from_iter);

    let filtered = match crate::parse_config_file_with_filters(
        &file_content,
        &format,
        dimension_data.as_ref(),
        prefix_list.as_ref(),
        exclude_prefix_list.as_ref(),
    ) {
        Ok(c) => c,
        Err(e) => {
            copy_string(ebuf, e.to_string());
            return ptr::null_mut();
        }
    };

    match serde_json::to_string(&filtered) {
        Ok(json_str) => string_to_c_str(json_str),
        Err(e) => {
            copy_string(ebuf, format!("JSON serialization error: {}", e));
            ptr::null_mut()
        }
    }
}
