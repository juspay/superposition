// src/ffi.rs
use rand::Rng;
use serde_json::{Map, Value};
use std::cell::RefCell;
use std::collections::HashMap;
use std::ffi::{c_char, CStr, CString};
use std::ptr;

use crate::config::{self, MergeStrategy};
use crate::experiment::ExperimentationArgs;
use crate::{get_applicable_variants, Experiments};
use superposition_types::{Context, DimensionInfo, Overrides};
// Thread-local storage for error handling
thread_local! {
    static LAST_ERROR: RefCell<Option<String>> = const { RefCell::new(None) };
    static ERROR_DETAILS: RefCell<Option<String>> = const {RefCell::new(None)};
}

// Helper functions for FFI
fn set_last_error(err: String) {
    log::error!("FFI core error: {}", err);
    LAST_ERROR.with(|prev| {
        *prev.borrow_mut() = Some(err);
    })
}

fn c_str_to_string(s: *const c_char) -> Result<String, String> {
    if s.is_null() {
        return Err("Null pointer provided".into());
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

fn string_to_c_str(s: String) -> *mut c_char {
    match CString::new(s) {
        Ok(c_str) => c_str.into_raw(),
        Err(e) => {
            set_last_error(format!("Failed to create C string: {}", e));
            ptr::null_mut()
        }
    }
}

// Core config resolution - no client logic
#[no_mangle]
pub extern "C" fn core_get_resolved_config(
    default_config_json: *const c_char,
    contexts_json: *const c_char,
    overrides_json: *const c_char,
    dimensions: *const c_char,
    query_data_json: *const c_char,
    merge_strategy_str: *const c_char,
    filter_prefixes_json: *const c_char,
    experimentation_json: *const c_char,
) -> *mut c_char {
    // Parameter validation
    if default_config_json.is_null()
        || contexts_json.is_null()
        || overrides_json.is_null()
        || dimensions.is_null()
        || query_data_json.is_null()
        || merge_strategy_str.is_null()
    {
        set_last_error("Null pointer provided".into());
        return ptr::null_mut();
    }

    // Parse all parameters
    let default_config = match parse_json::<Map<String, Value>>(default_config_json) {
        Ok(config) => config,
        Err(e) => {
            set_last_error(format!("Failed to parse default_config: {}", e));
            return ptr::null_mut();
        }
    };

    let contexts = match parse_json::<Vec<Context>>(contexts_json) {
        Ok(contexts) => contexts,
        Err(e) => {
            set_last_error(format!("Failed to parse contexts: {}", e));
            return ptr::null_mut();
        }
    };

    let overrides = match parse_json::<HashMap<String, Overrides>>(overrides_json) {
        Ok(overrides) => overrides,
        Err(e) => {
            set_last_error(format!("Failed to parse overrides: {}", e));
            return ptr::null_mut();
        }
    };

    let mut query_data = match parse_json::<Map<String, Value>>(query_data_json) {
        Ok(data) => data,
        Err(e) => {
            set_last_error(format!("Failed to parse query_data: {}", e));
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
            set_last_error(format!("Failed to parse merge_strategy: {}", e));
            return ptr::null_mut();
        }
    };
    let filter_prefixes: Option<Vec<String>> = if filter_prefixes_json.is_null() {
        None
    } else {
        match parse_json::<Vec<String>>(filter_prefixes_json) {
            Ok(prefixes) => Some(prefixes),
            Err(e) => {
                set_last_error(format!("Failed to parse filter_prefixes: {}", e));
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
                set_last_error(format!("Failed to parse experimentation: {}", e));
                return ptr::null_mut();
            }
        }
    };

    if let Some(e_args) = experimentation {
        let toss = e_args
            .targeting_key
            .parse::<i8>()
            .unwrap_or(rand::rng().random_range(0..=99))
            % 100;

        match get_applicable_variants(
            &e_args.experiments,
            &query_data,
            toss,
            filter_prefixes.clone(),
        ) {
            Ok(variants) => {
                query_data.insert("variantIds".to_string(), variants.into());
            }
            Err(e) => {
                set_last_error(format!("Failed to get applicable variants: {}", e));
                return ptr::null_mut();
            }
        }
    }

    let dimensions = match parse_json::<HashMap<String, DimensionInfo>>(dimensions) {
        Ok(dimensions) => dimensions,
        Err(e) => {
            set_last_error(format!("Failed to parse dimensions: {}", e));
            return ptr::null_mut();
        }
    };

    // Call pure config resolution logic
    match config::eval_config(
        default_config,
        &contexts,
        &overrides,
        &dimensions,
        &query_data,
        merge_strategy,
        filter_prefixes,
    ) {
        Ok(result) => match serde_json::to_string(&result) {
            Ok(json_str) => string_to_c_str(json_str),
            Err(e) => {
                set_last_error(format!("Failed to serialize result: {}", e));
                ptr::null_mut()
            }
        },
        Err(e) => {
            set_last_error(e);
            ptr::null_mut()
        }
    }
}

#[no_mangle]
pub extern "C" fn core_get_resolved_config_with_reasoning(
    default_config_json: *const c_char,
    contexts_json: *const c_char,
    overrides_json: *const c_char,
    dimensions: *const c_char,
    query_data_json: *const c_char,
    merge_strategy_str: *const c_char,
    filter_prefixes_json: *const c_char,
    experimentation_json: *const c_char,
) -> *mut c_char {
    // Same parameter validation as above...
    if default_config_json.is_null()
        || contexts_json.is_null()
        || overrides_json.is_null()
        || dimensions.is_null()
        || query_data_json.is_null()
        || merge_strategy_str.is_null()
    {
        set_last_error("Null pointer provided".into());
        return ptr::null_mut();
    }

    // Parse parameters (same logic as above)
    let default_config = match parse_json::<Map<String, Value>>(default_config_json) {
        Ok(config) => config,
        Err(e) => {
            set_last_error(format!("Failed to parse default_config: {}", e));
            return ptr::null_mut();
        }
    };

    let contexts = match parse_json::<Vec<Context>>(contexts_json) {
        Ok(contexts) => contexts,
        Err(e) => {
            set_last_error(format!("Failed to parse contexts: {}", e));
            return ptr::null_mut();
        }
    };

    let overrides = match parse_json::<HashMap<String, Overrides>>(overrides_json) {
        Ok(overrides) => overrides,
        Err(e) => {
            set_last_error(format!("Failed to parse overrides: {}", e));
            return ptr::null_mut();
        }
    };

    let mut query_data = match parse_json::<Map<String, Value>>(query_data_json) {
        Ok(data) => data,
        Err(e) => {
            set_last_error(format!("Failed to parse query_data: {}", e));
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
            set_last_error(format!("Failed to parse merge_strategy: {}", e));
            return ptr::null_mut();
        }
    };

    let filter_prefixes: Option<Vec<String>> = if filter_prefixes_json.is_null() {
        None
    } else {
        match parse_json::<Vec<String>>(filter_prefixes_json) {
            Ok(prefixes) => Some(prefixes),
            Err(e) => {
                set_last_error(format!("Failed to parse filter_prefixes: {}", e));
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
                set_last_error(format!("Failed to parse experimentation: {}", e));
                return ptr::null_mut();
            }
        }
    };

    if let Some(e_args) = experimentation {
        let toss = e_args
            .targeting_key
            .parse::<i8>()
            .unwrap_or(rand::rng().random_range(0..=99))
            % 100;

        match get_applicable_variants(
            &e_args.experiments,
            &query_data,
            toss,
            filter_prefixes.clone(),
        ) {
            Ok(variants) => {
                query_data.insert("variantIds".to_string(), variants.into());
            }
            Err(e) => {
                set_last_error(format!("Failed to get applicable variants: {}", e));
                return ptr::null_mut();
            }
        }
    }

    let dimensions = match parse_json::<HashMap<String, DimensionInfo>>(dimensions) {
        Ok(dimensions) => dimensions,
        Err(e) => {
            set_last_error(format!("Failed to parse dimensions: {}", e));
            return ptr::null_mut();
        }
    };

    // Call config resolution with reasoning
    match config::eval_config_with_reasoning(
        default_config,
        &contexts,
        &overrides,
        &dimensions,
        &query_data,
        merge_strategy,
        filter_prefixes,
    ) {
        Ok(result) => match serde_json::to_string(&result) {
            Ok(json_str) => string_to_c_str(json_str),
            Err(e) => {
                set_last_error(format!("Failed to serialize result: {}", e));
                ptr::null_mut()
            }
        },
        Err(e) => {
            set_last_error(e);
            ptr::null_mut()
        }
    }
}

// Add helper functions following existing pattern
#[no_mangle]
pub extern "C" fn core_test_connection() -> i32 {
    1 // Return 1 for success
}

#[no_mangle]
pub extern "C" fn core_get_error_details() -> *mut c_char {
    ERROR_DETAILS.with(|details| match details.borrow().clone() {
        Some(details_str) => match CString::new(details_str) {
            Ok(c_str) => c_str.into_raw(),
            Err(_) => ptr::null_mut(),
        },
        None => ptr::null_mut(),
    })
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

#[no_mangle]
pub extern "C" fn core_last_error_message() -> *const c_char {
    LAST_ERROR.with(|prev| match prev.borrow().clone() {
        Some(err) => match CString::new(err) {
            Ok(c_str) => c_str.into_raw(),
            Err(_) => ptr::null(),
        },
        None => ptr::null(),
    })
}

#[no_mangle]
pub extern "C" fn core_last_error_length() -> i32 {
    LAST_ERROR.with(|prev| match *prev.borrow() {
        Some(ref err) => err.len() as i32 + 1,
        None => 0,
    })
}

#[no_mangle]
pub extern "C" fn core_get_applicable_variants(
    experiments_json: *const c_char,
    query_data_json: *const c_char,
    toss: i8,
    filter_prefixes_json: *const c_char,
) -> *mut c_char {
    if experiments_json.is_null() || query_data_json.is_null() {
        set_last_error("Null pointer provided".into());
        return ptr::null_mut();
    }

    let experiments = match parse_json::<Experiments>(experiments_json) {
        Ok(experiments) => experiments,
        Err(e) => {
            set_last_error(format!("Failed to parse experiments: {}", e));
            return ptr::null_mut();
        }
    };

    let query_data = match parse_json::<Map<String, Value>>(query_data_json) {
        Ok(data) => data,
        Err(e) => {
            set_last_error(format!("Failed to parse query_data: {}", e));
            return ptr::null_mut();
        }
    };

    let filter_prefixes: Option<Vec<String>> = if filter_prefixes_json.is_null() {
        None
    } else {
        match parse_json::<Vec<String>>(filter_prefixes_json) {
            Ok(prefixes) => Some(prefixes),
            Err(e) => {
                set_last_error(format!("Failed to parse filter_prefixes: {}", e));
                return ptr::null_mut();
            }
        }
    };

    // Call the experimentation logic
    match get_applicable_variants(&experiments, &query_data, toss, filter_prefixes) {
        Ok(result) => match serde_json::to_string(&result) {
            Ok(json_str) => string_to_c_str(json_str),
            Err(e) => {
                set_last_error(format!("Failed to serialize result: {}", e));
                ptr::null_mut()
            }
        },
        Err(e) => {
            set_last_error(e);
            ptr::null_mut()
        }
    }
}
