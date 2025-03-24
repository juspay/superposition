// Primary interface so CAC client can work with other languages like haskell
use std::{
    ffi::{c_char, c_ulong, CStr},
    sync::Arc,
};

use crate::{utils::core::MapError, Client, MergeStrategy, CLIENT_FACTORY};
use once_cell::sync::Lazy;
use serde_json::{Map, Value};
use std::{
    cell::RefCell,
    ffi::{c_int, CString},
    time::Duration,
};
use tokio::runtime::Runtime;

thread_local! {
    static LAST_ERROR: RefCell<Option<String>> = const { RefCell::new(None) };
}

static CAC_RUNTIME: Lazy<Runtime> =
    Lazy::new(|| Runtime::new().expect("The runtime was not intialized"));

macro_rules! null_check {
    ($client: ident, $err: literal, $return: stmt) => {
        if $client.is_null() {
            update_last_error($err.into());
            $return
        }
    };
}

macro_rules! unwrap_safe {
    ($result: expr, $return: stmt) => {
        match $result {
            Ok(value) => value,
            Err(err) => {
                update_last_error(err.to_string());
                $return
            }
        }
    };
}

fn cstring_to_rstring(s: *const c_char) -> Result<String, String> {
    null_check!(
        s,
        "Invalid C string passed: string was a NULL pointer",
        return Err("Invalid C string passed: string was a NULL pointer".into())
    );
    let s = unsafe { CStr::from_ptr(s) };
    s.to_str().map(str::to_string).map_err_to_string()
}

fn rstring_to_cstring(s: String) -> CString {
    CString::new(s.as_str()).unwrap_or_default()
}

pub fn update_last_error(err: String) {
    println!("Setting LAST_ERROR: {}", err);
    LAST_ERROR.with(|prev| {
        *prev.borrow_mut() = Some(err);
    });
}

pub fn take_last_error() -> Option<String> {
    LAST_ERROR.with(|prev| prev.borrow_mut().take())
}

#[no_mangle]
pub extern "C" fn cac_last_error_length() -> c_int {
    LAST_ERROR.with(|prev| match *prev.borrow() {
        Some(ref err) => err.to_string().len() as c_int + 1,
        None => 0,
    })
}

#[no_mangle]
pub unsafe extern "C" fn cac_last_error_message() -> *const c_char {
    let last_error = unwrap_safe!(
        take_last_error().ok_or("No error found"),
        return std::ptr::null_mut()
    );
    let error_message = last_error.to_string();
    // println!("Error in last_error_message {error_message}");
    let err = rstring_to_cstring(error_message);
    err.into_raw()
}

#[no_mangle]
pub unsafe extern "C" fn cac_free_string(s: *mut c_char) {
    if s.is_null() {
        return;
    }
    unsafe {
        let _ = CString::from_raw(s);
    }
}

#[no_mangle]
pub extern "C" fn cac_new_client(
    tenant: *const c_char,
    update_frequency: c_ulong,
    hostname: *const c_char,
) -> c_int {
    #[allow(clippy::useless_conversion)] // done for windows support
    let duration = Duration::new(update_frequency.into(), 0);
    let tenant = unwrap_safe!(cstring_to_rstring(tenant), return 1);
    let hostname = unwrap_safe!(cstring_to_rstring(hostname), return 1);
    // println!("Creating cac client thread for tenant {tenant}");
    CAC_RUNTIME.block_on(async move {
        match CLIENT_FACTORY
            .create_client(tenant.clone(), duration, hostname)
            .await
        {
            Ok(_) => 0,
            Err(err) => {
                update_last_error(err);
                1
            }
        }
    })
}

#[no_mangle]
pub extern "C" fn cac_new_client_with_cache_properties(
    tenant: *const c_char,
    update_frequency: c_ulong,
    hostname: *const c_char,
    cache_max_capacity: c_ulong,
    cache_ttl: c_ulong,
    cache_tti: c_ulong,
) -> c_int {
    #[allow(clippy::useless_conversion)] // done for windows support
    let duration = Duration::new(update_frequency.into(), 0);
    let tenant = unwrap_safe!(cstring_to_rstring(tenant), return 1);
    let hostname = unwrap_safe!(cstring_to_rstring(hostname), return 1);
    // println!("Creating cac client thread for tenant {tenant}");
    CAC_RUNTIME.block_on(async move {
        #[allow(clippy::useless_conversion)]
        match CLIENT_FACTORY
            .create_client_with_cache_properties(
                tenant.clone(),
                duration,
                hostname,
                cache_max_capacity.into(),
                cache_ttl.into(),
                cache_tti.into(),
            )
            .await
        {
            Ok(_) => 0,
            Err(err) => {
                update_last_error(err);
                1
            }
        }
    })
}

#[no_mangle]
pub extern "C" fn cac_start_polling_update(tenant: *const c_char) {
    null_check!(tenant, "NULL pointer provided for tenant", return);
    unsafe {
        let client = cac_get_client(tenant);
        null_check!(client, "CAC client for tenant not found", return);
        // println!("in FFI polling");
        let _handle = CAC_RUNTIME.spawn((*client).clone().run_polling_updates());
    }
}

#[no_mangle]
pub extern "C" fn cac_free_client(ptr: *mut Arc<Client>) {
    if ptr.is_null() {
        return;
    }
    unsafe {
        let _ = Box::from_raw(ptr);
    }
}

#[no_mangle]
pub extern "C" fn cac_get_client(tenant: *const c_char) -> *mut Arc<Client> {
    let ten = unwrap_safe!(cstring_to_rstring(tenant), return std::ptr::null_mut());
    // println!("fetching cac client thread for tenant {ten}");
    CAC_RUNTIME.block_on(async move {
        unwrap_safe!(
            CLIENT_FACTORY
                .get_client(ten)
                .await
                .map(|client| Box::into_raw(Box::new(client))),
            std::ptr::null_mut()
        )
    })
}

#[no_mangle]
pub extern "C" fn cac_get_last_modified(client: *mut Arc<Client>) -> *const c_char {
    null_check!(
        client,
        "an invalid null pointer client is being used, please call get_client()",
        return std::ptr::null()
    );
    CAC_RUNTIME.block_on(async move {
        unsafe {
            let datetime = (*client).get_last_modified().await;
            rstring_to_cstring(datetime.to_string()).into_raw()
        }
    })
}

#[no_mangle]
pub extern "C" fn cac_get_config(
    client: *mut Arc<Client>,
    filter_query: *const c_char,
    filter_prefix: *const c_char,
) -> *const c_char {
    null_check!(
        client,
        "an invalid null pointer client is being used, please call get_client()",
        return std::ptr::null()
    );

    let filters = if filter_query.is_null() {
        None
    } else {
        let filter_string =
            unwrap_safe!(cstring_to_rstring(filter_query), return std::ptr::null());
        let filters: Map<String, Value> = unwrap_safe!(
            serde_json::from_str::<Map<String, Value>>(filter_string.as_str()),
            return std::ptr::null()
        );

        Some(filters).filter(|filters| !filters.is_empty())
    };

    let prefix_list = if filter_prefix.is_null() {
        None
    } else {
        let filter_string =
            unwrap_safe!(cstring_to_rstring(filter_prefix), return std::ptr::null());
        let prefix_list: Vec<String> =
            filter_string.split(',').map(String::from).collect();

        Some(prefix_list).filter(|list| !list.is_empty())
    };
    CAC_RUNTIME.block_on(async move {
        unsafe {
            unwrap_safe!(
                (*client)
                    .get_full_config_state_with_filter(filters, prefix_list)
                    .await
                    .map(|config| {
                        rstring_to_cstring(
                            serde_json::to_value(config).unwrap().to_string(),
                        )
                        .into_raw()
                    }),
                std::ptr::null_mut()
            )
        }
    })
}

#[no_mangle]
pub extern "C" fn cac_get_resolved_config(
    client: *mut Arc<Client>,
    query: *const c_char,
    filter_keys: *const c_char,
    merge_strategy: *const c_char,
) -> *const c_char {
    null_check!(
        client,
        "an invalid null pointer client is being used, please call get_client()",
        return std::ptr::null()
    );

    let keys: Option<Vec<String>> = if filter_keys.is_null() {
        None
    } else {
        let filter_string =
            unwrap_safe!(cstring_to_rstring(filter_keys), return std::ptr::null());
        Some(filter_string.split('|').map(str::to_string).collect())
    };

    let query = unwrap_safe!(cstring_to_rstring(query), return std::ptr::null());
    let merge_strategem =
        unwrap_safe!(cstring_to_rstring(merge_strategy), return std::ptr::null());
    println!(
        "key vector {:#?}, merge strategy {:#?}",
        keys, merge_strategem
    );

    let context = unwrap_safe!(
        serde_json::from_str::<Map<String, Value>>(query.as_str()),
        return std::ptr::null()
    );
    CAC_RUNTIME.block_on(async move {
        unsafe {
            unwrap_safe!(
                (*client)
                    .get_resolved_config(
                        context,
                        keys,
                        MergeStrategy::from(merge_strategem),
                    )
                    .await
                    .map(|ov| {
                        unwrap_safe!(
                        serde_json::to_string::<Map<String, Value>>(&ov)
                            .map(|overrides| rstring_to_cstring(overrides).into_raw()),
                        std::ptr::null()
                    )
                    }),
                std::ptr::null()
            )
        }
    })
}

#[no_mangle]
pub extern "C" fn cac_get_default_config(
    client: *mut Arc<Client>,
    filter_keys: *const c_char,
) -> *const c_char {
    let keys: Option<Vec<String>> = if filter_keys.is_null() {
        None
    } else {
        let filter_string = match cstring_to_rstring(filter_keys) {
            Ok(s) => s,
            Err(err) => {
                update_last_error(err);
                return std::ptr::null();
            }
        };
        Some(filter_string.split('|').map(str::to_string).collect())
    };
    CAC_RUNTIME.block_on(async move {
        unwrap_safe!(
            unsafe {
                (*client).get_default_config(keys).await.map(|ov| {
                    unwrap_safe!(
                        serde_json::to_string::<Map<String, Value>>(&ov)
                            .map(|overrides| rstring_to_cstring(overrides).into_raw()),
                        std::ptr::null()
                    )
                })
            },
            std::ptr::null()
        )
    })
}
