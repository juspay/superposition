// Primary interface so CAC client can work with other languages like haskell
#[warn(unused_assignments)]
use std::{
    ffi::{c_char, c_ulong, CStr},
    sync::Arc,
};

use crate::{utils::core::MapError, Client, MergeStrategy, CLIENT_FACTORY};
use serde_json::{Map, Value};
use std::{
    cell::RefCell,
    ffi::{c_int, CString},
    time::Duration,
};
use tokio::{runtime::Runtime, task};

thread_local! {
    static LAST_ERROR: RefCell<Option<Box<String>>> = RefCell::new(None);
}

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
        *prev.borrow_mut() = Some(Box::new(err));
    });
}

pub fn take_last_error() -> Option<Box<String>> {
    LAST_ERROR.with(|prev| prev.borrow_mut().take())
}

#[no_mangle]
pub extern "C" fn last_error_length() -> c_int {
    LAST_ERROR.with(|prev| match *prev.borrow() {
        Some(ref err) => err.to_string().len() as c_int + 1,
        None => 0,
    })
}

#[no_mangle]
pub unsafe extern "C" fn last_error_message() -> *const c_char {
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
pub unsafe extern "C" fn free_string(s: *mut c_char) {
    if s.is_null() {
        return;
    }
    unsafe {
        let _ = CString::from_raw(s);
    }
}

#[no_mangle]
pub extern "C" fn new_client(
    tenant: *const c_char,
    update_frequency: c_ulong,
    hostname: *const c_char,
) -> c_int {
    let duration = Duration::new(update_frequency, 0);
    let tenant = unwrap_safe!(cstring_to_rstring(tenant), return 1);
    let hostname = unwrap_safe!(cstring_to_rstring(hostname), return 1);

    // println!("Creating cac client thread for tenant {tenant}");
    let local = task::LocalSet::new();
    local.block_on(&Runtime::new().unwrap(), async move {
        match CLIENT_FACTORY
            .create_client(tenant.clone(), duration, hostname)
            .await
        {
            Ok(_) => return 0,
            Err(err) => {
                update_last_error(err);
                return 1;
            }
        }
    });
    return 0;
}

#[no_mangle]
pub extern "C" fn start_polling_update(tenant: *const c_char) {
    null_check!(tenant, "NULL pointer provided for tenant", return ());
    unsafe {
        let client = get_client(tenant);
        null_check!(client, "CAC client for tenant not found", return ());
        let local = task::LocalSet::new();
        // println!("in FFI polling");
        local.block_on(
            &Runtime::new().unwrap(),
            (*client).clone().run_polling_updates(),
        );
    }
}

#[no_mangle]
pub extern "C" fn free_client(ptr: *mut Arc<Client>) {
    if ptr.is_null() {
        return;
    }
    unsafe {
        let _ = Box::from_raw(ptr);
    }
}

#[no_mangle]
pub extern "C" fn get_client(tenant: *const c_char) -> *mut Arc<Client> {
    let ten = unwrap_safe!(cstring_to_rstring(tenant), return std::ptr::null_mut());
    // println!("fetching cac client thread for tenant {ten}");
    unwrap_safe!(
        CLIENT_FACTORY
            .get_client(ten)
            .map(|client| Box::into_raw(Box::new(client))),
        std::ptr::null_mut()
    )
}

#[no_mangle]
pub extern "C" fn get_last_modified(client: *mut Arc<Client>) -> *const c_char {
    null_check!(
        client,
        "an invalid null pointer client is being used, please call get_client()",
        return std::ptr::null()
    );
    unwrap_safe!(
        unsafe {
            (*client)
                .get_last_modified()
                .map(|date| rstring_to_cstring(date.to_string()).into_raw())
        },
        std::ptr::null()
    )
}

#[no_mangle]
pub extern "C" fn get_config(client: *mut Arc<Client>) -> *const c_char {
    null_check!(
        client,
        "an invalid null pointer client is being used, please call get_client()",
        return std::ptr::null()
    );
    unwrap_safe!(
        unsafe {
            (*client).get_full_config_state().map(|config| {
                rstring_to_cstring(serde_json::to_value(config).unwrap().to_string())
                    .into_raw()
            })
        },
        return std::ptr::null_mut()
    )
}

#[no_mangle]
pub extern "C" fn get_resolved_config(
    client: *mut Arc<Client>,
    query: *const c_char,
    keys: *const c_char,
    merge_strategy: *const c_char,
) -> *const c_char {
    null_check!(
        client,
        "an invalid null pointer client is being used, please call get_client()",
        return std::ptr::null()
    );

    let key = unwrap_safe!(cstring_to_rstring(keys), return std::ptr::null());
    let key_vector = if key.is_empty() {
        vec![]
    } else {
        key.split("|").map(str::to_string).collect()
    };

    let query = unwrap_safe!(cstring_to_rstring(query), return std::ptr::null());
    let merge_strategem =
        unwrap_safe!(cstring_to_rstring(merge_strategy), return std::ptr::null());
    println!(
        "key vector {:#?}, merge strategy {:#?}",
        key_vector, merge_strategem
    );

    let context = unwrap_safe!(
        serde_json::from_str::<Map<String, Value>>(query.as_str()),
        return std::ptr::null()
    );

    unwrap_safe!(
        unsafe {
            (*client)
                .get_resolved_config(
                    context,
                    key_vector,
                    MergeStrategy::from(merge_strategem),
                )
                .map(|ov| {
                    unwrap_safe!(
                        serde_json::to_string::<Map<String, Value>>(&ov)
                            .map(|overrides| rstring_to_cstring(overrides).into_raw()),
                        return std::ptr::null()
                    )
                })
        },
        return std::ptr::null()
    )
}

#[no_mangle]
pub extern "C" fn get_default_config(
    client: *mut Arc<Client>,
    keys: *const c_char,
) -> *const c_char {
    let key = unwrap_safe!(cstring_to_rstring(keys), return std::ptr::null());
    let key_vector = if key.is_empty() {
        vec![]
    } else {
        key.split("|").map(str::to_string).collect()
    };
    unwrap_safe!(
        unsafe {
            (*client).get_default_config(key_vector).map(|ov| {
                unwrap_safe!(
                    serde_json::to_string::<Map<String, Value>>(&ov)
                        .map(|overrides| rstring_to_cstring(overrides).into_raw()),
                    return std::ptr::null()
                )
            })
        },
        return std::ptr::null()
    )
}
