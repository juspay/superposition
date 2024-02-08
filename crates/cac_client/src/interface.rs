// Primary interface so CAC client can work with other languages like haskell
#[warn(unused_assignments)]
use std::{
    ffi::{c_char, c_ulong, CStr},
    sync::Arc,
};

use crate::{utils::core::MapError, Client, CLIENT_FACTORY};
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

fn cstring_to_rstring(s: *const c_char) -> Result<String, String> {
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
    let last_error = match take_last_error() {
        Some(err) => err,
        None => return std::ptr::null_mut(),
    };
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
    update_periodically: bool,
    update_frequency: c_ulong,
    hostname: *const c_char,
) -> c_int {
    let duration = Duration::new(update_frequency, 0);
    let tenant = match cstring_to_rstring(tenant) {
        Ok(value) => value,
        Err(err) => {
            update_last_error(err);
            return 1;
        }
    };
    let hostname = match cstring_to_rstring(hostname) {
        Ok(value) => value,
        Err(err) => {
            update_last_error(err);
            return 1;
        }
    };

    // println!("Creating cac client thread for tenant {tenant}");
    let local = task::LocalSet::new();
    local.block_on(&Runtime::new().unwrap(), async move {
        match CLIENT_FACTORY
            .create_client(tenant.clone(), update_periodically, duration, hostname)
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
    if tenant.is_null() {
        return ();
    }
    unsafe {
        let client = get_client(tenant);
        let local = task::LocalSet::new();
        // println!("in FFI polling");
        local.block_on(
            &Runtime::new().unwrap(),
            (**client).clone().ffi_polling_update(),
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
    let ten = match cstring_to_rstring(tenant) {
        Ok(t) => t,
        Err(err) => {
            update_last_error(err);
            return std::ptr::null_mut();
        }
    };
    // println!("fetching cac client thread for tenant {ten}");
    match CLIENT_FACTORY.get_client(ten).map_err(|e| format!("{}", e)) {
        Ok(client) => Box::into_raw(Box::new(client)),
        Err(err) => {
            // println!("error occurred {err}");
            update_last_error(err);
            // println!("error set");
            std::ptr::null_mut()
        }
    }
}

#[no_mangle]
pub extern "C" fn cac_eval(
    client: *mut Arc<Client>,
    query: *const c_char,
) -> *const c_char {
    let context_string = match cstring_to_rstring(query) {
        Ok(s) => s,
        Err(err) => {
            update_last_error(err);
            return std::ptr::null();
        }
    };
    let context: Map<String, Value> =
        match serde_json::from_str::<Map<String, Value>>(context_string.as_str()) {
            Ok(json) => json,
            Err(err) => {
                update_last_error(err.to_string());
                return std::ptr::null();
            }
        };
    let overrides = match unsafe { (*client).eval(context) } {
        Ok(ov) => match serde_json::to_string::<Map<String, Value>>(&ov) {
            Ok(ove) => ove,
            Err(err) => {
                update_last_error(err.to_string());
                return std::ptr::null();
            }
        },
        Err(err) => {
            update_last_error(err);
            return std::ptr::null();
        }
    };
    rstring_to_cstring(overrides).into_raw()
}

#[no_mangle]
pub extern "C" fn get_last_modified(client: *mut Arc<Client>) -> *const c_char {
    let last_modified = unsafe { (*client).get_last_modified() };
    match last_modified {
        Ok(date) => rstring_to_cstring(date.to_string()).into_raw(),
        Err(err) => {
            update_last_error(err);
            std::ptr::null()
        }
    }
}

#[no_mangle]
pub extern "C" fn get_config(client: *mut Arc<Client>) -> *const c_char {
    let config = unsafe { (*client).get_config() };
    match config {
        Ok(c) => {
            rstring_to_cstring(serde_json::to_value(c).unwrap().to_string()).into_raw()
        }
        Err(err) => {
            update_last_error(err);
            std::ptr::null_mut()
        }
    }
}
