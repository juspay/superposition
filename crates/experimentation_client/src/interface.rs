use std::{
    collections::HashMap,
    ffi::{c_char, c_ulong, CStr},
    sync::Arc,
};

use crate::{Client, CLIENT_FACTORY};
use once_cell::sync::Lazy;
use serde_json::{Map, Value};
use std::{
    cell::RefCell,
    ffi::{c_int, CString},
};
use superposition_types::{
    logic::{evaluate_local_cohorts, evaluate_local_cohorts_skip_unresolved},
    DimensionInfo,
};
use tokio::{runtime::Runtime, task};

thread_local! {
    static LAST_ERROR: RefCell<Option<String>> = const { RefCell::new(None) };
}

static EXP_RUNTIME: Lazy<Runtime> =
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

fn to_string<E>(e: E) -> String
where
    E: ToString,
{
    e.to_string()
}

fn error_block<E>(err: String) -> *mut E {
    update_last_error(err);
    std::ptr::null_mut()
}

fn cstring_to_rstring(s: *const c_char) -> Result<String, String> {
    let s = unsafe { CStr::from_ptr(s) };
    s.to_str().map(str::to_string).map_err(to_string)
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
    LAST_ERROR.with(|prev| prev.take())
}

#[no_mangle]
pub extern "C" fn expt_last_error_length() -> c_int {
    LAST_ERROR.with(|prev| match *prev.borrow() {
        Some(ref err) => err.to_string().len() as c_int + 1,
        None => 0,
    })
}

#[no_mangle]
pub unsafe extern "C" fn expt_last_error_message() -> *const c_char {
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
pub unsafe extern "C" fn expt_free_string(s: *mut c_char) {
    if s.is_null() {
        return;
    }
    unsafe {
        let _ = CString::from_raw(s);
    }
}

#[no_mangle]
pub extern "C" fn expt_new_client(
    tenant: *const c_char,
    update_frequency: c_ulong,
    hostname: *const c_char,
) -> c_int {
    let tenant = unwrap_safe!(cstring_to_rstring(tenant), return 1);
    let hostname = unwrap_safe!(cstring_to_rstring(hostname), return 1);

    // println!("Creating cac client thread for tenant {tenant}");
    EXP_RUNTIME.block_on(async move {
        #[allow(clippy::useless_conversion)]
        match CLIENT_FACTORY
            .create_client(tenant.clone(), update_frequency.into(), hostname)
            .await
        {
            Ok(_) => 0,
            Err(err) => {
                update_last_error(err);
                1
            }
        }
    });
    0
}

#[no_mangle]
pub extern "C" fn expt_start_polling_update(tenant: *const c_char) {
    null_check!(tenant, "Tenant cannot be a null string", return);
    unsafe {
        let client = expt_get_client(tenant);
        let local = task::LocalSet::new();
        // println!("in FFI polling");
        local.block_on(&EXP_RUNTIME, (*client).clone().run_polling_updates());
    }
}

#[no_mangle]
pub extern "C" fn expt_free_client(ptr: *mut Arc<Client>) {
    null_check!(ptr, "cannot free a null pointer", return);
    unsafe {
        let _ = Box::from_raw(ptr);
    }
}

#[no_mangle]
pub extern "C" fn expt_get_client(tenant: *const c_char) -> *mut Arc<Client> {
    let ten = unwrap_safe!(cstring_to_rstring(tenant), return std::ptr::null_mut());
    EXP_RUNTIME.block_on(async move {
        match CLIENT_FACTORY.get_client(ten).await {
            Ok(client) => Box::into_raw(Box::new(client)),
            Err(err) => {
                // println!("error occurred {err}");
                update_last_error(err);
                // println!("error set");
                std::ptr::null_mut()
            }
        }
    })
}

#[no_mangle]
pub extern "C" fn expt_get_applicable_variant(
    client: *mut Arc<Client>,
    c_dimensions: *const c_char,
    c_context: *const c_char,
    identifier: *const c_char,
    filter_prefix: *const c_char,
) -> *mut c_char {
    let dimensions = unwrap_safe!(
        cstring_to_rstring(c_dimensions),
        return std::ptr::null_mut()
    );
    let context =
        unwrap_safe!(cstring_to_rstring(c_context), return std::ptr::null_mut());
    let identifier =
        unwrap_safe!(cstring_to_rstring(identifier), return std::ptr::null_mut());

    let dimensions = unwrap_safe!(
        serde_json::from_str::<HashMap<String, DimensionInfo>>(dimensions.as_str()),
        return std::ptr::null_mut()
    );
    let context = unwrap_safe!(
        serde_json::from_str::<Map<String, Value>>(context.as_str()),
        return std::ptr::null_mut()
    );
    let prefix_list = if filter_prefix.is_null() {
        None
    } else {
        let filter_string = unwrap_safe!(
            cstring_to_rstring(filter_prefix),
            return std::ptr::null_mut()
        );
        let prefix_list = filter_string.split(',').map(String::from).collect();
        Some(prefix_list)
    };
    let variants_result = EXP_RUNTIME.block_on(unsafe {
        (*client).get_applicable_variant(&dimensions, &context, &identifier, prefix_list)
    });
    variants_result
        .map(|result| {
            serde_json::to_string(&result)
                .map(|json| rstring_to_cstring(json).into_raw())
                .unwrap_or_else(|err| error_block(err.to_string()))
        })
        .unwrap_or_else(|err| error_block(err.to_string()))
}

#[no_mangle]
pub extern "C" fn expt_get_satisfied_experiments(
    client: *mut Arc<Client>,
    c_dimensions: *const c_char,
    c_context: *const c_char,
    filter_prefix: *const c_char,
) -> *mut c_char {
    let context =
        unwrap_safe!(cstring_to_rstring(c_context), return std::ptr::null_mut());

    let context = unwrap_safe!(
        serde_json::from_str::<Map<String, Value>>(context.as_str()),
        return std::ptr::null_mut()
    );

    let dimensions = unwrap_safe!(
        cstring_to_rstring(c_dimensions),
        return std::ptr::null_mut()
    );

    let dimensions = unwrap_safe!(
        serde_json::from_str::<HashMap<String, DimensionInfo>>(dimensions.as_str()),
        return std::ptr::null_mut()
    );

    let prefix_list = if filter_prefix.is_null() {
        None
    } else {
        let filter_string = unwrap_safe!(
            cstring_to_rstring(filter_prefix),
            return std::ptr::null_mut()
        );
        let prefix_list = filter_string.split(',').map(String::from).collect();
        Some(prefix_list)
    };

    let context = evaluate_local_cohorts(&dimensions, &context);

    let local = task::LocalSet::new();
    local.block_on(&Runtime::new().unwrap(), async move {
        unsafe {
            unwrap_safe!(
                (*client)
                    .get_satisfied_experiments(&context, prefix_list)
                    .await
                    .map(|exp| {
                        rstring_to_cstring(serde_json::to_value(exp).unwrap().to_string())
                            .into_raw()
                    }),
                std::ptr::null_mut()
            )
        }
    })
}

#[no_mangle]
pub extern "C" fn expt_get_filtered_satisfied_experiments(
    client: *mut Arc<Client>,
    c_dimensions: *const c_char,
    c_context: *const c_char,
    filter_prefix: *const c_char,
) -> *mut c_char {
    let context =
        unwrap_safe!(cstring_to_rstring(c_context), return std::ptr::null_mut());

    let context = unwrap_safe!(
        serde_json::from_str::<Map<String, Value>>(context.as_str()),
        return std::ptr::null_mut()
    );

    let dimensions = unwrap_safe!(
        cstring_to_rstring(c_dimensions),
        return std::ptr::null_mut()
    );

    let dimensions = unwrap_safe!(
        serde_json::from_str::<HashMap<String, DimensionInfo>>(dimensions.as_str()),
        return std::ptr::null_mut()
    );

    let prefix_list = if filter_prefix.is_null() {
        None
    } else {
        let filter_string = unwrap_safe!(
            cstring_to_rstring(filter_prefix),
            return std::ptr::null_mut()
        );
        let prefix_list: Vec<String> =
            filter_string.split(',').map(String::from).collect();

        Some(prefix_list).filter(|list| !list.is_empty())
    };

    let context = evaluate_local_cohorts_skip_unresolved(&dimensions, &context);

    let local = task::LocalSet::new();
    local.block_on(&Runtime::new().unwrap(), async move {
        unsafe {
            unwrap_safe!(
                (*client)
                    .get_filtered_satisfied_experiments(&context, prefix_list)
                    .await
                    .map(|exp| {
                        rstring_to_cstring(serde_json::to_value(exp).unwrap().to_string())
                            .into_raw()
                    }),
                std::ptr::null_mut()
            )
        }
    })
}

#[no_mangle]
pub extern "C" fn expt_get_running_experiments(client: *mut Arc<Client>) -> *mut c_char {
    let local = task::LocalSet::new();
    local.block_on(&Runtime::new().unwrap(), async move {
        unsafe {
            unwrap_safe!(
                (*client).get_running_experiments().await.map(|exp| {
                    rstring_to_cstring(serde_json::to_value(exp).unwrap().to_string())
                        .into_raw()
                }),
                std::ptr::null_mut()
            )
        }
    })
}
