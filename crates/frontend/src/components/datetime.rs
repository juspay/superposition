#[cfg(target_arch = "wasm32")]
use chrono::Datelike;
use chrono::{offset::Utc, DateTime, NaiveDate};
#[cfg(target_arch = "wasm32")]
use js_sys::Date;
use leptos::*;
#[cfg(target_arch = "wasm32")]
use wasm_bindgen::JsValue;

use crate::providers::csr_provider::use_client_side_ready;

#[cfg(target_arch = "wasm32")]
fn browser_locale() -> String {
    web_sys::window()
        .and_then(|w| w.navigator().language())
        .unwrap_or_else(|| "en-GB".to_string())
}

#[cfg(target_arch = "wasm32")]
fn format_local_datetime(time: &DateTime<Utc>) -> String {
    let millis = time.timestamp_millis();
    let date = Date::new(&JsValue::from_f64(millis as f64));
    let locale = browser_locale();

    let opts = js_sys::Object::new();

    js_sys::Reflect::set(&opts, &"dateStyle".into(), &"medium".into()).ok();
    js_sys::Reflect::set(&opts, &"timeStyle".into(), &"medium".into()).ok();
    js_sys::Reflect::set(&opts, &"hour12".into(), &JsValue::FALSE).ok();

    date.to_locale_string(&locale, &opts).into()
}

#[cfg(not(target_arch = "wasm32"))]
fn format_local_datetime(time: &DateTime<Utc>) -> String {
    time.format("%v %T UTC").to_string()
}

pub fn use_format_local_datetime(datetime: &DateTime<Utc>) -> String {
    let _ = use_client_side_ready().get(); // need this to rehydrate on client side
    format_local_datetime(datetime)
}

/// Component to display a datetime in local format
/// # Arguments
/// * `datetime` - A DateTime<Utc> object
/// # Returns
/// A view displaying the formatted datetime based on the user's locale
/// For SSR, it will display the datetime as `%v %T UTC` format
#[component]
pub fn datetime(datetime: DateTime<Utc>) -> impl IntoView {
    move || view! { <span>{use_format_local_datetime(&datetime)}</span> }
}

fn format_local_datetime_from_str(time: &str) -> String {
    match DateTime::parse_from_rfc3339(time) {
        Ok(dt) => format_local_datetime(&dt.with_timezone(&Utc)),
        Err(_) => time.to_string(),
    }
}

pub fn use_local_datetime_from_str(datetime: &str) -> String {
    let _ = use_client_side_ready().get(); // need this to rehydrate on client side
    format_local_datetime_from_str(datetime)
}

/// Component to display a datetime string in local format
/// # Arguments
/// * `datetime` - A datetime string in RFC3339 format UTC time zone
/// # Returns
/// A view displaying the formatted datetime based on the user's locale
/// For SSR, it will display the datetime as `%v %T UTC` format
#[component]
pub fn datetime_str(datetime: String) -> impl IntoView {
    move || view! { <span>{use_local_datetime_from_str(&datetime)}</span> }
}

#[cfg(target_arch = "wasm32")]
pub fn format_local_date(time: &DateTime<Utc>) -> String {
    let millis = time.timestamp_millis();
    let date = Date::new(&JsValue::from_f64(millis as f64));

    let year = date.get_full_year();
    let month = date.get_month() + 1; // JS months are 0-based
    let day = date.get_date();

    format!("{:04}-{:02}-{:02}", year, month, day)
}

#[cfg(not(target_arch = "wasm32"))]
pub fn format_local_date(time: &DateTime<Utc>) -> String {
    time.format("%Y-%m-%d").to_string()
}

pub fn use_format_local_date(datetime: &DateTime<Utc>) -> String {
    let _ = use_client_side_ready().get(); // need this to rehydrate on client side
    format_local_date(datetime)
}

#[cfg(target_arch = "wasm32")]
pub fn try_utc_date_from_locale_date(locale_date: &str) -> Option<DateTime<Utc>> {
    let date = NaiveDate::parse_from_str(locale_date, "%Y-%m-%d").ok()?;
    let millis = Date::new_with_year_month_day(
        date.year() as u32,
        date.month() as i32 - 1, // JS months are 0-based
        date.day() as i32,
    )
    .get_time();

    let secs = (millis / 1000.0) as i64;
    let nanos = ((millis % 1000.0) * 1_000_000.0) as u32;

    DateTime::<Utc>::from_timestamp(secs, nanos)
}

#[cfg(not(target_arch = "wasm32"))]
pub fn try_utc_date_from_locale_date(locale_date: &str) -> Option<DateTime<Utc>> {
    use chrono::NaiveTime;

    let date = NaiveDate::parse_from_str(locale_date, "%Y-%m-%d").ok()?;
    let date_time =
        DateTime::from_naive_utc_and_offset(date.and_time(NaiveTime::default()), Utc);

    Some(date_time)
}
