#[cfg(target_arch = "wasm32")]
use chrono::Datelike;
use chrono::{DateTime, NaiveDate, offset::Utc};
#[cfg(target_arch = "wasm32")]
use js_sys::{Date, Object};
use leptos::*;
use leptos_meta::*;
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

    date.to_locale_string(&locale, &Object::new()).into()
}

#[cfg(not(target_arch = "wasm32"))]
fn format_local_datetime(time: &DateTime<Utc>) -> String {
    time.to_rfc3339()
}

fn format_local_datetime_from_str(time: &str) -> String {
    match DateTime::parse_from_rfc3339(time) {
        Ok(dt) => format_local_datetime(&dt.with_timezone(&Utc)),
        Err(_) => time.to_string(),
    }
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

fn format_local_date_from_str(time: &str) -> String {
    match DateTime::parse_from_rfc3339(time) {
        Ok(dt) => format_local_date(&dt.with_timezone(&Utc)),
        Err(_) => time.to_string(),
    }
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

#[derive(Default)]
pub enum DatetimeFormat {
    Date,
    #[default]
    DateTime,
}

impl DatetimeFormat {
    fn class(&self) -> &'static str {
        match self {
            Self::Date => "datetime_date",
            Self::DateTime => "datetime",
        }
    }
}

/// Component to display a datetime in local format
/// # Arguments
/// * `datetime` - A DateTime<Utc> object
/// * `class` - An optional CSS class for styling the span element
/// * `format` - An optional format enum to specify date or datetime display
/// # Returns
/// A view displaying the formatted datetime based on the user's locale
/// For SSR, it will display the datetime as in RFC 3339 format, which is converted
/// to local format on the client side using JavaScript on page load.
/// To ensure proper conversion, include the `DatetimeConversionScript` component is added in the app root.
#[component]
pub fn Datetime(
    datetime: DateTime<Utc>,
    #[prop(into, optional)] class: String,
    #[prop(optional)] format: DatetimeFormat,
) -> impl IntoView {
    let class = format!("{} {}", format.class(), class);
    view! {
        <time class=class datetime=datetime.to_rfc3339()>
            {match format {
                DatetimeFormat::Date => format_local_date(&datetime),
                DatetimeFormat::DateTime => format_local_datetime(&datetime),
            }}
        </time>
    }
}

/// Component to display a datetime string in local format
/// # Arguments
/// * `datetime` - A datetime string in RFC3339 format UTC time zone
/// * `class` - An optional CSS class for styling the span element
/// * `format` - An optional format enum to specify whether to display date only or date and time
/// # Returns
/// A view displaying the formatted datetime based on the user's locale
/// For SSR, it will display the datetime as in RFC 3339 format, which is converted
/// to local format on the client side using JavaScript on page load.
/// To ensure proper conversion, include the `DatetimeConversionScript` component is added in the app root.
#[component]
pub fn DatetimeStr(
    datetime: String,
    #[prop(into, optional)] class: String,
    #[prop(optional)] format: DatetimeFormat,
) -> impl IntoView {
    let class = format!("{} {}", format.class(), class);
    view! {
        <time class=class datetime=datetime.clone()>
            {match format {
                DatetimeFormat::Date => format_local_date_from_str(&datetime),
                DatetimeFormat::DateTime => format_local_datetime_from_str(&datetime),
            }}
        </time>
    }
}

/// Script to fetch the inner content of both the time tag with class 'datetime' and time tag with class 'datetime_date' to
/// convert it to local datetime and local date, respectively, from date strings in RFC 3339 format
/// to avoid hydration mismatches between server and client rendering, leading to a flicker effect.
#[component]
pub fn DatetimeConversionScript() -> impl IntoView {
    view! {
        <Script type_="text/javascript">
            r#"
            const dateToString = (date) => {
                const year = date.getFullYear();
                const month = String(date.getMonth() + 1).padStart(2, '0');
                const day = String(date.getDate()).padStart(2, '0');
            
                return `${year}-${month}-${day}`;
            }
            
            const convertDates = () => {
                const elements = document.querySelectorAll('.datetime, .datetime_date');
                elements.forEach(elem => {
                    const date = new Date(elem.getAttribute('datetime'));
                    if (isNaN(date.getTime())) return;
            
                    if (elem.classList.contains('datetime_date')) {
                        elem.textContent = dateToString(date);
                    } else {
                        elem.textContent = date.toLocaleString(navigator.language);
                    }
                });
            };
            window.addEventListener('DOMContentLoaded', convertDates);
            "#
        </Script>
    }
}
