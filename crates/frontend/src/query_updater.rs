use leptos::*;
use leptos_router::{use_location, use_navigate, NavigateOptions};
use superposition_types::custom_query::QueryParam;

use crate::utils::use_service_prefix;

pub trait DisplayDefault: QueryParam {
    fn default(&self) -> String;
}

impl<T: QueryParam + Default> DisplayDefault for T {
    fn default(&self) -> String {
        T::default().to_query_param()
    }
}

pub fn use_param_updater(source: impl Fn() -> Vec<Box<dyn DisplayDefault>> + 'static) {
    let navigate = use_navigate();
    let location = use_location();
    let service_prefix = use_service_prefix();

    Effect::new(move |_| {
        let desired_query = source()
            .into_iter()
            .map(|s| s.to_query_param())
            .filter(|s| !s.is_empty())
            .collect::<Vec<_>>()
            .join("&");

        let query_string = if desired_query.is_empty() {
            String::new()
        } else {
            format!("?{}", desired_query)
        };

        let current_query_string = location.search.get_untracked();
        if current_query_string != query_string {
            let path = location.pathname.get_untracked();
            let prefix_stripped_path = path
                .strip_prefix(&format!("/{service_prefix}"))
                .map_or(path.clone(), String::from);
            let new_url = format!("{prefix_stripped_path}{query_string}");

            let default_query = source()
                .into_iter()
                .map(|s| s.default())
                .filter(|s| !s.is_empty())
                .collect::<Vec<_>>()
                .join("&");

            let replace =
                default_query == desired_query && current_query_string.is_empty();

            navigate(
                &new_url,
                NavigateOptions {
                    replace,
                    ..NavigateOptions::default()
                },
            );
        }
    });
}

fn use_query_string() -> Memo<String> {
    Memo::new(move |_| {
        use_location()
            .search
            .try_get()
            .map(|s| s.strip_prefix('?').map(String::from).unwrap_or(s))
            .unwrap_or_default()
    })
}

pub fn use_signal_from_query<T: Clone + PartialEq>(
    parser: impl Fn(String) -> T + 'static,
) -> RwSignal<T> {
    let query_string = use_query_string();
    let signal = RwSignal::new(parser(query_string.get_untracked()));

    Effect::new(move |_| {
        let new_value = parser(query_string.get());
        if new_value != signal.get_untracked() {
            signal.set(new_value);
        }
    });

    signal
}

/// meant for single valued query params only
pub fn use_update_url_query() -> impl Fn(&str, Option<String>) -> String {
    |param: &str, value: Option<String>| {
        let mut params = use_location().search.get_untracked().clone();

        // find range of the param in the query string starting from `{param}=` to immediately next `&` or end of string
        let range = params.find(&format!("{}=", param)).map(|start| {
            let end = params[start..]
                .find('&')
                .map(|e| e + start)
                .unwrap_or(params.len());
            (start, end)
        });

        if let Some((start, end)) = range {
            if let Some(value) = value {
                let end = if end == params.len() {
                    end
                } else {
                    end + 1 // include the '&' character
                };
                params.replace_range(start..end, &format!("{param}={value}"));
            } else {
                params.replace_range(start..end, "");
            }
        } else {
            if let Some(value) = value {
                if params.is_empty() {
                    params.push_str(&format!("{}={}", param, value));
                } else {
                    params.push_str(&format!("&{}={}", param, value));
                }
            }
        }

        format!("?{params}")
    }
}
