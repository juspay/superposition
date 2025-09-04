use leptos::*;
use leptos_router::{use_location, use_navigate, use_query_map, NavigateOptions};
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

        let current_query_string = location.query.with_untracked(|q| q.to_query_string());
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
        use_query_map()
            .try_get()
            .map(|q| q.to_query_string())
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

pub fn use_update_url_query() -> impl Fn(&str, Option<String>) -> String {
    |param: &str, value: Option<String>| {
        let mut params = use_query_map().get_untracked().clone();
        if let Some(value) = value {
            params.insert(param.to_string(), value);
        } else {
            params.remove(param);
        }
        params.to_query_string()
    }
}
