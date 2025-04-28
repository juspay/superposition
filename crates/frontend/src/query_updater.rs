use std::fmt::Display;

use leptos::*;
use leptos_router::{use_location, use_navigate, use_query_map};

use crate::utils::use_service_prefix;

pub fn use_param_updater(source: impl Fn() -> Vec<Box<dyn Display>> + 'static) {
    let navigate = use_navigate();
    let location = use_location();
    let service_prefix = use_service_prefix();

    Effect::new(move |_| {
        let desired_query_parts = source()
            .into_iter()
            .map(|s| s.to_string())
            .filter(|s| !s.is_empty())
            .collect::<Vec<_>>();

        let query_string = if desired_query_parts.is_empty() {
            String::new()
        } else {
            format!("?{}", desired_query_parts.join("&"))
        };

        let current_query_string = location.query.with_untracked(|q| q.to_query_string());
        if current_query_string != query_string {
            let path = location.pathname.get_untracked();
            let prefix_stripped_path = path
                .strip_prefix(&format!("/{service_prefix}"))
                .map_or(path.clone(), String::from);
            let new_url = format!("{prefix_stripped_path}{query_string}");

            navigate(&new_url, Default::default());
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
