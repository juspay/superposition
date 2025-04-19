use std::fmt::Display;

use leptos::*;
use leptos_router::{use_location, use_navigate, use_query_map, NavigateOptions};

// Trait to convert a tuple into Vec<String>
pub trait ToStringVec {
    fn to_str_vec(&self) -> Vec<String>;
}

impl ToStringVec for () {
    fn to_str_vec(&self) -> Vec<String> {
        vec![]
    }
}

impl<Head, Tail> ToStringVec for (Head, Tail)
where
    Head: Display,
    Tail: ToStringVec,
{
    fn to_str_vec(&self) -> Vec<String> {
        let (head, tail) = self;
        let mut parts = vec![head.to_string()];
        parts.extend(tail.to_str_vec());
        parts
    }
}

pub fn use_param_updater<T>(source: impl Fn() -> T + 'static)
where
    T: ToStringVec + 'static,
{
    let navigate = use_navigate();
    let pathname = use_location().pathname;

    Effect::new(move |_| {
        let query_params = source()
            .to_str_vec()
            .into_iter()
            .filter(|s| !s.is_empty())
            .collect::<Vec<_>>()
            .join("&");

        if !query_params.is_empty() {
            let mut url = pathname.get().to_string();
            url.push('?');
            url.push_str(&query_params);

            navigate(
                &url,
                NavigateOptions {
                    replace: false,
                    ..NavigateOptions::default()
                },
            );
        }
    });
}

pub fn use_query_string() -> StoredValue<String> {
    StoredValue::new(
        use_query_map()
            .try_get_untracked()
            .map(|q| q.to_query_string())
            .map(|s| s.strip_prefix('?').map(String::from).unwrap_or(s))
            .unwrap_or_default(),
    )
}
