use leptos::*;
use leptos_router::{NavigateOptions, use_location, use_navigate};
use superposition_types::custom_query::QueryParam;

use crate::utils::use_service_prefix;

pub trait TupleSignals: Sized {
    type Signals: Clone + Default;
    type Data: PartialEq;

    fn new_signals(value: Self) -> Self::Signals;
    fn update_signals(signals: &Self::Signals, value: Self);
    fn to_query_param(signals: &Self::Signals) -> String;
    fn default_data() -> Self::Data;
    fn get_data(signals: &Self::Signals) -> Self::Data;
    fn destruct(value: Self) -> Self::Data;
}

macro_rules! impl_tuple_signals {
    ($(($T:ident, $s:ident)),+ $(,)?) => {
        #[allow(non_snake_case)]
        impl<$($T: Default + QueryParam + Clone + PartialEq + 'static),+> TupleSignals for ($($T,)+) {
            type Signals = ($(RwSignal<$T>,)+);
            type Data = ($($T,)+);

            fn destruct(value: Self) -> Self::Data {
                let ($($s,)+) = value;
                ($($s,)+)
            }

            fn new_signals(value: Self) -> Self::Signals {
                let ($($s,)+) = value;
                ($(RwSignal::new($s),)+)
            }

            fn update_signals(signals: &Self::Signals, value: Self) {
                let ($($T,)+) = value;
                let ($($s,)+) = signals;
                batch(|| {
                    $(
                        if $T != $s.get_untracked() {
                            $s.set($T);
                        }
                    )+
                })
            }

            fn to_query_param(signals: &Self::Signals) -> String {
                let ($($s,)+) = signals;
                [
                    $(
                        $s.get().to_query_param(),
                    )+
                ]
                .into_iter()
                .filter(|s| !s.is_empty())
                .collect::<Vec<_>>()
                .join("&")
            }

            fn default_data() -> Self::Data {
                ($($T::default(),)+)
            }

            fn get_data(signals: &Self::Signals) -> Self::Data {
                let ($($s,)+) = signals;
                ($($s.get(),)+)
            }
        }
    };
}

impl_tuple_signals!((A, a));
impl_tuple_signals!((A, a), (B, b));
impl_tuple_signals!((A, a), (B, b), (C, c));
impl_tuple_signals!((A, a), (B, b), (C, c), (D, d));

pub fn use_signal_from_query<T: TupleSignals + 'static>(
    parser: impl Fn(&str) -> T + Clone + 'static,
) -> T::Signals {
    let query_string = use_query_string();
    let navigate = use_navigate();
    let location = use_location();
    let service_prefix = use_service_prefix();

    let signals = T::new_signals(parser(&query_string.get_untracked()));

    let signals_for_update_effect = signals.clone();
    let last_query_rws = RwSignal::new(query_string.get_untracked());

    Effect::new(move |_| {
        let current_query = query_string.get();
        let last_query = last_query_rws.get_untracked();

        if current_query != last_query {
            logging::log!("Query has changed, updating signals accordingly");
            let new_value = parser(&current_query);
            last_query_rws.set(current_query);
            T::update_signals(&signals_for_update_effect, new_value);
            // Re-read signals so the effect stays subscribed to them
            let _ = T::get_data(&signals_for_update_effect);
            return;
        }

        let desired_data = T::get_data(&signals_for_update_effect);

        let desired_query = T::to_query_param(&signals_for_update_effect);
        let query_string = if desired_query.is_empty() {
            String::new()
        } else {
            format!("?{}", desired_query)
        };

        let current_query_string = location.search.get_untracked();
        let current_data = T::destruct(parser(&current_query));
        if current_data != desired_data || current_query != desired_query {
            logging::log!("Updating URL query to: {}", query_string);
            let path = location.pathname.get_untracked();
            let prefix_stripped_path = path
                .strip_prefix(&format!("/{service_prefix}"))
                .map_or(path.clone(), String::from);
            let new_url = format!("{prefix_stripped_path}{query_string}");

            let default_data = T::default_data();
            let replace = default_data == desired_data && current_query_string.is_empty();

            last_query_rws.set(desired_query);

            navigate(
                &new_url,
                NavigateOptions {
                    replace,
                    ..NavigateOptions::default()
                },
            );
        }
    });

    signals
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

/// meant for updating values of single valued query params only
pub fn use_update_url_query() -> impl Fn(&[(&str, Option<String>)]) -> String {
    let location = use_location();
    move |input: &[(&str, Option<String>)]| {
        let mut params = location
            .search
            .get_untracked()
            .split("&")
            .map(String::from)
            .collect::<Vec<_>>();

        for (param, value) in input {
            if let Some(value) = value {
                let mut found = false;
                for p in params.iter_mut() {
                    if p.starts_with(&format!("{param}=")) {
                        *p = format!("{param}={value}");
                        found = true;
                        break;
                    }
                }
                if !found {
                    params.push(format!("{param}={value}"));
                }
            } else {
                params.retain(|p| !p.starts_with(&format!("{param}=")));
            }
        }

        format!("?{}", params.join("&"))
    }
}
