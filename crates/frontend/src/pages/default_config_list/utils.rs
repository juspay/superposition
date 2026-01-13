use std::collections::HashSet;

use leptos::*;
use leptos_router::A;
use serde_json::{Map, Value, json};

use crate::types::BreadCrums;
use crate::utils::unwrap_option_or_default_with_error;

#[component]
pub fn bread_crums(
    bread_crums: Vec<BreadCrums>,
    #[prop(into)] redirect_url: Callback<Option<String>, String>,
    #[prop(default = true)] show_root: bool,
) -> impl IntoView {
    if !show_root && bread_crums.len() <= 1 {
        return ().into_view();
    }

    let root_title_class = if show_root { "first:card-title" } else { "" };

    view! {
        <div class="flex items-center flex-wrap gap-2">
            {bread_crums
                .iter()
                .map(|ele| {
                    view! {
                        <h2 class=format!(
                            "{root_title_class} flex gap-2 after:content-['>'] after:font-normal after:text-base after:last:hidden",
                        )>
                            {if ele.is_link {
                                let href = redirect_url.call(ele.value.clone());
                                let label = ele.key.clone();
                                view! {
                                    <A class="text-blue-500 underline underline-offset-2" href>
                                        {label}
                                    </A>
                                }
                                    .into_view()
                            } else {
                                view! { <span>{ele.key.clone()}</span> }.into_view()
                            }}
                        </h2>
                    }
                })
                .collect_view()}
        </div>
    }.into_view()
}

pub fn get_bread_crums(
    key_prefix: Option<String>,
    initial_key: String,
) -> Vec<BreadCrums> {
    let mut default_bread_crums = vec![BreadCrums {
        key: initial_key,
        value: None,
        is_link: true,
    }];

    let mut bread_crums = match key_prefix {
        Some(prefix) => {
            let prefix_arr = prefix
                .trim_matches('.')
                .split('.')
                .map(str::to_string)
                .collect::<Vec<String>>();
            prefix_arr
                .into_iter()
                .fold(String::new(), |mut prefix, ele| {
                    prefix.push_str(&ele);
                    prefix.push('.');
                    default_bread_crums.push(BreadCrums {
                        key: ele.clone(),
                        value: Some(prefix.clone()),
                        is_link: true,
                    });
                    prefix
                });
            default_bread_crums
        }
        None => default_bread_crums,
    };
    if let Some(last_crumb) = bread_crums.last_mut() {
        last_crumb.is_link = false;
    }
    bread_crums
}

pub fn modify_rows(
    filtered_rows: Vec<Map<String, Value>>,
    key_prefix: Option<String>,
    cols: Vec<String>,
    key_col: &str,
) -> Vec<Map<String, Value>> {
    let mut groups: HashSet<String> = HashSet::new();
    let mut grouped_rows: Vec<Map<String, Value>> = filtered_rows
        .into_iter()
        .filter_map(|mut ele| {
            let key = ele
                .get(key_col)
                .and_then(|v| v.as_str())
                .map(String::from)
                .unwrap_or_default();

            let subkey = key_prefix.as_ref().map_or_else(
                || Some(key.clone()),
                |p| key.strip_prefix(p).map(String::from),
            );

            if let Some(filtered_key) = subkey {
                let new_key = filtered_key
                    .split('.')
                    .map(String::from)
                    .collect::<Vec<String>>();
                let key = new_key.first().map(String::from).unwrap_or_default();
                if new_key.len() == 1 {
                    // key
                    ele.insert(key_col.to_string(), json!(key));
                } else {
                    // folder
                    let folder = key + ".";
                    if !groups.contains(&folder) {
                        cols.iter().for_each(|col| {
                            ele.insert(
                                col.to_string(),
                                Value::String(if *col == key_col {
                                    folder.clone()
                                } else {
                                    "-".to_string()
                                }),
                            );
                        });
                        groups.insert(folder);
                    } else {
                        return None;
                    }
                }
                Some(ele)
            } else {
                None
            }
        })
        .collect();
    grouped_rows.sort_by(|a, b| {
        let key_a = unwrap_option_or_default_with_error(
            a.get(key_col).and_then(Value::as_str),
            "",
        );
        let key_b = unwrap_option_or_default_with_error(
            b.get(key_col).and_then(Value::as_str),
            "",
        );

        match (key_a.contains('.'), key_b.contains('.')) {
            (true, false) => std::cmp::Ordering::Less,
            (false, true) => std::cmp::Ordering::Greater,
            _ => std::cmp::Ordering::Equal,
        }
    });
    grouped_rows
}
