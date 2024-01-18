use crate::components::{
    condition_pills::condition_pills::ContextPills, table::types::Column,
};
use core::time::Duration;
use leptos::*;
use leptos_router::A;
use serde_json::{json, Map, Value};
use std::vec::Vec;
use web_sys::MouseEvent;

pub fn experiment_table_columns() -> Vec<Column> {
    vec![
        Column::new(
            "name".to_string(),
            None,
            |value: &str, row: &Map<String, Value>| {
                let (copied, set_copied) = create_signal(false);

                let experiment_name = value.to_string();
                let experiment_id = row.get("id").map_or(String::from(""), |value| {
                    value.as_str().unwrap_or("").to_string()
                });
                let experiment_id_copy = experiment_id.to_string();
                let handle_copy = move |event: MouseEvent| {
                    event.prevent_default();

                    let copy_code =
                        format!("navigator.clipboard.writeText({})", experiment_id_copy);
                    match js_sys::eval(&copy_code) {
                        Ok(_) => {
                            set_copied.set(true);
                            set_timeout(
                                move || {
                                    set_copied.set(false);
                                },
                                Duration::new(1, 0),
                            );
                        }
                        Err(_) => logging::log!("unable to copy to clipboard"),
                    }
                };
                view! {
                        <div>
                            <A href=experiment_id.to_string() class="btn-link">
                                {experiment_name}
                            </A>
                            <div class="text-gray-500">
                                <span class="text-xs">
                                    {experiment_id}
                                </span>
                                <i class="ri-file-copy-line cursor-pointer ml-2" on:click:undelegated=handle_copy></i>
                                <Show when=move || copied.get()>
                                    <div class="inline-block bg-gray-600 ml-2 rounded-xl px-2">
                                        <span class="text-white text-xs font-semibold">
                                            "copied!"
                                        </span>
                                    </div>
                                </Show>
                            </div>
                        </div>
                    }
                    .into_view()
            },
        ),
        Column::new("status".to_string(), None, |value: &str, _| {
            let badge_color = match value {
                "CREATED" => "badge-info",
                "INPROGRESS" => "badge-warning",
                "CONCLUDED" => "badge-success",
                &_ => "info",
            };
            let class = format!("badge {}", badge_color);
            view! {
                <div class={class}>
                    <span class="text-white font-semibold text-xs">
                        {value.to_string()}
                    </span>
                </div>
            }
            .into_view()
        }),
        Column::new(
            "context".to_string(),
            None,
            |_, row: &Map<String, Value>| {
                let context = match row.get("context") {
                    Some(value) => value.to_owned(),
                    None => json!(""),
                };

                view! {
                    <div class="inline-flex flex-col gap-y-2">
                        <ContextPills context=context />
                    </div>
                }
                .into_view()
            },
        ),
        Column::new("chosen_variant".to_string(), None, |value: &str, _| {
            let label = match value {
                "null" => "¯\\_(ツ)_/¯".to_string(),
                other => other.to_string(),
            };

            view! {
                <span>{label}</span>
            }
            .into_view()
        }),
        Column::default("created_at".to_string()),
        Column::default("created_by".to_string()),
        Column::default("last_modified".to_string()),
    ]
}
