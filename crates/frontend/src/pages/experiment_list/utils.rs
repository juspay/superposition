use crate::{
    components::{
        condition_pills::utils::extract_conditions,
        condition_pills::Condition as ConditionComponent,
        table::types::{Column, ColumnSortable},
    },
    types::ExperimentListFilters,
};
use core::time::Duration;
use leptos::*;
use leptos_router::A;
use serde::{Deserialize, Serialize};
use serde_json::{json, Map, Value};
use std::vec::Vec;
use strum_macros::Display;
use web_sys::MouseEvent;

#[derive(Copy, Display, Debug, Clone, Deserialize, Serialize, PartialEq)]
#[serde(rename_all = "snake_case")]
pub enum ExperimentSortOn {
    #[strum(to_string = "last_modified_at")]
    LastModifiedAt,
    #[strum(to_string = "created_at")]
    CreatedAt,
}

impl Default for ExperimentSortOn {
    fn default() -> Self {
        Self::LastModifiedAt
    }
}

pub fn experiment_table_columns(
    filters_rws: RwSignal<ExperimentListFilters>,
) -> Vec<Column> {
    let current_filters = filters_rws.get();
    let current_sort_on = current_filters.sort_on.unwrap_or_default();
    let current_sort_by = current_filters.sort_by.unwrap_or_default();
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
                let experiment_id_copy = experiment_id.clone();
                let handle_copy = move |event: MouseEvent| {
                    event.prevent_default();

                    let copy_code = format!(
                        "navigator.clipboard.writeText('{}')",
                        &experiment_id_copy
                    );
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
            ColumnSortable::No,
        ),
        Column::new(
            "status".to_string(),
            None,
            |value: &str, row: &Map<String, Value>| {
                let badge_color = match value {
                    "CREATED" => "badge-info",
                    "INPROGRESS" => "badge-warning",
                    "CONCLUDED" => "badge-success",
                    &_ => "info",
                };
                let class = format!("badge {}", badge_color);
                let traffic_percentage = row.get("traffic_percentage");
                let traffic_percentage = traffic_percentage
                    .map(|val| val.as_u64().unwrap_or(0))
                    .unwrap_or(0);
                view! {
                    <div class={class}>
                        <span class="text-white font-semibold text-xs">
                            {
                                if value == "INPROGRESS" {
                                    format!("{}: {}%", value, traffic_percentage)
                                } else {
                                    value.to_string()
                                }
                            }
                        </span>
                    </div>
                }
                .into_view()
            },
            ColumnSortable::No,
        ),
        Column::new(
            "context".to_string(),
            None,
            |_, row: &Map<String, Value>| {
                let context = match row.get("context") {
                    Some(value) => value.to_owned(),
                    None => json!(""),
                };
                let id = row.get("id").map_or(String::from(""), |value| {
                    value.as_str().unwrap_or("").to_string()
                });

                view! {
                    <div class="w-[400px]">
                        <ConditionComponent conditions=extract_conditions(&context) grouped_view=false id />
                    </div>
                }
                .into_view()
            },
            ColumnSortable::No,
        ),
        Column::new(
            "chosen_variant".to_string(),
            None,
            |value: &str, _| {
                let label = match value {
                    "null" => "¯\\_(ツ)_/¯".to_string(),
                    other => other.to_string(),
                };

                view! {
                    <span>{label}</span>
                }
                .into_view()
            },
            ColumnSortable::No,
        ),
        Column::default_with_sort(
            "created_at".to_string(),
            ColumnSortable::Yes {
                sort_fn: Callback::new(move |_| {
                    let filters = filters_rws.get();
                    let sort_by = filters.sort_by.unwrap_or_default().flip();
                    let new_filters = ExperimentListFilters {
                        sort_on: Some(ExperimentSortOn::CreatedAt),
                        sort_by: Some(sort_by),
                        ..filters
                    };
                    filters_rws.set(new_filters);
                }),
                sort_by: current_sort_by.clone(),
                currently_sorted: current_sort_on == ExperimentSortOn::CreatedAt,
            },
        ),
        Column::default("created_by".to_string()),
        Column::default_with_sort(
            "last_modified".to_string(),
            ColumnSortable::Yes {
                sort_fn: Callback::new(move |_| {
                    let filters = filters_rws.get();
                    let sort_by = filters.sort_by.unwrap_or_default().flip();
                    let new_filters = ExperimentListFilters {
                        sort_on: Some(ExperimentSortOn::LastModifiedAt),
                        sort_by: Some(sort_by),
                        ..filters
                    };
                    filters_rws.set(new_filters);
                }),
                sort_by: current_sort_by,
                currently_sorted: current_sort_on == ExperimentSortOn::LastModifiedAt,
            },
        ),
    ]
}
