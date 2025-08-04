use crate::{
    components::{
        condition_pills::Condition as ConditionComponent,
        table::types::{
            default_column_formatter, default_formatter, Column, ColumnSortable,
            Expandable,
        },
    },
    logic::Conditions,
};
use core::time::Duration;
use leptos::*;
use leptos_router::A;
use serde_json::{Map, Value};
use std::vec::Vec;
use superposition_types::api::experiments::{ExperimentListFilters, ExperimentSortOn};
use web_sys::MouseEvent;

pub fn experiment_table_columns(
    filters_rws: RwSignal<ExperimentListFilters>,
    strict_mode: bool,
) -> Vec<Column> {
    let current_filters = filters_rws.get();
    let current_sort_on = current_filters.sort_on.unwrap_or_default();
    let current_sort_by = current_filters.sort_by.unwrap_or_default();
    vec![
        Column::new(
            "name".to_string(),
            false,
            move |value: &str, row: &Map<String, Value>| {
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
                            <A href=experiment_id.to_string() class="text-blue-500 underline underline-offset-2">
                                {experiment_name}
                            </A>
                            <div class="text-gray-500">
                                <span class="text-xs">
                                    {experiment_id}
                                </span>
                                <i class="ri-file-copy-line ml-2 cursor-pointer" on:click:undelegated=handle_copy></i>
                                <Show when=move || copied.get()>
                                    <div class="w-fit ml-2 px-2 flex justify-center items-center bg-gray-600 rounded-xl">
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
            Expandable::Disabled,
            default_column_formatter,
        ),
        Column::default_with_cell_formatter(
            "status".to_string(),
            |value: &str, row: &Map<String, Value>| {
                let badge_color = match value {
                    "CREATED" => "badge-info",
                    "INPROGRESS" => "badge-warning",
                    "CONCLUDED" => "badge-success",
                    "DISCARDED" => "badge-neutral",
                    "PAUSED" => "badge-error",
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
        ),
        Column::new(
            "context".to_string(),
            false,
            move |_, row: &Map<String, Value>| {
                let context = row
                    .get("context")
                    .and_then(|v| v.as_object().cloned())
                    .unwrap_or_default();
                let id = row.get("id").map_or(String::from(""), |value| {
                    value.as_str().unwrap_or("").to_string()
                });
                let conditions =
                    Conditions::from_context_json(&context).unwrap_or_default();

                view! {
                    <div class="w-[400px]">
                        <ConditionComponent conditions grouped_view=false id strict_mode  />
                    </div>
                }
                .into_view()
            },
            ColumnSortable::No,
            Expandable::Disabled,
            default_column_formatter,
        ),
        Column::default_with_cell_formatter(
            "chosen_variant".to_string(),
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
        Column::new(
            "last_modified".to_string(),
            false,
            default_formatter,
            ColumnSortable::Yes {
                sort_fn: Callback::new(move |_| {
                    let filters = filters_rws.get();
                    let sort_by = filters.sort_by.as_ref().map(|i| i.flip());
                    let new_filters = ExperimentListFilters {
                        sort_on: Some(ExperimentSortOn::LastModifiedAt),
                        sort_by,
                        ..filters
                    };
                    filters_rws.set(new_filters);
                }),
                sort_by: current_sort_by,
                currently_sorted: current_sort_on == ExperimentSortOn::LastModifiedAt,
            },
            Expandable::Enabled(100),
            |_| default_column_formatter("Modified At"),
        ),
        Column::default("last_modified_by".to_string()),
    ]
}
