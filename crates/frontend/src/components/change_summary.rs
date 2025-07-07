use std::collections::HashSet;

use leptos::*;
use serde_json::{Map, Value};

use crate::components::table::Table;

use super::{
    form::label::Label,
    table::types::{
        default_column_formatter, default_formatter, Column, ColumnSortable, Expandable,
    },
};

#[allow(clippy::type_complexity)]
pub fn gen_change_table(
    key_column: &str,
    old_values: &Map<String, Value>,
    new_values: &Map<String, Value>,
) -> (Vec<Map<String, Value>>, Vec<Column>) {
    let deleted_value_formatter = |value: &str, _row: &Map<String, Value>| {
        if value == "##DELETED##" {
            view! { <span class="text-red-500 bg-red-100 px-2 py-1 rounded">"Deleted"</span> }
            .into_view()
        } else if value == "##NOT_PRESENT##" {
            view! { <span class="text-[#505050] bg-[#cdcdcd] px-2 py-1 rounded">"Not Present"</span> }
            .into_view()
        } else {
            default_formatter(value, _row)
        }
    };

    let columns = vec![
        Column::default_no_collapse(key_column.to_string()),
        Column::new(
            "Old Value".to_string(),
            false,
            deleted_value_formatter,
            ColumnSortable::No,
            Expandable::Enabled(100),
            default_column_formatter,
        ),
        Column::new(
            "New Value".to_string(),
            false,
            deleted_value_formatter,
            ColumnSortable::No,
            Expandable::Enabled(100),
            default_column_formatter,
        ),
    ];

    let keys = old_values
        .keys()
        .chain(new_values.keys())
        .collect::<HashSet<_>>();

    let changes = keys
        .into_iter()
        .filter_map(|key| {
            let old_value = old_values
                .get(key)
                .cloned()
                .unwrap_or(Value::String("##NOT_PRESENT##".to_string()));
            let new_value = new_values
                .get(key)
                .cloned()
                .unwrap_or(Value::String("##DELETED##".to_string()));
            if old_value == new_value {
                return None;
            }
            Some(Map::from_iter(vec![
                (key_column.to_string(), Value::String(key.to_string())),
                ("Old Value".to_string(), old_value),
                ("New Value".to_string(), new_value),
            ]))
        })
        .collect();

    (changes, columns)
}

#[component]
fn no_change() -> impl IntoView {
    view! { <div class="text-gray-500 text-center text-sm">"No changes detected."</div> }
}

#[component]
pub fn change_summary(
    #[prop(into)] title: String,
    #[prop(into, default = "Config Key".to_string())] key_column: String,
    old_values: Map<String, Value>,
    new_values: Map<String, Value>,
) -> impl IntoView {
    let (rows, columns) = gen_change_table(&key_column, &old_values, &new_values);

    view! {
        <div class="flex flex-col gap-4">
            <Label title />
            {if rows.is_empty() {
                view! { <NoChange /> }
            } else {
                view! { <Table rows key_column="Config Key".to_string() columns /> }
            }}
        </div>
    }
}

#[component]
pub fn json_change_summary(
    #[prop(into)] title: String,
    old_values: Option<Value>,
    new_values: Option<Value>,
) -> impl IntoView {
    view! {
        <div class="w-[inherit] flex flex-col gap-4">
            <Label title />
            {if old_values == new_values {
                view! { <NoChange /> }
            } else {
                view! {
                    <div class="w-[inherit] flex-1 flex gap-4 justify-between">
                        <div class="flex-1 flex flex-col gap-2">
                            <span class="text-sm text-gray-500">"Old Value"</span>
                        </div>
                        <div class="flex-1 flex flex-col gap-2">
                            <span class="text-sm text-gray-500">"New Value"</span>
                        </div>
                    </div>
                    <div class="w-[inherit] flex-1 flex gap-4 justify-between">
                        {match old_values {
                            None => {
                                view! {
                                    <div class="min-w-[200px] w-1/2 flex-1">
                                        <span class="h-fit text-[#505050] bg-[#cdcdcd] px-2 py-1 rounded">
                                            "Not Present"
                                        </span>
                                    </div>
                                }
                                    .into_view()
                            }
                            Some(old_value) => {
                                view! {
                                    <andypf-json-viewer
                                        indent="3"
                                        expanded="true"
                                        show-data-types="false"
                                        show-toolbar="true"
                                        expand-icon-type="arrow"
                                        expanded="1"
                                        show-copy="true"
                                        show-size="false"
                                        class="min-w-[200px] w-1/2 flex-1"
                                        data=serde_json::to_string_pretty(&old_value)
                                            .unwrap_or_default()
                                    />
                                }
                                    .into_view()
                            }
                        }}
                        {match new_values {
                            None => {
                                view! {
                                    <div class="min-w-[200px] w-1/2 flex-1">
                                        <span class="h-fit text-red-500 bg-red-100 px-2 py-1 rounded">
                                            "Deleted"
                                        </span>
                                    </div>
                                }
                                    .into_view()
                            }
                            Some(new_value) => {
                                view! {
                                    <andypf-json-viewer
                                        indent="3"
                                        expanded="true"
                                        show-data-types="false"
                                        show-toolbar="true"
                                        expand-icon-type="arrow"
                                        expanded="1"
                                        show-copy="true"
                                        show-size="false"
                                        class="min-w-[200px] w-1/2 flex-1"
                                        data=serde_json::to_string_pretty(&new_value)
                                            .unwrap_or_default()
                                    />
                                }
                                    .into_view()
                            }
                        }}
                    </div>
                }
                    .into_view()
            }}

        </div>
    }
    .into_view()
}

#[component]
pub fn change_log_popup(
    #[prop(into)] title: String,
    #[prop(into)] description: String,
    #[prop(into, default = "Yes".to_string())] confirm_text: String,
    #[prop(into, default = "No".to_string())] close_text: String,
    #[prop(into)] on_confirm: Callback<()>,
    #[prop(into)] on_close: Callback<()>,
    #[prop(into, default = Signal::derive(|| false))] disabled: Signal<bool>,
    children: ChildrenFn,
) -> impl IntoView {
    let style = "font-medium rounded-lg text-sm text-center text-white px-5 py-2.5 hover:opacity-75";

    view! {
        <Portal>
            <div class="fixed inset-0 bg-black bg-opacity-50 backdrop-blur-sm flex items-center justify-center z-[99999999]">
                <dialog class="modal" open=true>
                    <div class="modal-box max-h-[80%] !max-w-[90%] !w-fit p-6 flex flex-col gap-4 overflow-hidden bg-white rounded-lg shadow-xl border-2 border-lightgray">
                        <h4 class="flex-0 text-2xl font-semibold text-gray-800">{title.clone()}</h4>
                        <p class="flex-0 text-sm text-gray-600">{description.clone()}</p>
                        <div class="flex-1 flex flex-col gap-8 overflow-auto">{children()}</div>
                        <div class="flex-0 flex justify-end gap-4">
                            <button
                                disabled=disabled
                                class=format!("btn bg-purple-500 {style} hover:bg-purple-500")
                                on:click=move |_| on_confirm.call(())
                            >
                                {confirm_text.clone()}
                            </button>
                            <button
                                class=format!("btn bg-gray-400 {style} hover:bg-gray-300")
                                on:click=move |_| on_close.call(())
                            >
                                {close_text.clone()}
                            </button>
                        </div>
                    </div>
                </dialog>
            </div>
        </Portal>
    }
}
