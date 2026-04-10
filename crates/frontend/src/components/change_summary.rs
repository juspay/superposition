use std::collections::HashSet;

use leptos::*;
use serde_json::{Map, Value};
use similar::{ChangeTag, TextDiff};

use crate::components::{button::Button, monaco_editor::MonacoDiffEditor, table::Table};

use super::{
    form::label::Label,
    table::types::{Column, ColumnSortable, Expandable, default_column_formatter},
};

const DELETED: &str = "##DELETED##";
const NOT_PRESENT: &str = "##NOT_PRESENT##";

fn render_sentinel(value: &str) -> Option<View> {
    match value {
        DELETED => Some(
            view! { <span class="text-red-500 bg-red-100 px-2 py-1 rounded">"Deleted"</span> }
                .into_view(),
        ),
        NOT_PRESENT => Some(
            view! { <span class="text-[#505050] bg-[#cdcdcd] px-2 py-1 rounded">"Not Present"</span> }
                .into_view(),
        ),
        _ => None,
    }
}

fn get_counterpart(row: &Map<String, Value>, column: &str) -> Option<String> {
    row.get(column).and_then(|v| match v {
        Value::String(s) if s != DELETED && s != NOT_PRESENT => Some(s.clone()),
        _ => None,
    })
}

#[derive(Clone, Copy)]
enum DiffSide {
    Old,
    New,
}

impl DiffSide {
    fn counterpart_column(&self) -> &'static str {
        match self {
            DiffSide::Old => "New Value",
            DiffSide::New => "Old Value",
        }
    }

    fn wrapper_class(&self) -> &'static str {
        match self {
            DiffSide::Old => "bg-red-50 px-2 py-1 rounded border-l-2 border-red-300",
            DiffSide::New => "bg-green-50 px-2 py-1 rounded border-l-2 border-green-300",
        }
    }

    fn highlight(&self) -> (ChangeTag, &'static str) {
        match self {
            DiffSide::Old => (ChangeTag::Delete, "bg-red-200 text-red-900 rounded-sm"),
            DiffSide::New => {
                (ChangeTag::Insert, "bg-green-200 text-green-900 rounded-sm")
            }
        }
    }
}

fn render_inline_diff(old_text: &str, new_text: &str, side: DiffSide) -> View {
    let (highlight_tag, highlight_class) = side.highlight();

    let diff = TextDiff::from_chars(old_text, new_text);
    let spans: Vec<View> = diff
        .iter_all_changes()
        .filter_map(|change| {
            let tag = change.tag();
            if tag == ChangeTag::Equal {
                let t = change.value().to_string();
                Some(view! { <span>{t}</span> }.into_view())
            } else if tag == highlight_tag {
                let t = change.value().to_string();
                Some(view! { <span class=highlight_class>{t}</span> }.into_view())
            } else {
                None
            }
        })
        .collect();

    view! { <span class=side.wrapper_class()>{spans}</span> }.into_view()
}

fn make_diff_formatter(side: DiffSide) -> impl Fn(&str, &Map<String, Value>) -> View {
    move |value: &str, row: &Map<String, Value>| {
        if let Some(v) = render_sentinel(value) {
            return v;
        }
        match get_counterpart(row, side.counterpart_column()) {
            Some(counterpart) => {
                let (old, new) = match side {
                    DiffSide::Old => (value, counterpart.as_str()),
                    DiffSide::New => (counterpart.as_str(), value),
                };
                render_inline_diff(old, new, side)
            }
            None => view! { <span class=side.wrapper_class()>{value.to_string()}</span> }
                .into_view(),
        }
    }
}

#[allow(clippy::type_complexity)]
pub fn gen_change_table(
    key_column: &str,
    old_values: &Map<String, Value>,
    new_values: &Map<String, Value>,
) -> (Vec<Map<String, Value>>, Vec<Column>) {
    let columns = vec![
        Column::default_no_collapse(key_column.to_string()),
        Column::new(
            "Old Value".to_string(),
            false,
            make_diff_formatter(DiffSide::Old),
            ColumnSortable::No,
            Expandable::Enabled(100),
            default_column_formatter,
        ),
        Column::new(
            "New Value".to_string(),
            false,
            make_diff_formatter(DiffSide::New),
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
                .unwrap_or(Value::String(NOT_PRESENT.to_string()));
            let new_value = new_values
                .get(key)
                .cloned()
                .unwrap_or(Value::String(DELETED.to_string()));
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
fn NoChange() -> impl IntoView {
    view! { <div class="text-gray-500 text-center text-sm">"No changes detected."</div> }
}

#[component]
pub fn ChangeSummary(
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
                view! { <Table rows key_column columns /> }
            }}
        </div>
    }
}

#[component]
pub fn JsonChangeSummary(
    #[prop(into)] title: String,
    old_values: Option<Value>,
    new_values: Option<Value>,
) -> impl IntoView {
    let node_id = format!("diff-editor-{}", title.replace(' ', "-").to_lowercase());

    view! {
        <div class="w-[inherit] flex flex-col gap-4">
            <Label title />
            {if old_values == new_values {
                view! { <NoChange /> }
            } else {
                match (old_values, new_values) {
                    (Some(old_value), Some(new_value)) => {
                        let original_text = serde_json::to_string_pretty(&old_value)
                            .unwrap_or_default();
                        let modified_text = serde_json::to_string_pretty(&new_value)
                            .unwrap_or_default();
                        view! {
                            <div class="flex flex-col gap-2">
                                <div class="flex justify-between">
                                    <span class="text-sm font-medium text-red-700 w-1/2">
                                        "Old Value"
                                    </span>
                                    <span class="text-sm font-medium text-green-700 w-1/2">
                                        "New Value"
                                    </span>
                                </div>
                                <MonacoDiffEditor
                                    node_id=node_id
                                    original=original_text
                                    modified=modified_text
                                    classes=vec![
                                        "w-full",
                                        "min-h-[300px]",
                                        "border",
                                        "border-gray-200",
                                        "rounded",
                                    ]
                                />
                            </div>
                        }
                            .into_view()
                    }
                    (None, Some(new_value)) => {
                        view! {
                            <div class="flex gap-4">
                                <div class="flex-1 bg-red-50 border-l-4 border-red-300 rounded-r p-2">
                                    <span class="text-sm font-medium text-red-700">
                                        "Old Value"
                                    </span>
                                    <div class="mt-2">
                                        <span class="text-[#505050] bg-[#cdcdcd] px-2 py-1 rounded">
                                            "Not Present"
                                        </span>
                                    </div>
                                </div>
                                <div class="flex-1 bg-green-50 border-l-4 border-green-300 rounded-r p-2">
                                    <span class="text-sm font-medium text-green-700">
                                        "New Value"
                                    </span>
                                    <andypf-json-viewer
                                        indent="3"
                                        expanded="true"
                                        show-data-types="false"
                                        show-toolbar="true"
                                        expand-icon-type="arrow"
                                        expanded="1"
                                        show-copy="true"
                                        show-size="false"
                                        class="mt-2 w-full"
                                        data=serde_json::to_string_pretty(&new_value)
                                            .unwrap_or_default()
                                    />
                                </div>
                            </div>
                        }
                            .into_view()
                    }
                    (Some(old_value), None) => {
                        view! {
                            <div class="flex gap-4">
                                <div class="flex-1 bg-red-50 border-l-4 border-red-300 rounded-r p-2">
                                    <span class="text-sm font-medium text-red-700">
                                        "Old Value"
                                    </span>
                                    <andypf-json-viewer
                                        indent="3"
                                        expanded="true"
                                        show-data-types="false"
                                        show-toolbar="true"
                                        expand-icon-type="arrow"
                                        expanded="1"
                                        show-copy="true"
                                        show-size="false"
                                        class="mt-2 w-full"
                                        data=serde_json::to_string_pretty(&old_value)
                                            .unwrap_or_default()
                                    />
                                </div>
                                <div class="flex-1 bg-green-50 border-l-4 border-green-300 rounded-r p-2">
                                    <span class="text-sm font-medium text-green-700">
                                        "New Value"
                                    </span>
                                    <div class="mt-2">
                                        <span class="text-red-500 bg-red-100 px-2 py-1 rounded">
                                            "Deleted"
                                        </span>
                                    </div>
                                </div>
                            </div>
                        }
                            .into_view()
                    }
                    (None, None) => view! { <NoChange /> }.into_view(),
                }
            }}
        </div>
    }
    .into_view()
}

#[component]
pub fn ChangeLogPopup(
    #[prop(into)] title: String,
    #[prop(into)] description: String,
    #[prop(into, default = "Yes".to_string())] confirm_text: String,
    #[prop(into, default = "No".to_string())] close_text: String,
    #[prop(into)] on_confirm: Callback<()>,
    #[prop(into)] on_close: Callback<()>,
    #[prop(into, default = Signal::derive(|| false))] disabled: Signal<bool>,
    #[prop(into, default = Signal::derive(|| false))] inprogress: Signal<bool>,
    children: ChildrenFn,
) -> impl IntoView {
    let style = "font-medium rounded-lg text-sm text-center text-white px-5 py-2.5 hover:opacity-75";
    let confirm_text = StoredValue::new(confirm_text);

    view! {
        <Portal>
            <div class="z-[999999999] fixed inset-0 bg-black bg-opacity-50 backdrop-blur-sm flex items-center justify-center">
                <dialog class="modal" open=true>
                    <div class="modal-box max-h-[80%] !max-w-[90%] !w-fit p-6 flex flex-col gap-4 overflow-hidden bg-white rounded-lg shadow-xl border-2 border-lightgray">
                        <h4 class="flex-0 text-2xl font-semibold text-gray-800">{title.clone()}</h4>
                        <p class="flex-0 text-sm text-gray-600">{description.clone()}</p>
                        <div class="flex-1 flex flex-col gap-8 overflow-auto">{children()}</div>
                        <div class="flex-0 flex justify-end gap-4">
                            {move || {
                                view! {
                                    <Button
                                        loading=disabled.get() || inprogress.get()
                                        on_click=move |_| on_confirm.call(())
                                        text=confirm_text.get_value()
                                        icon_class=""
                                    />
                                }
                            }}
                            <button
                                disabled=inprogress.get()
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
