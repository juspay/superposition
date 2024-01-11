use std::collections::HashMap;

use crate::components::button::button::Button;
use crate::components::dimension_form::dimension_form::DimensionForm;
use crate::components::modal::modal::Modal;
use crate::components::{
    stat::stat::Stat,
    table::{table::Table, types::Column},
};
use crate::utils::{close_modal, show_modal};
use leptos::*;
use serde_json::{json, Map, Value};

use crate::pages::Dimensions::helper::fetch_dimensions;

#[derive(Clone, Debug, Default)]
pub struct RowData {
    pub dimension: String,
    pub priority: u16,
    pub type_: String,
    pub pattern: String,
}

#[component]
pub fn Dimensions() -> impl IntoView {
    let tenant_rs = use_context::<ReadSignal<String>>().unwrap();
    let (open_form, set_open_form) = create_signal(false);

    let dimensions = create_blocking_resource(
        move || tenant_rs.get(),
        |current_tenant| async move {
            match fetch_dimensions(&current_tenant).await {
                Ok(data) => data,
                Err(_) => vec![],
            }
        },
    );

    let selected_dimension = create_rw_signal::<Option<RowData>>(None);

    let table_columns = create_memo(move |_| {
        let edit_col_formatter = move |_: &str, row: &Map<String, Value>| {
            let row_dimension = row["dimension"].clone().to_string().replace("\"", "");
            let row_priority_str = row["priority"].clone().to_string().replace("\"", "");
            let row_priority = match row_priority_str.parse::<u16>() {
                Ok(val) => val,
                Err(_) => 0 as u16,
            };

            let schema = row["schema"].clone().to_string();
            let schema_object = serde_json::from_str::<HashMap<String, Value>>(&schema)
                .unwrap_or(HashMap::new());

            let pattern_or_enum = schema_object
                .keys()
                .find(|key| {
                    key.to_string() == "pattern".to_string()
                        || key.to_string() == "enum".to_string()
                })
                .and_then(|val| Some(val.clone()))
                .unwrap_or(String::new());

            let row_type = match schema_object.get("type") {
                Some(Value::String(type_)) if type_ == "string" => {
                    pattern_or_enum.clone()
                }
                Some(Value::String(type_)) if type_ == "number" => type_.clone(),
                Some(Value::String(_)) => String::from("other"),
                Some(_) | None => String::new(),
            };

            let row_pattern = match schema_object.get("type") {
                Some(Value::String(type_))
                    if type_ == "string" && pattern_or_enum == "pattern" =>
                {
                    schema_object
                        .get(&pattern_or_enum)
                        .and_then(|val| Some(val.clone().to_string()))
                        .unwrap_or(String::new())
                        .replace("\"", "")
                }
                Some(Value::String(type_))
                    if type_ == "string" && pattern_or_enum == "enum" =>
                {
                    schema_object
                        .get(&pattern_or_enum)
                        .and_then(|val| {
                            if let Value::Array(v) = val {
                                return format!(
                                    "[{}]",
                                    v.iter()
                                        .map(|v| v.to_string())
                                        .collect::<Vec<String>>()
                                        .join(",")
                                )
                                .into();
                            }
                            None
                        })
                        .unwrap_or(String::new())
                }
                Some(Value::String(type_)) if type_ == "number" => String::new(),
                Some(Value::String(_)) => schema,
                _ => String::new(),
            };

            let edit_click_handler = move |_| {
                let row_data = RowData {
                    dimension: row_dimension.clone(),
                    priority: row_priority.clone(),
                    type_: row_type.clone(),
                    pattern: row_pattern.clone(),
                };
                selected_dimension.set(Some(row_data));
                set_open_form.set(true);
                show_modal("dimension_form_modal");
            };

            let edit_icon: HtmlElement<html::I> = view! { <i class="ri-pencil-line ri-xl text-blue-500" on:click=edit_click_handler></i> };

            view! { <span class="cursor-pointer">{edit_icon}</span> }.into_view()
        };
        vec![
            Column::default("dimension".to_string()),
            Column::default("priority".to_string()),
            Column::default("schema".to_string()),
            Column::default("created_by".to_string()),
            Column::default("created_at".to_string()),
            Column::new("EDIT".to_string(), None, edit_col_formatter),
        ]
    });

    view! {
        <div class="p-8">
            <Suspense fallback=move || view! { <p>"Loading (Suspense Fallback)..."</p> }>
                <div class="pb-4">
                    {move || {
                        let value = dimensions.get();
                        let total_items = match value {
                            Some(v) => v.len().to_string(),
                            _ => "0".to_string(),
                        };
                        view! {
                            <Stat heading="Dimensions" icon="ri-ruler-2-fill" number=total_items/>
                        }
                    }}
                    <Show when=move || { open_form.get() }>
                        <Modal
                            id="dimension_form_modal".to_string()
                            handle_close=move || {
                                close_modal("dimension_form_modal");
                                selected_dimension.set(None);
                            }
                        >

                            {move || {
                                if let Some(selected_dimension_data) = selected_dimension.get() {
                                    view! {
                                        <DimensionForm
                                            edit=true
                                            priority=selected_dimension_data.priority
                                            dimension_name=selected_dimension_data.dimension
                                            dimension_type=selected_dimension_data.type_
                                            dimension_pattern=selected_dimension_data.pattern
                                            handle_submit=move || {
                                                set_open_form.set(false);
                                                dimensions.refetch();
                                                selected_dimension.set(None);
                                            }
                                        />
                                    }
                                } else {
                                    view! {
                                        <DimensionForm handle_submit=move || {
                                            set_open_form.set(false);
                                            dimensions.refetch()
                                        }/>
                                    }
                                }
                            }}

                        </Modal>
                    </Show>
                </div>

                <div class="card rounded-xl w-full bg-base-100 shadow">
                    <div class="card-body">
                        <div class="flex justify-between mb-2">
                            <h2 class="card-title">Dimensions</h2>
                            <Button
                                text="Create Dimension".to_string()
                                on_click=move |_| {
                                    set_open_form.set(true);
                                    show_modal("dimension_form_modal");
                                }
                            />

                        </div>
                        <div>

                            {move || {
                                let value = dimensions.get();
                                match value {
                                    Some(v) => {
                                        let data = v
                                            .iter()
                                            .map(|ele| {
                                                let mut ele_map = json!(ele).as_object().unwrap().clone();
                                                ele_map
                                                    .insert(
                                                        "created_at".to_string(),
                                                        json!(ele.created_at.format("%v").to_string()),
                                                    );
                                                ele_map
                                            })
                                            .collect::<Vec<Map<String, Value>>>()
                                            .to_owned();
                                        view! {
                                            <Table
                                                table_style="abc".to_string()
                                                rows=data
                                                key_column="id".to_string()
                                                columns=table_columns.get()
                                            />
                                        }
                                    }
                                    None => view! { <div>Loading....</div> }.into_view(),
                                }
                            }}

                        </div>
                    </div>
                </div>
            </Suspense>
        </div>
    }
}
