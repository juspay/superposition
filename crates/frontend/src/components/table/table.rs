use crate::components::table::types::TableSettings;

use super::types::Column;
use leptos::*;
use leptos_router::use_navigate;
use serde_json::{json, Map, Value};

fn generate_table_row_str(row: &Value) -> String {
    match row {
        Value::Null => "null".to_string(),
        Value::String(rstr) => rstr.to_string(),
        Value::Number(rnum) => rnum.to_string(),
        Value::Bool(rbool) => rbool.to_string(),
        Value::Array(rarr) => rarr
            .iter()
            .map(|ele| generate_table_row_str(ele))
            .collect::<Vec<String>>()
            .join(","),
        Value::Object(robj) => json!(robj).to_string(),
    }
}

#[component]
pub fn Table(
    _key_column: String,
    _table_style: String,
    columns: Vec<Column>,
    rows: Vec<Map<String, Value>>,
    settings: TableSettings,
) -> impl IntoView {
    let (redirect_info, _) = create_signal(settings.redirect_prefix);
    view! {
        <div class="overflow-x-auto">
            <table class="table table-zebra">
                <thead>
                    <tr>
                        <th></th>
                        {
                            columns
                                .iter()
                                .filter(|column| !column.hidden)
                                .map(|column| view! {
                                    <th class="uppercase">{&column.name}</th>
                                })
                                .collect_view()
                        }
                    </tr>
                </thead>
                <tbody>

                    {rows
                        .iter()
                        .enumerate()
                        .map(|(index, row)| {
                            let row_id = row
                                .get("id")
                                .unwrap_or(&json!(""))
                                .as_str()
                                .unwrap()
                                .to_string();
                            view! {
                                <tr
                                    on:click=move |_| {
                                        let redirect_info = redirect_info.get();
                                        if redirect_info.is_some() {
                                            let prefix = redirect_info.unwrap();
                                            let nav = use_navigate();
                                            nav(
                                                format!("{prefix}/{row_id}").as_str(),
                                                Default::default(),
                                            );
                                        }
                                    }

                                    style:cursor=move || {
                                        if redirect_info.get().is_some() {
                                            "pointer"
                                        } else {
                                            "default"
                                        }
                                    }
                                >

                                    <th>{index + 1}</th>
                                    {
                                        columns
                                            .iter()
                                            .filter(|column| !column.hidden)
                                            .map(|column| {
                                                let cname = &column.name;
                                                let value: String = generate_table_row_str(
                                                    row
                                                        .get(cname)
                                                        .unwrap_or(&Value::String("".to_string()))
                                                );
                                                view! {
                                                    <td>{(column.formatter)(&value, row)}</td>
                                                }
                                            })
                                            .collect_view()
                                    }
                                </tr>
                            })
                            .collect_view()
                    }
                </tbody>
            </table>
        </div>
    }
}
