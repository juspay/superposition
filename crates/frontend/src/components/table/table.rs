use leptos::*;
use super::types::Column;
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
    cx: Scope,
    key_column: String,
    table_style: String,
    columns: Vec<Column>,
    rows: Vec<Map<String, Value>>,
) -> impl IntoView {
    view! {
        cx,
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
                                    cx,
                                    <th class="uppercase">{&column.name}</th>
                                })
                                .collect_view(cx)
                        }
                    </tr>
                </thead>
                <tbody>
                    {
                        rows
                            .iter()
                            .enumerate()
                            .map(|(index, row)| view! {
                                cx,
                                <tr>
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
                                                    cx,
                                                    <td>{(column.formatter)(cx, &value, row)}</td>
                                                }
                                            })
                                            .collect_view(cx)
                                    }
                                </tr>
                            })
                            .collect_view(cx)
                    }
                </tbody>
            </table>
        </div>
    }
}