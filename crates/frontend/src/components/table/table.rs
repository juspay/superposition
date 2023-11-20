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
    key_column: String,
    table_style: String,
    columns: Vec<Column>,
    rows: Vec<Map<String, Value>>,
) -> impl IntoView {
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
                    {
                        rows
                            .iter()
                            .enumerate()
                            .map(|(index, row)| view! {
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