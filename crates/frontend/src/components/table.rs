pub mod types;

use crate::{components::pagination::Pagination, schema::HtmlDisplay};

use self::types::{Column, TablePaginationProps};
use leptos::*;
use serde_json::{json, Map, Value};

#[component]
pub fn table(
    key_column: String,
    columns: Vec<Column>,
    rows: Vec<Map<String, Value>>,
    #[prop(into, default = String::new())] class: String,
    #[prop(into, default = String::new())] cell_class: String,
    #[prop(into, default = String::new())] head_class: String,
    #[prop(default = TablePaginationProps::default())] pagination: TablePaginationProps,
) -> impl IntoView {
    let pagination_props = StoredValue::new(pagination);
    let container_style = format!("{} overflow-x-auto", class);
    view! {
        <div class=container_style>
            <table class="table table-zebra">
                <thead class=head_class>
                    <tr>
                        <th></th>

                        {columns
                            .iter()
                            .filter(|column| !column.hidden)
                            .map(|column| {
                                view! {
                                    <th class="uppercase">{&column.name.replace('_', " ")}</th>
                                }
                            })
                            .collect_view()}

                    </tr>
                </thead>
                <tbody>

                    {rows
                        .iter()
                        .enumerate()
                        .map(|(index, row)| {
                            let row_id = row
                                .get(&key_column)
                                .unwrap_or(&json!(""))
                                .as_str()
                                .unwrap()
                                .to_string();
                            let TablePaginationProps { enabled, current_page, count, .. } = pagination_props
                                .get_value();
                            let row_num = if enabled {
                                index as i64 + 1 + ((current_page - 1) * count)
                            } else {
                                index as i64 + 1
                            };
                            view! {
                                <tr id=row_id>
                                    <th>{row_num}</th>

                                    {columns
                                        .iter()
                                        .filter(|column| !column.hidden)
                                        .map(|column| {
                                            let cname = &column.name;
                                            let value: String =
                                                row.get(cname).unwrap_or(&Value::String(String::new())).html_display();
                                            view! {
                                                <td class=cell_class
                                                    .to_string()>{(column.formatter)(&value, row)}</td>
                                            }
                                        })
                                        .collect_view()}

                                </tr>
                            }
                        })
                        .collect_view()}

                </tbody>
            </table>
        </div>
        <Show when=move || {
            pagination_props.get_value().enabled
        }>

            {move || {
                let TablePaginationProps { current_page, total_pages, on_prev, on_next, .. } = pagination_props
                    .get_value();
                view! {
                    <div class="mt-2 flex justify-end">
                        <Pagination
                            current_page=current_page
                            total_pages=total_pages
                            next=on_next
                            previous=on_prev
                        />
                    </div>
                }
            }}

        </Show>
    }
}
