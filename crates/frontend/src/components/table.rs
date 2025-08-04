pub mod types;

use crate::{components::pagination::Pagination, schema::HtmlDisplay};

use self::types::{Column, Expandable, TablePaginationProps};
use leptos::*;
use serde_json::{json, Map, Value};
use superposition_types::SortBy;

#[component]
pub fn expandable_text(
    value: String,
    formatter: types::CellFormatter,
    row: Map<String, Value>,
    class_name: String,
    is_expandable: Expandable,
) -> impl IntoView {
    let stored_value = StoredValue::new(value);
    let get_child = move |val: &str| formatter(val, &row);
    let (is_expanded_rs, is_expanded_ws) = create_signal(false);
    view! {
        <td class=format!(
            "{class_name} align-top",
        )>
            {move || {
                let value = stored_value.get_value();
                let is_expanded = is_expanded_rs.get();
                let (should_expand, displayed_text) = match is_expandable {
                    Expandable::Enabled(len) if value.len() > len => {
                        let displayed_text = if is_expanded {
                            value
                        } else {
                            value.chars().take(len).collect::<String>()
                        };
                        (true, displayed_text)
                    }
                    _ => (false, value),
                };
                view! {
                    {get_child(&displayed_text).into_view()}
                    {should_expand
                        .then(|| {
                            view! {
                                <div
                                    class=format!(
                                        "inline w-fit text-[#4a00ff] cursor-pointer {}",
                                        if is_expanded { "pl-2" } else { "" },
                                    )
                                    on:click=move |_| is_expanded_ws.update(|val| *val = !*val)
                                >
                                    {if is_expanded { "less" } else { "...more" }}
                                </div>
                            }
                        })}
                }
            }}
        </td>
    }
}

#[component]
pub fn table(
    #[prop(into)] key_column: String,
    columns: Vec<Column>,
    rows: Vec<Map<String, Value>>,
    #[prop(into, default = String::new())] class: String,
    #[prop(into, default = String::new())] cell_class: String,
    #[prop(into, default = String::new())] head_class: String,
    #[prop(into, default = String::new())] body_class: String,
    #[prop(default = TablePaginationProps::default())] pagination: TablePaginationProps,
) -> impl IntoView {
    let pagination_props = StoredValue::new(pagination);
    let container_style = format!("{} overflow-x-auto overflow-y-clip", class);
    let sticky_shadow_class = "after:content-[''] after:absolute after:right-0 after:top-0 after:bottom-[-1px] after:w-[5px] after:bg-[linear-gradient(90deg,rgba(0,0,0,0.1)_0%,rgba(0,0,0,0)_100%)]";
    let get_sticky_position_classes = |index: usize, header: bool| match (header, index) {
        (true, 0) => format!("sticky left-20 top-0 z-30 {}", sticky_shadow_class),
        (true, _) => "sticky z-20 top-0".to_string(),
        (false, 0) => format!("sticky left-20 z-20 {}", sticky_shadow_class),
        _ => String::new(),
    };

    view! {
        <div class=container_style>
            <table class="table table-zebra">
                <thead class=head_class>
                    <tr class="bg-white">
                        <th class="sticky left-0 top-0 z-30 bg-inherit min-w-[5rem] px-3" />

                        {columns
                            .iter()
                            .filter(|column| !column.hidden)
                            .enumerate()
                            .map(|(index, column)| {
                                let sticky_class = get_sticky_position_classes(index, true);
                                let col_formatter = column.column_formatter.clone();
                                match column.sortable.clone() {
                                    types::ColumnSortable::Yes {
                                        sort_fn,
                                        sort_by,
                                        currently_sorted,
                                    } => {
                                        view! {
                                            <th
                                                class=format!(
                                                    "px-3 bg-inherit cursor-pointer {sticky_class}",
                                                )
                                                on:click=move |_| sort_fn.call(())
                                            >
                                                <div class="flex items-center gap-1">
                                                    {col_formatter(&column.name)}
                                                    {match (currently_sorted, sort_by) {
                                                        (false, _) => {
                                                            view! {
                                                                <i class="ri-expand-up-down-fill ri-xl self-center" />
                                                            }
                                                        }
                                                        (_, SortBy::Desc) => {
                                                            view! {
                                                                <i class="ri-arrow-down-s-fill ri-xl text-purple-700" />
                                                            }
                                                        }
                                                        (_, SortBy::Asc) => {
                                                            view! {
                                                                <i class="ri-arrow-up-s-fill ri-xl text-purple-700" />
                                                            }
                                                        }
                                                    }}
                                                </div>
                                            </th>
                                        }
                                    }
                                    types::ColumnSortable::No => {
                                        view! {
                                            <th class=format!(
                                                "px-3 bg-inherit {sticky_class}",
                                            )>{col_formatter(&column.name)}</th>
                                        }
                                    }
                                }
                            })
                            .collect_view()}

                    </tr>
                </thead>
                <tbody class=body_class>

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
                            let cell_class_clone = cell_class.clone();
                            view! {
                                <tr id=row_id class="odd:bg-white even:bg-[#f2f2f2]">
                                    <td class="sticky z-20 bg-inherit left-0 min-w-[5rem] px-3 align-top">
                                        {row_num}
                                    </td>

                                    {columns
                                        .iter()
                                        .filter(|column| !column.hidden)
                                        .enumerate()
                                        .map(move |(index, column)| {
                                            let column = column.clone();
                                            let value: String = row
                                                .get(&column.name)
                                                .unwrap_or(&Value::String(String::new()))
                                                .html_display();
                                            let sticky_class = get_sticky_position_classes(
                                                index,
                                                false,
                                            );
                                            view! {
                                                <ExpandableText
                                                    value
                                                    formatter=column.formatter
                                                    row=row.clone()
                                                    class_name=format!(
                                                        "min-w-48 max-w-106 px-3 bg-inherit break-words {cell_class_clone} {sticky_class}",
                                                    )
                                                    is_expandable=column.expandable
                                                />
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
                let TablePaginationProps { current_page, total_pages, on_page_change, .. } = pagination_props
                    .get_value();
                view! {
                    <div class="mt-2 flex justify-end">
                        <Pagination
                            current_page=current_page
                            total_pages=total_pages
                            on_change=on_page_change
                        />
                    </div>
                }
            }}

        </Show>
    }
}
