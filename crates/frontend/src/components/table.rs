pub mod types;

use crate::{components::pagination::Pagination, schema::HtmlDisplay};

use self::types::{Column, TablePaginationProps};
use leptos::*;
use serde_json::{json, Map, Value};
use superposition_types::SortBy;

#[component]
pub fn expandable_text(
    value: String,
    max_length: usize,
    formatter: types::CellFormatter,
    row: Map<String, Value>,
    class_name: String,
    is_expandable: bool,
) -> impl IntoView {
    let value = StoredValue::new(value);
    let get_child = move |val: &str| (formatter)(val, &row);
    let (is_expanded_rs, set_is_expanded_ws) = create_signal(false);
    view! {
        <td class=class_name>
            <span>
                {move || {
                    view! {
                        <span class="flex">
                            {if !is_expandable || value.get_value().len() < max_length
                                || is_expanded_rs.get()
                            {
                                get_child(&value.get_value()).into_view()
                            } else {
                                get_child(&(value.get_value()[..max_length].to_string()))
                                    .into_view()
                            }}
                            {if value.get_value().len() > max_length && is_expandable {
                                view! {
                                    <span
                                        class=format!(
                                            "text-[#4a00ff] cursor-pointer{}",
                                            if is_expanded_rs.get() { " ml-2" } else { "" },
                                        )
                                        on:click=move |_| {
                                            set_is_expanded_ws.update(|val| *val = !*val)
                                        }
                                    >
                                        {if is_expanded_rs.get() { "less" } else { "...more" }}
                                    </span>
                                }
                            } else {
                                view! { <span></span> }
                            }}
                        </span>
                    }
                }}
            </span>
        </td>
    }
}

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
    let get_sticky_position_classes = |index: usize| match index {
        0 => "sticky left-20 z-20 bg-inherit",
        _ => "",
    };

    view! {
        <div class=container_style>
            <table class="table table-zebra">
                <thead class=head_class>
                    <tr class="bg-white">
                        <th class="sticky left-0 bg-inherit min-w-[5rem]"></th>

                        {columns
                            .iter()
                            .filter(|column| !column.hidden)
                            .enumerate()
                            .map(|(index, column)| {
                                let column_name = (&column.name).replace('_', " ");
                                let sticky_class = get_sticky_position_classes(index);
                                match column.sortable.clone() {
                                    types::ColumnSortable::Yes {
                                        sort_fn,
                                        sort_by,
                                        currently_sorted,
                                    } => {
                                        view! {
                                            <th
                                                class=format!("uppercase cursor-pointer {}", sticky_class)
                                                on:click=move |_| sort_fn.call(())
                                            >
                                                {column_name}
                                                {match (currently_sorted, sort_by) {
                                                    (false, _) => {
                                                        view! { <i class="ri-expand-up-down-line"></i> }
                                                    }
                                                    (_, SortBy::Desc) => {
                                                        view! { <i class="ri-arrow-down-s-line"></i> }
                                                    }
                                                    (_, SortBy::Asc) => {
                                                        view! { <i class="ri-arrow-up-s-line"></i> }
                                                    }
                                                }}

                                            </th>
                                        }
                                    }
                                    types::ColumnSortable::No => {
                                        view! {
                                            <th class=format!(
                                                "uppercase {}",
                                                sticky_class,
                                            )>{column_name}</th>
                                        }
                                    }
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
                            let cell_class_clone = cell_class.clone();
                            view! {
                                <tr id=row_id class="odd:bg-white even:bg-[#f2f2f2]">
                                    <th class="sticky z-20 bg-inherit left-0 min-w-[5rem]">
                                        {row_num}
                                    </th>

                                    {columns
                                        .iter()
                                        .filter(|column| !column.hidden)
                                        .enumerate()
                                        .map(move |(index, column)| {
                                            let column = column.clone();
                                            let cname = column.name.clone();
                                            let value: String = row
                                                .get(&cname)
                                                .unwrap_or(&Value::String(String::new()))
                                                .html_display();
                                            let sticky_class = get_sticky_position_classes(index);
                                            view! {
                                                <ExpandableText
                                                    value
                                                    max_length=50
                                                    formatter=column.formatter
                                                    row=row.clone()
                                                    class_name=format!("{} {}", cell_class_clone, sticky_class)
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
