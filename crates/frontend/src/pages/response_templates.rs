use leptos::*;
use leptos_router::{use_navigate, A};
use serde_json::{json, Map, Value};
use superposition_macros::box_params;
use superposition_types::custom_query::{CustomQuery, PaginationParams, Query};

use crate::api::response_templates;
use crate::components::datetime::DatetimeStr;
use crate::components::table::types::TablePaginationProps;
use crate::components::{
    button::Button,
    skeleton::Skeleton,
    stat::Stat,
    table::{
        types::{default_column_formatter, Column, ColumnSortable, Expandable},
        Table,
    },
};
use crate::query_updater::{use_param_updater, use_signal_from_query};
use crate::types::{OrganisationId, Workspace};
use crate::utils::unwrap_option_or_default_with_error;

#[component]
pub fn ResponseTemplates() -> impl IntoView {
    let workspace = use_context::<Signal<Workspace>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();
    let pagination_params_rws = use_signal_from_query(move |query_string| {
        Query::<PaginationParams>::extract_non_empty(&query_string).into_inner()
    });

    use_param_updater(move || box_params!(pagination_params_rws.get()));

    let templates_resource = create_blocking_resource(
        move || (workspace.get().0, org.get().0, pagination_params_rws.get()),
        |(workspace, org_id, pagination_params)| async move {
            response_templates::fetch(&pagination_params, &workspace, &org_id)
                .await
                .unwrap_or_default()
        },
    );

    let table_columns = StoredValue::new(vec![
        Column::new(
            "name".to_string(),
            false,
            move |name: &str, _row: &Map<String, Value>| {
                let name = name.to_string();
                view! {
                    <A href=name.clone() class="text-blue-500 underline underline-offset-2">
                        {name}
                    </A>
                }
            },
            ColumnSortable::No,
            Expandable::Disabled,
            default_column_formatter,
        ),
        Column::default("context_id".to_string()),
        Column::default("content_type".to_string()),
        Column::default("created_by".to_string()),
        Column::default_with_cell_formatter("created_at".to_string(), |value, _| {
            view! {
                <DatetimeStr datetime=value.into() />
            }
        }),
        Column::new(
            "last_modified_at".to_string(),
            false,
            |value, _| {
                view! {
                    <DatetimeStr datetime=value.into() />
                }
            },
            ColumnSortable::No,
            Expandable::Enabled(100),
            |_| default_column_formatter("Modified At"),
        ),
    ]);

    let handle_page_change = Callback::new(move |page: i64| {
        pagination_params_rws.update(|f| f.page = Some(page));
    });

    let handle_create = move |_| {
        let navigate = use_navigate();
        let url = format!(
            "/admin/{}/{}/response-templates/create",
            org.get().0,
            workspace.get().0
        );
        navigate(&url, Default::default());
    };

    view! {
        <Suspense fallback=move || {
            view! { <Skeleton /> }
        }>
            {move || {
                let templates = templates_resource.get().unwrap_or_default();
                let data = templates
                    .data
                    .iter()
                    .map(|ele| {
                        unwrap_option_or_default_with_error(json!(ele).as_object(), &Map::new())
                            .to_owned()
                    })
                    .collect::<Vec<Map<String, Value>>>()
                    .to_owned();
                let pagination_params = pagination_params_rws.get();
                let pagination_props = TablePaginationProps {
                    enabled: true,
                    count: pagination_params.count.unwrap_or_default(),
                    current_page: pagination_params.page.unwrap_or_default(),
                    total_pages: templates.total_pages,
                    on_page_change: handle_page_change,
                };

                view! {
                    <div class="h-full flex flex-col gap-4">
                        <div class="flex justify-between">
                            <Stat
                                heading="Response Templates"
                                icon="ri-file-text-fill"
                                number=templates.total_items.to_string()
                            />
                            <Button
                                class="self-end"
                                text="Create Response Template"
                                icon_class="ri-add-line"
                                on_click=handle_create
                            />
                        </div>
                        <div class="card w-full bg-base-100 rounded-xl overflow-hidden shadow">
                            <div class="card-body overflow-y-auto overflow-x-visible">
                                <Table
                                    class="!overflow-y-auto"
                                    rows=data
                                    key_column="name"
                                    columns=table_columns.get_value()
                                    pagination=pagination_props
                                />
                            </div>
                        </div>
                    </div>
                }
            }}
        </Suspense>
    }
}
