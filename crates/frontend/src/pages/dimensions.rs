use leptos::*;
use leptos_router::A;
use serde_json::{Map, Value, json};
use superposition_macros::box_params;
use superposition_types::custom_query::{CustomQuery, PaginationParams, Query};
use superposition_types::database::models::cac::DimensionType;

use crate::api::dimensions;
use crate::components::button::ButtonAnchor;
use crate::components::datetime::DatetimeStr;
use crate::components::skeleton::Skeleton;
use crate::components::table::types::{
    ColumnSortable, Expandable, default_column_formatter,
};
use crate::components::{
    stat::Stat,
    table::{
        Table,
        types::{Column, TablePaginationProps},
    },
};
use crate::query_updater::{use_param_updater, use_signal_from_query};
use crate::types::{DimensionTypeOptions, OrganisationId, Workspace};

#[component]
pub fn Dimensions() -> impl IntoView {
    let workspace = use_context::<Signal<Workspace>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();
    let pagination_params_rws = use_signal_from_query(move |query_string| {
        Query::<PaginationParams>::extract_non_empty(&query_string).into_inner()
    });

    use_param_updater(move || box_params!(pagination_params_rws.get()));

    let dimensions_resource = create_blocking_resource(
        move || (workspace.get().0, pagination_params_rws.get(), org.get().0),
        |(workspace, pagination_params, org_id)| async move {
            dimensions::fetch(&pagination_params, &workspace, &org_id)
                .await
                .unwrap_or_default()
        },
    );

    let handle_page_change = Callback::new(move |page: i64| {
        pagination_params_rws.update(|f| f.page = Some(page));
    });

    let table_columns = StoredValue::new(vec![
        Column::new(
            "dimension".to_string(),
            false,
            move |dimension_name: &str, _row: &Map<String, Value>| {
                let dimension_name = dimension_name.to_string();
                view! {
                    <A href=dimension_name.clone() class="text-blue-500 underline underline-offset-2">
                        {dimension_name}
                    </A>
                }
            },
            ColumnSortable::No,
            Expandable::Disabled,
            default_column_formatter,
        ),
        Column::default("position".to_string()),
        Column::default_with_cell_formatter(
            "dimension_type".to_string(),
            move |dimension_type: &str, _| {
                let dim_type = serde_json::from_str::<DimensionType>(dimension_type)
                    .unwrap_or_default();
                let dimension_type = DimensionTypeOptions::from(&dim_type);
                view! {
                    <span>{dimension_type.to_string()}</span>
                }
                .into_view()
            },
        ),
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

    view! {
        <Suspense fallback=move || {
            view! { <Skeleton /> }
        }>
            {move || {
                let value = dimensions_resource.get().unwrap_or_default();
                let table_rows = value
                    .data
                    .iter()
                    .map(|ele| json!(ele).as_object().unwrap().clone())
                    .collect::<Vec<Map<String, Value>>>();
                let pagination_params = pagination_params_rws.get();
                let pagination_props = TablePaginationProps {
                    enabled: true,
                    count: pagination_params.count.unwrap_or_default(),
                    current_page: pagination_params.page.unwrap_or_default(),
                    total_pages: value.total_pages,
                    on_page_change: handle_page_change,
                };
                view! {
                    <div class="h-full flex flex-col gap-4">
                        <div class="flex justify-between">
                            <Stat
                                heading="Dimensions"
                                icon="ri-ruler-2-fill"
                                number=value.total_items.to_string()
                            />
                            <ButtonAnchor
                                class="self-end h-10"
                                text="Create Dimension"
                                icon_class="ri-add-line"
                                href="create"
                            />
                        </div>
                        <div class="card w-full bg-base-100 rounded-xl overflow-hidden shadow">
                            <div class="card-body overflow-y-auto overflow-x-visible">
                                <Table
                                    class="!overflow-y-auto"
                                    rows=table_rows
                                    key_column="dimension"
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
