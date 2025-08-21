use leptos::*;
use leptos_router::A;
use serde_json::{json, Map, Value};
use superposition_macros::box_params;
use superposition_types::custom_query::{CustomQuery, PaginationParams, Query};

use crate::api::fetch_dimensions;
use crate::components::button::Button;
use crate::components::dimension_form::DimensionForm;
use crate::components::drawer::PortalDrawer;
use crate::components::skeleton::Skeleton;
use crate::components::table::types::{
    default_column_formatter, ColumnSortable, Expandable,
};
use crate::components::{
    stat::Stat,
    table::{
        types::{Column, TablePaginationProps},
        Table,
    },
};
use crate::query_updater::{use_param_updater, use_signal_from_query};
use crate::types::{OrganisationId, Tenant};

#[derive(Clone)]
enum Action {
    Create,
    None,
}

#[component]
pub fn dimensions() -> impl IntoView {
    let workspace = use_context::<Signal<Tenant>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();
    let action_rws = RwSignal::new(Action::None);
    let pagination_params_rws = use_signal_from_query(move |query_string| {
        Query::<PaginationParams>::extract_non_empty(&query_string).into_inner()
    });

    use_param_updater(move || box_params!(pagination_params_rws.get()));

    let dimensions_resource = create_blocking_resource(
        move || (workspace.get().0, pagination_params_rws.get(), org.get().0),
        |(current_tenant, pagination_params, org_id)| async move {
            fetch_dimensions(&pagination_params, current_tenant, org_id)
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
        Column::default("created_at".to_string()),
        Column::default_with_column_formatter("last_modified_at".to_string(), |_| {
            default_column_formatter("Modified At")
        }),
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
                    .map(|ele| {
                        let mut ele_map = json!(ele).as_object().unwrap().clone();
                        ele_map
                            .insert(
                                "created_at".to_string(),
                                Value::String(ele.created_at.format("%v %T").to_string()),
                            );
                        ele_map
                            .insert(
                                "last_modified_at".to_string(),
                                Value::String(ele.last_modified_at.format("%v %T").to_string()),
                            );
                        ele_map
                    })
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
                            <Button
                                class="self-end"
                                text="Create Dimension"
                                icon_class="ri-add-line"
                                on_click=move |_| action_rws.set(Action::Create)
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
            {move || match action_rws.get() {
                Action::Create => {
                    view! {
                        <PortalDrawer
                            title="Create New Dimension"
                            handle_close=move |_| action_rws.set(Action::None)
                        >
                            <DimensionForm
                                dimensions=dimensions_resource.get().unwrap_or_default().data
                                handle_submit=move |_| {
                                    pagination_params_rws.update(|f| f.reset_page());
                                    dimensions_resource.refetch();
                                    action_rws.set(Action::None);
                                }
                            />
                        </PortalDrawer>
                    }
                        .into_view()
                }
                Action::None => view! {}.into_view(),
            }}
        </Suspense>
    }
}
