use leptos::*;
use leptos_router::A;
use serde_json::{json, Map, Value};
use superposition_macros::box_params;
use superposition_types::custom_query::{CustomQuery, PaginationParams, Query};

use crate::api::fetch_types;
use crate::components::table::types::TablePaginationProps;
use crate::components::{
    button::Button,
    drawer::PortalDrawer,
    skeleton::Skeleton,
    stat::Stat,
    table::{
        types::{default_column_formatter, Column, ColumnSortable, Expandable},
        Table,
    },
    type_template_form::TypeTemplateForm,
};
use crate::query_updater::{use_param_updater, use_signal_from_query};
use crate::types::{OrganisationId, Tenant};
use crate::utils::unwrap_option_or_default_with_error;

#[derive(Clone)]
enum Action {
    Create,
    None,
}

#[component]
pub fn types_page() -> impl IntoView {
    let workspace = use_context::<Signal<Tenant>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();
    let action_rws = RwSignal::new(Action::None);
    let pagination_params_rws = use_signal_from_query(move |query_string| {
        Query::<PaginationParams>::extract_non_empty(&query_string).into_inner()
    });

    use_param_updater(move || box_params!(pagination_params_rws.get()));

    let types_resource = create_blocking_resource(
        move || (workspace.get().0, org.get().0, pagination_params_rws.get()),
        |(t, org_id, pagination_params)| async move {
            fetch_types(&pagination_params, t, org_id)
                .await
                .unwrap_or_default()
        },
    );

    let table_columns = StoredValue::new(vec![
        Column::new(
            "type_name".to_string(),
            false,
            move |type_name: &str, _row: &Map<String, Value>| {
                let type_name = type_name.to_string();
                view! {
                    <A href=type_name.clone() class="text-blue-500 underline underline-offset-2">
                        {type_name}
                    </A>
                }
            },
            ColumnSortable::No,
            Expandable::Disabled,
            default_column_formatter,
        ),
        Column::default("created_by".to_string()),
        Column::default("created_at".to_string()),
        Column::default_with_column_formatter("last_modified_at".to_string(), |_| {
            default_column_formatter("Modified At")
        }),
    ]);

    let handle_page_change = Callback::new(move |page: i64| {
        pagination_params_rws.update(|f| f.page = Some(page));
    });

    view! {
        <Suspense fallback=move || {
            view! { <Skeleton /> }
        }>
            {move || {
                let types = types_resource.get().unwrap_or_default();
                let data = types
                    .data
                    .iter()
                    .map(|ele| {
                        let mut ele_map = unwrap_option_or_default_with_error(
                                json!(ele).as_object(),
                                &Map::new(),
                            )
                            .to_owned();
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
                    .collect::<Vec<Map<String, Value>>>()
                    .to_owned();
                let pagination_params = pagination_params_rws.get();
                let pagination_props = TablePaginationProps {
                    enabled: true,
                    count: pagination_params.count.unwrap_or_default(),
                    current_page: pagination_params.page.unwrap_or_default(),
                    total_pages: types.total_pages,
                    on_page_change: handle_page_change,
                };

                view! {
                    <div class="h-full flex flex-col gap-4">
                        <div class="flex justify-between">
                            <Stat
                                heading="Type Templates"
                                icon="ri-t-box-fill"
                                number=types.total_items.to_string()
                            />
                            <Button
                                class="self-end"
                                text="Create Type"
                                icon_class="ri-add-line"
                                on_click=move |_| action_rws.set(Action::Create)
                            />
                        </div>
                        <div class="card w-full bg-base-100 rounded-xl overflow-hidden shadow">
                            <div class="card-body overflow-y-auto overflow-x-visible">
                                <Table
                                    class="!overflow-y-auto"
                                    rows=data
                                    key_column="type_name"
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
                            title="Create New Type Template"
                            handle_close=move |_| action_rws.set(Action::None)
                        >
                            <TypeTemplateForm handle_submit=move |_| {
                                pagination_params_rws.update(|f| f.reset_page());
                                types_resource.refetch();
                                action_rws.set(Action::None);
                            } />
                        </PortalDrawer>
                    }
                        .into_view()
                }
                Action::None => view! {}.into_view(),
            }}
        </Suspense>
    }
}
