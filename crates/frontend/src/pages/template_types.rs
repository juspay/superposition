use leptos::*;
use leptos_router::A;
use serde_json::{json, Map, Value};
use superposition_types::custom_query::PaginationParams;

use crate::components::button::Button;
use crate::components::table::types::ColumnSortable;
use crate::components::type_template_form::utils::delete_type;
use crate::components::{skeleton::Skeleton, stat::Stat, table::Table};
use crate::types::{OrganisationId, Tenant};
use crate::utils::unwrap_option_or_default_with_error;
use crate::{api::fetch_types, components::table::types::Column};

#[component]
pub fn types_page() -> impl IntoView {
    let tenant_s = use_context::<Signal<Tenant>>().unwrap();
    let org_s = use_context::<Signal<OrganisationId>>().unwrap();
    let types_resource = create_blocking_resource(
        move || (tenant_s.get(), org_s.get()),
        |(tenant, org)| async move {
            fetch_types(&PaginationParams::default(), tenant.0, org.0)
                .await
                .map_or_else(
                    |err| {
                        logging::log!("failed to get types due to: {:?}", err);
                        vec![]
                    },
                    |types| types.data,
                )
        },
    );

    let table_columns = create_memo(move |_| {
        vec![
            Column::default("type_name".to_string()),
            Column::default("type_schema".to_string()),
            Column::default("created_by".to_string()),
            Column::default("created_at".to_string()),
            Column::default("last_modified_at".to_string()),
            Column::new(
                "actions".into(),
                None,
                move |_: &str, row: &Map<String, Value>| {
                    let type_name = row
                        .get("type_name")
                        .map(|v| v.as_str().unwrap().to_string())
                        .unwrap();

                    let delete_type_name = type_name.clone();
                    let delete_click = move |_| {
                        let type_name = delete_type_name.clone();
                        spawn_local({
                            async move {
                                let tenant = tenant_s.get();
                                let org = org_s.get();
                                let _ = delete_type(tenant.0, type_name, org.0).await;
                                types_resource.refetch();
                            }
                        });
                    };
                    view! {
                        <div class="join">
                            <A href={format!("{}/update", type_name)}>
                                <i class="ri-pencil-line ri-xl text-blue-500"></i>
                            </A>

                            <span class="cursor-pointer" on:click=delete_click>
                                <i class="ri-delete-bin-line ri-xl text-red-500"></i>
                            </span>
                        </div>
                    }
                    .into_view()
                },
                ColumnSortable::No,
            ),
        ]
    });

    view! {
        <Suspense fallback=move || view! { <Skeleton /> }>
            <div class="pb-4">
                {move || {
                    let types = types_resource.get().unwrap_or(vec![]);
                    let data = types
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
                                    json!(ele.created_at.format("%v").to_string()),
                                );
                            ele_map
                                .insert(
                                    "last_modified_at".to_string(),
                                    json!(ele.last_modified_at.format("%v").to_string()),
                                );
                            ele_map
                        })
                        .collect::<Vec<Map<String, Value>>>()
                        .to_owned();
                    view! {
                        <div class="pb-4">
                            <Stat
                                heading="Type Templates"
                                icon="ri-t-box-fill"
                                number=types.len().to_string()
                            />
                        </div>
                        <div class="card rounded-xl w-full bg-base-100 shadow">
                            <div class="card-body">
                                <div class="flex justify-between">
                                    <h2 class="card-title">Type Templates</h2>
                                    <div>
                                        <A href="new">
                                            <Button text="Create Type" on_click=move |_| {} />
                                        </A>
                                    </div>
                                </div>
                                <Table
                                    cell_class="".to_string()
                                    rows=data
                                    key_column="id".to_string()
                                    columns=table_columns.get()
                                />
                            </div>
                        </div>
                    }
                }}

            </div>
        </Suspense>
    }
}
