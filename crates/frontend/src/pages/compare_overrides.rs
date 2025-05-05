use std::collections::HashMap;

use crate::{
    api::{fetch_dimensions, resolve_config},
    components::{
        alert::AlertType,
        button::Button,
        context_form::ContextForm,
        drawer::{close_drawer, Drawer, DrawerBtn},
        dropdown::DropdownDirection,
        skeleton::Skeleton,
        table::{types::Column, Table},
    },
    logic::Conditions,
    providers::{alert_provider::enqueue_alert, editor_provider::EditorProvider},
    types::{OrganisationId, Tenant},
};
use leptos::*;
use serde_json::{Map, Value};
use superposition_types::custom_query::PaginationParams;

// this maps the column context and the row config key to a particular value
type ComparisonTable = HashMap<String, Map<String, Value>>;

fn table_columns(contexts: Vec<String>) -> Vec<Column> {
    let mut fixed_columns = vec![Column::default("config_key".into())];
    for context in contexts {
        fixed_columns.push(Column::default(context));
    }
    fixed_columns
}

#[component]
pub fn compare_overrides() -> impl IntoView {
    let tenant_rws = use_context::<RwSignal<Tenant>>().unwrap();
    let org_rws = use_context::<RwSignal<OrganisationId>>().unwrap();
    let (context_rs, context_ws) = create_signal::<Conditions>(Conditions::default());
    let (req_inprogess_rs, req_inprogress_ws) = create_signal(false);
    // this vector stores the list of contexts the user is comparing
    let contexts_vector_rws = create_rw_signal(vec!["default_config".to_string()]);
    let source = move || {
        let tenant = tenant_rws.get().0;
        let org_id = org_rws.get().0;
        let contexts = contexts_vector_rws.get();
        (tenant, org_id, contexts)
    };
    let dimension_resource = create_resource(
        move || (tenant_rws.get().0, org_rws.get().0),
        |(tenant, org)| async {
            fetch_dimensions(&PaginationParams::all_entries(), tenant, org)
                .await
                .unwrap_or_default()
        },
    );
    let resolved_config_resource =
        create_blocking_resource(source, |(tenant, org_id, contexts)| async move {
            let mut contexts_config_vector_map: ComparisonTable = HashMap::new();
            logging::log!("Contexts vector: {:#?}", contexts);
            for context in contexts {
                match resolve_config(&tenant, &context, &org_id, false).await {
                    Ok(Value::Object(config)) => {
                        for (config_key, resolved_value) in config {
                            if let Some(row_vector) =
                                contexts_config_vector_map.get_mut(&config_key)
                            {
                                row_vector.insert(
                                    "config_key".into(),
                                    Value::String(config_key.clone()),
                                );
                                row_vector.insert(context.clone(), resolved_value);
                                continue;
                            }
                            let mut row_vector = Map::new();
                            row_vector.insert(
                                "config_key".into(),
                                Value::String(config_key.clone()),
                            );
                            row_vector.insert(context.clone(), resolved_value);
                            contexts_config_vector_map.insert(config_key, row_vector);
                        }
                    }
                    Err(e) => {
                        logging::error!(
                            "Error resolving config for context {}: {}",
                            context,
                            e
                        );
                        enqueue_alert(e.clone(), AlertType::Error, 1000);
                    }
                    _ => {
                        logging::error!("Error resolving config for context {}", context);
                        enqueue_alert(
                            "Could not decode the resolved config".into(),
                            AlertType::Error,
                            1000,
                        );
                    }
                }
            }
            contexts_config_vector_map
        });
    view! {
        <div class="p-8">
            <Suspense fallback=move || view! { <Skeleton /> }>
                { move || {
                    let resolved_config_map = resolved_config_resource.get().unwrap_or_default();
                    let table_columns = table_columns(contexts_vector_rws.get());
                    let dimensions = dimension_resource.get().unwrap_or_default();
                    let data = resolved_config_map.into_values().collect();
                    view! {
                        <div class="card rounded-xl w-full bg-base-100 shadow">
                            <div class="card-body">
                                <div class="flex justify-between">
                                    <h2 class="card-title">Compare Overrides</h2>
                                    <div>
                                        <DrawerBtn drawer_id="add_comparison_drawer"
                                            .to_string()>
                                            Add Comparison <i class="ri-edit-2-line ml-2"></i>
                                        </DrawerBtn>
                                    </div>
                                </div>
                                <Table
                                    rows=data
                                    key_column="config_key".to_string()
                                    columns=table_columns
                                />
                            </div>
                        </div>
                        <Drawer
                            id="add_comparison_drawer".to_string()
                            header="Add a context to compare"
                            handle_close=move || {
                                close_drawer("add_comparison_drawer");
                            }
                        >

                            <EditorProvider>
                                <ContextForm
                                    dimensions=dimensions.data
                                    context=Conditions::default()
                                    heading_sub_text="Compare to...".to_string()
                                    dropdown_direction=DropdownDirection::Right
                                    resolve_mode=true
                                    handle_change=move |new_context| context_ws.update(|value| *value = new_context)
                                />
                                { move || {
                                    let loading = req_inprogess_rs.get();
                                    view! {
                                        <Button
                                            id="resolve_btn".to_string()
                                            text="Submit".to_string()
                                            class="my-4".into()
                                            on_click=move |_| {
                                                req_inprogress_ws.set(true);
                                                let query = context_rs.get().as_query_string();
                                                contexts_vector_rws.update(|value| value.push(query));
                                                req_inprogress_ws.set(false);
                                            }
                                            loading=loading
                                        />
                                    }
                                }}

                            </EditorProvider>
                        </Drawer>

                    }
                }}

            </Suspense>
        </div>
    }
}
