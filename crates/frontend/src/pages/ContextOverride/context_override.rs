use crate::api::fetch_config;
use crate::api::{delete_context, fetch_default_config, fetch_dimensions};
use crate::components::button::button::Button;
use crate::components::condition_pills::{
    condition_pills::ContextPills,
    utils::{extract_and_format, parse_conditions},
};
use crate::components::context_form::context_form::ContextForm;
use crate::components::context_form::utils::{create_context, update_context};
use crate::components::drawer::drawer::{close_drawer, open_drawer, Drawer, DrawerBtn};
use crate::components::override_form::override_form::OverrideForm;
use crate::components::table::{table::Table, types::Column};
use crate::types::{Config, DefaultConfig, Dimension};
use futures::join;
use leptos::*;
use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};

#[derive(Clone, Debug, Default)]
pub struct TableData {
    pub context: Vec<(String, String, String)>,
    pub overrides: Map<String, Value>,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
struct CombinedResourceOverride {
    config: Option<Config>,
    dimensions: Vec<Dimension>,
    default_config: Vec<DefaultConfig>,
}

#[component]
pub fn context_override() -> impl IntoView {
    let tenant_rs = use_context::<ReadSignal<String>>().unwrap();

    let selected_context_and_override = create_rw_signal::<Option<TableData>>(None);
    let form_data = create_rw_signal::<Option<TableData>>(None);

    let table_columns = create_memo(move |_| {
        vec![
            Column::default("KEY".to_string()),
            Column::default("VALUE".to_string()),
        ]
    });

    let edit_signal = create_rw_signal(false);

    let combined_resource: Resource<String, CombinedResourceOverride> =
        create_blocking_resource(
            move || tenant_rs.get().clone(),
            |current_tenant| async move {
                let (config_result, dimensions_result, default_config_result) = join!(
                    fetch_config(current_tenant.to_string()),
                    fetch_dimensions(current_tenant.to_string()),
                    fetch_default_config(current_tenant.to_string())
                );
                CombinedResourceOverride {
                    config: config_result.ok(),
                    dimensions: dimensions_result.unwrap_or_else(|_| vec![]),
                    default_config: default_config_result.unwrap_or_else(|_| vec![]),
                }
            },
        );

    let handle_create_click = Callback::new(move |_| {
        edit_signal.set(false);
        form_data.set(Some(TableData {
            context: vec![],
            overrides: Map::new(),
        }));
    });

    view! {
        <div class="p-8">
            <div class="flex justify-between">
                <h2 class="card-title">Overrides</h2>
                <DrawerBtn drawer_id="context_and_override_drawer".to_string() on_click = handle_create_click.clone() >
                Create Override <i class="ri-edit-2-line ml-2"></i>
                </DrawerBtn>
            </div>

            <Suspense fallback=move || {
                view! { <p>"Loading (Suspense Fallback)..."</p> }
            }>
            <div class="space-y-6">
                {
                    move || {
                        let dimension_value = combined_resource.get().map(|r| r.dimensions.clone());
                        let default_config_value = combined_resource.get().map(|r| r.default_config.clone());
                        let on_submit =
                            move |_| {
                                spawn_local(async move {
                                   let form_data_clone = form_data.get(); // This holds both the context and overrides
                                   let dimensions = combined_resource.get().map(|r| r.dimensions.clone());
                                   if let Some(final_form_data) = form_data_clone {
                                      let result = if edit_signal.get_untracked() {
                                        update_context(
                                            tenant_rs.get().clone(),
                                            final_form_data.overrides.clone(), // Access by cloning the inner data
                                            final_form_data.context.clone(),  // Access by cloning the inner data
                                            dimensions.unwrap_or_default()
                                        ).await
                                      } else {
                                        create_context(
                                            tenant_rs.get().clone(),
                                            final_form_data.overrides.clone(), // Access by cloning the inner data
                                            final_form_data.context.clone(),  // Access by cloning the inner data
                                            dimensions.unwrap_or_default()
                                        ).await
                                      };


                                       match result {
                                           Ok(_) => {
                                               combined_resource.refetch();
                                               logging::log!("Context and overrides submitted successfully");
                                               form_data.set(None);
                                               selected_context_and_override.set(None); // Reset the state
                                               edit_signal.set(false);
                                               close_drawer("context_and_override_drawer");
                                           },
                                           Err(e) => {
                                               logging::log!("Error submitting context and overrides: {:?}", e);
                                           }
                                       }
                                   } else {
                                       logging::log!("No data to submit");
                                   }
                          });
                          };
                        if let Some(_selected_data) = selected_context_and_override.get() {
                            logging::log!("it entered 1");
                            let handle_close =
                                move || {
                                    close_drawer("context_and_override_drawer");
                                    selected_context_and_override.set(None);
                                    form_data.set(None);
                                    edit_signal.set(false);
                                };
                            logging::log!("form data overrides {:?}", form_data.get_untracked().unwrap().overrides);
                            logging::log!("form data contexts {:?}", form_data.get_untracked().unwrap().context);

                            let form_data_untracked = form_data.get_untracked().unwrap();

                            let cleaned_context = form_data_untracked.context.iter().map(|(dim, op, val)| {
                                let new_op = match op.trim().to_uppercase().as_str() {
                                    "IS" => "==",
                                    "HAS" => "IN",
                                    "BETWEEN (inclusive)" => "<=",
                                    _ => op.trim()
                                };
                                (dim.trim().to_string(), new_op.to_string(), val.clone())
                            }).collect::<Vec<(String, String, String)>>();

                            let overrides = form_data_untracked.overrides.clone();
                            let cleaned_overrides = overrides.iter().map(|(key, value)| {
                                let clean_key = key.trim_matches('\"').to_string();

                                let clean_value = match value {
                                    Value::String(s) => Value::String(s.clone()),
                                    _ => value.clone(),
                                };

                                (clean_key, clean_value)
                            }).collect::<Map<String, Value>>();

                            view! {
                                <Drawer id="context_and_override_drawer".to_string() header="Edit Override" handle_close=handle_close>
                                    <ContextForm
                                        dimensions=dimension_value.unwrap_or_default()
                                        context=cleaned_context
                                        is_standalone=false
                                      handle_change=move |new_context| {
                                         form_data.update(|prev| {
                                             if let Some(inner) = prev {
                                                 inner.context = new_context;
                                             }
                                         })
                                     }
                                         disabled = edit_signal.get()
                                    />
                                    <OverrideForm
                                        overrides=cleaned_overrides
                                        default_config=default_config_value.unwrap_or_default()
                                        is_standalone=false
                                        handle_change=
                                        move |new_overrides| {
                                            form_data.update(|prev| {
                                                if let Some(inner) = prev {
                                                    inner.overrides = new_overrides;
                                                }
                                            });
                                        }

                                    />
                                    <div class="form-control grid w-full justify-end">
                                    <Button class="pl-[70px] pr-[70px]".to_string() text="Submit".to_string() on_click= on_submit.clone() />
                                    </div>
                                </Drawer>
                            }
                        } else {
                            logging::log!("it entered 2" );
                            view! {
                                <Drawer id="context_and_override_drawer".to_string() header="Create New Dimension" handle_close=move || {form_data.set(None); close_drawer("context_and_override_drawer"); edit_signal.set(false); combined_resource.refetch() }>
                                    <ContextForm
                                      dimensions= dimension_value.unwrap_or_default()
                                      context=vec![]
                                      handle_change=
                                    move |new_context| {
                                        form_data.update(|prev| {
                                            if let Some(inner) = prev {
                                                inner.context = new_context;
                                            }
                                        });
                                    }
                                    />
                                    <OverrideForm
                                        overrides=Map::new()
                                        default_config= default_config_value.unwrap_or_default()
                                        handle_change=
                                    move |new_overrides| {
                                        form_data.update(|prev| {
                                            if let Some(ref mut inner) = prev {
                                                inner.overrides = new_overrides;
                                            }
                                        });
                                    }
                                    />
                                    <div class="form-control grid w-full justify-end">
                                    <Button class="pl-[70px] pr-[70px]".to_string() text="Submit".to_string() on_click= on_submit.clone() />
                                    </div>
                                </Drawer>
                            }
                        }
                    }
                }

                {
                        move || {
                            let handle_delete =
                            move |_,context_id| {
                                spawn_local(async move {
                                       let result = delete_context(
                                           tenant_rs.get().clone(),
                                           context_id
                                       ).await;

                                       match result {
                                           Ok(_) => {
                                               logging::log!("Context and overrides deleted successfully");
                                               form_data.set(None);
                                               selected_context_and_override.set(None); // Reset the state
                                               combined_resource.refetch();
                                               close_drawer("context_and_override_drawer");
                                           },
                                           Err(e) => {
                                               logging::log!("Error deleting context and overrides: {:?}", e);
                                           }
                                       }
                          });
                          };
                            if let Some(resource) = combined_resource.get() {
                                        let config = resource.config.as_ref().unwrap();
                                        let mut contexts: Vec<Map<String, Value>> = Vec::new();
                                        let mut context_views = Vec::new();
                                        let mut overrides = Map::new();

                                        for context in config.contexts.iter() {
                                            for key in context.override_with_keys.iter() {
                                                let mut map = Map::new();
                                                let ovr = config.overrides.get(key).unwrap();
                                                let ovr_obj = ovr.as_object().unwrap();
                                                for (key, value) in ovr_obj.iter() {
                                                    let trimmed_key =
                                                        key.trim_matches('"').to_string();
                                                    let formatted_value = Value::String(
                                                        format!("{}", value).trim_matches('"').to_string(),
                                                    );
                                                    overrides.insert(trimmed_key.clone(), formatted_value.clone());
                                                    map.insert("KEY".to_string(), Value::String(trimmed_key));
                                                    map.insert("VALUE".to_string(), formatted_value);
                                                    contexts.push(map.clone());
                                                }
                                            }
                                            let overrides_clone = overrides.clone();
                                            let context_data_clone_for_display = context.condition.clone();
                                            let context_data_clone_for_click = context.condition.clone();
                                            let context_id = context.id.clone();
                                            logging::log!("override signal: {:?}", overrides_clone.clone());

                                            let create_view = || {
                                                        let override_data = overrides_clone.clone();
                                                        let override_data_for_edit = override_data.clone();
                                                        let context_data_for_edit = context_data_clone_for_display.clone();

                                                        view! {
                                                            <div class="rounded-lg shadow bg-base-100 p-6 shadow">
                                                                <div class="flex justify-between">
                                                                    <div class="flex items-center space-x-4">
                                                                        <h3 class="card-title text-base timeline-box text-gray-800 bg-base-100 shadow-md font-mono">
                                                                            "Condition"
                                                                        </h3>
                                                                        <i class="ri-arrow-right-fill ri-xl text-blue-500"></i>
                                                                        <ContextPills context=context_data_clone_for_display/>
                                                                    </div>
                                                                    <div class= "flex space-x-4">
                                                                    <i class="ri-pencil-line ri-xl text-blue-500" on:click = move |_| {
                                                                        edit_signal.set(true);
                                                                        logging::log!("entered 1") ;
                                                                        let condition = extract_and_format(&context_data_clone_for_click);
                                                                        let ctx_values = parse_conditions(condition.clone());
                                                                        let transformed_context_data = ctx_values.iter().map(|item| {
                                                                            (item.left_operand.clone(), item.operator.clone(),item.right_operand.clone())
                                                                        }).collect::<Vec<(String, String, String)>>();

                                                                        logging::log!("override data for edit: {:?}", override_data_for_edit.clone());

                                                                        form_data.set(Some(TableData {
                                                                            context: transformed_context_data.clone(),
                                                                            overrides: override_data_for_edit.clone()
                                                                        }));
                                                                        selected_context_and_override.set(Some(TableData {
                                                                            context: vec![],
                                                                            overrides: override_data_for_edit.clone()
                                                                        }));
                                                                        open_drawer("context_and_override_drawer");
                                                                    }> </i>
                                                                    <i class="ri-file-copy-line ri-xl text-blue-500" on:click = move |_| {
                                                                        edit_signal.set(false);
                                                                        logging::log!("entered 1") ;
                                                                        let condition = extract_and_format(&context_data_for_edit);
                                                                        let ctx_values = parse_conditions(condition.clone());
                                                                        let transformed_context_data = ctx_values.iter().map(|item| {
                                                                            (item.left_operand.trim().to_string(), item.operator.trim().to_string(), item.right_operand.trim().to_string())
                                                                        }).collect::<Vec<(String, String, String)>>();
                                                                        logging::log!("transformed data {:?}",transformed_context_data.clone());

                                                                        form_data.set(Some(TableData {
                                                                            context: transformed_context_data.clone(),
                                                                            overrides: override_data.clone()
                                                                        }));
                                                                        selected_context_and_override.set(Some(TableData {
                                                                            context: vec![],
                                                                            overrides: override_data.clone()
                                                                        }));
                                                                        open_drawer("context_and_override_drawer");
                                                                    }> </i>
                                                                    <i class="ri-delete-bin-5-line ri-xl text-blue-500" on:click = move |e| {

                                                                        edit_signal.set(false);
                                                                        logging::log!("entered delete");
                                                                        logging::log!("context_id {:?}", context_id.clone());
                                                                        handle_delete(e, context_id.clone()) } ></i>
                                                                    </div>
                                                                </div>
                                                                <div class="space-x-4">
                                                                    <Table
                                                                        cell_style="min-w-48 font-mono".to_string()
                                                                        rows=contexts.clone()
                                                                        key_column="id".to_string()
                                                                        columns=table_columns.get()
                                                                    />
                                                                </div>
                                                            </div>
                                                        }
                                                    };
                                            context_views.push(create_view());
                                            contexts.clear();
                                            overrides.clear();
                                        }
                                        context_views
                                    }
                                    else {
                                        vec![view! { <div>Loading....</div> }]
                                    }
                            }
                    }

            </div>
            </Suspense>
        </div>
    }
}
