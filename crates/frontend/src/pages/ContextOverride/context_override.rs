use std::rc::Rc;

use crate::api::fetch_config;
use crate::api::{fetch_default_config, fetch_dimensions};
use crate::components::button::button::Button;
use crate::components::condition_pills::condition_pills::ContextPills;
use crate::components::context_form::context_form::ContextForm;
use crate::components::context_form::utils::create_context;
use crate::components::override_form::override_form::OverrideForm;
use crate::components::skeleton::{Skeleton, SkeletonVariant};
use crate::components::table::{table::Table, types::Column};
use crate::types::Dimension;
use crate::utils::modal_action;
use leptos::*;
use serde_json::{Map, Value};
use web_sys::MouseEvent;

#[component]
fn context_modal_form<NF>(
    handle_change: NF,
    dimensions: Resource<String, Result<Vec<Dimension>, ServerFnError>>,
) -> impl IntoView
where
    NF: Fn(Vec<(String, String, String)>) + 'static + Clone,
{
    view! {
        <div>
            <Suspense fallback=move || {
                view! { <p>Loading ...</p> }
            }>

                {
                    let handle_change_clone = handle_change.clone();
                    move || {
                        let handle_change_clone_clone = handle_change_clone.clone();
                        dimensions
                            .with(move |result| {
                                match result {
                                    Some(Ok(dimension)) => {
                                        view! {
                                            <div>
                                                <ContextForm
                                                    dimensions=dimension.clone()
                                                    context=vec![]
                                                    is_standalone=false
                                                    handle_change=handle_change_clone_clone.clone()
                                                />
                                            </div>
                                        }
                                    }
                                    Some(Err(error)) => {
                                        view! {
                                            <div class="text-red-500">
                                                {"Failed to fetch config data: "} {error.to_string()}
                                            </div>
                                        }
                                    }
                                    None => {
                                        view! { <div>Loading....</div> }
                                    }
                                }
                            })
                    }
                }

            </Suspense>
        </div>
    }
}

#[component]
fn override_modal_form<NF>(handle_change: NF) -> impl IntoView
where
    NF: Fn(Map<String, Value>) + 'static + Clone,
{
    let tenant_rs = use_context::<ReadSignal<String>>().unwrap();
    let default_config = create_blocking_resource(
        move || tenant_rs.get(),
        move |current_tenant| fetch_default_config(current_tenant.clone()),
    );

    view! {
        <div>
            <Suspense fallback=move || {
                view! { <p>Loading ...</p> }
            }>

                {
                    let handle_change_clone = handle_change.clone();
                    move || {
                        let handle_change_clone_clone = handle_change_clone.clone();
                        default_config
                            .with(move |result| {
                                match result {
                                    Some(Ok(config)) => {
                                        view! {
                                            <div>
                                                <OverrideForm
                                                    overrides=Map::new()
                                                    default_config=config.clone()
                                                    is_standalone=false
                                                    handle_change=handle_change_clone_clone.clone()
                                                />
                                            </div>
                                        }
                                    }
                                    Some(Err(error)) => {
                                        view! {
                                            <div class="text-red-500">
                                                {"Failed to fetch config data: "} {error.to_string()}
                                            </div>
                                        }
                                    }
                                    None => {
                                        view! { <div>Loading....</div> }
                                    }
                                }
                            })
                    }
                }

            </Suspense>
        </div>
    }
}

#[component]
fn modal_component(handle_submit: Rc<dyn Fn()>) -> impl IntoView {
    let (context_condition, set_context_condition) =
        create_signal::<Vec<(String, String, String)>>(vec![]);
    let (overrides, set_overrides) = create_signal::<Map<String, Value>>(Map::new());

    let tenant_rs = use_context::<ReadSignal<String>>().unwrap();

    let handle_context_change = move |updated_ctx: Vec<(String, String, String)>| {
        set_context_condition.set(updated_ctx);
    };
    let handle_overrides_change = move |updated_overrides: Map<String, Value>| {
        set_overrides.set(updated_overrides);
    };

    let dimensions = create_blocking_resource(
        move || tenant_rs.get(),
        move |current_tenant| fetch_dimensions(current_tenant.clone()),
    );

    let (error_message, set_error_message) = create_signal("".to_string());

    let on_submit = {
        move |ev: MouseEvent| {
            let handle_submit_clone = handle_submit.clone();
            let current_tenant = tenant_rs.get();
            ev.prevent_default();

            logging::log!("tirggering submit");

            spawn_local({
                let handle_submit = handle_submit_clone;
                let overrides = move || overrides.get();
                let context_conditions = move || context_condition.get();
                let dimensions = move || dimensions.get();
                async move {
                    let result = create_context(
                        current_tenant,
                        overrides(),
                        context_conditions(),
                        dimensions().unwrap().expect("resource not loaded"),
                    )
                    .await;

                    match result {
                        Ok(_) => {
                            handle_submit();
                            modal_action("my_modal_5", "close")
                        }
                        Err(e) => {
                            if e.is_empty() {
                                set_error_message
                                    .set("Internal_Server_error".to_string());
                            } else {
                                set_error_message.set(e);
                            }
                        }
                    }
                }
            });
        }
    };

    view! {
        <dialog id="my_modal_5" class="modal">
            <div class="modal-box relative bg-white w-12/12 max-w-4xl">
                <form method="dialog" class="flex justify-end">
                    <button>
                        <i class="ri-close-fill"></i>
                    </button>
                </form>
                <form class="form-control w-full mt-8 bg-white text-gray-700 font-mono">
                    <div>
                        <ContextModalForm
                            handle_change=handle_context_change
                            dimensions=dimensions
                        />
                    </div>
                    <div class="mt-7">
                        <OverrideModalForm handle_change=handle_overrides_change/>
                    </div>
                    <div class="form-control mt-7">
                        <Button
                            text="Submit".to_string()
                            on_click=move |ev: MouseEvent| on_submit(ev)
                        />
                    </div>
                    <div class="mt-7">
                        <p class="text-red-500">{move || error_message.get()}</p>
                    </div>
                </form>
            </div>
        </dialog>
    }
}

#[component]
pub fn context_override() -> impl IntoView {
    let tenant_rs = use_context::<ReadSignal<String>>().unwrap();

    let context_data: Vec<(String, String, String)> = vec![];
    let ctx: RwSignal<Vec<(String, String, String)>> = create_rw_signal(context_data);

    let override_data = Map::new();

    let ovr_data = create_rw_signal(override_data);

    provide_context(ctx);

    provide_context(ovr_data);

    let config_data = create_blocking_resource(
        move || tenant_rs.get(),
        move |current_tenant| fetch_config(current_tenant.clone()),
    );

    let table_columns = create_memo(move |_| {
        vec![
            Column::default("KEY".to_string()),
            Column::default("VALUE".to_string()),
        ]
    });

    view! {
        <div class="p-8">
            <div class="flex justify-between">
                <h2 class="card-title">Overrides</h2>
                <Button
                    text="Create Context Overrides".to_string()
                    on_click=|_| modal_action("my_modal_5", "open")
                />
            </div>
            <div class="space-y-6">
                <ModalComponent handle_submit=Rc::new(move || config_data.refetch())/>
                <Suspense fallback=move || {
                    view! { <Skeleton variant=SkeletonVariant::Block/> }
                }>

                    {move || {
                        config_data
                            .with(move |result| {
                                match result {
                                    Some(Ok(config)) => {
                                        let mut contexts: Vec<Map<String, Value>> = Vec::new();
                                        let mut context_views = Vec::new();
                                        let mut override_signal = Map::new();
                                        for context in config.contexts.iter() {
                                            for key in context.override_with_keys.iter() {
                                                let mut map = Map::new();
                                                let ovr = config.overrides.get(key).unwrap();
                                                let ovr_obj = ovr.as_object().unwrap();
                                                for (key, value) in ovr_obj.iter() {
                                                    let trimmed_key = Value::String(
                                                        key.trim_matches('"').to_string(),
                                                    );
                                                    let formatted_value = Value::String(
                                                        format!("{}", value).trim_matches('"').to_string(),
                                                    );
                                                    override_signal
                                                        .insert(trimmed_key.to_string(), formatted_value.clone());
                                                    map.insert("KEY".to_string(), trimmed_key);
                                                    map.insert("VALUE".to_string(), formatted_value);
                                                    contexts.push(map.clone());
                                                }
                                            }
                                            context_views
                                                .push(
                                                    view! {
                                                        <div class="rounded-lg shadow bg-base-100 p-6 shadow">
                                                            <div class="flex justify-between">
                                                                <div class="flex items-center space-x-4">

                                                                    <h3 class="card-title text-base timeline-box text-gray-800 bg-base-100 shadow-md font-mono">
                                                                        "Condition"
                                                                    </h3>
                                                                    <i class="ri-arrow-right-fill ri-xl text-blue-500"></i>
                                                                    <ContextPills context=context.condition.clone()/>
                                                                </div>
                                                                <button class="p-2 rounded hover:bg-gray-200 transition-colors">
                                                                    <i class="ri-edit-line text-blue-500"></i>
                                                                </button>
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
                                                    },
                                                );
                                            contexts.clear();
                                        }
                                        ovr_data.set(override_signal);
                                        context_views
                                    }
                                    Some(Err(error)) => {
                                        vec![
                                            view! {
                                                <div class="text-red-500">
                                                    {"Failed to fetch config data: "} {error.to_string()}
                                                </div>
                                            },
                                        ]
                                    }
                                    None => vec![view! { <div>Loading....</div> }],
                                }
                            })
                    }}

                </Suspense>
            </div>
        </div>
    }
}
