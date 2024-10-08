use crate::api::fetch_config;
use crate::api::{delete_context, fetch_default_config, fetch_dimensions};
use crate::components::alert::AlertType;
use crate::components::button::Button;
use crate::components::context_card::ContextCard;
use crate::components::context_form::utils::{create_context, update_context};
use crate::components::context_form::ContextForm;
use crate::components::delete_modal::DeleteModal;
use crate::components::drawer::{close_drawer, open_drawer, Drawer, DrawerBtn};
use crate::components::override_form::OverrideForm;
use crate::components::skeleton::{Skeleton, SkeletonVariant};
use crate::logic::{Condition, Conditions, Operator};
use crate::providers::alert_provider::enqueue_alert;
use crate::providers::condition_collapse_provider::ConditionCollapseProvider;
use crate::providers::editor_provider::EditorProvider;
use crate::schema::SchemaType;
use crate::types::{Config, Context, DefaultConfig, Dimension};
use futures::join;
use leptos::*;
use serde::{Deserialize, Serialize};
use serde_json::{json, Map, Value};

#[derive(Clone, Debug, Default)]
pub struct Data {
    pub context: Conditions,
    pub overrides: Vec<(String, Value)>,
}

#[derive(Serialize, Deserialize, Clone, Debug, Default)]
struct PageResource {
    config: Config,
    dimensions: Vec<Dimension>,
    default_config: Vec<DefaultConfig>,
}

#[derive(Debug, Clone)]
enum FormMode {
    Edit,
    Create,
}

#[component]
fn form(
    context: Conditions,
    overrides: Vec<(String, Value)>,
    dimensions: Vec<Dimension>,
    edit: bool,
    default_config: Vec<DefaultConfig>,
    handle_submit: Callback<(), ()>,
) -> impl IntoView {
    let tenant_rs = use_context::<ReadSignal<String>>().unwrap();
    let (context, set_context) = create_signal(context);
    let (overrides, set_overrides) = create_signal(overrides);
    let dimensions = StoredValue::new(dimensions);
    let (req_inprogess_rs, req_inprogress_ws) = create_signal(false);

    let on_submit = move |_| {
        req_inprogress_ws.set(true);
        spawn_local(async move {
            let f_context = context.get();
            let f_overrides = overrides.get();
            let result = if edit {
                update_context(
                    tenant_rs.get().clone(),
                    Map::from_iter(f_overrides),
                    f_context,
                )
                .await
            } else {
                create_context(
                    tenant_rs.get().clone(),
                    Map::from_iter(f_overrides),
                    f_context,
                )
                .await
            };

            match result {
                Ok(_) => {
                    logging::log!("Context and overrides submitted successfully");
                    handle_submit.call(());
                }
                Err(e) => {
                    logging::log!("Error submitting context and overrides: {:?}", e);
                }
            }
            req_inprogress_ws.set(false);
        });
    };
    view! {
        <ContextForm
            dimensions=dimensions.get_value()
            context=context.get_untracked()
            handle_change=move |new_context| {
                set_context
                    .update(|value| {
                        *value = new_context;
                    });
            }

            disabled=edit
        />
        <OverrideForm
            overrides=overrides.get_untracked()
            default_config=default_config
            handle_change=move |new_overrides| {
                set_overrides
                    .update(|value| {
                        *value = new_overrides;
                    });
            }
        />

        <div class="flex justify-start w-full mt-10">
            {move || {
                let loading = req_inprogess_rs.get();
                view! {
                    <Button
                        class="pl-[70px] pr-[70px] w-48 h-12".to_string()
                        text="Submit".to_string()
                        on_click=on_submit.clone()
                        loading
                    />
                }
            }}

        </div>
    }
}

#[component]
pub fn context_override() -> impl IntoView {
    let tenant_rs = use_context::<ReadSignal<String>>().unwrap();

    let (selected_context_rs, selected_context_ws) = create_signal::<Option<Data>>(None);
    let (form_mode, set_form_mode) = create_signal::<Option<FormMode>>(None);
    let (modal_visible, set_modal_visible) = create_signal(false);
    let (delete_id, set_delete_id) = create_signal::<Option<String>>(None);

    let page_resource: Resource<String, PageResource> = create_blocking_resource(
        move || tenant_rs.get().clone(),
        |current_tenant| async move {
            let (config_result, dimensions_result, default_config_result) = join!(
                fetch_config(current_tenant.to_string()),
                fetch_dimensions(current_tenant.to_string()),
                fetch_default_config(current_tenant.to_string())
            );
            PageResource {
                config: config_result.unwrap_or_default(),
                dimensions: dimensions_result
                    .unwrap_or_default()
                    .into_iter()
                    .filter(|d| d.dimension != "variantIds")
                    .collect(),
                default_config: default_config_result.unwrap_or_default(),
            }
        },
    );

    let on_create_context_click = Callback::new(move |_| {
        set_form_mode.set(Some(FormMode::Create));
        let PageResource { dimensions, .. } = page_resource.get().unwrap_or_default();
        let mut default_ctx: Conditions = Conditions(vec![]);
        for dim in dimensions.iter().filter(|v| v.mandatory) {
            let r#type = SchemaType::try_from(dim.schema.clone());
            if let Err(_) = r#type {
                //TODO emit an alert and return
                return;
            }

            let condition = Condition::try_from((
                Operator::Is,
                dim.dimension.clone(),
                r#type.unwrap(),
            ));
            if let Err(_) = condition {
                //TODO emit and alert and return
                return;
            }

            default_ctx.push(condition.unwrap());
        }

        selected_context_ws.set(Some(Data {
            context: default_ctx,
            overrides: vec![],
        }));
        open_drawer("context_and_override_drawer");
    });

    let on_submit = Callback::new(move |_| {
        close_drawer("context_and_override_drawer");
        set_form_mode.set(None);
        selected_context_ws.set(None);
        page_resource.refetch();
    });

    let on_context_edit = Callback::new(move |data: (Context, Map<String, Value>)| {
        let (context, overrides) = data;
        let conditions = Conditions::from_context_json(context.condition).unwrap();

        selected_context_ws.set(Some(Data {
            context: conditions,
            overrides: overrides.into_iter().collect::<Vec<(String, Value)>>(),
        }));
        set_form_mode.set(Some(FormMode::Edit));

        open_drawer("context_and_override_drawer");
    });

    let on_context_clone = Callback::new(move |data: (Context, Map<String, Value>)| {
        let (context, overrides) = data;
        let conditions = Conditions::from_context_json(context.condition).unwrap();

        selected_context_ws.set(Some(Data {
            context: conditions,
            overrides: overrides.into_iter().collect::<Vec<(String, Value)>>(),
        }));
        set_form_mode.set(Some(FormMode::Create));

        open_drawer("context_and_override_drawer");
    });

    let on_context_delete = Callback::new(move |id: String| {
        set_delete_id.set(Some(id.clone()));
        set_modal_visible.set(true);
    });

    let on_delete_confirm = Callback::new(move |_| {
        if let Some(id) = delete_id.get().clone() {
            spawn_local(async move {
                let result = delete_context(tenant_rs.get(), id).await;

                match result {
                    Ok(_) => {
                        logging::log!("Context and overrides deleted successfully");
                        page_resource.refetch();
                    }
                    Err(e) => {
                        logging::log!("Error deleting context and overrides: {:?}", e);
                    }
                }
            });
        }
        set_delete_id.set(None);
        set_modal_visible.set(false);
    });

    view! {
        <div class="p-8">
            <div class="flex justify-between">
                <h2 class="card-title">Overrides</h2>
                <DrawerBtn
                    drawer_id="context_and_override_drawer".to_string()
                    on_click=on_create_context_click
                >
                    Create Override
                    <i class="ri-edit-2-line ml-2"></i>
                </DrawerBtn>
            </div>

            <Suspense fallback=move || {
                view! { <Skeleton variant=SkeletonVariant::Block/> }
            }>
                <div class="space-y-6">

                    {move || {
                        let PageResource { config: _, dimensions, default_config } = page_resource
                            .get()
                            .unwrap_or_default();
                        let data = selected_context_rs.get();
                        let drawer_header = match form_mode.get() {
                            Some(FormMode::Edit) => "Update Overrides",
                            Some(FormMode::Create) => "Create Overrides",
                            None => "",
                        };
                        view! {
                            <Drawer
                                id="context_and_override_drawer".to_string()
                                header=drawer_header
                                handle_close=move || {
                                    close_drawer("context_and_override_drawer");
                                    set_form_mode.set(None);
                                    selected_context_ws.set(None);
                                }
                            >

                                <EditorProvider>
                                    {match (form_mode.get_untracked(), data) {
                                        (Some(FormMode::Edit), Some(data)) => {
                                            view! {
                                                <Form
                                                    context=data.context
                                                    overrides=data.overrides
                                                    dimensions=dimensions
                                                    default_config=default_config
                                                    handle_submit=on_submit
                                                    edit=true
                                                />
                                            }
                                                .into_view()
                                        }
                                        (Some(FormMode::Create), data) => {
                                            let Data { context, overrides } = data.unwrap_or_default();
                                            view! {
                                                <Form
                                                    context=context
                                                    overrides=overrides
                                                    dimensions=dimensions
                                                    default_config=default_config
                                                    handle_submit=on_submit
                                                    edit=false
                                                />
                                            }
                                                .into_view()
                                        }
                                        (Some(FormMode::Edit), None) => {
                                            enqueue_alert(
                                                String::from("Something went wrong, failed to load form"),
                                                AlertType::Error,
                                                5000,
                                            );
                                            view! {}.into_view()
                                        }
                                        (None, _) => view! {}.into_view(),
                                    }}

                                </EditorProvider>
                            </Drawer>
                        }
                    }}
                    {move || {
                        let config = page_resource.get().map(|v| v.config).unwrap_or_default();
                        let ctx_n_overrides = config
                            .contexts
                            .into_iter()
                            .map(|context| {
                                let overrides = context
                                    .override_with_keys
                                    .iter()
                                    .flat_map(|id| {
                                        config
                                            .overrides
                                            .get(id)
                                            .cloned()
                                            .unwrap_or(json!("{}"))
                                            .as_object()
                                            .cloned()
                                            .unwrap_or(Map::new())
                                            .into_iter()
                                            .collect::<Vec<(String, Value)>>()
                                    })
                                    .collect::<Map<String, Value>>();
                                (context.clone(), overrides)
                            })
                            .collect::<Vec<(Context, Map<String, Value>)>>();
                        let is_empty = ctx_n_overrides.is_empty();
                        view! {
                            <Show when=move || is_empty>
                                <div class="flex-row" style="margin-top:20rem;">
                                    <div class="flex justify-center text-gray-400">
                                        <i class="ri-file-add-line ri-xl"></i>
                                    </div>
                                    <div class="flex mt-4 font-semibold items-center text-gray-400 text-xl justify-center">
                                        "Start with creating an override"
                                    </div>
                                </div>
                            </Show>
                            <ConditionCollapseProvider>

                                {ctx_n_overrides
                                    .into_iter()
                                    .map(|(context, overrides)| {
                                        view! {
                                            <ContextCard
                                                context=context
                                                overrides=overrides
                                                handle_edit=on_context_edit
                                                handle_clone=on_context_clone
                                                handle_delete=on_context_delete
                                            />
                                        }
                                    })
                                    .collect_view()}

                            </ConditionCollapseProvider>
                        }
                    }}

                </div>

                <DeleteModal
                    modal_visible=modal_visible
                    confirm_delete=on_delete_confirm
                    set_modal_visible=set_modal_visible
                    header_text="Are you sure you want to delete this context? Action is irreversible."
                        .to_string()
                />

            </Suspense>
        </div>
    }
}
