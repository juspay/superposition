use std::ops::Deref;

use leptos::*;
use leptos_router::{A, use_navigate, use_params_map};
use serde::Deserialize;
use serde_json::{Map, Value};
use superposition_derives::{IsEmpty, QueryParam};
use superposition_types::{
    IsEmpty,
    custom_query::QueryParam,
    custom_query::{CustomQuery, Query},
    database::models::cac::DefaultConfig,
};

use crate::api::default_configs;
use crate::components::button::ButtonAnchor;
use crate::components::{
    alert::AlertType,
    button::Button,
    default_config_form::{ChangeLogSummary, ChangeType, DefaultConfigForm},
    description::ContentDescription,
    input::{Input, InputType},
    skeleton::{Skeleton, SkeletonVariant},
};
use crate::providers::{alert_provider::enqueue_alert, editor_provider::EditorProvider};
use crate::query_updater::use_signal_from_query;
use crate::schema::{EnumVariants, JsonSchemaType, SchemaType};
use crate::types::{OrganisationId, Workspace};

#[component]
fn ConfigInfo(default_config: DefaultConfig) -> impl IntoView {
    let schema: &Map<String, Value> = &default_config.schema;
    let Ok(schema_type) = SchemaType::try_from(schema) else {
        return view! { <span class="text-red-500">"Invalid schema"</span> }.into_view();
    };
    let Ok(enum_variants) = EnumVariants::try_from(schema) else {
        return view! { <span class="text-red-500">"Invalid schema"</span> }.into_view();
    };
    let input_type = InputType::from((schema_type.clone(), enum_variants));

    view! {
        <div class="card bg-base-100 max-w-screen shadow">
            <div class="card-body">
                <h2 class="card-title">"Info"</h2>
                <div class="flex flex-col gap-4">
                    <EditorProvider>
                        <div class="flex gap-4">
                            <div class="stat-title">"Value"</div>
                            <Input
                                id="default-config-value-input"
                                disabled=true
                                class=match input_type {
                                    InputType::Toggle | InputType::Select(_) => String::new(),
                                    InputType::Integer | InputType::Number => {
                                        "w-full max-w-md".into()
                                    }
                                    _ => "rounded-md resize-y w-full max-w-md".into(),
                                }
                                schema_type=schema_type
                                value=default_config.value
                                on_change=move |_| {}
                                r#type=input_type
                            />
                        </div>
                        <div class="flex gap-4">
                            <div class="stat-title">"Schema"</div>
                            <Input
                                disabled=true
                                id="type-schema"
                                class="rounded-md resize-y w-full max-w-md"
                                schema_type=SchemaType::Single(JsonSchemaType::Object)
                                value=Value::from(default_config.schema)
                                on_change=move |_| {}
                                r#type=InputType::Monaco(vec![])
                            />
                        </div>
                    </EditorProvider>
                    {if default_config.value_validation_function_name.is_some()
                        || default_config.value_compute_function_name.is_some()
                    {
                        view! {
                            <div class="flex flex-row gap-6 flex-wrap">
                                {default_config
                                    .value_validation_function_name
                                    .map(|name| {
                                        view! {
                                            <div class="h-fit w-[250px]">
                                                <div class="stat-title">"Validation Function"</div>
                                                <A
                                                    href=format!("../../function/{name}")
                                                    class="text-blue-500 underline underline-offset-2"
                                                >
                                                    {name}
                                                </A>
                                            </div>
                                        }
                                    })}
                                {default_config
                                    .value_compute_function_name
                                    .map(|name| {
                                        view! {
                                            <div class="h-fit w-[250px]">
                                                <div class="stat-title">"Value Compute Function"</div>
                                                <A
                                                    href=format!("../../function/{name}")
                                                    class="text-blue-500 underline underline-offset-2"
                                                >
                                                    {name}
                                                </A>
                                            </div>
                                        }
                                    })}
                            </div>
                        }
                            .into_view()
                    } else {
                        ().into_view()
                    }}
                </div>
            </div>
        </div>
    }.into_view()
}

#[derive(Clone)]
enum Action {
    None,
    Delete,
}

#[component]
pub fn DefaultConfig() -> impl IntoView {
    let path_params = use_params_map();
    let workspace = use_context::<Signal<Workspace>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();
    let default_config_key = Memo::new(move |_| {
        path_params.with(|params| params.get("config_key").cloned().unwrap_or("1".into()))
    });
    let action_rws = RwSignal::new(Action::None);
    let delete_inprogress_rws = RwSignal::new(false);

    let default_config_resource = create_blocking_resource(
        move || (default_config_key.get(), workspace.get().0, org.get().0),
        |(default_config_key, workspace, org_id)| async move {
            default_configs::get(&default_config_key, &workspace, &org_id)
                .await
                .ok()
        },
    );

    let confirm_delete = move |_| {
        delete_inprogress_rws.set(true);
        spawn_local(async move {
            let result = default_configs::delete(
                default_config_key.get_untracked(),
                &workspace.get_untracked(),
                &org.get_untracked(),
            )
            .await;
            delete_inprogress_rws.set(false);
            match result {
                Ok(_) => {
                    logging::log!("Config deleted successfully");
                    let navigate = use_navigate();
                    let redirect_url = format!(
                        "/admin/{}/{}/default-config",
                        org.get().0,
                        workspace.get().0,
                    );
                    navigate(&redirect_url, Default::default());
                    enqueue_alert(
                        String::from("Config deleted successfully"),
                        AlertType::Success,
                        5000,
                    );
                }
                Err(e) => {
                    logging::error!("Error deleting default config: {:?}", e);
                    enqueue_alert(e, AlertType::Error, 5000);
                }
            }
        });
    };

    view! {
        <Suspense fallback=move || {
            view! { <Skeleton variant=SkeletonVariant::DetailPage /> }
        }>
            {move || {
                let default_config = match default_config_resource.get() {
                    Some(Some(config)) => config,
                    _ => return view! { <h1>"Error fetching default config"</h1> }.into_view(),
                };
                view! {
                    <div class="flex flex-col gap-4">
                        <div class="flex justify-between items-center">
                            <h1 class="text-2xl font-extrabold">{default_config.key.clone()}</h1>
                            <div class="w-full max-w-fit flex flex-row join">
                                <ButtonAnchor
                                    force_style="btn join-item px-5 py-2.5 text-white bg-gradient-to-r from-purple-500 via-purple-600 to-purple-700 shadow-lg rounded-lg"
                                    href="edit"
                                    icon_class="ri-edit-line"
                                    text="Edit"
                                />
                                <Button
                                    force_style="btn join-item px-5 py-2.5 text-white bg-gradient-to-r from-purple-500 via-purple-600 to-purple-700 shadow-lg rounded-lg"
                                    on_click=move |_| action_rws.set(Action::Delete)
                                    icon_class="ri-delete-bin-line"
                                    text="Delete"
                                />
                            </div>
                        </div>
                        <ContentDescription
                            description=default_config.description.clone()
                            change_reason=default_config.change_reason.clone()
                            created_by=default_config.created_by.clone()
                            created_at=default_config.created_at
                            last_modified_by=default_config.last_modified_by.clone()
                            last_modified_at=default_config.last_modified_at
                        />
                        <ConfigInfo default_config=default_config.clone() />
                    </div>
                    <Show when=move || matches!(action_rws.get(), Action::Delete)>
                        <ChangeLogSummary
                            key_name=default_config_key.get()
                            change_type=ChangeType::Delete
                            on_close=move |_| action_rws.set(Action::None)
                            on_confirm=confirm_delete
                            inprogress=delete_inprogress_rws
                        />
                    </Show>
                }
                    .into_view()
            }}
        </Suspense>
    }
}

#[component]
pub fn EditDefaultConfig() -> impl IntoView {
    let path_params = use_params_map();
    let workspace = use_context::<Signal<Workspace>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();
    let default_config_key = Memo::new(move |_| {
        path_params.with(|params| params.get("config_key").cloned().unwrap_or("1".into()))
    });

    let default_config_resource = create_blocking_resource(
        move || (default_config_key.get(), workspace.get().0, org.get().0),
        |(default_config_key, workspace, org_id)| async move {
            default_configs::get(&default_config_key, &workspace, &org_id)
                .await
                .ok()
        },
    );

    view! {
        <Suspense fallback=move || {
            view! { <Skeleton variant=SkeletonVariant::DetailPage /> }
        }>
            {move || {
                let default_config = match default_config_resource.get() {
                    Some(Some(default_config)) => default_config,
                    _ => return view! { <h1>"Error fetching default config"</h1> }.into_view(),
                };

                view! {
                    <DefaultConfigForm
                        edit=true
                        config_key=default_config.key.clone()
                        config_value=default_config.value.clone()
                        type_schema=Value::from(&default_config.schema)
                        description=default_config.description.deref().to_string()
                        validation_function_name=default_config
                            .value_validation_function_name
                            .clone()
                        value_compute_function_name=default_config
                            .value_compute_function_name
                            .clone()
                        redirect_url_cancel=format!("../../{}", default_config.key)
                    />
                }
            }}
        </Suspense>
    }
}

#[derive(PartialEq, Clone, IsEmpty, QueryParam, Default, Deserialize)]
pub struct CreatePageParams {
    #[query_param(skip_if_empty)]
    pub prefix: Option<String>,
}

#[component]
pub fn CreateDefaultConfig() -> impl IntoView {
    let (page_params_rws,) = use_signal_from_query(move |query_string| {
        (Query::<CreatePageParams>::extract_non_empty(query_string).into_inner(),)
    });

    let cancel_url = format!(
        "../../../default-config?{}",
        page_params_rws.with(|params| params.to_query_param())
    );

    view! {
        <DefaultConfigForm
            redirect_url_cancel=cancel_url
            prefix=page_params_rws.with(|params| params.prefix.clone())
        />
    }
}
