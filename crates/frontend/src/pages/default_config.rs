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
fn ConfigInfo(
    default_config: DefaultConfig,
    /// The target key this row links to, when it is a symlink. The API
    /// resolves a symlink's value/schema/functions to the target's on read
    /// (so existing machine clients keep seeing a real type), but a human
    /// reading this card should see the link, not a schema that looks like
    /// it belongs to this key - see the schema row below.
    #[prop(default = None)]
    symlink_to: Option<String>,
    /// Prepended to every relative link this card builds (the symlink
    /// target link and the validation/compute function links), so the same
    /// card renders correct links whether it's shown on the key's own
    /// detail page (the default, `""`) or one path segment deeper on its
    /// edit page (`"../"`).
    #[prop(into, default = String::new())]
    path_depth_prefix: String,
) -> impl IntoView {
    // Stored rather than a plain `String` so each of the three links below
    // can take its own clone, instead of the first `move` closure taking
    // ownership and leaving the others nothing to borrow.
    let path_depth_prefix = StoredValue::new(path_depth_prefix);
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
                        <div class="flex flex-col gap-1">
                            <div class="flex gap-4 items-center">
                                <div class="stat-title">
                                    {if symlink_to.is_some() { "Symlink" } else { "Schema" }}
                                </div>
                                {symlink_to
                                    .clone()
                                    .map(|target| {
                                        view! {
                                            <span class="text-sm flex items-center gap-1">
                                                "→"
                                                <A
                                                    href=format!("{}../{target}", path_depth_prefix.get_value())
                                                    class="text-blue-500 underline underline-offset-2"
                                                >
                                                    {target}
                                                </A>
                                            </span>
                                        }
                                    })}
                            </div>
                            {symlink_to
                                .is_some()
                                .then(|| {
                                    view! {
                                        <div class="text-xs text-gray-500 italic">
                                            "Type is inherited from the target key and is read-only here."
                                        </div>
                                    }
                                })}
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
                                                    href=format!(
                                                        "{}../../function/{name}",
                                                        path_depth_prefix.get_value(),
                                                    )
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
                                                    href=format!(
                                                        "{}../../function/{name}",
                                                        path_depth_prefix.get_value(),
                                                    )
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
                let extracted = default_config_resource
                    .with(|r| {
                        r.as_ref()
                            .and_then(|opt| opt.as_ref())
                            .map(|response| (response.config.clone(), response.symlink_to.clone()))
                    });
                let Some((default_config, symlink_to)) = extracted else {
                    return // `DefaultConfigResponse` doesn't derive `Clone` (it isn't
                    // meant to be copied around wholesale), so pull the two
                    // owned pieces this view needs out through `.with()` rather
                    // than `.get()`.
                    view! { <h1>"Error fetching default config"</h1> }
                        .into_view();
                };
                let symlink_banner = symlink_to
                    .clone()
                    .map(|target| {
                        view! {
                            <div role="alert" class="alert alert-info">
                                <i class="ri-links-line text-lg" />
                                <span>
                                    <span class="font-bold">"Symlink → "</span>
                                    <A
                                        href=format!("../{target}")
                                        class="font-semibold underline underline-offset-2"
                                    >
                                        {target}
                                    </A>
                                    <span class="ml-1">
                                        "This key has no value of its own - its type and value below are inherited from the target key and are read-only here."
                                    </span>
                                </span>
                            </div>
                        }
                    });
                view! {
                    <div class="flex flex-col gap-4">
                        <div class="flex justify-between items-center">
                            <h1 class="text-2xl font-extrabold flex items-center">
                                {default_config.key.clone()}
                            </h1>
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
                        {symlink_banner}
                        <ContentDescription
                            description=default_config.description.clone()
                            change_reason=default_config.change_reason.clone()
                            created_by=default_config.created_by.clone()
                            created_at=default_config.created_at
                            last_modified_by=default_config.last_modified_by.clone()
                            last_modified_at=default_config.last_modified_at
                        />
                        <ConfigInfo
                            default_config=default_config.clone()
                            symlink_to=symlink_to.clone()
                        />
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
            // A symlink's value, schema and function names are resolved from
            // its target on read, and a write to any of them redirects to
            // that target rather than this key - so, unlike before, this
            // page keeps `symlink_to` and threads it into both the read-only
            // card and the form below, instead of silently editing fields
            // that don't actually belong to this key.
            default_configs::get(&default_config_key, &workspace, &org_id)
                .await
                .ok()
                .map(|response| (response.config, response.symlink_to))
        },
    );

    view! {
        <Suspense fallback=move || {
            view! { <Skeleton variant=SkeletonVariant::DetailPage /> }
        }>
            {move || {
                let (default_config, symlink_to) = match default_config_resource.get() {
                    Some(Some(pair)) => pair,
                    _ => return view! { <h1>"Error fetching default config"</h1> }.into_view(),
                };
                let is_symlink = symlink_to.is_some();
                let config_info_default_config = default_config.clone();
                let config_info_symlink_to = symlink_to.clone();
                // `Show`'s children callback is `Fn`, so it moves whatever it
                // captures by reference into an owned closure on first build;
                // clone into dedicated bindings rather than feeding it
                // `default_config`/`symlink_to` directly, since both are
                // still needed below for `DefaultConfigForm`.

                view! {
                    <div class="flex flex-col gap-4">
                        <Show when=move || is_symlink>
                            <ConfigInfo
                                default_config=config_info_default_config.clone()
                                symlink_to=config_info_symlink_to.clone()
                                path_depth_prefix="../"
                            />
                        </Show>
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
                            symlink_to=symlink_to.clone()
                            redirect_url_cancel=format!("../../{}", default_config.key)
                        />
                    </div>
                }
                    .into_view()
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
