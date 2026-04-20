use std::ops::Deref;

use futures::join;
use leptos::*;
use leptos_router::use_navigate;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use superposition_types::{
    api::{default_config::DefaultConfigFilters, dimension::DimensionResponse},
    custom_query::PaginationParams,
    database::models::cac::DefaultConfig,
};

use crate::{
    api::{default_configs, delete_context, dimensions, get_context},
    components::{
        alert::AlertType,
        button::Button,
        skeleton::{Skeleton, SkeletonVariant},
    },
    logic::Conditions,
    providers::alert_provider::enqueue_alert,
    types::{OrganisationId, Workspace},
};

use super::context_override::{ChangeLogSummary, ChangeType};

/// Page resource for create/edit/detail pages
#[derive(Serialize, Deserialize, Clone, Debug, Default)]
struct FormPageResource {
    dimensions: Vec<DimensionResponse>,
    default_config: Vec<DefaultConfig>,
}

/// Detail page for viewing a single override
#[component]
pub fn OverridePage() -> impl IntoView {
    let path_params = leptos_router::use_params_map();
    let workspace = use_context::<Signal<Workspace>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();
    let context_id = Memo::new(move |_| {
        path_params.with(|params| params.get("context_id").cloned().unwrap_or("1".into()))
    });
    let action_rws = RwSignal::new(None::<String>);
    let delete_inprogress_rws = RwSignal::new(false);

    let context_resource = create_blocking_resource(
        move || (context_id.get(), workspace.get().0, org.get().0),
        |(context_id, workspace, org_id)| async move {
            get_context(&context_id, &workspace, &org_id).await.ok()
        },
    );

    let confirm_delete = move |context_id: String| {
        delete_inprogress_rws.set(true);
        spawn_local(async move {
            let result = delete_context(
                context_id.clone(),
                &workspace.get_untracked(),
                &org.get_untracked(),
            )
            .await;
            delete_inprogress_rws.set(false);
            match result {
                Ok(_) => {
                    logging::log!("Context deleted successfully");
                    let navigate = use_navigate();
                    let redirect_url = format!(
                        "/admin/{}/{}/overrides",
                        org.get().0,
                        workspace.get().0,
                    );
                    navigate(&redirect_url, Default::default());
                    enqueue_alert(
                        String::from("Override deleted successfully"),
                        AlertType::Success,
                        5000,
                    );
                }
                Err(e) => {
                    logging::error!("Error deleting context: {:?}", e);
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
                let context = match context_resource.get() {
                    Some(Some(ctx)) => ctx,
                    _ => return view! { <h1>"Error fetching override"</h1> }.into_view(),
                };
                let context_id = context.id.clone();
                let override_id = context.override_id.clone();
                let conditions = Conditions::from_iter(context.value.clone().into_inner());
                let overrides: Vec<(String, Value)> = context
                    .override_
                    .clone()
                    .into_iter()
                    .collect();
                let description = context.description.clone();
                let change_reason = context.change_reason.deref().to_string();
                let created_by = context.created_by.clone();
                let created_at = context.created_at;
                let last_modified_by = context.last_modified_by.clone();
                let last_modified_at = context.last_modified_at;
                let context_id_for_delete = context_id.clone();

                view! {
                    <div class="flex flex-col gap-4">
                        <div class="flex justify-between items-center">
                            <h1 class="text-2xl font-extrabold">"Override"</h1>
                            <div class="w-full max-w-fit flex flex-row join">
                                <crate::components::button::ButtonAnchor
                                    force_style="btn join-item px-5 py-2.5 text-white bg-gradient-to-r from-purple-500 via-purple-600 to-purple-700 shadow-lg rounded-lg"
                                    href="edit"
                                    icon_class="ri-edit-line"
                                    text="Edit"
                                />
                                <Button
                                    force_style="btn join-item px-5 py-2.5 text-white bg-gradient-to-r from-purple-500 via-purple-600 to-purple-700 shadow-lg rounded-lg"
                                    on_click=move |_| {
                                        action_rws.set(Some(context_id_for_delete.clone()))
                                    }
                                    icon_class="ri-delete-bin-line"
                                    text="Delete"
                                />
                            </div>
                        </div>

                        <crate::components::context_override_form::OverrideDetailView
                            context=conditions
                            overrides=overrides
                            description=description
                            override_id=override_id
                            change_reason=change_reason.clone()
                            created_by=created_by
                            created_at=created_at
                            last_modified_by=last_modified_by
                            last_modified_at=last_modified_at
                        />
                    </div>

                    <Show when=move || action_rws.get().is_some()>
                        <ChangeLogSummary
                            context_id=action_rws.get().unwrap_or_default()
                            change_type=ChangeType::Delete
                            on_confirm=Callback::new(move |_| {
                                if let Some(id) = action_rws.get() {
                                    confirm_delete(id);
                                }
                            })
                            on_close=Callback::new(move |_| action_rws.set(None))
                            inprogress=Signal::derive(move || delete_inprogress_rws.get())
                        />
                    </Show>
                }
                    .into_view()
            }}
        </Suspense>
    }
}

/// Edit page for editing an existing override
#[component]
pub fn EditOverride() -> impl IntoView {
    let path_params = leptos_router::use_params_map();
    let workspace = use_context::<Signal<Workspace>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();
    let context_id = Memo::new(move |_| {
        path_params.with(|params| params.get("context_id").cloned().unwrap_or("1".into()))
    });

    let context_resource = create_blocking_resource(
        move || (context_id.get(), workspace.get().0, org.get().0),
        |(context_id, workspace, org_id)| async move {
            get_context(&context_id, &workspace, &org_id).await.ok()
        },
    );

    let page_resource: Resource<(String, String), FormPageResource> =
        create_blocking_resource(
            move || (workspace.get().0, org.get().0),
            |(workspace, org_id)| async move {
                let empty_list_filters = PaginationParams::all_entries();
                let default_config_filters = DefaultConfigFilters::default();
                let (dimensions_result, default_config_result) = join!(
                    dimensions::list(&empty_list_filters, &workspace, &org_id),
                    default_configs::list(
                        &empty_list_filters,
                        &default_config_filters,
                        &workspace,
                        &org_id
                    ),
                );
                FormPageResource {
                    dimensions: dimensions_result.unwrap_or_default().data,
                    default_config: default_config_result.unwrap_or_default().data,
                }
            },
        );

    view! {
        <Suspense fallback=move || {
            view! { <Skeleton variant=SkeletonVariant::DetailPage /> }
        }>
            {move || {
                let context = match context_resource.get() {
                    Some(Some(ctx)) => ctx,
                    _ => return view! { <h1>"Error fetching override"</h1> }.into_view(),
                };
                let ctx_id = context.id.clone();
                let conditions = Conditions::from_iter(context.value.clone().into_inner());
                let overrides: Vec<(String, Value)> = context
                    .override_
                    .clone()
                    .into_iter()
                    .collect();
                let description = context.description.deref().to_string();
                let change_reason = context.change_reason.deref().to_string();
                let FormPageResource { dimensions, default_config } = page_resource
                    .get()
                    .unwrap_or_default();
                let redirect_url_cancel = format!(
                    "/admin/{}/{}/overrides/{}",
                    org.get().0,
                    workspace.get().0,
                    ctx_id,
                );
                let redirect_url_success = format!(
                    "/admin/{}/{}/overrides/{}",
                    org.get().0,
                    workspace.get().0,
                    ctx_id,
                );

                view! {
                    <crate::components::context_override_form::ContextOverrideForm
                        edit=true
                        context_id=ctx_id
                        context=conditions
                        overrides=overrides
                        dimensions=dimensions
                        default_config=default_config
                        description=description
                        change_reason=change_reason
                        redirect_url_cancel=redirect_url_cancel
                        redirect_url_success=redirect_url_success
                    />
                }
                    .into_view()
            }}
        </Suspense>
    }
}

/// Create page for creating a new override
#[component]
pub fn CreateOverride() -> impl IntoView {
    let workspace = use_context::<Signal<Workspace>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();
    let query = leptos_router::use_query_map();
    let clone_id = Memo::new(move |_| query.with(|q| q.get("clone").cloned()));

    let clone_context = create_blocking_resource(
        move || clone_id.get(),
        move |id| async move {
            let id = id?;
            get_context(&id, &workspace.get().0, &org.get().0)
                .await
                .ok()
        },
    );

    let page_resource = create_blocking_resource(
        move || (workspace.get().0, org.get().0),
        |(workspace, org_id)| async move {
            let empty_list_filters = PaginationParams::all_entries();
            let default_config_filters = DefaultConfigFilters::default();
            let (dimensions_result, default_config_result) = join!(
                dimensions::list(&empty_list_filters, &workspace, &org_id),
                default_configs::list(
                    &empty_list_filters,
                    &default_config_filters,
                    &workspace,
                    &org_id
                ),
            );
            FormPageResource {
                dimensions: dimensions_result.unwrap_or_default().data,
                default_config: default_config_result.unwrap_or_default().data,
            }
        },
    );

    let redirect_url_cancel = store_value(format!(
        "/admin/{}/{}/overrides",
        org.get().0,
        workspace.get().0
    ));
    let redirect_url_success = store_value(format!(
        "/admin/{}/{}/overrides",
        org.get().0,
        workspace.get().0
    ));

    view! {
        <Suspense fallback=move || {
            view! { <Skeleton variant=SkeletonVariant::DetailPage /> }
        }>
            {move || {
                let FormPageResource { dimensions, default_config } = page_resource
                    .get()
                    .unwrap_or_default();
                let cloned_context = clone_context.get().flatten();
                let (context, overrides) = match cloned_context {
                    Some(ctx) => {
                        (
                            Conditions::from_iter(ctx.value.clone().into_inner()),
                            ctx.override_.clone().into_iter().collect::<Vec<_>>(),
                        )
                    }
                    _ => (Conditions(vec![]), vec![]),
                };

                view! {
                    <crate::components::context_override_form::ContextOverrideForm
                        edit=false
                        context=context
                        overrides=overrides
                        dimensions=dimensions
                        default_config=default_config
                        redirect_url_cancel=redirect_url_cancel.get_value()
                        redirect_url_success=redirect_url_success.get_value()
                    />
                }
                    .into_view()
            }}
        </Suspense>
    }
}
