use std::ops::Deref;

use leptos::*;
use leptos_router::{use_navigate, use_params_map};
use serde_json::Value;
use superposition_types::database::models::cac::ResponseTemplate;

use crate::api::response_templates::{self, get};
use crate::components::description::ContentDescription;
use crate::components::{
    alert::AlertType,
    button::Button,
    drawer::PortalDrawer,
    input::{Input, InputType},
    response_template_form::{ChangeLogSummary, ChangeType, ResponseTemplateForm},
    skeleton::{Skeleton, SkeletonVariant},
};
use crate::providers::{alert_provider::enqueue_alert, editor_provider::EditorProvider};
use crate::schema::{JsonSchemaType, SchemaType};
use crate::types::{OrganisationId, Workspace};

#[component]
fn response_template_info(response_template: ResponseTemplate) -> impl IntoView {
    view! {
        <div class="card bg-base-100 max-w-screen shadow">
            <div class="card-body">
                <h2 class="card-title">"Info"</h2>
                <div class="flex flex-col gap-4">
                    <div class="flex gap-4">
                        <div class="stat-title">"Context ID"</div>
                        <div class="stat-value">{response_template.context_id}</div>
                    </div>
                    <div class="flex flex-col gap-2">
                        <div class="stat-title">"Context"</div>
                        <EditorProvider>
                            <Input
                                disabled=true
                                id="response-context"
                                class="rounded-md resize-y w-full max-w-md"
                                schema_type=SchemaType::Single(JsonSchemaType::Object)
                                value=Value::from(&response_template.context)
                                on_change=move |_| {}
                                r#type=InputType::Monaco(vec![])
                            />
                        </EditorProvider>
                    </div>
                    <div class="flex gap-4">
                        <div class="stat-title">"Content Type"</div>
                        <div class="stat-value">{response_template.content_type}</div>
                    </div>
                    <div class="flex flex-col gap-2">
                        <div class="stat-title">"Template"</div>
                        <EditorProvider>
                            <Input
                                disabled=true
                                id="response-template"
                                class="rounded-md resize-y w-full max-w-md"
                                schema_type=SchemaType::Single(JsonSchemaType::String)
                                value=Value::String(response_template.template.clone())
                                on_change=move |_| {}
                                r#type=InputType::Monaco(vec![])
                            />
                        </EditorProvider>
                    </div>
                </div>
            </div>
        </div>
    }
    .into_view()
}

#[derive(Clone)]
enum Action {
    None,
    Edit,
    Delete,
}

#[component]
pub fn ResponseTemplate() -> impl IntoView {
    let path_params = use_params_map();
    let workspace = use_context::<Signal<Workspace>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();
    let name = Memo::new(move |_| {
        path_params.with(|params| params.get("name").cloned().unwrap_or("1".into()))
    });
    let action_rws = RwSignal::new(Action::None);
    let delete_inprogress_rws = RwSignal::new(false);

    let response_template_resource = create_blocking_resource(
        move || (name.get(), workspace.get().0, org.get().0),
        |(name, workspace, org_id)| async move { get(&name, &workspace, &org_id).await.ok() },
    );

    let confirm_delete = move |_| {
        delete_inprogress_rws.set(true);
        spawn_local(async move {
            let result = response_templates::delete(
                name.get_untracked(),
                &workspace.get_untracked(),
                &org.get_untracked(),
            )
            .await;
            delete_inprogress_rws.set(false);
            match result {
                Ok(_) => {
                    logging::log!("Response template deleted successfully");
                    let navigate = use_navigate();
                    let redirect_url = format!(
                        "/admin/{}/{}/response-templates",
                        org.get().0,
                        workspace.get().0
                    );
                    navigate(&redirect_url, Default::default());
                    enqueue_alert(
                        String::from("Response template deleted successfully"),
                        AlertType::Success,
                        5000,
                    );
                }
                Err(e) => {
                    logging::error!("Error deleting response template: {:?}", e);
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
                let response_template = match response_template_resource.get() {
                    Some(Some(rt)) => rt,
                    _ => return view! { <h1>"Error fetching response template"</h1> }.into_view(),
                };
                let response_template_st = StoredValue::new(response_template.clone());
                view! {
                    <div class="flex flex-col gap-4">
                        <div class="flex justify-between items-center">
                            <h1 class="text-2xl font-extrabold">
                                {response_template.name.clone()}
                            </h1>
                            <div class="flex flex-row join">
                                <Button
                                    force_style="btn join-item px-5 py-2.5 text-white bg-gradient-to-r from-purple-500 via-purple-600 to-purple-700 shadow-lg rounded-lg"
                                    on_click=move |_| action_rws.set(Action::Edit)
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
                            description=response_template.description.clone()
                            change_reason=response_template.change_reason.clone()
                            created_by=response_template.created_by.clone()
                            created_at=response_template.created_at
                            last_modified_by=response_template.last_modified_by.clone()
                            last_modified_at=response_template.last_modified_at
                        />
                        <ResponseTemplateInfo response_template=response_template.clone() />
                    </div>
                    {match action_rws.get() {
                        Action::None => ().into_view(),
                        Action::Edit => {
                            view! {
                                <PortalDrawer
                                    title="Edit Response Template"
                                    handle_close=move |_| action_rws.set(Action::None)
                                >
                                    <ResponseTemplateForm
                                        edit=true
                                        name=response_template_st
                                            .with_value(|rt| rt.name.clone())
                                        context=response_template_st
                                            .with_value(|rt| Value::from(&rt.context))
                                        content_type=response_template_st
                                            .with_value(|rt| rt.content_type.clone())
                                        template=response_template_st
                                            .with_value(|rt| rt.template.clone())
                                        description=response_template_st
                                            .with_value(|rt| rt.description.deref().to_string())
                                        handle_submit=move |_| {
                                            response_template_resource.refetch();
                                            action_rws.set(Action::None);
                                        }
                                    />
                                </PortalDrawer>
                            }
                                .into_view()
                        }
                        Action::Delete => {
                            view! {
                                <ChangeLogSummary
                                    name=name.get()
                                    change_type=ChangeType::Delete
                                    on_close=move |_| action_rws.set(Action::None)
                                    on_confirm=confirm_delete
                                    inprogress=delete_inprogress_rws
                                />
                            }
                                .into_view()
                        }
                    }}
                }
                    .into_view()
            }}
        </Suspense>
    }
}
