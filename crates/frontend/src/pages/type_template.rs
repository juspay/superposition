use std::ops::Deref;

use leptos::*;
use leptos_router::{use_navigate, use_params_map};
use superposition_types::database::models::cac::TypeTemplate;

use crate::api::get_type_template;
use crate::components::description::ContentDescription;
use crate::components::{
    alert::AlertType,
    button::Button,
    drawer::PortalDrawer,
    input::{Input, InputType},
    skeleton::{Skeleton, SkeletonVariant},
    type_template_form::{
        utils::delete_type, ChangeLogSummary, ChangeType, TypeTemplateForm,
    },
};
use crate::providers::{alert_provider::enqueue_alert, editor_provider::EditorProvider};
use crate::schema::{JsonSchemaType, SchemaType};
use crate::types::{OrganisationId, Tenant};
use crate::utils::use_url_base;

#[component]
fn type_info(type_template: TypeTemplate) -> impl IntoView {
    view! {
        <div class="card bg-base-100 max-w-screen shadow">
            <div class="card-body">
                <h2 class="card-title">"Info"</h2>
                <div class="flex flex-col gap-4">
                    <div class="flex gap-4">
                        <div class="stat-title">"Schema"</div>
                        <EditorProvider>
                            <Input
                                disabled=true
                                id="type-schema"
                                class="rounded-md resize-y w-full max-w-md"
                                schema_type=SchemaType::Single(JsonSchemaType::Object)
                                value=type_template.type_schema
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
pub fn type_page() -> impl IntoView {
    let path_params = use_params_map();
    let workspace = use_context::<Signal<Tenant>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();
    let type_name = Memo::new(move |_| {
        path_params.with(|params| params.get("type_name").cloned().unwrap_or("1".into()))
    });
    let action_rws = RwSignal::new(Action::None);
    let delete_inprogress_rws = RwSignal::new(false);

    let type_template_resource = create_blocking_resource(
        move || (type_name.get(), workspace.get().0, org.get().0),
        |(type_name, workspace, org_id)| async move {
            get_type_template(&type_name, &workspace, &org_id)
                .await
                .ok()
        },
    );

    let confirm_delete = move |_| {
        delete_inprogress_rws.set(true);
        spawn_local(async move {
            let result = delete_type(
                type_name.get_untracked(),
                workspace.get_untracked().0,
                org.get_untracked().0,
            )
            .await;
            delete_inprogress_rws.set(false);
            match result {
                Ok(_) => {
                    logging::log!("Type deleted successfully");
                    let navigate = use_navigate();
                    let base = use_url_base();
                    let redirect_url = format!(
                        "{base}/admin/{}/{}/types",
                        org.get().0,
                        workspace.get().0,
                    );
                    navigate(&redirect_url, Default::default());
                    enqueue_alert(
                        String::from("Type deleted successfully"),
                        AlertType::Success,
                        5000,
                    );
                }
                Err(e) => {
                    logging::error!("Error deleting type: {:?}", e);
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
                let type_template = match type_template_resource.get() {
                    Some(Some(type_)) => type_,
                    _ => return view! { <h1>"Error fetching type"</h1> }.into_view(),
                };
                let type_template_st = StoredValue::new(type_template.clone());
                view! {
                    <div class="flex flex-col gap-4">
                        <div class="flex justify-between items-center">
                            <h1 class="text-2xl font-extrabold">
                                {type_template.type_name.clone()}
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
                            description=type_template.description.clone()
                            change_reason=type_template.change_reason.clone()
                            created_by=type_template.created_by.clone()
                            created_at=type_template.created_at
                            last_modified_by=type_template.last_modified_by.clone()
                            last_modified_at=type_template.last_modified_at
                        />
                        <TypeInfo type_template=type_template.clone() />
                    </div>
                    {match action_rws.get() {
                        Action::None => ().into_view(),
                        Action::Edit => {
                            view! {
                                <PortalDrawer
                                    title="Edit Config"
                                    handle_close=move |_| action_rws.set(Action::None)
                                >
                                    <TypeTemplateForm
                                        edit=true
                                        type_name=type_template_st
                                            .with_value(|t| t.type_name.clone())
                                        type_schema=type_template_st
                                            .with_value(|t| t.type_schema.clone())
                                        description=type_template_st
                                            .with_value(|t| t.description.deref().to_string())
                                        handle_submit=move |_| {
                                            type_template_resource.refetch();
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
                                    type_name=type_name.get()
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
