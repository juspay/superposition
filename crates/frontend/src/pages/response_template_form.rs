use leptos::*;
use leptos_router::{use_navigate, use_params_map};
use serde_json::{Map, Value};
use std::ops::Deref;
use superposition_types::{
    api::{
        functions::FunctionEnvironment, response_templates::ResponseTemplateUpdateRequest,
    },
    custom_query::PaginationParams,
};
use web_sys::MouseEvent;

use crate::api::{dimensions, response_templates};
use crate::components::alert::AlertType;
use crate::components::button::{Button, ButtonAnchor};
use crate::components::change_form::ChangeForm;
use crate::components::context_form::ContextForm;
use crate::components::form::label::Label;
use crate::components::monaco_editor::{Languages, MonacoEditor};
use crate::components::skeleton::{Skeleton, SkeletonVariant};
use crate::logic::Conditions;
use crate::providers::alert_provider::enqueue_alert;
use crate::types::{OrganisationId, Workspace};

#[component]
pub fn response_template_form(
    #[prop(default = false)] edit: bool,
    #[prop(default = String::new())] name: String,
    #[prop(default = Conditions::default())] context: Conditions,
    #[prop(default = String::new())] content_type: String,
    #[prop(default = String::new())] template: String,
    #[prop(default = String::new())] description: String,
    #[prop(into)] redirect_url_cancel: String,
) -> impl IntoView {
    let workspace = use_context::<Signal<Workspace>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();

    let (error_message, set_error_message) = create_signal("".to_string());
    let (name_rs, name_ws) = create_signal(name);
    let (context_rs, context_ws) = create_signal(context);
    let (content_type_rs, content_type_ws) = create_signal(content_type);
    let (template_rs, template_ws) = create_signal(template);
    let (req_inprogess_rs, req_inprogress_ws) = create_signal(false);
    let (description_rs, description_ws) = create_signal(description);
    let (change_reason_rs, change_reason_ws) = create_signal(String::new());

    let update_request_rws = RwSignal::new(None);
    let redirect_url_cancel = StoredValue::new(redirect_url_cancel);

    let dimensions_resource = create_blocking_resource(
        move || (workspace.get().0, org.get().0),
        |(workspace, org_id)| async move {
            dimensions::fetch(&PaginationParams::all_entries(), &workspace, &org_id)
                .await
                .map(|d| d.data)
                .unwrap_or_default()
        },
    );

    let fn_environment = Memo::new(move |_| FunctionEnvironment {
        context: context_rs.get().as_context_json(),
        overrides: Map::new(),
    });

    let on_submit = move |_| {
        req_inprogress_ws.set(true);

        let name = name_rs.get_untracked();
        let context = Value::Object(context_rs.get_untracked().as_context_json());
        let content_type = content_type_rs.get_untracked();
        let template = template_rs.get_untracked();
        let description = description_rs.get_untracked();
        let change_reason = change_reason_rs.get_untracked();
        let workspace_val = workspace.get_untracked();
        let org_id_val = org.get_untracked();

        spawn_local({
            async move {
                let result = match (edit, update_request_rws.get_untracked()) {
                    (true, Some((_, update_payload))) => {
                        let future = response_templates::update(
                            name,
                            update_payload,
                            &workspace_val,
                            &org_id_val,
                        );
                        update_request_rws.set(None);
                        future.await.map(|_| ())
                    }
                    (true, None) => {
                        let update_payload = response_templates::try_update_payload(
                            content_type,
                            template,
                            change_reason,
                        );
                        match update_payload {
                            Ok(payload) => {
                                update_request_rws.set(Some((name.clone(), payload)));
                                Ok(())
                            }
                            Err(e) => Err(e),
                        }
                    }
                    _ => response_templates::create(
                        name,
                        description,
                        change_reason,
                        context,
                        content_type,
                        template,
                        &workspace_val,
                        &org_id_val,
                    )
                    .await
                    .map(|_| ()),
                };

                req_inprogress_ws.set(false);
                match result {
                    Ok(()) => {
                        if update_request_rws.get_untracked().is_some() {
                            return;
                        }
                        let success_message = if edit {
                            "Response template updated successfully!"
                        } else {
                            "New Response template created successfully!"
                        };
                        enqueue_alert(
                            String::from(success_message),
                            AlertType::Success,
                            5000,
                        );
                        let navigate = use_navigate();
                        let redirect_url = format!(
                            "/admin/{}/{}/response-templates",
                            org_id_val.0, workspace_val.0
                        );
                        navigate(&redirect_url, Default::default());
                    }
                    Err(e) => {
                        set_error_message.set(e.clone());
                        enqueue_alert(e, AlertType::Error, 5000);
                    }
                }
            }
        });
    };

    view! {
        <Suspense fallback=move || {
            view! { <Skeleton variant=SkeletonVariant::DetailPage /> }
        }>
            {move || {
                let loading = req_inprogess_rs.get();
                let title = if edit {
                    name_rs.get()
                } else {
                    "Create Response Template".to_string()
                };
                let dimensions = dimensions_resource.get().unwrap_or_default();

                view! {
                    <form class="flex flex-col gap-10">
                        <div class="flex justify-between items-center gap-2">
                            <h1 class="text-2xl font-extrabold">{title}</h1>
                            <div class="w-full max-w-fit flex flex-row join">
                                <Button
                                    force_style="btn join-item px-5 py-2.5 text-white bg-gradient-to-r from-purple-500 via-purple-600 to-purple-700 shadow-lg rounded-lg"
                                    on_click=move |ev: MouseEvent| {
                                        ev.prevent_default();
                                        on_submit(());
                                    }
                                    icon_class="ri-send-plane-line"
                                    text="Submit"
                                    loading
                                />
                                <ButtonAnchor
                                    force_style="btn join-item px-5 py-2.5 text-white bg-gradient-to-r from-purple-500 via-purple-600 to-purple-700 shadow-lg rounded-lg"
                                    href=redirect_url_cancel.get_value()
                                    icon_class="ri-forbid-line"
                                    text="Cancel"
                                    loading
                                />
                            </div>
                        </div>

                        // Context Form at the top
                        <ContextForm
                            dimensions=dimensions.clone()
                            context=context_rs.get_untracked()
                            on_context_change=move |new_context| context_ws.set(new_context)
                            fn_environment
                            disabled=edit
                            heading_sub_text="Define context conditions for this template"
                        />

                        <div class="flex gap-10">
                            // Left side - Form fields
                            <div class="w-1/2 flex flex-col gap-5 text-gray-700">
                                <Show when=move || !edit>
                                    <div class="form-control">
                                        <Label title="Name" />
                                        <input
                                            type="text"
                                            placeholder="Template name"
                                            name="name"
                                            id="name"
                                            class="input input-bordered w-full max-w-md"
                                            value=move || name_rs.get()
                                            on:change=move |ev| {
                                                let value = event_target_value(&ev);
                                                name_ws.set(value);
                                            }
                                        />
                                    </div>
                                </Show>

                                <div class="flex flex-wrap gap-x-10 gap-y-5">
                                    <ChangeForm
                                        title="Description".to_string()
                                        placeholder="Enter a description".to_string()
                                        value=description_rs.get_untracked()
                                        on_change=move |new_description| {
                                            description_ws.set(new_description)
                                        }
                                    />
                                    <ChangeForm
                                        title="Reason for Change".to_string()
                                        placeholder="Enter a reason for this change".to_string()
                                        value=change_reason_rs.get_untracked()
                                        on_change=move |new_change_reason| {
                                            change_reason_ws.set(new_change_reason)
                                        }
                                    />
                                </div>

                                <div class="form-control">
                                    <Label title="Content Type" />
                                    <input
                                        type="text"
                                        placeholder="Content type (e.g., application/json)"
                                        name="content_type"
                                        id="content_type"
                                        class="input input-bordered w-full max-w-md"
                                        value=move || content_type_rs.get()
                                        on:change=move |ev| {
                                            let value = event_target_value(&ev);
                                            content_type_ws.set(value);
                                        }
                                    />
                                </div>
                            </div>

                            // Right side - Template Monaco editor
                            <div class="w-1/2 flex flex-col gap-2">
                                <Label title="Template" />
                                <div class="h-[calc(100vh-450px)] border border-gray-300 rounded-lg overflow-hidden">
                                    <MonacoEditor
                                        node_id="template-editor"
                                        data=template_rs.get_untracked()
                                        on_change=Callback::new(move |new_template: String| {
                                            template_ws.set(new_template);
                                        })
                                        language=Languages::Json
                                        classes=vec!["h-full", "w-full"]
                                    />
                                </div>
                            </div>
                        </div>

                        <p class="text-red-500">{move || error_message.get()}</p>
                    </form>

                    {move || match update_request_rws.get() {
                        None => ().into_view(),
                        Some((name, update_request)) => {
                            view! {
                                <ChangeSummaryModal
                                    name
                                    update_request
                                    on_confirm=on_submit
                                    on_close=move |_| update_request_rws.set(None)
                                />
                            }
                                .into_view()
                        }
                    }}
                }
            }}
        </Suspense>
    }
}

#[component]
fn change_summary_modal(
    name: String,
    update_request: ResponseTemplateUpdateRequest,
    #[prop(into)] on_confirm: Callback<()>,
    #[prop(into)] on_close: Callback<()>,
) -> impl IntoView {
    let workspace = use_context::<Signal<Workspace>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();

    let update_request_st = StoredValue::new(update_request);

    let response_template = create_local_resource(
        move || (name.clone(), workspace.get().0, org.get().0),
        |(name, workspace, org)| async move {
            response_templates::get(&name, &workspace, &org).await
        },
    );

    view! {
        <div class="modal modal-open">
            <div class="modal-box max-w-2xl">
                <h3 class="font-bold text-lg">"Confirm Update"</h3>
                <p class="py-4">"Are you sure you want to update this response template?"</p>
                <Suspense fallback=move || {
                    view! {
                        <Skeleton variant=SkeletonVariant::Block style_class="h-10".to_string() />
                    }
                }>
                    {move || match response_template.get() {
                        Some(Ok(resp_temp)) => {
                            let update_request = update_request_st.get_value();
                            let new_content_type = update_request.content_type.clone();
                            let new_template = update_request.template.clone();
                            view! {
                                <div class="overflow-x-auto">
                                    <table class="table table-zebra">
                                        <thead>
                                            <tr>
                                                <th>"Property"</th>
                                                <th>"Old Value"</th>
                                                <th>"New Value"</th>
                                            </tr>
                                        </thead>
                                        <tbody>
                                            <tr>
                                                <td>"Content Type"</td>
                                                <td>{resp_temp.content_type.clone()}</td>
                                                <td>
                                                    {new_content_type
                                                        .unwrap_or_else(|| resp_temp.content_type.clone())}
                                                </td>
                                            </tr>
                                        </tbody>
                                    </table>
                                </div>
                                <div class="mt-4">
                                    <h4 class="font-bold">"Template Changes"</h4>
                                    <pre class="bg-gray-100 p-4 rounded mt-2 overflow-x-auto text-sm">
                                        {format!(
                                            "Old:\n{}\n\nNew:\n{}",
                                            resp_temp.template.clone(),
                                            new_template.unwrap_or_default(),
                                        )}
                                    </pre>
                                </div>
                            }
                                .into_view()
                        }
                        Some(Err(e)) => {
                            logging::error!("Error fetching response template: {}", e);
                            view! { <div>"Error fetching response template"</div> }.into_view()
                        }
                        None => view! { <div>"Loading..."</div> }.into_view(),
                    }}
                </Suspense>
                <div class="modal-action">
                    <Button text="Cancel" on_click=move |_| on_close.call(()) />
                    <Button text="Yes, Update" on_click=move |_| on_confirm.call(()) />
                </div>
            </div>
        </div>
    }
}

#[component]
pub fn edit_response_template() -> impl IntoView {
    let path_params = use_params_map();
    let workspace = use_context::<Signal<Workspace>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();
    let name = Memo::new(move |_| {
        path_params.with(|params| params.get("name").cloned().unwrap_or("1".into()))
    });

    let response_template_resource = create_blocking_resource(
        move || (name.get(), workspace.get().0, org.get().0),
        |(name, workspace, org_id)| async move {
            response_templates::get(&name, &workspace, &org_id)
                .await
                .ok()
        },
    );

    view! {
        <Suspense fallback=move || {
            view! { <Skeleton variant=SkeletonVariant::DetailPage /> }
        }>
            {move || {
                let rt = match response_template_resource.get() {
                    Some(Some(rt)) => rt,
                    _ => return view! { <h1>"Error fetching response template"</h1> }.into_view(),
                };

                let conditions =
                    Conditions::from_context_json(&rt.context).unwrap_or_default();

                view! {
                    <ResponseTemplateForm
                        edit=true
                        name=rt.name.clone()
                        context=conditions
                        content_type=rt.content_type.clone()
                        template=rt.template.clone()
                        description=rt.description.deref().to_string()
                        redirect_url_cancel=format!("../../{}", rt.name)
                    />
                }
            }}
        </Suspense>
    }
}

#[component]
pub fn create_response_template() -> impl IntoView {
    view! { <ResponseTemplateForm redirect_url_cancel="../../response-templates" /> }
}
