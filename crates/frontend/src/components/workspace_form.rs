pub mod types;
pub mod utils;

use leptos::*;
use serde_json::to_string;
use superposition_types::api::workspace::{
    CreateWorkspaceRequest, UpdateWorkspaceRequest,
};
use superposition_types::database::models::WorkspaceStatus;
use web_sys::MouseEvent;

use crate::components::input::Toggle;
use crate::components::workspace_form::utils::string_to_vec;
use crate::components::{alert::AlertType, button::Button};
use crate::types::OrganisationId;
use crate::{
    components::workspace_form::utils::{create_workspace, update_workspace},
    providers::{alert_provider::enqueue_alert, editor_provider::EditorProvider},
};

#[component]
pub fn workspace_form(
    org_id: Signal<OrganisationId>,
    #[prop(default = false)] edit: bool,
    #[prop(default = String::new())] workspace_admin_email: String,
    #[prop(default = String::new())] workspace_name: String,
    #[prop(default = WorkspaceStatus::ENABLED)] workspace_status: WorkspaceStatus,
    #[prop(default = vec![])] mandatory_dimensions: Vec<String>,
    #[prop(into)] handle_submit: Callback<(), ()>,
) -> impl IntoView {
    let (workspace_name_rs, workspace_name_ws) = create_signal(workspace_name);
    let (workspace_admin_email_rs, workspace_admin_email_ws) =
        create_signal(workspace_admin_email);
    let (workspace_status_rs, workspace_status_ws) = create_signal(workspace_status);
    let (mandatory_dimensions_rs, mandatory_dimensions_ws) =
        create_signal(mandatory_dimensions);
    let (req_inprogess_rs, req_inprogress_ws) = create_signal(false);

    let on_submit = move |ev: MouseEvent| {
        req_inprogress_ws.set(true);
        ev.prevent_default();
        let create_payload = CreateWorkspaceRequest {
            workspace_admin_email: workspace_admin_email_rs.get(),
            workspace_name: workspace_name_rs.get(),
            workspace_status: Some(workspace_status_rs.get()),
        };

        let update_payload = UpdateWorkspaceRequest {
            workspace_admin_email: workspace_admin_email_rs.get(),
            workspace_status: Some(workspace_status_rs.get()),
            mandatory_dimensions: Some(mandatory_dimensions_rs.get()),
        };

        let is_edit = edit;
        spawn_local({
            async move {
                let result = if is_edit {
                    update_workspace(
                        workspace_name_rs.get(),
                        org_id.get().to_string(),
                        update_payload,
                    )
                    .await
                } else {
                    create_workspace(org_id.get().to_string(), create_payload).await
                };

                match result {
                    Ok(_) => {
                        handle_submit.call(());
                        let success_message = if is_edit {
                            "Workspace updated successfully!"
                        } else {
                            "New workspace created successfully!"
                        };
                        enqueue_alert(
                            String::from(success_message),
                            AlertType::Success,
                            5000,
                        );
                    }
                    Err(e) => {
                        logging::error!(
                            "An error occurred while trying to {} the workspace: {}",
                            if is_edit { "update" } else { "create" },
                            e
                        );
                        enqueue_alert(e, AlertType::Error, 5000);
                    }
                }
                req_inprogress_ws.set(false);
            }
        });
    };

    view! {
        <EditorProvider>
            <form class="form-control w-full space-y-4 bg-white text-gray-700 font-mono">
                <div class="form-control">
                    <label class="label">
                        <span class="label-text">Workspace Name</span>
                    </label>
                    <input
                        disabled=edit
                        type="text"
                        placeholder="Workspace Name"
                        class="input input-bordered w-full max-w-md"
                        value=workspace_name_rs.get_untracked()
                        on:change=move |ev| {
                            let value = event_target_value(&ev);
                            workspace_name_ws.set(value);
                        }
                    />
                </div>

                <div class="form-control">
                    <label class="label">
                        <span class="label-text">Workspace Admin Email</span>
                    </label>
                    <input
                        type="text"
                        placeholder="Admin Email"
                        class="input input-bordered w-full max-w-md"
                        value=workspace_admin_email_rs.get_untracked()
                        on:change=move |ev| {
                            let value = event_target_value(&ev);
                            workspace_admin_email_ws.set(value);
                        }
                    />
                </div>

                <Show when=move || edit>
                    <div class="form-control">
                        <label class="label">
                            <span class="label-text">Mandatory Dimensions</span>
                        </label>
                        <input
                            type="text"
                            placeholder="Mandatory Dimensions"
                            class="input input-bordered w-full max-w-md"
                            value=to_string(&mandatory_dimensions_rs.get_untracked())
                                .unwrap_or_default()
                            on:change=move |ev| {
                                let value = event_target_value(&ev);
                                mandatory_dimensions_ws
                                    .set(string_to_vec(&value).unwrap_or_default());
                            }
                        />
                    </div>
                </Show>

                <div class="form-control">
                    <label class="label">
                        <span class="label-text">Workspace Status</span>
                    </label>
                    <Toggle
                        name="workspace-status"
                        value=if workspace_status_rs.get_untracked() == WorkspaceStatus::ENABLED {
                            true
                        } else {
                            false
                        }
                        on_change=Callback::new(move |flag: serde_json::Value| {
                            let flag = flag.as_bool().unwrap();
                            if flag {
                                workspace_status_ws.set(WorkspaceStatus::ENABLED)
                            } else {
                                workspace_status_ws.set(WorkspaceStatus::DISABLED)
                            }
                        })
                        class=String::from("mt-2")
                    />
                </div>

                <div class="form-control grid w-full justify-start">
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
            </form>
        </EditorProvider>
    }
}
