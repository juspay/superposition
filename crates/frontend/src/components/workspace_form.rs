pub mod types;
pub mod utils;

use leptos::*;
use serde_json::{to_string, Value};
use superposition_types::api::workspace::CreateWorkspaceRequest;
use superposition_types::database::models::{Metrics, WorkspaceStatus};
use web_sys::MouseEvent;

use crate::components::metrics_form::MetricsForm;
use crate::components::workspace_form::utils::string_to_vec;
use crate::components::{alert::AlertType, button::Button};
use crate::types::OrganisationId;
use crate::{
    components::input::{Input, InputType, Toggle},
    schema::{JsonSchemaType, SchemaType},
};
use crate::{
    components::workspace_form::utils::{create_workspace, update_workspace},
    providers::{alert_provider::enqueue_alert, editor_provider::EditorProvider},
};

#[component]
pub fn workspace_form(
    org_id: RwSignal<OrganisationId>,
    #[prop(default = false)] edit: bool,
    #[prop(default = String::new())] workspace_admin_email: String,
    #[prop(default = String::new())] workspace_name: String,
    #[prop(default = WorkspaceStatus::ENABLED)] workspace_status: WorkspaceStatus,
    #[prop(default = vec![])] mandatory_dimensions: Vec<String>,
    #[prop(default = Value::Null)] config_version: Value,
    #[prop(default = Metrics::default())] metrics: Metrics,
    #[prop(default = false)] allow_experiment_self_approval: bool,
    #[prop(into)] handle_submit: Callback<(), ()>,
) -> impl IntoView {
    let (workspace_name_rs, workspace_name_ws) = create_signal(workspace_name);
    let (workspace_admin_email_rs, workspace_admin_email_ws) =
        create_signal(workspace_admin_email);
    let (config_version_rs, config_version_ws) = create_signal(config_version);
    let (workspace_status_rs, workspace_status_ws) = create_signal(workspace_status);
    let (mandatory_dimensions_rs, mandatory_dimensions_ws) =
        create_signal(mandatory_dimensions);
    let (req_inprogess_rs, req_inprogress_ws) = create_signal(false);
    let (strict_mode_rs, strict_mode_ws) = create_signal(true);
    let metrics_rws = RwSignal::new(metrics);
    let allow_experiment_self_approval_rs = RwSignal::new(allow_experiment_self_approval);

    let on_submit = move |ev: MouseEvent| {
        req_inprogress_ws.set(true);
        ev.prevent_default();

        let is_edit = edit;
        spawn_local({
            async move {
                let result = if is_edit {
                    update_workspace(
                        workspace_name_rs.get_untracked(),
                        org_id.get_untracked().0,
                        workspace_admin_email_rs.get_untracked(),
                        config_version_rs.get_untracked(),
                        workspace_status_rs.get_untracked(),
                        mandatory_dimensions_rs.get_untracked(),
                        metrics_rws.get_untracked(),
                        allow_experiment_self_approval_rs.get_untracked(),
                    )
                    .await
                } else {
                    let create_payload = CreateWorkspaceRequest {
                        workspace_admin_email: workspace_admin_email_rs.get_untracked(),
                        workspace_name: workspace_name_rs.get_untracked(),
                        workspace_status: Some(workspace_status_rs.get_untracked()),
                        strict_mode: strict_mode_rs.get_untracked(),
                        metrics: Some(metrics_rws.get_untracked()),
                        allow_experiment_self_approval: allow_experiment_self_approval_rs
                            .get_untracked(),
                    };
                    create_workspace(org_id.get_untracked().0, create_payload).await
                };

                req_inprogress_ws.set(false);
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
            }
        });
    };

    view! {
        <EditorProvider>
            <form class="flex flex-col gap-2 form-control w-full space-y-4 bg-white text-gray-700 font-mono">
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

                <div class="form-control">
                    <label class="label w-fit flex items-center gap-2">
                        <span class="label-text">"Config Version"</span>
                        <div class="group relative inline-block text-[10px] text-gray-700 cursor-pointer">
                            <p class="z-[1000] hidden absolute top-full left-1/2 w-[320px] p-2.5 group-hover:flex flex-col gap-4 bg-black text-white rounded shadow-[0_4px_6px_rgba(0,0,0,0.1)] whitespace-normal translate-x-[20px] -translate-y-1/2">
                                "This will affect the config resolve since it will use now this version to generate the config"
                            </p>
                            <i class="ri-information-line ri-lg" />
                        </div>
                    </label>
                    <Input
                        r#type=InputType::Text
                        placeholder="Config Version"
                        class="input input-bordered w-full max-w-md"
                        schema_type=SchemaType::Single(JsonSchemaType::String)
                        value=config_version_rs.get_untracked()
                        on_change=move |val: Value| {
                            let value = if val.as_str().map_or(false, |s| s.is_empty()) {
                                Value::Null
                            } else {
                                val
                            };
                            config_version_ws.set(value)
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
                        value=workspace_status_rs.get_untracked() == WorkspaceStatus::ENABLED
                        on_change=Callback::new(move |flag: serde_json::Value| {
                            let flag = flag.as_bool().unwrap();
                            if flag {
                                workspace_status_ws.set(WorkspaceStatus::ENABLED)
                            } else {
                                workspace_status_ws.set(WorkspaceStatus::DISABLED)
                            }
                        })
                    />
                </div>

                <div class="form-control">
                    <label class="label">
                        <span class="label-text">"Allow self approval for Experiments"</span>
                    </label>
                    <Toggle
                        name="workspace-status"
                        value=allow_experiment_self_approval_rs.get_untracked()
                        on_change=move |flag: serde_json::Value| {
                            let flag = flag.as_bool().unwrap();
                            allow_experiment_self_approval_rs.set(flag);
                        }
                    />
                </div>

                <Show when=move || !edit>
                    <div class="form-control">
                        <label class="label w-fit flex items-center gap-2">
                            <span class="label-text">"Strict Mode"</span>
                            <div class="group relative inline-block text-[10px] text-gray-700 cursor-pointer">
                                <p class="z-[1000] hidden absolute top-full left-1/2 w-[320px] p-2.5 group-hover:flex flex-col gap-4 bg-black text-white rounded shadow-[0_4px_6px_rgba(0,0,0,0.1)] whitespace-normal translate-x-[20px] -translate-y-1/2">
                                    "Strict Mode limits the operators available to just ==. This is the recommended mode for production environments"
                                </p>
                                <i class="ri-information-line ri-lg" />
                            </div>
                        </label>
                        <Toggle
                            name="workspace-strict-mode"
                            value=strict_mode_rs.get()
                            on_change=Callback::new(move |_| {
                                strict_mode_ws.update(|v| *v = !*v);
                            })
                        />
                    </div>
                </Show>

                <MetricsForm
                    metrics=metrics_rws.get_untracked()
                    on_change=Callback::new(move |metrics| metrics_rws.set(metrics))
                />

                <div class="form-control grid w-full justify-start">
                    {move || {
                        let loading = req_inprogess_rs.get();
                        view! {
                            <Button
                                class="pl-[70px] pr-[70px] w-48 h-12".to_string()
                                text="Submit".to_string()
                                on_click=on_submit
                                loading
                            />
                        }
                    }}
                </div>
            </form>
        </EditorProvider>
    }
}
