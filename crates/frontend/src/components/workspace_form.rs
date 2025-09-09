pub mod types;
pub mod utils;

use leptos::*;
use serde_json::{to_string, Value};
use superposition_types::api::workspace::CreateWorkspaceRequest;
use superposition_types::database::models::{Metrics, WorkspaceStatus};
use web_sys::MouseEvent;

use crate::components::form::label::Label;
use crate::components::metrics_form::MetricsForm;
use crate::components::workspace_form::utils::string_to_vec;
use crate::components::{alert::AlertType, button::Button};
use crate::providers::{alert_provider::enqueue_alert, editor_provider::EditorProvider};
use crate::types::OrganisationId;
use crate::{
    api::workspaces,
    components::input::{Input, InputType, Toggle},
    schema::{JsonSchemaType, SchemaType},
};

#[component]
pub fn workspace_form(
    org_id: Signal<OrganisationId>,
    #[prop(default = false)] edit: bool,
    #[prop(default = String::new())] workspace_admin_email: String,
    #[prop(default = String::new())] workspace_name: String,
    #[prop(default = WorkspaceStatus::ENABLED)] workspace_status: WorkspaceStatus,
    #[prop(default = vec![])] mandatory_dimensions: Vec<String>,
    #[prop(default = Value::Null)] config_version: Value,
    #[prop(default = Metrics::default())] metrics: Metrics,
    #[prop(default = false)] allow_experiment_self_approval: bool,
    #[prop(default = true)] auto_populate_control: bool,
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
    #[cfg(feature = "jsonlogic")]
    let (strict_mode_rs, strict_mode_ws) = create_signal(true);
    let metrics_rws = RwSignal::new(metrics);
    let allow_experiment_self_approval_rs = RwSignal::new(allow_experiment_self_approval);
    let (auto_populate_control_rs, auto_populate_control_ws) =
        create_signal(auto_populate_control);

    let on_submit = move |ev: MouseEvent| {
        req_inprogress_ws.set(true);
        ev.prevent_default();

        let is_edit = edit;
        spawn_local({
            async move {
                let result = if is_edit {
                    let update_payload = workspaces::try_update_payload(
                        workspace_admin_email_rs.get_untracked(),
                        config_version_rs.get_untracked(),
                        workspace_status_rs.get_untracked(),
                        mandatory_dimensions_rs.get_untracked(),
                        metrics_rws.get_untracked(),
                        allow_experiment_self_approval_rs.get_untracked(),
                        auto_populate_control_rs.get_untracked(),
                    );
                    match update_payload {
                        Ok(payload) => {
                            workspaces::update(
                                &workspace_name_rs.get_untracked(),
                                payload,
                                &org_id.get_untracked().0,
                            )
                            .await
                        }
                        Err(e) => Err(e),
                    }
                } else {
                    let create_payload = CreateWorkspaceRequest {
                        workspace_admin_email: workspace_admin_email_rs.get_untracked(),
                        workspace_name: workspace_name_rs.get_untracked(),
                        workspace_status: Some(workspace_status_rs.get_untracked()),
                        #[cfg(feature = "jsonlogic")]
                        strict_mode: strict_mode_rs.get_untracked(),
                        metrics: Some(metrics_rws.get_untracked()),
                        allow_experiment_self_approval: allow_experiment_self_approval_rs
                            .get_untracked(),
                        auto_populate_control: auto_populate_control_rs.get_untracked(),
                    };
                    workspaces::create(create_payload, &org_id.get_untracked().0).await
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

    cfg_if::cfg_if! {
        if #[cfg(feature = "jsonlogic")] {
            let strict_mode =
                view! {
                    <Show when=move || !edit>
                        <div class="w-fit flex items-center gap-2">
                            <Toggle
                                name="workspace-strict-mode"
                                value=strict_mode_rs.get_untracked()
                                on_change=move |v| strict_mode_ws.set(v)
                            />
                            <Label
                                title="Strict Mode"
                                extra_info="Strict Mode limits the operators available to just ==. This is the recommended mode for production environments"
                            />
                        </div>
                    </Show>
                }.into_view();
        } else {
            let strict_mode = ().into_view();
        }
    };

    view! {
        <EditorProvider>
            <form class="w-full flex flex-col gap-5 bg-white text-gray-700">
                <div class="form-control">
                    <Label title="Workspace Name" />
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
                    <Label title="Workspace Admin Email" />
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
                    <Label
                        title="Config Version"
                        extra_info="This will affect the config resolve since it will use now this version to generate the config"
                    />
                    <Input
                        r#type=InputType::Text
                        placeholder="Config Version"
                        class="input input-bordered w-full max-w-md"
                        schema_type=SchemaType::Single(JsonSchemaType::String)
                        value=config_version_rs.get_untracked()
                        on_change=move |val: Value| {
                            let value = if val.as_str().is_some_and(|s| s.is_empty()) {
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
                        <Label title="Mandatory Dimensions" />
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

                <div class="w-fit flex items-center gap-2">
                    <Toggle
                        name="workspace-status"
                        value=workspace_status_rs.get_untracked() == WorkspaceStatus::ENABLED
                        on_change=move |flag| {
                            if flag {
                                workspace_status_ws.set(WorkspaceStatus::ENABLED)
                            } else {
                                workspace_status_ws.set(WorkspaceStatus::DISABLED)
                            }
                        }
                    />
                    <Label title="Workspace Status" />
                </div>

                <div class="w-fit flex items-center gap-2">
                    <Toggle
                        name="workspace-self-approval"
                        value=allow_experiment_self_approval_rs.get_untracked()
                        on_change=move |v| allow_experiment_self_approval_rs.set(v)
                    />
                    <Label title="Allow self approval for Experiments" />
                </div>

                {strict_mode}

                <div class="w-fit flex items-center gap-2">
                    <Toggle
                        name="workspace-auto-populate-control"
                        value=auto_populate_control_rs.get_untracked()
                        on_change=move |v| auto_populate_control_ws.set(v)
                    />
                    <Label
                        title="Auto-populate Control"
                        extra_info="This will automatically populate the control variant for experiments with the current values for the given context."
                    />
                </div>

                <MetricsForm
                    metrics=metrics_rws.get_untracked()
                    on_change=Callback::new(move |metrics| metrics_rws.set(metrics))
                />

                {move || {
                    let loading = req_inprogess_rs.get();
                    view! {
                        <Button
                            class="self-end h-12 w-48"
                            text="Submit"
                            icon_class="ri-send-plane-line"
                            on_click=on_submit
                            loading
                        />
                    }
                }}

            </form>
        </EditorProvider>
    }
}
