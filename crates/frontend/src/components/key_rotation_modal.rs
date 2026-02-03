use leptos::*;
use web_sys::MouseEvent;

use crate::{
    api::{master_encryption_key, workspaces},
    components::{
        alert::AlertType, button::Button, form::label::Label, modal::PortalModal,
    },
    providers::alert_provider::enqueue_alert,
    types::OrganisationId,
};

#[component]
fn Title(
    #[prop(into)] title: String,
    #[prop(into)] description: String,
) -> impl IntoView {
    view! {
        <div class="flex items-start gap-3">
            <i class="ri-key-2-line text-4xl text-yellow-500" />
            <div class="flex-1 flex flex-col gap-1">
                <h3 class="text-xl font-bold text-gray-900">{title}</h3>
                <p class="text-sm text-gray-600">{description}</p>
            </div>
        </div>
    }
}

#[component]
fn FooterActions(
    #[prop(into)] continue_text: String,
    #[prop(into)] on_close: Callback<()>,
    #[prop(into)] on_rotate: Callback<MouseEvent>,
    #[prop(into)] req_inprogress_rs: ReadSignal<bool>,
    #[prop(into)] continue_btn_class: String,
) -> impl IntoView {
    view! {
        <div class="pt-4 flex justify-end gap-3 border-t">
            <Button
                class="btn-ghost"
                icon_class="ri-forbid-line"
                text="Cancel"
                on_click=move |_| on_close.call(())
            />
            <Button
                class=continue_btn_class
                text=continue_text
                icon_class="ri-arrow-right-line"
                on_click=on_rotate
                loading=req_inprogress_rs.get()
            />
        </div>
    }
}

#[component]
fn CommonModal(
    #[prop(into)] on_close: Callback<()>,
    #[prop(into)] on_rotate: Callback<MouseEvent>,
    req_inprogress_rs: ReadSignal<bool>,
    #[prop(into)] options: Vec<String>,
    #[prop(into)] description: String,
) -> impl IntoView {
    view! {
        <PortalModal class="p-6 flex flex-col gap-6" handle_close=on_close>
            <Title title="⚠️ Final Confirmation Required" description=description.clone() />

            <div class="p-4 flex flex-col gap-2 bg-yellow-50 border border-yellow-200 rounded-lg">
                <h4 class="flex items-center gap-2 font-semibold text-yellow-900">
                    <i class="ri-alert-line" />
                    "Important Information"
                </h4>
                <ul class="flex flex-col gap-1 text-sm text-yellow-800 list-disc list-inside">
                    {options.iter().map(|option| view! { <li>{option}</li> }).collect_view()}
                </ul>
            </div>

            <FooterActions
                continue_text="Yes, Rotate Key Now"
                on_close=on_close
                on_rotate=on_rotate
                req_inprogress_rs=req_inprogress_rs
                continue_btn_class="bg-red-600 hover:bg-red-700 text-white"
            />
        </PortalModal>
    }
}

#[component]
pub fn WorkspaceKeyRotationModal(
    #[prop(into)] on_close: Callback<()>,
    #[prop(into)] on_success: Callback<()>,
    workspace_name: String,
) -> impl IntoView {
    let org_id = use_context::<Signal<OrganisationId>>().unwrap();
    let workspace_name = StoredValue::new(workspace_name);
    let (req_inprogress_rs, req_inprogress_ws) = create_signal(false);

    let on_rotate = move |_: MouseEvent| {
        req_inprogress_ws.set(true);

        let workspace = workspace_name.get_value();
        let org = org_id.get();

        spawn_local(async move {
            let result = workspaces::rotate_key(&workspace, &org).await;
            req_inprogress_ws.set(false);

            match result {
                Ok(response) => {
                    enqueue_alert(
                        format!(
                            "Encryption key rotated successfully! {} secret(s) re-encrypted.",
                            response.total_secrets_re_encrypted
                        ),
                        AlertType::Success,
                        8000,
                    );
                    on_success.call(());
                }
                Err(e) => {
                    logging::error!("Error rotating workspace encryption key: {:?}", e);
                    enqueue_alert(
                        format!("Failed to rotate encryption key: {}", e),
                        AlertType::Error,
                        8000,
                    );
                }
            }
        });
    };

    view! {
        <CommonModal
            on_close=on_close
            on_rotate=on_rotate
            req_inprogress_rs=req_inprogress_rs
            options=[
                "All secrets will be re-encrypted with the new key".to_string(),
                "This operation is atomic - either all secrets rotate or none do".to_string(),
                "Applications using secrets will continue to work without changes".to_string(),
            ]
            description="This action will immediately rotate the encryption key for all secrets in this workspace. Are you absolutely sure you want to proceed?"
        />
    }
}

fn key_rotation_instructions() -> [View; 5] {
    let code_class = "px-1 py-0.5 bg-base-300 rounded text-xs font-mono";

    [
        view! { "Generate a new master encryption key using a secure random generator" }
        .into_view(),
        view! {
            "Set the current key as "
            <code class=code_class>"PREVIOUS_MASTER_ENCRYPTION_KEY"</code>
            " and the new key as "
            <code class=code_class>"MASTER_ENCRYPTION_KEY"</code>
        }
        .into_view(),
        view! {
            "Configure environment variables:"
            <code class="block p-2 bg-base-300 rounded text-xs font-mono">
                "PREVIOUS_MASTER_ENCRYPTION_KEY=<current_key>\nMASTER_ENCRYPTION_KEY=<new_key>"
            </code>
        }
        .into_view(),
        view! { "Restart the service with the updated environment variables" }
        .into_view(),
        view! { "The system will function normally during the transition period" }
        .into_view(),
    ]
}

#[component]
pub fn MasterKeyRotationModal(
    #[prop(into)] on_close: Callback<()>,
    #[prop(into)] on_success: Callback<()>,
) -> impl IntoView {
    let (req_inprogress_rs, req_inprogress_ws) = create_signal(false);
    let (show_instructions_rs, show_instructions_ws) = create_signal(true);

    let on_rotate = move |_: MouseEvent| {
        if show_instructions_rs.get_untracked() {
            show_instructions_ws.set(false);
            return;
        }

        req_inprogress_ws.set(true);

        spawn_local(async move {
            let result = master_encryption_key::rotate().await;
            req_inprogress_ws.set(false);

            match result {
                Ok(response) => {
                    enqueue_alert(
                        format!(
                            "Encryption key rotated successfully! {} secret(s) re-encrypted across {} workspace(s).",
                            response.total_secrets_re_encrypted,
                            response.workspaces_rotated
                        ),
                        AlertType::Success,
                        8000,
                    );
                    show_instructions_ws.set(true);
                    on_success.call(());
                }
                Err(e) => {
                    logging::error!("Error rotating master encryption key: {:?}", e);
                    enqueue_alert(
                        format!("Failed to rotate master encryption key: {}", e),
                        AlertType::Error,
                        8000,
                    );
                    show_instructions_ws.set(true);
                }
            }
        });
    };

    move || {
        if show_instructions_rs.get() {
            view! {
                <PortalModal class="p-6 flex flex-col gap-6" handle_close=on_close>
                    <Title
                        title="Rotate Master Encryption Key"
                        description="Please review the following instructions before proceeding with key rotation."
                    />
                    <div class="control-form">
                        <Label title="Instructions" />
                        <ol class="p-4 flex flex-col gap-3 bg-base-200 rounded-lg list-none">
                            {key_rotation_instructions()
                                .into_iter()
                                .enumerate()
                                .map(|(index, instruction)| {
                                    view! {
                                        <li class="flex gap-3 items-center">
                                            <span class="flex-shrink-0 w-6 h-6 flex items-center justify-center bg-primary text-primary-content rounded-full text-xs font-semibold">
                                                {(index + 1).to_string()}
                                            </span>
                                            <div class="text-[14px] flex-1">{instruction}</div>
                                        </li>
                                    }
                                })
                                .collect_view()}
                        </ol>
                    </div>
                    <FooterActions
                        continue_text="I Understand, Continue"
                        on_close=on_close
                        on_rotate=on_rotate
                        req_inprogress_rs=req_inprogress_rs
                        continue_btn_class="bg-blue-600 hover:bg-blue-700 text-white"
                    />
                </PortalModal>
            }
        } else {
            view! {
                <CommonModal
                    on_close=on_close
                    on_rotate=on_rotate
                    req_inprogress_rs=req_inprogress_rs
                    options=[
                        "All workspace encryption keys will be re-encrypted".to_string(),
                        "This is a critical operation affecting the entire system".to_string(),
                        "Workspaces will continue to function normally after rotation".to_string(),
                    ]
                    description="This action will immediately rotate the master encryption key for the entire organization. All workspace keys will be re-encrypted. Are you absolutely sure you want to proceed?"
                />
            }
        }
    }
}
