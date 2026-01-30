use leptos::*;
use web_sys::MouseEvent;

use crate::{
    api::workspaces,
    components::{alert::AlertType, button::Button, modal::PortalModal},
    providers::alert_provider::enqueue_alert,
};

#[component]
pub fn KeyRotationModal(
    #[prop(default = false)] visible: bool,
    #[prop(into)] on_close: Callback<()>,
    #[prop(into)] on_success: Callback<()>,
    workspace_name: String,
    org_id: String,
) -> impl IntoView {
    let workspace_name_stored = StoredValue::new(workspace_name);
    let org_id_stored = StoredValue::new(org_id);

    let (req_inprogress_rs, req_inprogress_ws) = create_signal(false);
    let (confirmation_step_rs, confirmation_step_ws) = create_signal(false);

    let on_rotate = Callback::new(move |_: MouseEvent| {
        logging::log!("Key rotation button clicked");
        if !confirmation_step_rs.get_untracked() {
            logging::log!("Moving to confirmation step");
            confirmation_step_ws.set(true);
            return;
        }

        req_inprogress_ws.set(true);
        let workspace_val = workspace_name_stored.get_value();
        let org_val = org_id_stored.get_value();
        logging::log!(
            "Starting key rotation for workspace: {}, org: {}",
            workspace_val,
            org_val
        );

        spawn_local(async move {
            let result = workspaces::rotate_key(&workspace_val, &org_val).await;

            req_inprogress_ws.set(false);

            match result {
                Ok(status) => {
                    logging::log!(
                        "Key rotation successful: {} secrets re-encrypted",
                        status.total_secrets_re_encrypted
                    );
                    enqueue_alert(
                        format!(
                            "Encryption key rotated successfully! {} secret(s) re-encrypted.",
                            status.total_secrets_re_encrypted
                        ),
                        AlertType::Success,
                        8000,
                    );
                    confirmation_step_ws.set(false);
                    on_success.call(());
                    on_close.call(());
                }
                Err(e) => {
                    logging::error!("Error rotating encryption key: {:?}", e);
                    enqueue_alert(
                        format!("Failed to rotate encryption key: {}", e),
                        AlertType::Error,
                        8000,
                    );
                    confirmation_step_ws.set(false);
                }
            }
        });
    });

    let handle_close = Callback::new(move |_| {
        confirmation_step_ws.set(false);
        on_close.call(());
    });

    view! {
        <Show when=move || visible>
            <PortalModal class="p-6 flex flex-col gap-6" handle_close>
                <div class="flex items-start gap-3">
                    <i class="ri-key-2-line text-4xl text-yellow-500"></i>
                    <div class="flex-1">
                        <h3 class="text-xl font-bold text-gray-900">
                            {move || {
                                if confirmation_step_rs.get() {
                                    "⚠️ Final Confirmation Required"
                                } else {
                                    "Rotate Workspace Encryption Key"
                                }
                            }}
                        </h3>
                        <p class="text-sm text-gray-600 mt-2">
                            {move || {
                                if confirmation_step_rs.get() {
                                    "This action will immediately rotate the encryption key for all secrets in this workspace. Are you absolutely sure you want to proceed?"
                                } else {
                                    "This will generate a new encryption key and re-encrypt all secrets in this workspace."
                                }
                            }}
                        </p>
                    </div>
                </div>

                <Show when=move || !confirmation_step_rs.get()>
                    <div class="bg-yellow-50 border border-yellow-200 rounded-lg p-4">
                        <h4 class="font-semibold text-yellow-900 mb-2 flex items-center gap-2">
                            <i class="ri-alert-line"></i>
                            "Important Information"
                        </h4>
                        <ul class="text-sm text-yellow-800 space-y-1 list-disc list-inside">
                            <li>"All secrets will be re-encrypted with the new key"</li>
                            <li>"The old key will be retained temporarily for rollback"</li>
                            <li>
                                "This operation is atomic - either all secrets rotate or none do"
                            </li>
                            <li>
                                "Applications using secrets will continue to work without changes"
                            </li>
                        </ul>
                    </div>
                </Show>

                <Show when=move || confirmation_step_rs.get()>
                    <div class="bg-red-50 border-2 border-red-300 rounded-lg p-4">
                        <p class="text-red-900 font-semibold">
                            "⚠️ WARNING: This action cannot be undone automatically. Please ensure you have documented this change."
                        </p>
                    </div>
                </Show>

                <div class="flex justify-end gap-3 pt-4 border-t">
                    <Button
                        class="btn-ghost"
                        text="Cancel"
                        on_click=move |_| {
                            confirmation_step_ws.set(false);
                            handle_close.call(());
                        }
                        loading=false
                    />
                    {move || {
                        let is_confirmed = confirmation_step_rs.get();
                        view! {
                            <Button
                                class=if is_confirmed {
                                    "bg-red-600 hover:bg-red-700 text-white"
                                } else {
                                    "bg-yellow-600 hover:bg-yellow-700 text-white"
                                }
                                text=if is_confirmed {
                                    "Yes, Rotate Key Now"
                                } else {
                                    "Proceed to Confirmation"
                                }
                                icon_class=if is_confirmed {
                                    "ri-lock-unlock-line"
                                } else {
                                    "ri-arrow-right-line"
                                }
                                on_click=on_rotate
                                loading=req_inprogress_rs.get()
                            />
                        }
                    }}
                </div>
            </PortalModal>
        </Show>
    }
}
