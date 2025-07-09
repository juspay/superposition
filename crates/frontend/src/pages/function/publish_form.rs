use leptos::*;
use web_sys::MouseEvent;

use crate::{
    components::{alert::AlertType, button::Button, change_form::ChangeForm},
    pages::function::utils::publish_function,
    providers::alert_provider::enqueue_alert,
    types::{OrganisationId, Tenant},
};

#[component]
pub fn publish_form(
    function_name: String,
    #[prop(into)] handle_submit: Callback<(), ()>,
    #[prop(into)] handle_close: Callback<(), ()>,
) -> impl IntoView {
    let workspace = use_context::<Signal<Tenant>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();
    let change_reason_rws =
        RwSignal::new("Publishing new version of the function".to_string());
    let (req_inprogress_rs, req_inprogress_ws) = create_signal(false);
    let function_name = StoredValue::new(function_name);

    let handle_publish = move |event: MouseEvent| {
        req_inprogress_ws.set(true);
        event.prevent_default();

        spawn_local(async move {
            let workspace = workspace.get_untracked().0;
            let org = org.get_untracked().0;

            let result = publish_function(
                function_name.get_value(),
                change_reason_rws.get_untracked(),
                workspace,
                org,
            )
            .await;

            req_inprogress_ws.set(false);
            match result {
                Ok(_) => {
                    handle_submit.call(());
                    let message = "Function published successfully!".to_string();
                    enqueue_alert(message, AlertType::Success, 5000);
                }
                Err(e) => {
                    enqueue_alert(e, AlertType::Error, 5000);
                }
            }
        });
    };

    view! {
        <Portal>
            <div class="fixed inset-0 bg-black bg-opacity-50 z-50 flex items-center justify-center">
                <div class="relative box-content w-full max-w-md p-6 flex flex-col gap-4 bg-white rounded-lg shadow-lg">
                    <button
                        class="absolute right-2 top-2 btn btn-sm btn-circle btn-ghost"
                        on:click=move |_| { handle_close.call(()) }
                    >
                        <i class="ri-close-line" />
                    </button>
                    <h3 class="font-bold text-lg">"Publish Function"</h3>
                    <p class="py-2">"Are you sure you want to publish this function?"</p>
                    <form class="flex flex-col gap-4">
                        <ChangeForm
                            title="Reason for Change".to_string()
                            placeholder="Enter a reason for this change".to_string()
                            value=change_reason_rws.get_untracked()
                            on_change=move |new_change_reason| {
                                change_reason_rws.set(new_change_reason)
                            }
                        />
                        {move || {
                            view! {
                                <Button
                                    text="Publish"
                                    on_click=handle_publish
                                    loading=req_inprogress_rs.get()
                                    class="w-fit self-end"
                                    icon_class="ri-article-line"
                                />
                            }
                        }}
                    </form>
                </div>
            </div>
        </Portal>
    }
}
