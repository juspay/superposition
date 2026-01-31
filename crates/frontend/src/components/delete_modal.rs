use leptos::*;

use crate::components::change_form::ChangeForm;

#[component]
pub fn DeleteModal(
    modal_visible: ReadSignal<bool>,
    confirm_delete: Callback<String>,
    set_modal_visible: WriteSignal<bool>,
    #[prop(default = false)] with_change_reason: bool,
    header_text: String,
) -> impl IntoView {
    let style = "font-medium rounded-lg text-sm text-center text-white px-5 py-2.5 hover:opacity-75";
    let change_reason_rws = create_rw_signal(String::new());
    view! {
        <Show when=move || modal_visible.get()>
            <div class="fixed inset-0 bg-black bg-opacity-50 backdrop-blur-sm flex items-center justify-center z-[99999999]">
                <dialog id="my_modal_2" class="modal" open=modal_visible.get()>
                    <div class="modal-box bg-white rounded-lg p-6 shadow-xl border-2 border-lightgray">
                        <h4 class="text-xl font-semibold text-gray-800 mb-4">Confirm Delete</h4>
                        <p class="text-sm text-gray-600 mb-6">{header_text.clone()}</p>
                        <Show when=move || with_change_reason>
                            <ChangeForm
                                title="Change Reason".to_string()
                                placeholder="Enter a reason for this change".to_string()
                                class="my-4".to_string()
                                value=change_reason_rws.get_untracked()
                                on_change=move |new_reason| change_reason_rws.set(new_reason)
                            />
                        </Show>
                        <div class="flex justify-end space-x-4">
                            <button
                                class=format!("btn bg-purple-500 {style} hover:bg-purple-500")
                                on:click=move |_| confirm_delete.call(change_reason_rws.get())
                            >
                                Yes, Delete
                            </button>
                            <button
                                class=format!("btn bg-gray-400 {style} hover:bg-gray-300")
                                on:click=move |_| set_modal_visible.set(false)
                            >
                                No
                            </button>
                        </div>
                    </div>
                </dialog>
            </div>
        </Show>
    }
}
