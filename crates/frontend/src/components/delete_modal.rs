use leptos::*;

#[component]
pub fn delete_modal(
    modal_visible: ReadSignal<bool>,
    confirm_delete: Callback<()>,
    set_modal_visible: WriteSignal<bool>,
    header_text: String,
) -> impl IntoView {
    let style = "font-medium rounded-lg text-sm text-center text-white px-5 py-2.5 hover:opacity-75";

    view! {
        <Show when=move || modal_visible.get()>
            <div class="fixed inset-0 bg-black bg-opacity-50 backdrop-blur-sm flex items-center justify-center">
                <dialog id="my_modal_2" class="modal" open=modal_visible.get()>
                    <div class="modal-box bg-white rounded-lg p-6 shadow-xl border-2 border-lightgray">
                        <h4 class="text-xl font-semibold text-gray-800 mb-4">Confirm Delete</h4>
                        <p class="text-sm text-gray-600 mb-6">{header_text.clone()}</p>
                        <div class="flex justify-end space-x-4">
                            <button
                                class=format!("btn bg-purple-500 {style} hover:bg-purple-500")
                                on:click=move |_| confirm_delete.call(())
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
