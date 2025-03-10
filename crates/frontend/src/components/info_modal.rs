use leptos::*;

#[derive(Clone, Debug, PartialEq)]
pub struct ConfigInfo {
    pub description: String,
    pub change_reason: String,
}


#[component]
pub fn info_modal(
modal_visible: ReadSignal<bool>, 
description: String, 
change_reason: String, 
set_modal_visible: WriteSignal<bool>,
) -> impl IntoView {

    view! {

    <div class= move || {
            if modal_visible.get() {
                "fixed inset-0 bg-black bg-opacity-50 z-50 flex items-center justify-center"
            }
            else {
                "hidden"
            }
        }>
        <div class="bg-white p-6 rounded-lg shadow-lg w-full max-w-xl">
        <div class="flex justify-between items-center mb-4">
            <h3 class="text-lg font-bold">Change Information</h3>
            <button
                on:click=move |_| set_modal_visible.set(false)
                class="text-gray-500 hover:text-gray-700"
            >
                <i class="ri-close-line ri-xl"></i>
            </button>
        </div>     
        <div class="mb-4">
            <h4 class="font-semibold mb-2">Description</h4>
            <p class="text-gray-700">{description}</p>
        </div>
        
        <div class="mb-4">
            <h4 class="font-semibold mb-2">Reason for Change</h4>
            <p class="text-gray-700">{change_reason}</p>
        </div>
        
        <div class="flex justify-end">
            <button
                on:click=move |_| set_modal_visible.set(false)
                class="px-4 py-2 bg-blue-500 text-white rounded hover:bg-blue-600"
            >
                Close
            </button>
        </div>
      </div>
    </div>

    }


}