use leptos::*;

#[derive(Clone, Debug, PartialEq)]
pub struct ConfigInfo {
    pub description: String,
    pub change_reason: String,
}

#[component]
pub fn info_modal(
    modal_visible: ReadSignal<bool>,
    #[prop(default = String::new())] description: String,
    #[prop(default = String::new())] change_reason: String,
    set_modal_visible: WriteSignal<bool>,
) -> impl IntoView {
    view! {
        <div class=move || {
            if modal_visible.get() {
                "fixed inset-0 bg-black bg-opacity-50 z-50 flex items-center justify-center"
            } else {
                "hidden"
            }
        }>
            <div class="bg-white p-6 rounded-lg shadow-lg w-full max-w-xl">
                <div class="flex justify-between items-center mb-5 border-b pb-3">
                    <h3 class="text-lg font-bold text-gray-800">Change Information</h3>
                    <button
                        on:click=move |_| set_modal_visible.set(false)
                        class="text-gray-500 hover:text-gray-700"
                    >
                        <i class="ri-close-line ri-xl"></i>
                    </button>
                </div>

                <div class="mb-5">
                    <h4 class="font-semibold mb-2 text-blue-600 flex items-center">
                        <i class="ri-information-line mr-2"></i>
                        Description
                    </h4>
                    <div class="bg-gray-50 p-3 rounded-md border border-gray-200">
                        <p class="text-gray-700">{description}</p>
                    </div>
                </div>

                <div class="mb-5">
                    <h4 class="font-semibold mb-2 text-blue-600 flex items-center">
                        <i class="ri-history-line mr-2"></i>
                        Reason for Change
                    </h4>
                    <div class="bg-gray-50 p-3 rounded-md border border-gray-200">
                        <p class="text-gray-700">{change_reason}</p>
                    </div>
                </div>

                <div class="flex justify-end pt-2">
                    <button
                        on:click=move |_| set_modal_visible.set(false)
                        class="px-5 py-2 bg-blue-500 text-white rounded-md hover:bg-blue-600 transition-colors font-medium flex items-center"
                    >
                        <i class="ri-check-line mr-1"></i>
                        Close
                    </button>
                </div>
            </div>
        </div>
    }
}
