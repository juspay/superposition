use leptos::*;

#[component]
fn InfoSection(
    title: &'static str,
    icon: &'static str,
    content: String,
) -> impl IntoView {
    view! {
        <div class="flex flex-col gap-2">
            <h4 class="font-semibold purple flex items-center gap-2">
                <i class=icon />
                {title}
            </h4>
            <div class="min-h-[48px] bg-gray-50 p-3 rounded-md border border-gray-200">
                <p class="text-gray-700">{content}</p>
            </div>
        </div>
    }
}

#[component]
pub fn InfoModal(
    #[prop(default = false)] visible: bool,
    #[prop(default = String::new())] description: String,
    #[prop(default = String::new())] change_reason: String,
    on_close: Callback<()>,
) -> impl IntoView {
    let modal_class = move || {
        if visible {
            "fixed inset-0 bg-black bg-opacity-50 z-50 flex items-center justify-center"
        } else {
            "hidden"
        }
    };

    let close_button_base_class = "flex items-center";

    view! {
        <div class=modal_class>
            <div class="w-full max-w-xl p-6 flex flex-col gap-4 bg-white rounded-lg shadow-lg">
                <div class="flex justify-between items-center border-b pb-3">
                    <h3 class="text-lg font-bold text-gray-800">Change Information</h3>
                    <button
                        on:click=move |_| on_close.call(())
                        class=format!("{close_button_base_class} text-gray-500 hover:text-gray-700")
                    >
                        <i class="ri-close-line ri-xl"></i>
                    </button>
                </div>

                <InfoSection title="Description" icon="ri-information-line" content=description />

                <InfoSection
                    title="Reason for Change"
                    icon="ri-history-line"
                    content=change_reason
                />

                <div class="flex justify-end">
                    <button
                        on:click=move |_| on_close.call(())
                        class=format!(
                            "{close_button_base_class} px-5 py-2 gap-1 bg-purple-500 text-white rounded-md hover:bg-purple-600 transition-colors font-medium",
                        )
                    >
                        <i class="ri-check-line"></i>
                        Close
                    </button>
                </div>
            </div>
        </div>
    }
}
