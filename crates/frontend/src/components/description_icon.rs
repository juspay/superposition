use crate::components::info_modal::InfoModal;
use leptos::*;

#[component]
pub fn InfoDescription(
    #[prop(into)] description: String,
    #[prop(into)] change_reason: String,
) -> impl IntoView {
    let (show_modal, set_show_modal) = create_signal(false);
    let stored_description = store_value(description);
    let stored_change_reason = store_value(change_reason);

    view! {
        <svg
            viewBox="0 0 32 16"
            class="inline w-4 h-2 text-gray-500 hover:text-gray-700 cursor-pointer"
            on:click=move |_| set_show_modal.set(true)
        >
            <rect
                width="32"
                height="16"
                rx="8"
                ry="8"
                fill="currentColor"
                stroke-width="0"
                stroke-linecap="round"
            />
            <ellipse rx="2" ry="2" transform="translate(7 8)" fill="white" stroke-width="0" />
            <ellipse rx="2" ry="2" transform="translate(25 8)" fill="white" stroke-width="0" />
            <ellipse rx="2" ry="2" transform="translate(16 8)" fill="white" stroke-width="0" />
        </svg>
        <Show when=move || show_modal.get() fallback=|| ()>
            <Portal mount=document().body().unwrap()>
                <InfoModal
                    visible=true
                    description=stored_description.get_value()
                    change_reason=stored_change_reason.get_value()
                    on_close=Callback::new(move |_| set_show_modal.set(false))
                />
            </Portal>
        </Show>
    }
}
