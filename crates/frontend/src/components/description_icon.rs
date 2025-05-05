use crate::components::info_modal::InfoModal;
use leptos::*;
use web_sys::MouseEvent;

#[component]
pub fn description_icon<F>(on_click: F) -> impl IntoView
where
    F: Fn(MouseEvent) + 'static,
{
    view! {
        <div
            class="w-4 h-2 flex items-center justify-center bg-gray-500 hover:bg-gray-700 rounded-[4px] cursor-pointer group"
            on:click=on_click
        >
            <div class="flex items-center gap-0.5">
                <div class="w-0.5 h-0.5 bg-white rounded-full"></div>
                <div class="w-0.5 h-0.5 bg-white rounded-full"></div>
                <div class="w-0.5 h-0.5 bg-white rounded-full"></div>
            </div>
        </div>
    }
}

#[component]
pub fn InfoDescription(
    #[prop(into)] description: String,
    #[prop(into)] change_reason: String,
) -> impl IntoView {
    let (show_modal, set_show_modal) = create_signal(false);
    let stored_description = store_value(description);
    let stored_change_reason = store_value(change_reason);

    view! {
        <div class="inline-flex items-cente">
            <DescriptionIcon on_click=move |_| set_show_modal.set(true) />

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
        </div>
    }
}
