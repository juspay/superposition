use leptos::*;
use web_sys::MouseEvent;

#[component]
pub fn button<F: Fn(MouseEvent) + 'static>(
    text: String,
    on_click: F,
    #[prop(default = String::new())] class: String,
    #[prop(default = String::new())] id: String,
) -> impl IntoView {
    view! {
        <button
            class=format!(
                "btn-purple font-medium rounded-lg text-sm px-5 py-2.5 text-center me-2 mb-2 {class}",
            )

            id=id
            on:click=on_click
        >
            {text}
            <i class="ri-edit-2-line ml-2"></i>
        </button>
    }
}
