use leptos::*;
use web_sys::MouseEvent;

#[component]
pub fn button<F: Fn(MouseEvent) + 'static>(
    text: String,
    on_click: F,
    #[prop(default = String::new())] class: String,
    #[prop(default = String::new())] id: String,
    #[prop(default = false)] loading: bool,
) -> impl IntoView {
    let mut button_class = format!("btn-purple font-medium rounded-lg text-sm px-5 py-2.5 text-center me-2 mb-2 {class}");
    if loading {
        button_class = button_class + "hover:cursor-not-allowed";
    }
    view! {
        <button class=button_class id=id on:click=on_click disabled=loading>
            {if loading {
                view! {
                    <>
                        <span class="loading loading-dots loading-sm"></span>
                    </>
                }
            } else {
                view! { <>{text} <i class="ri-edit-2-line ml-2"></i></> }
            }}

        </button>
    }
}
