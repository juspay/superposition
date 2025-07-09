use leptos::*;
use leptos_router::A;
use web_sys::MouseEvent;

#[component]
pub fn button<F: Fn(MouseEvent) + 'static>(
    #[prop(into)] text: String,
    on_click: F,
    #[prop(into, default = String::new())] class: String,
    #[prop(into, default = String::new())] id: String,
    #[prop(into, default = String::from("ri-edit-2-line"))] icon_class: String,
    #[prop(default = false)] loading: bool,
) -> impl IntoView {
    let mut button_class = format!(
        "btn-purple px-5 py-2.5 flex justify-center items-center gap-2 font-medium text-sm text-center rounded-lg {class}"
    );
    if loading {
        button_class += "hover:cursor-not-allowed";
    }
    view! {
        <button class=button_class id=id on:click=on_click disabled=loading>
            {if loading {
                view! { <span class="loading loading-dots loading-sm" /> }.into_view()
            } else {
                view! {
                    {text}
                    <i class=icon_class />
                }
                    .into_view()
            }}

        </button>
    }
}

#[component]
pub fn button_anchor(
    #[prop(into)] text: String,
    #[prop(into)] href: String,
    #[prop(into, default = String::new())] class: String,
    #[prop(into, default = String::new())] id: String,
    #[prop(into, default = String::from("ri-edit-2-line"))] icon_class: String,
    #[prop(default = false)] loading: bool,
) -> impl IntoView {
    let mut button_class = format!("btn-purple px-5 py-2.5 flex justify-center items-center gap-2 font-medium text-sm text-center rounded-lg {class}");

    if loading {
        button_class += "hover:cursor-not-allowed";
        view! {
            <div class=button_class id=id>
                <span class="loading loading-dots loading-sm" />
            </div>
        }
        .into_view()
    } else {
        view! {
            <A class=button_class id=id href=href>
                {text}
                <i class=icon_class />
            </A>
        }
        .into_view()
    }
}
