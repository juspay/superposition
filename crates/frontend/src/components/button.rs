use leptos::*;
use leptos_router::A;
use web_sys::MouseEvent;

use crate::providers::csr_provider::use_client_side_ready;

pub enum ButtonStyle {
    Fill,
    Outline,
}

#[component]
pub fn Button(
    #[prop(into)] text: String,
    #[prop(into)] on_click: Callback<MouseEvent, ()>,
    #[prop(into, default = String::new())] class: String,
    #[prop(into, default = String::new())] id: String,
    #[prop(into, default = String::from("ri-edit-2-line"))] icon_class: String,
    #[prop(default = false)] loading: bool,
    #[prop(default = ButtonStyle::Fill)] style: ButtonStyle,
    #[prop(into, optional)] force_style: Option<String>,
) -> impl IntoView {
    let client_side_ready = use_client_side_ready();
    let common_style =
        "flex justify-center items-center gap-2 font-medium text-sm text-center";
    let style = force_style.unwrap_or_else(|| {
        match style {
            ButtonStyle::Fill => "btn-purple px-5 py-2.5 rounded-lg",
            ButtonStyle::Outline => "btn btn-purple-outline",
        }
        .to_string()
    });

    move || {
        let loading = loading || !*client_side_ready.get();
        let loading_class = if loading {
            "hover:cursor-not-allowed"
        } else {
            ""
        };

        view! {
            <button
                class=format!("{common_style} {style} {class} {loading_class}")
                id=id.clone()
                on:click=move |e| on_click.call(e)
                disabled=loading
            >
                {if loading {
                    view! { <span class="loading loading-dots loading-sm" /> }.into_view()
                } else {
                    view! {
                        {text.clone()}
                        <i class=icon_class.clone() />
                    }
                        .into_view()
                }}
            </button>
        }
    }
}

#[component]
pub fn ButtonAnchor(
    #[prop(into)] text: String,
    #[prop(into)] href: String,
    #[prop(into, default = String::new())] class: String,
    #[prop(into, default = String::new())] id: String,
    #[prop(into, default = String::from("ri-edit-2-line"))] icon_class: String,
    #[prop(default = false)] loading: bool,
    #[prop(default = ButtonStyle::Fill)] style: ButtonStyle,
    #[prop(into, optional)] force_style: Option<String>,
) -> impl IntoView {
    let common_style =
        "flex justify-center items-center gap-2 font-medium text-sm text-center";
    let style = force_style.unwrap_or_else(|| {
        match style {
            ButtonStyle::Fill => "btn-purple px-5 py-2.5 rounded-lg",
            ButtonStyle::Outline => "btn btn-purple-outline",
        }
        .to_string()
    });

    if loading {
        view! {
            <div class=format!("{common_style} {style} {class} hover:cursor-not-allowed") id=id>
                <span class="loading loading-dots loading-sm" />
            </div>
        }
        .into_view()
    } else {
        view! {
            <A class=format!("{common_style} {style} {class}") id=id href=href>
                {text}
                <i class=icon_class />
            </A>
        }
        .into_view()
    }
}
