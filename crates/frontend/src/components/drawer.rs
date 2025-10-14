use leptos::*;
use web_sys::MouseEvent;

use crate::utils::get_element_by_id;

use super::button::{Button, ButtonStyle};

pub fn open_drawer(id: &str) {
    match get_element_by_id::<web_sys::HtmlInputElement>(id) {
        Some(ele) => ele.set_checked(true),
        None => {
            logging::log!("{} drawer checkbox not found", id);
        }
    };
}

pub fn close_drawer(id: &str) {
    match get_element_by_id::<web_sys::HtmlInputElement>(id) {
        Some(ele) => ele.set_checked(false),
        None => {
            logging::log!("{} drawer checkbox not found", id);
        }
    };
}

#[component]
pub fn drawer_btn(
    #[prop(into)] drawer_id: String,
    #[prop(into)] text: String,
    #[prop(into)] icon_class: String,
    #[prop(into, default = Callback::new(|_| {}))] on_click: Callback<MouseEvent, ()>,
    #[prop(into, default = String::new())] class: String,
    #[prop(default = ButtonStyle::Fill)] style: ButtonStyle,
) -> impl IntoView {
    let open_drawer_id = drawer_id.clone();

    view! {
        <Button
            style
            class
            id=format!("{drawer_id}-btn")
            on_click=move |e| {
                open_drawer(&open_drawer_id);
                on_click.call(e);
            }
            text
            icon_class
        />
    }
}

#[component]
pub fn drawer<NF>(
    #[prop(into)] id: String,
    children: Children,
    #[prop(into, default = String::new())] header: String,
    #[prop(default = "max-w-[610px] min-w-[560px] w-[35vw]")] width_class: &'static str,
    handle_close: NF,
) -> impl IntoView
where
    NF: Fn() + 'static + Clone,
{
    let close_drawer = move |_| {
        handle_close();
    };

    view! {
        <div class="h-0 w-0 drawer drawer-end">
            <input id=id.clone() type="checkbox" class="drawer-toggle" />

            <div class="drawer-side z-[9999999] w-full">
                <label for=id.clone() class="drawer-overlay" on:click=close_drawer.clone()></label>
                <div class=format!(
                    "h-full {width_class} flex flex-col bg-base-100 overflow-x-hidden",
                )>
                    <div class="px-4 py-4 flex justify-between items-center">
                        <h3 class="text-lg font-bold">{header}</h3>
                        <button class="btn btn-sm btn-circle btn-ghost" on:click=close_drawer>
                            <i class="ri-close-line"></i>
                        </button>
                    </div>
                    <div class="divider mt-0"></div>
                    <div class="p-4 relative overflow-y-scroll flex-1">{children()}</div>
                </div>
            </div>
        </div>
    }
}

#[component]
pub fn portal_drawer(
    children: ChildrenFn,
    #[prop(into)] title: String,
    #[prop(default = "max-w-[610px] min-w-[560px] w-[35vw]")] width_class: &'static str,
    #[prop(into)] handle_close: Callback<()>,
) -> impl IntoView {
    view! {
        <Portal>
            <div class="h-0 w-0 z-[1500] drawer drawer-end">
                <input type="checkbox" class="drawer-toggle" checked=true />
                <div class="drawer-side z-[9999999] w-full">
                    <label class="drawer-overlay" on:click=move |_| { handle_close.call(()) } />
                    <div class=format!(
                        "h-full {width_class} flex flex-col bg-base-100 overflow-x-hidden",
                    )>
                        <div class="px-4 py-4 flex justify-between items-center">
                            <h3 class="text-lg font-bold">{title.clone()}</h3>
                            <button
                                class="btn btn-sm btn-circle btn-ghost"
                                on:click=move |_| { handle_close.call(()) }
                            >
                                <i class="ri-close-line" />
                            </button>
                        </div>
                        <div class="divider m-0" />
                        <div class="p-4 relative overflow-y-scroll flex-1 bg-white">
                            {children()}
                        </div>
                    </div>
                </div>
            </div>
        </Portal>
    }
}
