use leptos::*;

use crate::utils::get_element_by_id;
use web_sys::MouseEvent;

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

pub enum DrawerButtonStyle {
    Fill,
    Outline,
}

#[component]
pub fn drawer_btn(
    #[prop(into)] drawer_id: String,
    children: Children,
    #[prop(into, default = Callback::new(|_| {}))] on_click: Callback<MouseEvent, ()>,
    #[prop(into, default = String::new())] class: String,
    #[prop(default = DrawerButtonStyle::Fill)] style: DrawerButtonStyle,
) -> impl IntoView {
    let open_drawer_id = drawer_id.clone();
    let style = match style {
        DrawerButtonStyle::Fill => "btn-purple drawer-button px-5 py-2.5 font-medium rounded-lg text-sm text-center",
        DrawerButtonStyle::Outline => "btn btn-purple-outline w-[8rem] cursor-pointer",
    }.to_string();

    view! {
        <button
            class=format!("{style} {class}")
            id=format!("{}-btn", drawer_id)
            on:click=move |e| {
                open_drawer(&open_drawer_id);
                on_click.call(e);
            }
        >

            {children()}
        </button>
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

            <div class="drawer-side drawer-zindex w-full">
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
