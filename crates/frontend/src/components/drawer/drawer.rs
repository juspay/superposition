use leptos::*;

use crate::utils::get_element_by_id;

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
pub fn drawer_btn(drawer_id: String, children: Children) -> impl IntoView {
    let open_drawer_id = drawer_id.clone();
    view! {
        <button
            class=format!(
                "btn-purple font-medium rounded-lg text-sm px-5 py-2.5 text-center me-2 mb-2 drawer-button",
            )

            id=format!("{}-btn", drawer_id.clone())
            on:click=move |_| { open_drawer(&open_drawer_id) }
        >
            {children()}
        </button>
    }
}

#[component]
pub fn drawer<NF>(
    id: String,
    children: Children,
    #[prop(default = "")] header: &'static str,
    #[prop(default = "w-[60vw]")] drawer_width: &'static str,
    handle_close: NF,
) -> impl IntoView
where
    NF: Fn() + 'static + Clone,
{
    let close_drawer = move |_| {
        handle_close();
    };

    view! {
        <div class="drawer drawer-end">
            <input id=id.clone() type="checkbox" class="drawer-toggle"/>

            <div class="drawer-side drawer-zindex w-full">
                <label for=id.clone() class="drawer-overlay" on:click=close_drawer.clone()></label>
                <div class=format!(
                    "min-h-full {drawer_width} bg-base-100 overflow-x-hidden overflow-y-auto",
                )>
                    <div class="px-4 py-4 flex justify-between items-center">
                        <h3 class="text-lg font-bold">{header}</h3>
                        <button class="btn btn-sm btn-circle btn-ghost" on:click=close_drawer>
                            <i class="ri-close-line"></i>
                        </button>
                    </div>
                    <div class="divider mt-0"></div>
                    <div class="p-4">{children()}</div>
                </div>
            </div>
        </div>
    }
}
