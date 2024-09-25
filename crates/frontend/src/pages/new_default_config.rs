use leptos::*;
use leptos_router::use_query_map;

use crate::components::default_config_form::DefaultConfigForm;

#[component]
pub fn new_default_config() -> impl IntoView {
    let query_map = use_query_map();
    view! {
        {move || {
            let prefix = query_map.get().get("prefix").cloned();
            view! {
                <DefaultConfigForm
                    prefix
                    handle_submit=move || {}
                    class="w-2/3 h-main-content p-8 rounded-2xl border bg-white overflow-y-auto"
                />
            }
        }}
    }
}
