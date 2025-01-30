use leptos::*;
use leptos_router::{use_navigate, use_query_map};

use crate::{
    components::default_config_form::DefaultConfigForm,
    types::{OrganisationId, Tenant},
    utils::{add_prefix, use_service_prefix},
};

#[component]
pub fn new_default_config() -> impl IntoView {
    let navigate = use_navigate();
    let query_map = use_query_map();
    let service_prefix = use_service_prefix();

    let tenant_s = use_context::<Signal<Tenant>>().unwrap();
    let org_s = use_context::<Signal<OrganisationId>>().unwrap();

    let prefix = query_map.get().get("prefix").cloned();

    let on_submit = move || {
        let url_prefix = add_prefix("", &service_prefix);
        let tenant = tenant_s.get().0;
        let org = org_s.get().0;
        navigate(
            format!("{url_prefix}/admin/{org}/{tenant}/default-config").as_str(),
            Default::default(),
        );
    };

    logging::log!("PREFIX: {:?}", prefix);

    view! {
        <DefaultConfigForm
            prefix
            handle_submit=on_submit
            class="h-main-content p-8 rounded-2xl border bg-white overflow-y-auto"
            width="w-2/3"
        />
    }
}
