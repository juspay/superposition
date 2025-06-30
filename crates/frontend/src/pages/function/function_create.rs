use leptos::*;
use leptos_router::use_navigate;
use serde::{Deserialize, Serialize};
use superposition_types::database::models::cac::Function;

use crate::{
    components::function_form::FunctionEditor,
    types::{OrganisationId, Tenant},
};

#[derive(Serialize, Deserialize, Clone, Debug)]
struct CombinedResource {
    function: Option<Function>,
}

#[component]
pub fn create_function_view() -> impl IntoView {
    let workspace = use_context::<Signal<Tenant>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();
    view! {
        <div>

            <div class="mt-20 mb-20">Create Function</div>
            <FunctionEditor
                edit=false
                handle_submit=move || {
                    let tenant = workspace.get().0;
                    let org = org.get().0;
                    let redirect_url = format!("admin/{org}/{tenant}/function");
                    let navigate = use_navigate();
                    navigate(redirect_url.as_str(), Default::default())
                }
            />

        </div>
    }
    .into_view()
}
