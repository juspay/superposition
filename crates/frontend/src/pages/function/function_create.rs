use leptos::*;
use leptos_router::use_navigate;
use serde::{Deserialize, Serialize};
use superposition_types::database::models::cac::Function;

use crate::{
    components::function_form::FunctionEditor,
    types::{OrganisationId, Tenant},
    utils::use_url_base,
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
        <div class="p-8 flex flex-col gap-10">
            <h1 class="text-2xl font-extrabold">"Create Function"</h1>
            <FunctionEditor
                edit=false
                handle_submit=move |_| {
                    let base = use_url_base();
                    let redirect_url = format!(
                        "{base}/admin/{}/{}/function",
                        org.get().0,
                        workspace.get().0,
                    );
                    let navigate = use_navigate();
                    navigate(&redirect_url, Default::default())
                }
            />
        </div>
    }
    .into_view()
}
