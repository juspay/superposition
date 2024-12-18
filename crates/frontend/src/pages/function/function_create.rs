use leptos::*;
use leptos_router::use_navigate;
use serde::{Deserialize, Serialize};
use superposition_types::database::models::cac::Function;

use crate::components::function_form::FunctionEditor;

#[derive(Serialize, Deserialize, Clone, Debug)]
struct CombinedResource {
    function: Option<Function>,
}

#[component]
pub fn create_function_view() -> impl IntoView {
    let tenant_rs = use_context::<ReadSignal<String>>().unwrap();
    view! {
        <div>

            <div class="mt-20 mb-20">Create Function</div>
            <FunctionEditor
                edit=false
                handle_submit=move || {
                    let tenant = tenant_rs.get();
                    let redirect_url = format!("admin/{tenant}/function");
                    let navigate = use_navigate();
                    navigate(redirect_url.as_str(), Default::default())
                }
            />

        </div>
    }
    .into_view()
}
