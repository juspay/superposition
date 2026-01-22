use leptos::*;
use leptos_router::use_navigate;

use crate::{
    components::function_form::FunctionEditor,
    types::{OrganisationId, Workspace},
};

#[component]
pub fn create_function_view() -> impl IntoView {
    let workspace = use_context::<Signal<Workspace>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();

    view! {
        <div class="flex flex-col gap-10">
            <h1 class="text-2xl font-extrabold">"Create Function"</h1>
            <FunctionEditor
                edit=false
                handle_submit=move |function_name| {
                    let redirect_url = format!(
                        "/admin/{}/{}/function/{}",
                        org.get().0,
                        workspace.get().0,
                        function_name,
                    );
                    let navigate = use_navigate();
                    navigate(&redirect_url, Default::default())
                }
                on_cancel=move |_| ()
            />
        </div>
    }
    .into_view()
}
