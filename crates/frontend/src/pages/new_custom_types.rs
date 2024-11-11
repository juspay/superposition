use leptos::*;

use crate::components::type_template_form::TypeTemplateForm;

#[component]
pub fn new_custom_types() -> impl IntoView {
    view! {
        <TypeTemplateForm
            class="mx-auto w-1/2 h-main-content p-8 rounded-2xl border bg-white overflow-y-auto"
            handle_submit=move || {}
        />
    }
}
