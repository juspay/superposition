use leptos::*;

use crate::components::type_template_form::TypeTemplateForm;

#[component]
pub fn new_template_type() -> impl IntoView {
    view! {
        <TypeTemplateForm
            class="w-1/2 h-main-content p-8 rounded-2xl border bg-white overflow-y-auto mx-auto"
            handle_submit=move || {}
        />
    }
}
