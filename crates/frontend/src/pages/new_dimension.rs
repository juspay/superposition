use leptos::*;

use crate::components::dimension_form::DimensionForm;

#[component]
pub fn new_dimension() -> impl IntoView {
    view! {
        <DimensionForm
            handle_submit=move || {}
            class="w-1/2 h-main-content p-8 rounded-2xl border bg-white overflow-y-auto"
        />
    }
}
