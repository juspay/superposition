use leptos::*;

#[component]
pub fn response_template_form(
    #[prop(into)] handle_submit: Callback<()>,
) -> impl IntoView {
    view! {
        <div>
            <h2>"Response Template Form"</h2>
        </div>
    }
}
