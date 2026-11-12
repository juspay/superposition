use leptos::*;

use crate::components::{form::label::Label, safe_text::SafeTextarea};

#[component]
pub fn ChangeForm(
    #[prop(into)] title: String,
    #[prop(into)] placeholder: String,
    #[prop(into, default = String::new())] class: String,
    #[prop(into, default = "textarea textarea-bordered w-full".to_string())]
    textarea_class: String,
    #[prop(into)] value: String,
    #[prop(into)] on_change: Callback<String, ()>,
    #[prop(default = false)] disabled: bool,
) -> impl IntoView {
    view! {
        <div class=format!("form-control flex-1 max-w-md min-w-[300px] {class}")>
            <Label title />
            <SafeTextarea
                disabled=disabled
                placeholder=placeholder
                class=textarea_class
                value=value
                on_change=on_change
            />
        </div>
    }
}
