use leptos::*;

use crate::components::form::label::Label;

#[component]
pub fn change_form(
    #[prop(into)] title: String,
    #[prop(into)] placeholder: String,
    #[prop(into, default = String::new())] class: String,
    #[prop(into, default = "textarea textarea-bordered w-full".to_string())]
    textarea_class: String,
    #[prop(into)] value: String,
    #[prop(into)] on_change: Callback<String, ()>,
    #[prop(default = false)] disabled: bool,
) -> impl IntoView {
    let handle_change = move |ev| {
        let new_value = event_target_value(&ev);
        on_change.call(new_value.clone());
    };
    view! {
        <div class=format!("form-control flex-1 max-w-md min-w-[300px] {class}")>
            <Label title />
            <textarea
                type="text"
                disabled=disabled
                placeholder=placeholder
                class=textarea_class
                on:change=handle_change
            >
                {value}
            </textarea>
        </div>
    }
}
