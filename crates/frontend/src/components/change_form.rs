use leptos::*;

#[component]
pub fn change_form(
    title: String,
    placeholder: String,
    #[prop(default = String::new())] class: String,
    #[prop(default = "textarea textarea-bordered w-full max-w-md".to_string())]
    textarea_class: String,
    value: String,
    #[prop(into)] on_change: Callback<String, ()>,
) -> impl IntoView {
    let handle_change = move |ev| {
        let new_value = event_target_value(&ev);
        on_change.call(new_value.clone());
    };
    view! {
        <div class=format!("form-control {class}")>
            <label class="label">
                <span class="label-text">{title}</span>
            </label>
            <textarea type="text" placeholder=placeholder class=textarea_class on:change=handle_change>
                {value}
            </textarea>
        </div>
    }
}
