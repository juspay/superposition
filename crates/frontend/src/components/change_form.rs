use leptos::*;

#[component]
pub fn change_form(
    title: String,
    placeholder: String,
    #[prop(default = "textarea textarea-bordered w-full max-w-md".to_string())]
    class: String,
    value: String,
    on_change: Callback<String, ()>,
) -> impl IntoView {
    let handle_change = move |ev| {
        let new_value = event_target_value(&ev);
        on_change.call(new_value.clone());
    };
    view! {
        <div class="form-control">
            <label class="label">
                <span class="label-text">{title}</span>
            </label>
            <textarea type="text" placeholder=placeholder class=class on:change=handle_change>
                {value}
            </textarea>
        </div>
    }
}
