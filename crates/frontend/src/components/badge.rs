use leptos::*;

use super::dropdown::utils::DropdownOption;

#[component]
pub fn badge<T>(
    options: ReadSignal<Vec<T>>,
    #[prop(default=Callback::new(|_| {}))] handle_remove: Callback<T, ()>,
    #[prop(default = false)] deletable: bool,
) -> impl IntoView
where
    T: DropdownOption + Clone + 'static,
{
    let deletable = StoredValue::new(deletable);

    view! {
        <Show when=move || { !options.get().is_empty() }>
            <div class="flex flex-wrap gap-2 break-words w-[inherit]">
                <For
                    each=move || { options.get() }
                    key=move |option| { option.key() }
                    children=move |option| {
                        let option = StoredValue::new(option.clone());
                        let label = option.get_value().label();
                        view! {
                            <div class="flex justify-between badge badge-primary badge-outline">
                                {label.to_string()}
                                <Show when=move || { deletable.get_value() }>
                                    <button
                                        class="btn btn-xs btn-circle btn-ghost"
                                        on:click=move |_| { handle_remove.call(option.get_value()) }
                                    >
                                        <i class="ri-close-line"></i>
                                    </button>
                                </Show>
                            </div>
                        }
                    }
                />
            </div>
        </Show>
    }
}
