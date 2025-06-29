use std::fmt::Display;

use leptos::*;

use super::dropdown::utils::DropdownOption;

#[component]
pub fn badge<T>(
    #[prop(into)] options: Signal<Vec<T>>,
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
                        let label = option.with_value(|o| o.label());
                        view! {
                            <div class="flex justify-between badge badge-primary badge-outline">
                                {label.to_string()} <Show when=move || { deletable.get_value() }>
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

#[component]
pub fn gray_pill(
    text: String,
    #[prop(into, default = Callback::new(move |_| () ))] on_delete: Callback<()>,
) -> impl IntoView {
    view! {
        <div class="badge badge-sm !h-fit py-[1px] bg-gray-200 flex gap-1">
            {text}
            <i class="ri-close-line cursor-pointer" on:click=move |_| { on_delete.call(()) } />
        </div>
    }
}

#[component]
pub fn list_pills<T>(
    #[prop(into)] label: String,
    #[prop(into)] items: Vec<T>,
    #[prop(into)] on_delete: Callback<usize>,
) -> impl IntoView
where
    T: Display,
{
    if items.is_empty() {
        return ().into_view();
    }

    view! {
        <div class="flex gap-2 items-center">
            <div class="min-w-fit flex items-center gap-[2px] text-xs">
                {label} <span class="text-[10px] text-slate-400">"(any of)"</span>
            </div>
            <div class="flex gap-[2px] items-center flex-wrap">
                {items
                    .iter()
                    .enumerate()
                    .map(|(idx, item)| {
                        view! {
                            <GrayPill
                                text=item.to_string()
                                on_delete=move |_| on_delete.call(idx)
                            />
                        }
                    })
                    .collect_view()}
            </div>
        </div>
    }
    .into_view()
}
