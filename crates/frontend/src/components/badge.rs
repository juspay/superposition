use std::{collections::HashSet, fmt::Display, hash::Hash};

use leptos::*;
use leptos_router::A;
use strum::IntoEnumIterator;

use crate::components::form::label::Label;

use super::dropdown::utils::DropdownOption;

#[component]
pub fn badge<T>(
    #[prop(into)] options: Signal<Vec<T>>,
    #[prop(into, optional)] handle_remove: Option<Callback<T, ()>>,
    #[prop(into, default = String::new())] class: String,
    #[prop(into, optional)] href_fn: Option<Callback<T, String>>,
) -> impl IntoView
where
    T: DropdownOption + Clone + 'static,
{
    view! {
        <Show when=move || { !options.get().is_empty() }>
            <div class=format!("flex flex-wrap gap-2 break-words w-[inherit] {}", class)>
                <For
                    each=move || { options.get() }
                    key=move |option| { option.key() }
                    children=move |option| {
                        let label = option.label();
                        view! {
                            <div class="flex justify-between badge badge-primary badge-outline">
                                {if let Some(href_fn) = href_fn {
                                    view! { <A href=href_fn.call(option.clone())>{label}</A> }
                                        .into_view()
                                } else {
                                    view! { {label} }.into_view()
                                }}
                                {if let Some(on_remove) = handle_remove {
                                    view! {
                                        <button
                                            class="btn btn-xs btn-circle btn-ghost"
                                            on:click=move |_| on_remove.call(option.clone())
                                        >
                                            <i class="ri-close-line"></i>
                                        </button>
                                    }
                                        .into_view()
                                } else {
                                    ().into_view()
                                }}
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
    #[prop(into)] text: String,
    #[prop(into, default = String::new())] icon_class: String,
    #[prop(default = true)] deletable: bool,
    #[prop(into, default = Callback::new(move |_| () ))] on_delete: Callback<()>,
) -> impl IntoView {
    view! {
        <div class="badge badge-sm !h-fit py-[1px] bg-gray-200 flex gap-1">
            {if !icon_class.is_empty() {
                view! { <i class=icon_class.clone() /> }.into_view()
            } else {
                ().into_view()
            }} {text} <Show when=move || { deletable }>
                <i class="ri-close-line cursor-pointer" on:click=move |_| { on_delete.call(()) } />
            </Show>
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

#[component]
pub fn glassy_pills<T: IntoEnumIterator + Display + Eq + Hash + Copy + 'static>(
    #[prop(into)] selected: Signal<Vec<T>>,
    #[prop(into)] title: String,
    #[prop(into)] on_click: Callback<Vec<T>, ()>,
) -> impl IntoView {
    let get_updated_items = move |checked: bool, item: T| {
        let mut old_group_vector: HashSet<T> = HashSet::from_iter(selected.get());

        if checked {
            old_group_vector.insert(item);
        } else {
            old_group_vector.remove(&item);
        }
        Vec::from_iter(old_group_vector)
    };

    view! {
        <div class="form-control w-full">
            <Label title />
            <div class="flex flex-row flex-wrap justify-start gap-5">
                {T::iter()
                    .map(|item| {
                        let label = item.to_string();
                        let input_id = format!("{label}-checkbox");

                        view! {
                            <div>
                                <input
                                    type="checkbox"
                                    id=&input_id
                                    class="peer hidden"
                                    checked=selected.get().iter().any(|it| *it == item)
                                    on:change=move |event| {
                                        let checked = event_target_checked(&event);
                                        on_click.call(get_updated_items(checked, item));
                                    }
                                />
                                <label
                                    for=&input_id
                                    class="badge h-[30px] px-6 py-2 peer-checked:bg-purple-500 peer-checked:border-purple-500 peer-checked:text-white cursor-pointer transition duration-300 ease-in-out peer-checked:shadow-purple-500 peer-checked:shadow-md shadow-inner shadow-slate-500"
                                >
                                    {label}
                                </label>
                            </div>
                        }
                    })
                    .collect_view()}
            </div>
        </div>
    }
}
