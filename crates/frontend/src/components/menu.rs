use leptos::*;

use super::dropdown::utils::DropdownOption;

#[component]
pub fn SelectionMenu<T>(
    options: Vec<T>,
    on_select: Callback<T, ()>,
    #[prop(into, default = String::from("w-96"))] menu_width: String,
    #[prop(into, default = String::new())] class: String,
    #[prop(into, default = String::new())] id: String,
) -> impl IntoView
where
    T: DropdownOption + Clone + 'static,
{
    view! {
        <div class="relative">
            <ul
                id=id
                class=format!(
                    "absolute z-[1] menu dropdown-content flex-nowrap p-2 shadow bg-base-100 rounded-box max-h-96 overflow-y-scroll overflow-x-hidden {class} {menu_width}",
                )
            >
                <For
                    each=move || options.clone()
                    key=|option: &T| option.key()
                    children=move |option: T| {
                        let label = option.label();
                        view! {
                            <li
                                class="w-full"
                                on:click=move |_| {
                                    let selected_option = option.clone();
                                    on_select.call(selected_option);
                                }
                            >

                                <a class="w-full word-break-break">{label.to_string()}</a>
                            </li>
                        }
                    }
                />
            </ul>
        </div>
    }
}
