pub mod utils;

use leptos::*;

use self::utils::DropdownOption;

#[derive(PartialEq)]
pub enum DropdownBtnType {
    Outline,
    Link,
    Fill,
    Select,
}

#[derive(PartialEq, Copy, Clone)]
pub enum DropdownDirection {
    Right,
    Left,
    Top,
    Down,
}

#[component]
pub fn dropdown<T>(
    dropdown_options: Vec<T>,
    on_select: Callback<T, ()>,
    #[prop(into)] dropdown_text: String,
    #[prop(into, default = String::new())] dropdown_icon: String,
    #[prop(default = DropdownDirection::Right)] dropdown_direction: DropdownDirection,
    #[prop(default = DropdownBtnType::Outline)] dropdown_btn_type: DropdownBtnType,
    #[prop(into, default = String::from("w-96"))] dropdown_width: String,
    #[prop(default = false)] disabled: bool,
    #[prop(default = true)] searchable: bool,
    #[prop(into, default = String::new())] name: String,
    #[prop(into, default = String::new())] class: String
) -> impl IntoView
where
    T: DropdownOption + Clone + 'static,
{
    let all_options = StoredValue::new(dropdown_options.clone());
    let (search_term, set_search_term) = create_signal(String::new());
    let dropdown_options = Signal::derive(move || {
        let term = search_term.get();
        all_options
            .get_value()
            .into_iter()
            .filter(|option| option.label().contains(&term))
            .collect::<Vec<T>>()
    });

    let btn_class = match dropdown_btn_type {
        DropdownBtnType::Outline => "btn btn-sm text-xs m-1 w-full btn-purple-outline",
        DropdownBtnType::Link => "btn btn-sm text-xs m-1 w-full btn-purple-link",
        DropdownBtnType::Fill => "btn btn-sm text-xs m-1 w-full btn-purple-fill",
        DropdownBtnType::Select => "select select-bordered w-[28rem] items-center",
    };

    view! {
        <div
            class="dropdown"
            class=("disable-click", disabled)
            class=("dropdown-right", dropdown_direction == DropdownDirection::Right)
            class=("dropdown-left", dropdown_direction == DropdownDirection::Left)
            class=("dropdown-top", dropdown_direction == DropdownDirection::Top)
            class=("dropdown-down", dropdown_direction == DropdownDirection::Down)
        >
            <label tabindex="0" class=format!("{} {}", class, btn_class)>
                <i class=format!("{dropdown_icon}")></i>
                {dropdown_text}
            </label>
            <ul
                tabindex="0"
                class=format!(
                    "{dropdown_width} dropdown-content z-[1] menu flex-nowrap p-2 shadow bg-base-100 rounded-box max-h-96 overflow-y-scroll overflow-x-hidden",
                )
            >

                {if searchable {
                    view! {
                        <div class="mb-3">
                            <label class="input input-bordered flex items-center gap-2 h-10">
                                <i class="ri-search-line"></i>
                                <input
                                    type="text"
                                    class="grow"
                                    placeholder="Search"
                                    name=name
                                    value=String::new()
                                    on:input=move |event| {
                                        set_search_term.set(event_target_value(&event));
                                    }
                                />

                            </label>
                        </div>
                    }
                        .into_view()
                } else {
                    view! {}.into_view()
                }}

                <For
                    each=move || dropdown_options.get()
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
