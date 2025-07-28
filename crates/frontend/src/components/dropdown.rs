pub mod utils;

use leptos::*;

use crate::components::badge::Badge;

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
    #[prop(into)] on_select: Callback<T, ()>,
    #[prop(into, default = false)] multi_select: bool,
    #[prop(into, default = Vec::new())] selected: Vec<T>,
    #[prop(into, default = Callback::from(|_| {}))] on_remove: Callback<T, ()>,
    #[prop(into)] dropdown_text: String,
    #[prop(into, default = String::new())] dropdown_icon: String,
    #[prop(default = DropdownDirection::Right)] dropdown_direction: DropdownDirection,
    #[prop(default = DropdownBtnType::Outline)] dropdown_btn_type: DropdownBtnType,
    #[prop(into, default = String::from("w-96"))] dropdown_width: String,
    #[prop(default = false)] disabled: bool,
    #[prop(default = true)] searchable: bool,
    #[prop(into, default = String::new())] name: String,
    #[prop(into, default = String::new())] class: String,
    #[prop(into, default = String::new())] id: String,
) -> impl IntoView
where
    T: DropdownOption + Clone + PartialEq + 'static,
{
    let all_options = StoredValue::new(dropdown_options.clone());
    let selected_text_rws = RwSignal::new(dropdown_text);
    let (selected_rs, selected_ws) = create_signal(selected);
    let (search_term, set_search_term) = create_signal(String::new());
    let dropdown_options = Signal::derive(move || {
        let term = search_term.get().to_lowercase();
        all_options
            .get_value()
            .into_iter()
            .filter(|option| {
                !selected_rs.get().contains(option)
                    && option.label().to_lowercase().contains(&term)
            })
            .collect::<Vec<T>>()
    });

    let btn_class = match dropdown_btn_type {
        DropdownBtnType::Outline => "btn btn-sm text-xs m-1 w-fit btn-purple-outline",
        DropdownBtnType::Link => "btn btn-sm text-xs m-1 w-full btn-purple-link",
        DropdownBtnType::Fill => "btn btn-sm text-xs m-1 w-[28rem] btn-purple-fill",
        DropdownBtnType::Select => "select select-bordered w-[28rem] items-center",
    };

    let node_ref = create_node_ref::<html::Input>();
    let dropdown_node_ref = create_node_ref::<html::Ul>();

    view! {
        <div class="flex flex-col gap-4">
            <div
                id=id
                class="w-fit dropdown"
                class=("disable-click", disabled)
                class=("dropdown-right", dropdown_direction == DropdownDirection::Right)
                class=("dropdown-left", dropdown_direction == DropdownDirection::Left)
                class=("dropdown-top", dropdown_direction == DropdownDirection::Top)
                class=("dropdown-down", dropdown_direction == DropdownDirection::Down)
            >
                <label
                    tabindex="0"
                    class=format!("{} {}", class, btn_class)
                    on:click:undelegated=move |_| {
                        if let Some(element) = node_ref.get() {
                            let _ = element.focus();
                        }
                    }
                >

                    <i class=format!("{dropdown_icon}")></i>
                    {move || { selected_text_rws.get() }}
                </label>
                <ul
                    ref_=dropdown_node_ref
                    tabindex="0"
                    class=format!(
                        "{dropdown_width} dropdown-content z-[50] menu flex-nowrap p-2 shadow bg-base-100 rounded-box max-h-96 overflow-y-scroll overflow-x-hidden",
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
                                        ref_=node_ref
                                        name=name.clone()
                                        value=search_term.get_untracked()
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
                                        if multi_select {
                                            selected_ws
                                                .update(|selected| {
                                                    selected.push(selected_option.clone());
                                                });
                                        } else {
                                            selected_text_rws.set(selected_option.label());
                                        }
                                        on_select.call(selected_option);
                                        if let Some(element) = dropdown_node_ref.get() {
                                            let _ = element.blur();
                                        }
                                    }
                                >

                                    <a class="w-full word-break-break">{label.to_string()}</a>
                                </li>
                            }
                        }
                    />

                </ul>
            </div>
            <Badge
                options=selected_rs
                handle_remove=move |option: T| {
                    let selected_option = option.clone();
                    selected_ws
                        .update(|selected| {
                            selected.retain(|x| x.key() != selected_option.key());
                        });
                    on_remove.call(selected_option);
                }
            />
        </div>
    }
}
