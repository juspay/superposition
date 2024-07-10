use leptos::*;
use serde_json::{Map, Value};

use crate::components::dropdown::{Dropdown, DropdownBtnType, DropdownDirection};

#[component]
pub fn enum_dropdown(
    schema: Map<String, Value>,
    config_value: String,
    handle_change: Callback<String, ()>,
    #[prop(default = String::new())] class: String,
    #[prop(default = false)] disabled: bool,
) -> impl IntoView {
    let (value, set_value) = create_signal(config_value.replace("\\", ""));
    let (selected_enum, set_selected_enum) = create_signal(String::from("Choose Enum"));

    let enum_array: Vec<String> = schema
        .get("enum")
        .and_then(|v| v.as_array())
        .map(|arr| arr.iter().filter_map(|v| v.as_str().map(|s| s.to_string())).collect())
        .unwrap_or_default();

    create_effect({
        logging::log!("create-effect");
        let value = value.clone();
        let enum_array = enum_array.clone();
        move |_|{
            if !value.get().is_empty() && enum_array.contains(&value.get()) {
                set_selected_enum.set(value.get());
            }
    }});

    logging::log!("<<>> {:?} {:?} {:?}", value.get(), schema.get("enum"), selected_enum.get());

    view! {
        <div class=format!("form-control {}", class)>
        { move || {
            view!{
            <Dropdown
                disabled=disabled
                dropdown_width="w-100"
                dropdown_icon="".to_string()
                dropdown_text=(move || {selected_enum.get()})()
                dropdown_direction=DropdownDirection::Down
                dropdown_btn_type=DropdownBtnType::Select
                dropdown_options={enum_array.clone()}
                on_select=Callback::new(move |selected: String| {
                    handle_change.call(selected.clone());
                    set_value.set(selected.clone());
                    set_selected_enum.set(selected.clone());
                    logging::log!("{:?}", selected_enum.get());
                })
            />
            }
        }}
        </div>
    }
}

#[component]
pub fn boolean_toggle(
    config_value: String,
    update_value: Callback<String, ()>,
    #[prop(default = String::new())] class: String,
    #[prop(default = false)] disabled: bool,
) -> impl IntoView {
    let (flag, set_flag) = create_signal(config_value.parse::<bool>().unwrap_or(false));
    logging::log!("<<>>");
    view! {
        <input
            disabled=disabled
            on:click=move |_| {
                set_flag.update(|val| *val = !*val);
                update_value.call((flag.get()).to_string());
            }

            type="checkbox"
            class=format!("toggle toggle-[#ffffff] flex items-center {class}")
            checked=flag
        />
    }
}
