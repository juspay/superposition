use leptos::*;
use serde_json::{Map, Value};

use crate::components::dropdown::{Dropdown, DropdownBtnType, DropdownDirection};

#[component]
pub fn enum_dropdown(
    schema: Map<String, Value>,
    config_value: String,
    handle_change: Callback<String, ()>,
) -> impl IntoView {
    let mut enum_array: Vec<String> = vec![];
    if let Some(enum_arr) = schema.get("enum") {
        enum_array = match enum_arr {
            Value::Array(arr) => arr
                .into_iter()
                .filter_map(|v| v.as_str().map(String::from))
                .collect(),
            _ => Vec::new(),
        };
    }
    let selected_enum = if config_value.len() == 0 || !enum_array.contains(&config_value)
    {
        String::from("Choose Enum")
    } else {
        config_value
    };
    view! {
        <div class="form-control w-2/5">
            <div class="mt-2">
                <Dropdown
                    dropdown_width="w-100"
                    dropdown_icon="".to_string()
                    dropdown_text=selected_enum
                    dropdown_direction=DropdownDirection::Down
                    dropdown_btn_type=DropdownBtnType::Select
                    dropdown_options=enum_array
                    on_select=Callback::new(move |selected_enum: String| {
                        handle_change.call(selected_enum);
                    })
                />

            </div>
        </div>
    }
}

#[component]
pub fn boolean_toggle(
    config_value: String,
    update_value: Callback<String, ()>,
) -> impl IntoView {
    let (flag, set_flag) = create_signal(config_value.parse::<bool>().unwrap_or(false));
    update_value.call(flag.get().to_string());
    view! {
        <input
            on:click=move |_| {
                set_flag.update(|val| *val= !*val);
                update_value.call((flag.get()).to_string());
            }
            type="checkbox"
            class="toggle toggle-[#ffffff] flex items-center"
            checked=flag
        />
    }
}
