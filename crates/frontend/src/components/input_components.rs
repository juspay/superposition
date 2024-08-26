use leptos::*;
use serde_json::{Map, Value};

use crate::{
    components::dropdown::{Dropdown, DropdownBtnType, DropdownDirection},
    form_types::SchemaType,
};

#[component]
pub fn enum_dropdown(
    schema: Map<String, Value>,
    config_value: String,
    handle_change: Callback<String, ()>,
    #[prop(default = String::new())] class: String,
    #[prop(default = false)] disabled: bool,
    #[prop(default = "")] name: &'static str,
) -> impl IntoView {
    let (value, set_value) = create_signal(config_value.replace("\\", ""));
    let (selected_enum, set_selected_enum) = create_signal(String::from("Choose Enum"));

    let enum_array: Vec<String> = schema
        .get("enum")
        .and_then(|v| v.as_array())
        .map(|arr| {
            arr.iter()
                .filter_map(|v| v.as_str().map(|s| s.to_string()))
                .collect()
        })
        .unwrap_or_default();

    create_effect({
        let value = value.clone();
        let enum_array = enum_array.clone();
        move |_| {
            if !value.get().is_empty() && enum_array.contains(&value.get()) {
                set_selected_enum.set(value.get());
            }
        }
    });

    view! {
        <div class=format!(
            "form-control {}",
            class,
        )>
            {move || {
                view! {
                    <Dropdown
                        disabled=disabled
                        dropdown_width="w-100"
                        dropdown_icon="".to_string()
                        dropdown_text=(move || { selected_enum.get() })()
                        dropdown_direction=DropdownDirection::Down
                        dropdown_btn_type=DropdownBtnType::Select
                        dropdown_options=enum_array.clone()
                        name
                        on_select=Callback::new(move |selected: String| {
                            handle_change.call(selected.clone());
                            set_value.set(selected.clone());
                            set_selected_enum.set(selected.clone());
                        })
                    />
                }
            }}

        </div>
    }
}

#[component]
pub fn toggle(
    value: bool,
    on_change: Callback<bool, ()>,
    #[prop(default = String::new())] class: String,
    #[prop(default = false)] disabled: bool,
    #[prop(default = "")] name: &'static str,
) -> impl IntoView {
    let (flag, set_flag) = create_signal(value);
    view! {
        <input
            disabled=disabled
            on:click=move |_| {
                set_flag.update(|val| *val = !*val);
                on_change.call(flag.get());
            }

            type="checkbox"
            name=name
            class=format!("toggle toggle-[#ffffff] flex items-center {class}")
            checked=flag
        />
    }
}

/***

pub enum SchemaType {
    Boolean,
    Number,
    String,
    Integer,
    Array,
    Object,
    Null,
}

Indicate just the type name for each override in the form

Boolean - toggle
Number - number field
Integer - number field with pattern and setp as 1
String - text
Array - monaco
Object - monaco

now when there are multiple types for an override

- Boolean, Number: Text
- Boolean, String: Text
- Boolean, Integer: Text
- Boolean, Array: Monaco
- Boolean, Object: Monaco
- Number, Integer: Text
- Number, String: Text
- Number, Array: Monaco
- Number, Object: Monaco
- String, Integer: Text
- String, Array: Monaco
- String, Object: Monaco
- Integer, Array: Monaco
- Integer, Object: Monaco
- Array, Object: Monaco

Rule:
    - is Object or has Object -> Monaco
    - is Array or has Array -> Monaco
    - is String or has String -> Text

    - is Number -> Number
    - is Integer -> Integer
    - is Boolean -> Boolean toggle
    - any mixture -> Text


  */

#[derive(Debug, Clone, PartialEq)]
enum InputType {
    Toggle,
    Dropdown(Vec<String>),
    Text,
    Integer,
    Number,
    Monaco,
}

impl From<Vec<SchemaType>> for InputType {
    fn from(schema_type: Vec<SchemaType>) -> Self {
        if schema_type.contains(&SchemaType::Object)
            || schema_type.contains(&SchemaType::Array)
        {
            InputType::Monaco
        } else if schema_type.contains(&SchemaType::Pattern) {
            InputType::Text
        } else if schema_type.len() == 1 {
            match schema_type[0] {
                SchemaType::Number => InputType::Number,
                SchemaType::Integer => InputType::Integer,
                SchemaType::Boolean => InputType::Toggle,
                SchemaType::Enum(options) => InputType::Dropdown(options),
                SchemaType::Pattern => InputType::Text,
                _ => InputType::Monaco,
            }
        } else {
            InputType::Text
        }
    }
}

impl InputType {
    fn html(&self, class: String, id: String, value: Signal<Value>) -> impl IntoView {
        match self {
            InputType::Toggle => {}
            InputType::Integer => {}
            InputType::Number => {}
            InputType::Dropdown(options) => {}
            InputType::Monaco => {}
            InputType::Text => {}
        }
    }
}

#[component]
pub fn input(
    #[prop(into)] r#type: InputType,
    value: Value,
    on_change: Callback<Value, ()>,
    #[prop(default = false)] disabled: bool,
    #[prop(into, default = String::new())] id: String,
    #[prop(into, default = String::new())] class: String,
    #[prop(into, default = String::new())] name: String,
) -> impl IntoView {

    match r#type {
        InputType::Text => {
            view! {
                <input type="text" />
            }.into_view()
        },
        InputType::Toggle => {
            view! {
                <Toggle value on_change />
            }.into_view()
        },
        InputType::Number => {
            view! {
                <input type="number" />
            }.into_view()
        },
        InputType::Integer => {
            view! {
                <input type="number" />
            }.into_view()
        },
        InputType::Dropdown(options) => {
            view! {
                <Dropdown
                        disabled
                        dropdown_width="w-100"
                        dropdown_icon="".to_string()
                        // add the singal
                        dropdown_text=""
                        dropdown_direction=DropdownDirection::Down
                        dropdown_btn_type=DropdownBtnType::Select
                        dropdown_options=options
                        name=""
                        on_select=Callback::new(move |selected: String| {
                            // handle_change.call(selected.clone());
                            // set_value.set(selected.clone());
                            // set_selected_enum.set(selected.clone());
                        })
                    />
            }.into_view()
        },
        InputType::Monaco => {
            view! {
                <input type="text" />
            }.into_view()
        },
    }
}
