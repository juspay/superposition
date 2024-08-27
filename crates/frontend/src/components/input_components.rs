use leptos::*;
use serde_json::{Map, Value};

use crate::{
    components::dropdown::{Dropdown, DropdownBtnType, DropdownDirection},
    form_types::{EnumVariants, JsonSchemaType, SchemaType},
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
    Text,
    Toggle,
    Number,
    Monaco,
    Integer,
    Disabled,
    Select(EnumVariants),
}

impl From<(SchemaType, EnumVariants)> for InputType {
    fn from((schema_type, enum_variants): (SchemaType, EnumVariants)) -> Self {
        if !enum_variants.is_empty() {
            return InputType::Select(enum_variants);
        }

        match schema_type {
            SchemaType::Single(JsonSchemaType::Number) => InputType::Number,
            SchemaType::Single(JsonSchemaType::Integer) => InputType::Integer,
            SchemaType::Single(JsonSchemaType::Boolean) => InputType::Toggle,
            SchemaType::Single(JsonSchemaType::String) => InputType::Text,
            SchemaType::Single(JsonSchemaType::Array) => InputType::Text,
            SchemaType::Single(JsonSchemaType::Object) => InputType::Text,
            SchemaType::Single(JsonSchemaType::Null) => InputType::Disabled,
            SchemaType::Multiple(types)
                if types.contains(&JsonSchemaType::Object)
                    || types.contains(&JsonSchemaType::Array) =>
            {
                InputType::Monaco
            }
            SchemaType::Multiple(_) => InputType::Text,
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
        InputType::Text => match value.as_str() {
            Some(v) => view! {
                <input type="text" value={v.to_string()} />
            }
            .into_view(),
            None => view! {
                <input type="text" value={value.to_string()} />
            }
            .into_view(),
        },
        InputType::Toggle => {
            let on_change = Callback::new(move |value: bool| {
                on_change.call(Value::Bool(value));
            });
            match value.as_bool() {
                Some(v) => view! {
                    <Toggle value={v} on_change />
                }
                .into_view(),
                None => view! {
                    <span>An error occured</span>
                }
                .into_view(),
            }
        }
        InputType::Number => {
            match value.as_f64() {
                Some(v) => view! {
                    <input type="number" value=v />
                }
                .into_view(),
                None => view! {
                    <span>An error occured</span>
                }
                .into_view(),
            }
        }
        .into_view(),
        InputType::Integer => match value.as_i64() {
            Some(v) => view! {
                <input type="number" value=v />
            }
            .into_view(),
            None => view! {
                <span>An error occured</span>
            }
            .into_view(),
        },
        InputType::Select(options) => {
            view! {
                <Dropdown
                        disabled
                        dropdown_width="w-100"
                        dropdown_icon=""
                        // add the singal
                        dropdown_text=value.as_str().map(String::from).unwrap_or(value.to_string())
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
            }
            .into_view()
        }
        InputType::Monaco => view! {
            <div>
                {format!("{}", value)}
            </div>
        }
        .into_view(),
    }
}
