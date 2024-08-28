use leptos::*;
use serde_json::{json, Map, Value};

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
    Number,
    Integer,

    Toggle,
    Monaco,
    Select(EnumVariants),

    Disabled,
}

impl InputType {
    pub fn to_html_input_type(self) -> &'static str {
        match self {
            InputType::Text => "text",
            InputType::Disabled => "text",

            InputType::Number => "number",
            InputType::Integer => "number",

            InputType::Toggle => "text",
            InputType::Monaco => "text",
            InputType::Select(_) => "text",
        }
    }
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
            SchemaType::Single(JsonSchemaType::Null) => InputType::Disabled,
            SchemaType::Single(JsonSchemaType::Array) => InputType::Monaco,
            SchemaType::Single(JsonSchemaType::Object) => InputType::Monaco,
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

fn str_to_value(s: &str, type_: &JsonSchemaType) -> Result<Value, String> {
    match type_ {
        JsonSchemaType::String => Ok(Value::String(s.to_string())),
        JsonSchemaType::Number => s
            .parse::<i64>()
            .map(|v| json!(v))
            .map_err(|_| "not a valid number".to_string()),
        JsonSchemaType::Integer => s
            .parse::<f64>()
            .map(|v| json!(v))
            .map_err(|_| "not a valid integer".to_string()),
        JsonSchemaType::Boolean => s
            .parse::<bool>()
            .map(Value::Bool)
            .map_err(|_| "value should be either true or false".to_string()),
        JsonSchemaType::Array => serde_json::from_str::<Vec<Value>>(s)
            .map(Value::Array)
            .map_err(|_| "not a valid array".to_string()),
        JsonSchemaType::Object => serde_json::from_str::<Map<String, Value>>(s)
            .map(Value::Object)
            .map_err(|_| "not a valid array".to_string()),
        JsonSchemaType::Null if s == "null" => Ok(Value::Null),
        JsonSchemaType::Null => Err("not a null value".to_string()),
    }
}

fn parse_input_value(value: String, schema_type: SchemaType) -> Result<Value, String> {
    match schema_type {
        SchemaType::Single(ref type_) => str_to_value(&value, type_),
        SchemaType::Multiple(types) => {
            for type_ in types.iter() {
                let v = str_to_value(&value, type_);
                if v.is_ok() {
                    return v;
                }
            }
            Err("not of valid type".to_string())
            // types.sort_by(|a, b| a.precedence().cmp(&b.precedence()))
        }
    }
}

#[component]
fn basic_input(
    id: String,
    name: String,
    class: String,
    r#type: InputType,
    disabled: bool,
    required: bool,
    value: Value,
    schema_type: SchemaType,
    on_change: Callback<Value, ()>,
) -> impl IntoView {
    let schema_type = store_value(schema_type);
    let (error_rs, error_ws) = create_signal::<Option<String>>(None);

    if r#type == InputType::Number && value.as_f64().is_none() {
        error_ws.set(Some(format!("{} is not a valid number", value)));
    }

    if r#type == InputType::Integer && value.as_i64().is_none() {
        error_ws.set(Some(format!("{} is not a valid integer", value)));
    }

    view! {
        <input
            id=id
            name=name
            class=class
            required=required
            disabled=disabled
            type={r#type.to_html_input_type()}

            value=format!("{}", value)
            on:change=move |e| {
                let v = event_target_value(&e);
                match parse_input_value(v, schema_type.get_value()) {
                    Ok(v) => on_change.call(v),
                    Err(e) => error_ws.set(Some(e))
                }
            }
        />

        {move || {
            let error = error_rs.get();
            match error {
                Some(msg) => view! { <span>{msg}</span> }.into_view(),
                None => ().into_view(),
            }
        }}
    }
}

#[component]
pub fn input(
    #[prop(into)] r#type: InputType,
    schema_type: SchemaType,
    value: Value,
    on_change: Callback<Value, ()>,
    #[prop(default = false)] disabled: bool,
    #[prop(into, default = String::new())] id: String,
    #[prop(into, default = String::new())] class: String,
    #[prop(into, default = String::new())] name: String,
) -> impl IntoView {
    match r#type {
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
        InputType::Select(options) => {
            view! {
                // <Dropdown
                //         disabled
                //         dropdown_width="w-100"
                //         dropdown_icon=""
                //         // add the singal
                //         dropdown_text=value.as_str().map(String::from).unwrap_or(value.to_string())
                //         dropdown_direction=DropdownDirection::Down
                //         dropdown_btn_type=DropdownBtnType::Select
                //         dropdown_options=options
                //         name=""
                //         on_select=Callback::new(move |selected: String| {
                //             // handle_change.call(selected.clone());
                //             // set_value.set(selected.clone());
                //             // set_selected_enum.set(selected.clone());
                //         })
                //     />
            }
            .into_view()
        }
        InputType::Monaco => view! {
            <div>
                {format!("{}", value)}
            </div>
        }
        .into_view(),
        _ => view! {
            <BasicInput
                id
                name
                class
                disabled
                required=true
                r#type
                value
                schema_type
                on_change
            />
        },
    }
}


// #[component]
// fn number_input(
//     value: f64,
//     on_change: Callback<f64, ()>,
//     disabled: bool,
//     id: String,
//     class: String,
//     name: String,
//     required: bool,
// ) -> impl IntoView {
//     let (error_rs, error_ws) = create_signal::<Option<String>>(None);
//
//     view! {
//         <input
//             id=id
//             name=name
//             class=class
//             required=required
//             disabled=disabled
//
//             value=value
//             on:change=move |e| {
//                 let v = event_target_value(&e).parse::<f64>();
//                 match v {
//                     Ok(v) => on_change.call(v),
//                     Err(_) => error_ws.set(Some(format!("not a valid number"))),
//                 }
//             }
//         />
//
//         {move || {
//             let error = error_rs.get();
//             match error {
//                 Some(msg) => view! { <span>{msg}</span> }.into_view(),
//                 None => ().into_view(),
//             }
//         }}
//     }
// }
//
// #[component]
// fn integer_input(
//     value: i64,
//     on_change: Callback<i64, ()>,
//     disabled: bool,
//     id: String,
//     class: String,
//     name: String,
//     required: bool,
// ) -> impl IntoView {
//     let (error_rs, error_ws) = create_signal::<Option<String>>(None);
//
//     view! {
//         <input
//             id=id
//             name=name
//             class=class
//             required=required
//             disabled=disabled
//
//             value=value
//             on:change=move |e| {
//                 let v = event_target_value(&e).parse::<i64>();
//                 match v {
//                     Ok(v) => on_change.call(v),
//                     Err(_) => error_ws.set(Some(format!("not a valid integer"))),
//                 }
//             }
//         />
//
//         {move || {
//             let error = error_rs.get();
//             match error {
//                 Some(msg) => view! { <span>{msg}</span> }.into_view(),
//                 None => ().into_view(),
//             }
//         }}
//     }
// }

