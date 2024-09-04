use std::time::Duration;

use leptos::*;
use serde_json::{json, Map, Value};

use crate::{
    components::{
        dropdown::{Dropdown, DropdownBtnType, DropdownDirection},
        monaco_editor::{Languages, MonacoEditor},
    },
    form_types::{EnumVariants, HtmlDisplay, JsonSchemaType, SchemaType},
    providers::editor_provider::use_editor,
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
pub fn boolean_toggle(
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

#[derive(Debug, Clone, PartialEq)]
pub enum InputType {
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
            .parse::<f64>()
            .map(|v| json!(v))
            .map_err(|_| "not a valid number".to_string()),
        JsonSchemaType::Integer => s
            .parse::<i64>()
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
            .map_err(|_| "not a valid object".to_string()),
        JsonSchemaType::Null if s == "null" => Ok(Value::Null),
        JsonSchemaType::Null => Err("not a null value".to_string()),
    }
}

fn parse_input_value(value: String, schema_type: SchemaType) -> Result<Value, String> {
    match schema_type {
        SchemaType::Single(ref type_) => str_to_value(&value, type_),
        SchemaType::Multiple(mut types) => {
            types.sort_by(|a, b| a.precedence().cmp(&b.precedence()));

            for type_ in types.iter() {
                let v = str_to_value(&value, type_);
                if v.is_ok() {
                    return v;
                }
            }
            Err("not of valid type".to_string())
        }
    }
}

#[component]
pub fn toggle(
    value: bool,
    on_change: Callback<Value, ()>,
    #[prop(into, default = String::new())] class: String,
    #[prop(default = false)] disabled: bool,
    #[prop(into, default = String::new())] name: String,
) -> impl IntoView {
    view! {
        <input
            disabled=disabled
            on:click=move |e| {
                on_change.call(Value::Bool(event_target_checked(&e)));
            }

            type="checkbox"
            name=name
            class=format!("toggle toggle-primary {class}")
            checked=value
        />
    }
}

#[component]
pub fn select(
    id: String,
    name: String,
    class: String,
    disabled: bool,
    required: bool,
    value: Value,
    options: Vec<Value>,
    schema_type: SchemaType,
    on_change: Callback<Value, ()>,
) -> impl IntoView {
    //TODO: Use id in dropdown and also other fields along with other required attributes
    let (selected_value_rs, selected_value_ws) = create_signal(value);
    view! {
        {move || {
            let selected_value = selected_value_rs.get();
            view! {
                <Dropdown
                    name=name.clone()
                    class=class.clone()
                    disabled=disabled
                    dropdown_width="w-100"
                    dropdown_icon="".to_string()
                    dropdown_text=selected_value.html_display()
                    dropdown_direction=DropdownDirection::Down
                    dropdown_btn_type=DropdownBtnType::Select
                    dropdown_options=options.clone()
                    on_select=Callback::new(move |selected: Value| {
                        selected_value_ws.set(selected.clone());
                        on_change.call(selected);
                    })
                />
            }
        }}
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
        <div class="flex flex-col gap-1">
            <input
                id=id
                name=name
                class=format!("input input-bordered  {}", class)
                required=required
                disabled=disabled
                type=r#type.to_html_input_type()

                value=value.html_display()
                on:change=move |e| {
                    let v = event_target_value(&e);
                    match parse_input_value(v, schema_type.get_value()) {
                        Ok(v) => {
                            on_change.call(v);
                            error_ws.set(None);
                        }
                        Err(e) => error_ws.set(Some(e)),
                    }
                }
            />

            {move || {
                let error = error_rs.get();
                match error {
                    Some(msg) => {
                        view! {
                            <span class="flex gap-2 px-4 text-xs font-semibold text-red-600">
                                <i class="ri-close-circle-line"></i>
                                {msg}
                            </span>
                        }
                            .into_view()
                    }
                    None => ().into_view(),
                }
            }}

        </div>
    }
}

#[component]
pub fn monaco_input(
    id: String,
    class: String,
    value: Value,
    on_change: Callback<Value, ()>,
    schema_type: SchemaType,
) -> impl IntoView {
    let id = store_value(id);
    let schema_type = store_value(schema_type);
    let (value_rs, value_ws) = create_signal(value);
    let (expand_rs, expand_ws) = create_signal(false);
    let (error_rs, error_ws) = create_signal::<Option<String>>(None);
    let show_error = Signal::derive(move || error_rs.get().is_some());

    let (editor_rs, editor_ws) = use_editor();

    let on_edit = Callback::new(move |s: String| {
        editor_ws.update_untracked(|v| {
            v.data = s;
        })
    });

    let on_edit_click = Callback::new(move |_| {
        editor_ws.update(|v| {
            v.data =
                serde_json::to_string_pretty(&value_rs.get()).unwrap_or(String::new());
            v.id = id.get_value();
        });
    });

    let on_save = Callback::new(move |_| {
        let editor_value = editor_rs.with(|v| v.data.clone());
        logging::log!("Saving editor value: {}", editor_value);

        let parsed_value =
            parse_input_value(editor_value.clone(), schema_type.get_value());
        match parsed_value {
            Ok(v) => {
                logging::log!("Saving parsed value: {}", editor_value);
                value_ws.set(v.clone());
                on_change.call(v);

                editor_ws.update(|v| v.reset());
                expand_ws.set(false);
                error_ws.set(None);
            }
            Err(e) => {
                logging::log!("Error parsing editor value: {}", e);
                error_ws.set(Some(e));
            }
        }
    });

    let on_cancel = Callback::new(move |_| {
        editor_ws.update(|v| {
            v.reset();
        });
        expand_ws.set(false);
        error_ws.set(None);
    });

    view! {
        <div class=format!("relative border rounded-lg bg-white p-2 {}", class)>
            <Show when=move || {
                editor_rs.with(|v| v.id != id.get_value())
            }>
                {move || {
                    let display_value = value_rs.get().html_display();
                    view! {
                        <div class="absolute top-[10px] right-[10px]">
                            <i
                                class="ri-pencil-line text-gray-500 cursor-pointer"
                                on:click=move |e| {
                                    on_edit_click.call(e);
                                }
                            >
                            </i>
                        </div>
                        <andypf-json-viewer
                            indent="4"
                            expanded="true"
                            theme="default-light"
                            show-data-types="false"
                            show-toolbar="true"
                            expand-icon-type="arrow"
                            expanded="1"
                            show-copy="true"
                            show-size="false"
                            data=display_value
                        ></andypf-json-viewer>
                    }
                }}

            </Show>

            <Show when=move || {
                editor_rs.with(|v| v.id == id.get_value())
            }>
                {move || {
                    let (tr, ts) = create_signal(String::new());
                    let container_class = if expand_rs.get() {
                        String::from("fixed top-0 left-0 z-10 flex flex-col w-full h-full bg-white")
                    } else {
                        String::from("w-full h-96 p-1")
                    };
                    view! {
                        <div class=container_class>
                            <Show when=move || { expand_rs.get() }>
                                <div class="breadcrumbs px-4 py-4 font-bold">
                                    <ul>
                                        <li>Override</li>
                                        <li>{id.get_value()}</li>
                                    </ul>
                                </div>
                                <div class="divider m-0"></div>
                            </Show>
                            <MonacoEditor
                                node_id=editor_rs.with(|v| v.id.clone())
                                data=editor_rs.with(|v| v.data.clone())
                                on_change=on_edit

                                language=Languages::Json
                                classes=vec!["h-full"]
                                data_rs=tr
                                data_ws=ts
                            />
                            <div class="absolute top-[0px] right-[0px]">
                                <button
                                    class="btn btn-sm btn-ghost font-normal rounded-lg"
                                    on:click=move |_| {
                                        expand_ws.update(|v| *v = !*v);
                                    }
                                >

                                    <i class="ri-expand-diagonal-line ri-lg cursor-pointer"></i>
                                </button>
                            </div>
                            <div class="flex justify-between w-[calc(100%-1rem)] absolute bottom-[10px] right-2">
                                <AnimatedShow
                                    when=show_error
                                    show_class="animate-slide-in-bottom"
                                    hide_class="animate-slide-out-bottom"
                                    hide_delay=Duration::from_millis(5000)
                                >

                                    <span class="flex gap-2 px-4 text-xs font-semibold text-red-600">
                                        <i class="ri-close-circle-line"></i>
                                        {move || { error_rs.get().unwrap_or(String::new()) }}
                                    </span>
                                </AnimatedShow>
                                <div class="join ml-auto">
                                    <button
                                        class="btn btn-sm join-item font-normal"
                                        on:click=move |e| {
                                            on_save.call(e);
                                        }
                                    >

                                        <i class="ri-check-line text-gray-500 cursor-pointer"></i>
                                        Save
                                    </button>
                                    <button
                                        class="btn btn-sm join-item font-normal"
                                        on:click=move |e| {
                                            on_cancel.call(e);
                                        }
                                    >

                                        <i class="ri-close-line text-gray-500 cursor-pointer"></i>
                                        Cancel
                                    </button>
                                </div>
                            </div>
                        </div>
                    }
                }}

            </Show>
        </div>
    }
}

#[component]
pub fn input(
    value: Value,
    schema_type: SchemaType,
    on_change: Callback<Value, ()>,
    #[prop(into)] r#type: InputType,
    #[prop(default = false)] disabled: bool,
    #[prop(into, default = String::new())] id: String,
    #[prop(into, default = String::new())] class: String,
    #[prop(into, default = String::new())] name: String,
) -> impl IntoView {
    match r#type {
        InputType::Toggle => match value.as_bool() {
            Some(ref v) => {
                view! { <Toggle value=v.clone() on_change class name/> }.into_view()
            }
            None => view! { <span>An error occured</span> }.into_view(),
        },
        InputType::Select(ref options) => view! {
            <Select
                id
                name
                class
                value
                on_change
                schema_type
                disabled
                required=true
                options=options.0.clone()
            />
        }
        .into_view(),
        InputType::Monaco => {
            view! { <MonacoInput id class value on_change schema_type/> }.into_view()
        }
        _ => {
            view! {
                <BasicInput
                    id=id
                    name=name
                    class=class
                    disabled=disabled
                    required=true
                    r#type=r#type
                    value=value
                    schema_type=schema_type
                    on_change=on_change
                />
            }
        }
    }
}
