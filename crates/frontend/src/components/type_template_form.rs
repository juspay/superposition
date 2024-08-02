pub mod utils;

use std::str::FromStr;

use crate::components::type_template_form::utils::create_type;
use crate::components::{
    button::Button,
    monaco_editor::{
        generate_uri_name, MonacoEditor, TextContentType, METASCHEMA_JSON_SCHEMA_URI,
    },
    type_template_form::utils::update_type,
};
use leptos::*;
use serde_json::{json, Value};
use web_sys::MouseEvent;

#[component]
pub fn type_template_form<NF>(
    #[prop(default = false)] edit: bool,
    #[prop(default = String::new())] type_name: String,
    #[prop(default = json!({"type": "number"}))] type_schema: Value,
    handle_submit: NF,
) -> impl IntoView
where
    NF: Fn() + 'static + Clone,
{
    let tenant_rs = use_context::<ReadSignal<String>>().unwrap();

    let (error_message, set_error_message) = create_signal("".to_string());
    let (type_name_rs, type_name_ws) = create_signal(type_name);
    let (type_schema_rs, type_schema_ws) = create_signal(type_schema);

    let on_submit = move |ev: MouseEvent| {
        ev.prevent_default();
        let type_name = type_name_rs.get();
        let type_schema = type_schema_rs.get();

        let handle_submit_clone = handle_submit.clone();
        spawn_local({
            let handle_submit = handle_submit_clone;
            async move {
                let result = if edit {
                    update_type(tenant_rs.get(), type_name, type_schema).await
                } else {
                    let payload = json!({
                        "type_name": type_name,
                        "type_schema": type_schema
                    });
                    create_type(tenant_rs.get(), payload.clone()).await
                };
                match result {
                    Ok(_) => {
                        handle_submit();
                    }
                    Err(e) => {
                        set_error_message.set(e);
                    }
                }
            }
        });
    };
    view! {
        <form class="form-control w-full space-y-4 bg-white text-gray-700 font-mono">
            <div class="form-control">
                <label class="label">
                    <span class="label-text">Type Name</span>
                </label>
                <input
                    disabled=edit
                    type="text"
                    placeholder="Type name"
                    name="type_name"
                    id="type_name"
                    class="input input-bordered w-full max-w-md"
                    value=move || type_name_rs.get()
                    on:change=move |ev| {
                        let value = event_target_value(&ev);
                        type_name_ws.set(value);
                    }
                />

            </div>

            <div class="divider"></div>
            <div class="form-control">
                <label class="label">
                    <span class="label-text">Type Schema</span>
                </label>
                {move || {
                    let schem = if type_schema_rs.get_untracked().is_null() {
                        String::from("")
                    } else {
                        format!("{}", type_schema_rs.get())
                    };
                    let (schem_rs, _) = create_signal(schem);
                    let uri_name = generate_uri_name();
                    view! {

                        <MonacoEditor
                            node_id="json_schema_editor"
                            data_rs=schem_rs
                            language=TextContentType::Json
                            uri_name=uri_name.clone()
                            schemas=json!(
                                [{
                                    "uri": METASCHEMA_JSON_SCHEMA_URI,
                                    "fileMatch": [uri_name]
                                }]
                            )
                            validation=true
                            classes=vec![
                                "min-h-[400px]",
                                "min-w-[300px]",
                                "border-2",
                                "border-purple-500",
                                "rounded-lg",
                                "mt-5",
                                "w-full",
                                "max-w-md",
                                "pt-3",
                                "pb-2",
                            ]
                            update_fn=move |ev| {
                                let value = event_target_value(&ev);
                                logging::log!("value is: {}", value);
                                match Value::from_str(&value) {
                                    Ok(test_val) => {
                                        type_schema_ws.set_untracked(test_val);
                                        set_error_message.set("".to_string());
                                    }
                                    Err(err) => set_error_message.set(err.to_string())
                                };
                            }
                        />
                    }
                }}

            </div>

            <div class="form-control grid w-full mt-5 justify-start">
                <Button
                    class="pl-[70px] pr-[70px]".to_string()
                    text="Submit".to_string()
                    on_click=on_submit
                />
            </div>
            <div>
                <p class="text-red-500">{move || error_message.get()}</p>
            </div>
        </form>
    }
}
