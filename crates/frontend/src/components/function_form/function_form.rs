use super::utils::{create_function, test_function, update_function};
use crate::components::button::button::Button;
use leptos::*;
use serde_json::{from_str, json, Value};
use web_sys::MouseEvent;

#[component]
pub fn function_editor<NF>(
    #[prop(default = false)] edit: bool,
    #[prop(default = String::new())] function_name: String,
    #[prop(default = String::new())] function: String,
    #[prop(default = String::new())] runtime_version: String,
    #[prop(default = String::new())] description: String,
    handle_submit: NF,
) -> impl IntoView
where
    NF: Fn() + 'static + Clone,
{
    let tenant_rs = use_context::<ReadSignal<String>>().unwrap();
    let (function_name, set_function_name) = create_signal(function_name);
    let (function, set_function) = create_signal(function);
    let (runtime_version, set_runtime_version) = create_signal(runtime_version);
    let (error_message, set_error_message) = create_signal("".to_string());
    let (description, set_description) = create_signal(description);
    if !edit {
        set_runtime_version.set("1.0.0".to_string())
    };
    let on_submit = move |event: MouseEvent| {
        event.prevent_default();
        logging::log!("Submitting function form");

        let tenant = tenant_rs.get();
        let f_function_name = function_name.get();
        let f_function = function.get();
        let f_runtime_version = runtime_version.get();
        let f_description = description.get();
        let handle_submit_clone = handle_submit.clone();

        logging::log!("Function Name in editor: {:?}", function_name);

        spawn_local({
            async move {
                let result = if edit == true {
                    update_function(
                        f_function_name,
                        f_function,
                        f_runtime_version,
                        f_description,
                        tenant,
                    )
                    .await
                } else {
                    create_function(
                        f_function_name,
                        f_function,
                        f_runtime_version,
                        f_description,
                        tenant,
                    )
                    .await
                };

                match result {
                    Ok(_) => {
                        handle_submit_clone();
                    }
                    Err(e) => {
                        set_error_message.set(e);
                    }
                }
            }
        });
    };

    view! {
        <div>
            <form id="MyForm">

                <div class="flex flex-row w-full justify-between">
                    <div class="form-group">

                        <div
                            class="monaco"
                            id="function"
                            style="min-height: 500px; min-width: 1000px"
                            on:change=move |ev| {
                                let value = event_target_value(&ev);
                                logging::log!("Function editor - Function Name: {:?}", value);
                                set_function.set(value);
                            }
                        ></div>
                    </div>

                    <div class="mx-auto w-auto" style="width: 250px">

                        <Show when=move || { edit == false }>
                            <div class="form-control ">
                                <label class="label">
                                    <span class="label-text">Function Name</span>
                                </label>
                                <input
                                    disabled=edit
                                    value=move || function_name.get()
                                    on:input=move |ev| {
                                        set_function_name.set(event_target_value(&ev))
                                    }
                                    type="text"
                                    name="funName"
                                    id="funName"
                                    placeholder="ex: myfunction"
                                    class="input input-bordered w-full max-w-md"
                                />
                            </div>
                        </Show>

                        <div class="form-control">
                            <label class="label">
                                <span class="label-text">Draft Runtime Version</span>
                            </label>
                            <input
                                disabled=true
                                value=move || runtime_version.get()
                                on:input=move |ev| set_runtime_version.set(event_target_value(&ev))
                                type="text"
                                name="runVersion"
                                id="runVersion"
                                placeholder="Js Runtime Version"
                                class="input input-bordered  "
                            />
                        </div>

                        <div class="form-control">
                            <label class="label">
                                <span class="label-text">Description</span>
                            </label>
                            <textarea
                                type="text"
                                class="input input-bordered shadow-md"
                                name="description"
                                id="description"
                                placeholder="explain function"
                                on:change=move |ev| {
                                    let value = event_target_value(&ev);
                                    logging::log!("{:?}", value);
                                    set_description.set(value);
                                }
                            >

                                {description.get()}
                            </textarea>
                        </div>

                        <div class="flex justify-end mt-8">
                            <Button text="Submit".to_string() on_click=on_submit/>
                        </div>

                        <div class="flex">
                            <p class="text-red-500">{move || error_message.get()}</p>
                        </div>
                    </div>

                </div>
            </form>
        </div>
    }
}

#[component]
pub fn test_form<NF>(
    function_name: String,
    stage: String,
    handle_submit: NF,
) -> impl IntoView
where
    NF: Fn() + 'static + Clone,
{
    let tenant_rs = use_context::<ReadSignal<String>>().unwrap();
    let (error_message, set_error_message) = create_signal("".to_string());
    let (output_message, set_output_message) = create_signal("".to_string());
    let (val, set_val) = create_signal(json!({}));
    let (key, set_key) = create_signal("".to_string());

    let on_submit = move |event: MouseEvent| {
        event.prevent_default();
        logging::log!("Submitting function form");

        let tenant = tenant_rs.get();
        let f_function_name = function_name.clone();
        let f_val = json!({
            "key": key.get(),
            "value": val.get()
        });
        let f_stage = stage.clone();

        logging::log!("{:?}", function_name);
        logging::log!("{:?}", val);

        spawn_local({
            async move {
                let result = test_function(f_function_name, f_stage, f_val, tenant).await;

                match result {
                    Ok(resp) => {
                        set_error_message.set("".to_string());
                        set_output_message.set(resp);
                    }
                    Err(e) => {
                        set_output_message.set("".to_string());
                        set_error_message.set(e);
                    }
                }
            }
        });
    };

    view! {
        <div class="flex flex-row  justify-between">

            <div class="monaco" style="min-height: 500px; min-width: 1000px"></div>

            <div class="mx-auto w-auto" style="width: 250px">

                <form id="MyForm">

                    <div class="form-control">
                        <label class="label">
                            <span class="label-text">Key Name</span>
                        </label>
                        <input
                            disabled=false
                            value=move || key.get()
                            on:input=move |ev| set_key.set(event_target_value(&ev))
                            type="text"
                            name="key name"
                            id="keyName"
                            placeholder="key"
                            class="input input-bordered"
                        />
                    </div>

                    <div class="form-control">
                        <label class="label">
                            <span class="label-text">Value</span>
                        </label>
                        <textarea
                            type="text"
                            class="input input-bordered shadow-md"
                            name="value"
                            id="value"
                            style="min-height: 150px"
                            placeholder="value"
                            on:change=move |ev| {
                                let value = event_target_value(&ev);
                                match from_str::<Value>(&value) {
                                    Ok(test_val) => {
                                        set_val.set(test_val);
                                        set_error_message.set("".to_string());
                                        set_output_message.set("".to_string());
                                    }
                                    Err(_) => {
                                        set_val.set(json!(value));
                                        set_error_message.set("".to_string());
                                        set_output_message.set("".to_string());

                                    }
                                };
                            }
                        >

                            {"".to_string()}
                        </textarea>

                    </div>

                    <div class="flex justify-end mt-8">
                        <Button text="Submit".to_string() on_click=on_submit/>
                    </div>

                    <div class="mt-7">
                        <p class="text-red-500">{move || error_message.get()}</p>
                    </div>

                    <div>
                        <p class="text-green-700">{move || output_message.get()}</p>
                    </div>

                </form>
            </div>
        </div>
    }
}
