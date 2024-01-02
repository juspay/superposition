use super::types::DimensionCreateReq;
use super::utils::{create_dimension, parse_string_to_json_value_vec};
use crate::components::button::button::Button;
use leptos::*;
use serde_json::json;
use web_sys::MouseEvent;

#[component]
pub fn dimension_form<NF>(
    edit: bool,
    priority: u16,
    dimension_name: String,
    dimension_type: String,
    dimension_pattern: String,
    handle_submit: NF,
) -> impl IntoView
where
    NF: Fn() + 'static + Clone,
{
    let tenant_rs = use_context::<ReadSignal<String>>().unwrap();

    let (priority, set_priority) = create_signal(priority);
    let (dimension_name, set_dimension_name) = create_signal(dimension_name);
    let (dimension_type, set_dimension_type) = create_signal(dimension_type);
    let (dimension_pattern, set_dimension_pattern) = create_signal(dimension_pattern);

    let (show_labels, set_show_labels) = create_signal(false);

    let (error_message, set_error_message) = create_signal("".to_string());

    let on_submit = move |ev: MouseEvent| {
        ev.prevent_default();
        let f_priority = priority.get();
        let f_name = dimension_name.get();
        let f_type = dimension_type.get();
        let f_pattern = dimension_pattern.get();

        let f_schema = match f_type.as_str() {
            "number" => {
                json!({
                    "type": f_type.to_string()
                })
            }
            "Enum" => {
                json!({
                    "type": "string",
                    "enum": parse_string_to_json_value_vec(f_pattern.as_str())
                })
            }
            "Pattern" => {
                json!({
                    "type": "string",
                    "pattern": f_pattern.to_string()
                })
            }
            _ => {
                json!(f_pattern.to_string())
            }
        };

        let payload = DimensionCreateReq {
            dimension: f_name,
            priority: f_priority,
            schema: f_schema,
        };

        let handle_submit_clone = handle_submit.clone();
        spawn_local({
            let handle_submit = handle_submit_clone;
            async move {
                let result = create_dimension(tenant_rs.get(), payload.clone()).await;

                match result {
                    Ok(_) => {
                        handle_submit();
                        // modal_action("my_modal_5","close");
                    }
                    Err(e) => {
                        set_error_message.set(e);
                        // Handle error
                        // Consider logging or displaying the error
                    }
                }
            }
        });
    };
    view! {
        <form class="form-control w-full space-y-4 bg-white text-gray-700 font-mono">
            <div class="form-control">
                <label class="label font-mono">
                    <span class="label-text text-gray-700 font-mono">Dimension</span>
                </label>
                <input
                    disabled=edit
                    type="text"
                    placeholder="Dimension"
                    class="input input-bordered w-full bg-white text-gray-700 shadow-md"
                    value=dimension_name.get()
                    on:change=move |ev| {
                        let value = event_target_value(&ev);
                        set_dimension_name.set(value);
                    }
                />

            </div>
            <select
                name="schemaType[]"
                on:change=move |ev| {
                    set_show_labels.set(true);
                    match event_target_value(&ev).as_str() {
                        "number" => {
                            set_dimension_type.set("number".to_string());
                        }
                        "Enum" => {
                            set_dimension_type.set("Enum".to_string());
                            set_dimension_pattern
                                .set(format!("{:?}", vec!["android", "web", "ios"]));
                        }
                        "Pattern" => {
                            set_dimension_type.set("Pattern".to_string());
                            set_dimension_pattern.set(".*".to_string());
                        }
                        _ => {
                            set_dimension_type.set("Other".to_string());
                            set_dimension_pattern.set("".to_string());
                        }
                    };
                }

                class="select select-bordered"
            >
                <option disabled selected>
                    Set Schema
                </option>

                <option
                    value="number"
                    selected=move || { dimension_type.get() == "number".to_string() }
                >
                    "Number"
                </option>
                <option
                    value="Enum"
                    selected=move || { dimension_type.get() == "Enum".to_string() }
                >
                    "String (Enum)"
                </option>
                <option
                    value="Pattern"
                    selected=move || { dimension_type.get() == "Pattern".to_string() }
                >
                    "String (regex)"
                </option>
                <option
                    value="Other"
                    selected=move || { dimension_type.get() == "Other".to_string() }
                >
                    "Other"
                </option>
            </select>

            {move || {
                view! {
                    <Show when=move || (dimension_type.get() == "number")>
                        <div class="form-control">
                            <label class="label font-mono">
                                <span class="label-text text-gray-700 font-mono">Priority</span>
                            </label>
                            <input
                                type="Number"
                                placeholder="Priority"
                                class="input input-bordered w-full bg-white text-gray-700 shadow-md"
                                value=priority.get()
                                on:change=move |ev| {
                                    logging::log!(
                                        "{:?}", event_target_value(& ev).parse::< u16 > ()
                                    );
                                    match event_target_value(&ev).parse::<u16>() {
                                        Ok(i_prio) => set_priority.set(i_prio),
                                        Err(e) => logging::log!("{e}"),
                                    };
                                }
                            />

                        </div>
                    </Show>

                    <Show when=move || (show_labels.get() && (dimension_type.get() != "number"))>
                        <div class="form-control">
                            <label class="label font-mono">
                                <span class="label-text text-gray-700 font-mono">Priority</span>
                            </label>
                            <input
                                type="Number"
                                placeholder="Priority"
                                class="input input-bordered w-full bg-white text-gray-700 shadow-md"
                                value=priority.get()
                                on:change=move |ev| {
                                    logging::log!(
                                        "{:?}", event_target_value(& ev).parse::< u16 > ()
                                    );
                                    match event_target_value(&ev).parse::<u16>() {
                                        Ok(i_prio) => set_priority.set(i_prio),
                                        Err(e) => logging::log!("{e}"),
                                    };
                                }
                            />

                        </div>
                        <div class="form-control">
                            <label class="label font-mono">
                                <span class="label-text text-gray-700 font-mono">
                                    {dimension_type.get()}
                                </span>
                            </label>
                            <textarea
                                type="text"
                                class="input input-bordered w-full bg-white text-gray-700 shadow-md"
                                on:change=move |ev| {
                                    let value = event_target_value(&ev);
                                    logging::log!("{:?}", value);
                                    set_dimension_pattern.set(value);
                                }
                            >

                                {dimension_pattern.get()}
                            </textarea>

                        </div>
                    </Show>
                }
            }}

            <div class="form-control mt-6">
                <Button text="Submit".to_string() on_click=on_submit/>
            </div>

            {
                view! {
                    <div>
                        <p class="text-red-500">{move || error_message.get()}</p>
                    </div>
                }
            }

        </form>
    }
}