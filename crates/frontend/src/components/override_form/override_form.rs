use crate::{
    components::dropdown::dropdown::{Dropdown, DropdownBtnType, DropdownDirection},
    types::DefaultConfig,
};
use leptos::*;
use serde_json::{json, Map, Value};
use std::str::FromStr;
use web_sys::MouseEvent;

pub fn get_default_config_type(
    default_configs: Vec<DefaultConfig>,
    key_name: &str,
) -> String {
    let default_config = default_configs
        .iter()
        .find(|&default_conf| default_conf.key == key_name.to_string());
    let schema = &default_config.unwrap().schema;
    let schema_type = schema.get("type").unwrap();
    schema_type.to_string()
}

#[component]
pub fn override_form<NF>(
    overrides: Map<String, Value>,
    default_config: Vec<DefaultConfig>,
    handle_change: NF,
    is_standalone: bool,
    #[prop(default = false)] disable_remove: bool,
    #[prop(default = true)] show_add_override: bool,
    #[prop(into, default = None)] handle_key_remove: Option<Callback<String, ()>>,
) -> impl IntoView
where
    NF: Fn(Map<String, Value>) + 'static,
{
    let default_config = StoredValue::new(default_config);
    let (overrides, set_overrides) = create_signal(overrides.clone());
    let unused_config_keys = Signal::derive(move || {
        default_config
            .get_value()
            .into_iter()
            .filter(|config| !overrides.get().contains_key(&config.key))
            .collect::<Vec<DefaultConfig>>()
    });
    let has_default_config = Signal::derive(move || unused_config_keys.get().len() > 0);

    let on_submit = move |event: MouseEvent| {
        event.prevent_default();
        logging::log!("{:?}", overrides.get());
    };

    let default_config_value =
        |name: &str, val: &str, default_config: &Vec<DefaultConfig>| {
            let dimension_type = get_default_config_type(default_config.clone(), name);
            match dimension_type.replace("\"", "").as_str() {
                "boolean" => match bool::from_str(val) {
                    Ok(boolean) => Value::Bool(boolean),
                    _ => Value::String("Invalid Boolean".to_string()),
                },
                "number" => match val.parse::<i64>() {
                    Ok(number) => Value::Number(number.into()),
                    Err(_) => Value::String(val.to_string()),
                },
                _ => Value::String(val.to_string()),
            }
        };

    let handle_config_key_select = move |default_config: DefaultConfig| {
        let config_key = default_config.key;
        set_overrides.update(|value| {
            value.insert(config_key.to_string(), json!(""));
        });
    };

    create_effect(move |_| {
        let f_override = overrides.get();
        handle_change(f_override.clone());
    });

    view! {
        <div>
            <div class="space-y-4">
                <div class="flex items-center justify-between gap-4">
                    <label class="label">
                        <span class="label-text font-semibold text-base">Overrides</span>
                    </label>
                    <Show when=move || show_add_override>
                        <Dropdown
                            dropdown_btn_type=DropdownBtnType::Link
                            dropdown_direction=DropdownDirection::Left
                            dropdown_text=String::from("Add Override")
                            dropdown_icon=String::from("ri-add-line")
                            dropdown_options=unused_config_keys.get()
                            on_select=Box::new(handle_config_key_select)
                        />
                    </Show>
                </div>
                <Show when=move || overrides.get().len() == 0>
                    <div class="p-4 text-gray-400 flex flex-col justify-center items-center">
                        <div>
                            <i class="ri-add-circle-line text-xl"></i>
                        </div>
                        <div>
                            <span class="text-semibold text-sm">Add Override</span>
                        </div>
                    </div>
                </Show>
                <For
                    each=move || { overrides.get().into_iter().collect::<Vec<(String, Value)>>() }
                    key=|(config_key, _)| config_key.to_string()
                    children=move |(config_key, config_value)| {
                        let config_key_label = config_key.to_string();
                        let config_key_value = config_key.to_string();
                        let config_value = config_value.to_string().replace("\"", "");
                        view! {
                            <div>
                                <div class="flex items-center gap-4">
                                    <div class="form-control">
                                        <label class="label font-medium font-mono text-sm">
                                            <span class="label-text">{config_key_label} ":"</span>
                                        </label>
                                    </div>
                                    <div class="form-control w-2/5">
                                        <input
                                            type="text"
                                            placeholder="Enter override here"
                                            name="override"
                                            class="input input-bordered w-full bg-white text-gray-700 shadow-md"
                                            value=config_value
                                            on:input=move |event| {
                                                let input_value = event_target_value(&event);
                                                let default_config_val = default_config_value(
                                                    &config_key_value,
                                                    &input_value,
                                                    &default_config.get_value(),
                                                );
                                                logging::log!("Default Config: {}", default_config_val);
                                                set_overrides
                                                    .update(|curr_overrides| {
                                                        curr_overrides
                                                            .insert(
                                                                config_key_value.to_string(),
                                                                json!(default_config_val),
                                                            );
                                                    });
                                            }
                                        />

                                    </div>
                                    <div class="w-1/5">

                                        {if !disable_remove {
                                            view! {
                                                <button
                                                    class="btn btn-ghost btn-circle btn-sm"
                                                    on:click=move |ev| {
                                                        ev.prevent_default();
                                                        match handle_key_remove {
                                                            Some(f) => f.call(config_key.clone()),
                                                            None => {
                                                                set_overrides
                                                                    .update(|value| {
                                                                        value.remove(&config_key);
                                                                    })
                                                            }
                                                        };
                                                    }
                                                >

                                                    <i class="ri-delete-bin-2-line text-xl text-2xl font-bold"></i>
                                                </button>
                                            }
                                                .into_view()
                                        } else {
                                            view! {}.into_view()
                                        }}

                                    </div>
                                </div>
                            </div>
                        }
                    }
                />

            </div>
            <Show when=move || is_standalone>
                <div class="flex justify-end">
                    <button class="btn" on:click:undelegated=on_submit>
                        Save
                    </button>
                </div>
            </Show>
        </div>
    }
}
