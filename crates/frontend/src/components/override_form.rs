use std::collections::HashMap;

use crate::{
    components::dropdown::{Dropdown, DropdownBtnType, DropdownDirection},
    types::DefaultConfig,
    utils::{get_config_value, ConfigType},
};
use leptos::*;
use serde_json::{json, Map, Value};
use web_sys::MouseEvent;

#[component]
pub fn override_form<NF>(
    overrides: Map<String, Value>,
    default_config: Vec<DefaultConfig>,
    handle_change: NF,
    #[prop(default = false)] is_standalone: bool,
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
    let default_config_map: HashMap<String, DefaultConfig> = default_config.get_value().into_iter().map(|ele| (ele.clone().key, ele)).collect();

    let on_submit = move |event: MouseEvent| {
        event.prevent_default();
        logging::log!("{:?}", overrides.get());
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
                <Show when=move || overrides.get().is_empty()>
                    <div class="p-4 text-gray-400 flex flex-col justify-center items-center">
                        <div>
                            <i class="ri-add-circle-line text-xl"></i>
                        </div>
                        <div>
                            <span class="text-semibold text-sm">Add Override</span>
                        </div>
                    </div>
                </Show>
                {move || {
                    overrides
                        .get()
                        .into_iter()
                        .map(|(config_key, config_value)| {
                            let config_key_label = config_key.to_string();
                            let config_key_value = config_key.to_string();
                            let config_value = config_value.to_string().replace('"', "");
                            let schema: Map<String, Value> = serde_json::from_value(
                                    default_config_map
                                        .get(&config_key_label)
                                        .unwrap()
                                        .schema
                                        .clone(),
                                )
                                .unwrap();
                            let key_type = if schema.contains_key("enum") {
                                "enum"
                            } else {
                                match schema.get("type").unwrap() {
                                    Value::String(str_) => str_,
                                    _ => "string",
                                }
                            };
                            view! {
                                <div>
                                    <div class="flex items-center gap-4">
                                        <div class="form-control">
                                            <label class="label font-medium font-mono text-sm">
                                                <span class="label-text">{config_key_label} ":"</span>
                                            </label>
                                        </div>
                                        <div class="flex justify-between items-center">
                                            <div class="form-control w-2/5">
                                                {match key_type {
                                                    "enum" => {
                                                        let mut enum_array: Vec<String> = vec![];
                                                        if let Some(enum_arr) = schema.get("enum") {
                                                            enum_array = match enum_arr {
                                                                Value::Array(arr) => {
                                                                    arr.into_iter()
                                                                        .filter_map(|v| v.as_str().map(String::from))
                                                                        .collect()
                                                                }
                                                                _ => Vec::new(),
                                                            };
                                                        }
                                                        let selected_enum = if config_value.len() == 0 {
                                                            String::from("Choose Enum")
                                                        } else {
                                                            config_value
                                                        };
                                                        view! {
                                                            <div class="form-control">
                                                                <div class="mt-2">
                                                                    <Dropdown
                                                                        dropdown_width="w-100"
                                                                        dropdown_icon="".to_string()
                                                                        dropdown_text=selected_enum
                                                                        dropdown_direction=DropdownDirection::Down
                                                                        dropdown_btn_type=DropdownBtnType::Select
                                                                        dropdown_options=enum_array
                                                                        on_select=Box::new(move |selected_enum: String| {
                                                                            let default_config_val = get_config_value(
                                                                                    &config_key_value,
                                                                                    &selected_enum,
                                                                                    &default_config
                                                                                        .get_value()
                                                                                        .into_iter()
                                                                                        .map(ConfigType::DefaultConfig)
                                                                                        .collect::<Vec<_>>(),
                                                                                )
                                                                                .expect("can't parse default config key");
                                                                            set_overrides
                                                                                .update(|curr_overrides| {
                                                                                    curr_overrides
                                                                                        .insert(
                                                                                            config_key_value.to_string(),
                                                                                            json!(default_config_val),
                                                                                        );
                                                                                });
                                                                        })
                                                                    />

                                                                </div>
                                                            </div>
                                                        }
                                                    }
                                                    "boolean" => {
                                                        let mut flag = config_value
                                                            .parse::<bool>()
                                                            .unwrap_or(false);
                                                        view! {
                                                            <div>
                                                                <input
                                                                    on:click=move |_| {
                                                                        let default_config_val = get_config_value(
                                                                                &config_key_value,
                                                                                &(!flag).to_string(),
                                                                                &default_config
                                                                                    .get_value()
                                                                                    .into_iter()
                                                                                    .map(ConfigType::DefaultConfig)
                                                                                    .collect::<Vec<_>>(),
                                                                            )
                                                                            .expect("can't parse default config key");
                                                                        set_overrides
                                                                            .update(|curr_overrides| {
                                                                                curr_overrides
                                                                                    .insert(
                                                                                        config_key_value.to_string(),
                                                                                        json!(default_config_val),
                                                                                    );
                                                                            });
                                                                        flag = !flag;
                                                                    }

                                                                    type="checkbox"
                                                                    class="toggle toggle-[#ffffff] flex items-center"
                                                                    checked=flag
                                                                />
                                                            </div>
                                                        }
                                                    }
                                                    _ => {
                                                        view! {
                                                            <div>
                                                                <textarea
                                                                    type="text"
                                                                    placeholder="Enter override here"
                                                                    name="override"
                                                                    class="input input-bordered w-[450px] flex items-center bg-white text-gray-700 shadow-md pt-2"
                                                                    on:blur=move |event| {
                                                                        event.prevent_default();
                                                                        let input_value = event_target_value(&event);
                                                                        let default_config_val = get_config_value(
                                                                                &config_key_value,
                                                                                &input_value,
                                                                                &default_config
                                                                                    .get_value()
                                                                                    .into_iter()
                                                                                    .map(ConfigType::DefaultConfig)
                                                                                    .collect::<Vec<_>>(),
                                                                            )
                                                                            .expect("can't parse default config key");
                                                                        set_overrides
                                                                            .update(|curr_overrides| {
                                                                                curr_overrides
                                                                                    .insert(
                                                                                        config_key_value.to_string(),
                                                                                        json!(default_config_val),
                                                                                    );
                                                                            });
                                                                    }
                                                                >

                                                                    {config_value}
                                                                </textarea>
                                                            </div>
                                                        }
                                                    }
                                                }}

                                            </div>
                                            <div class="ml-[50px]">

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
                                </div>
                            }
                        })
                        .collect_view()
                }}

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
