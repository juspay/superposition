use std::collections::HashSet;

use crate::{
    components::dropdown::{Dropdown, DropdownBtnType, DropdownDirection},
    types::DefaultConfig,
    utils::{get_config_value, ConfigType},
};
use leptos::*;
use serde_json::{json, Value};
use web_sys::MouseEvent;

#[component]
pub fn override_form<NF>(
    overrides: Vec<(String, Value)>,
    default_config: Vec<DefaultConfig>,
    handle_change: NF,
    #[prop(default = false)] is_standalone: bool,
    #[prop(default = false)] disable_remove: bool,
    #[prop(default = true)] show_add_override: bool,
    #[prop(into, default = None)] handle_key_remove: Option<Callback<String, ()>>,
) -> impl IntoView
where
    NF: Fn(Vec<(String, Value)>) + 'static,
{
    let default_config = StoredValue::new(default_config);
    let (overrides, set_overrides) = create_signal(overrides);
    let override_keys = Signal::derive(move || {
        HashSet::<String>::from_iter(overrides.get().iter().map(|(k, _)| String::from(k)))
    });

    let on_submit = move |event: MouseEvent| {
        event.prevent_default();
        logging::log!("{:?}", overrides.get());
    };

    let handle_config_key_select = Callback::new(move |default_config: DefaultConfig| {
        let config_key = default_config.key;
        set_overrides.update(|value| {
            value.push((config_key, json!("")));
        });
    });

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

                    {move || {
                        let unused_config_keys = default_config
                            .get_value()
                            .into_iter()
                            .filter(|config| !override_keys.get().contains(&config.key))
                            .collect::<Vec<DefaultConfig>>();
                        view! {
                            <Show when=move || show_add_override>
                                <Dropdown
                                    dropdown_btn_type=DropdownBtnType::Link
                                    dropdown_direction=DropdownDirection::Left
                                    dropdown_text=String::from("Add Override")
                                    dropdown_icon=String::from("ri-add-line")
                                    dropdown_options=unused_config_keys.clone()
                                    on_select=handle_config_key_select
                                />
                            </Show>
                        }
                    }}

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
                <For
                    each=move || { overrides.get().into_iter().collect::<Vec<(String, Value)>>() }
                    key=|(config_key, _)| config_key.to_string()
                    children=move |(config_key, config_value)| {
                        let config_key_label = config_key.to_string();
                        let config_key_value = config_key.to_string();
                        let config_value = config_value.to_string().replace('"', "");
                        logging::log!("config value {}", config_value.clone());
                        view! {
                            <div>
                                <div class="flex items-center gap-4">
                                    <div class="form-control w-3/5">
                                        <label class="label font-medium font-mono text-sm">
                                            <span class="label-text">{config_key_label}</span>
                                        </label>
                                        <textarea
                                            type="text"
                                            placeholder="Enter override here"
                                            name="override"
                                            class="input input-bordered w-full bg-white text-gray-700 shadow-md"
                                            on:change=move |event| {
                                                let input_value = event_target_value(&event);
                                                logging::log!("Changed the override");
                                                let default_config_val = get_config_value(
                                                        &config_key_value,
                                                        &input_value,
                                                        &default_config
                                                            .get_value()
                                                            .into_iter()
                                                            .map(ConfigType::DefaultConfig)
                                                            .collect::<Vec<_>>(),
                                                    )
                                                    .unwrap_or(json!(input_value));
                                                set_overrides
                                                    .update(|curr_overrides| {
                                                        let position = curr_overrides
                                                            .iter()
                                                            .position(|(k, _)| k.to_owned() == config_key_value);
                                                        if let Some(idx) = position {
                                                            curr_overrides[idx].1 = json!(default_config_val);
                                                        }
                                                    });
                                            }
                                        >
                                            {config_value}
                                        </textarea>

                                    </div>
                                    <div class="mt-10 w-1/5 items-end">

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
                                                                        let position = value
                                                                            .iter()
                                                                            .position(|(k, _)| k.to_owned() == config_key);
                                                                        if let Some(idx) = position {
                                                                            value.remove(idx);
                                                                        }
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
                <div class="flex justify-end mt-4">
                    <button class="btn" on:click:undelegated=on_submit>
                        Save
                    </button>
                </div>
            </Show>
        </div>
    }
}
