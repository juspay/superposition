use std::collections::{HashMap, HashSet};

use crate::{
    components::{
        dropdown::{Dropdown, DropdownDirection},
        input_components::{BooleanToggle, EnumDropdown},
    },
    types::DefaultConfig,
    utils::{get_config_value, get_key_type, ConfigType},
};
use leptos::*;
use serde_json::{json, Map, Value};
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
    let (override_keys, set_override_keys) = create_signal(HashSet::<String>::from_iter(
        overrides.clone().iter().map(|(k, _)| String::from(k)),
    ));
    let (overrides, set_overrides) = create_signal(overrides);

    let default_config_map: HashMap<String, DefaultConfig> = default_config
        .get_value()
        .into_iter()
        .map(|ele| (ele.clone().key, ele))
        .collect();

    let on_submit = move |event: MouseEvent| {
        event.prevent_default();
        logging::log!("{:?}", overrides.get());
    };

    let handle_config_key_select = Callback::new(move |default_config: DefaultConfig| {
        let config_key = default_config.key;
        set_overrides.update(|value| {
            value.push((config_key.clone(), json!("")));
        });
        set_override_keys.update(|keys| {
            keys.insert(config_key);
        })
    });

    let get_default_config_val = move |config_key_value: String, value: String| {
        get_config_value(
            &config_key_value,
            &value,
            &default_config
                .get_value()
                .into_iter()
                .map(ConfigType::DefaultConfig)
                .collect::<Vec<_>>(),
        )
        .expect(format!("can't parse default config key {} {}", config_key_value, value).as_str())
    };

    let update_overrides = move |config_key_value: &str, value: String| {
        let default_config_val =
            get_default_config_val(config_key_value.to_owned(), value);
        set_overrides.update(|curr_overrides| {
            let position = curr_overrides
                .iter()
                .position(|(k, _)| k.to_owned() == config_key_value);
            if let Some(idx) = position {
                curr_overrides[idx].1 = json!(default_config_val);
            }
        });
    };

    let update_overrides_untracked = move |config_key_value: &str, value: String| {
        let default_config_val =
            get_default_config_val(config_key_value.to_owned(), value);
        set_overrides.update_untracked(|curr_overrides| {
            let position = curr_overrides
                .iter()
                .position(|(k, _)| k.to_owned() == config_key_value);
            if let Some(idx) = position {
                curr_overrides[idx].1 = json!(default_config_val);
            }
        });
    };

    create_effect(move |_| {
        let f_override = overrides.get();
        handle_change(f_override.clone());
    });

    view! {
        <div class="pt-3">
            <div class="form-control space-y-4">
                <div class="flex items-center justify-between gap-4">
                    <label class="label">
                        <span class="label-text font-semibold text-base">Overrides</span>
                    </label>
                </div>
                <div class="card w-full bg-slate-50">
                    <div class="card-body">
                        <Show when=move || { overrides.get().is_empty() && show_add_override }>
                            <div class="flex justify-center">
                                <Dropdown
                                    dropdown_direction=DropdownDirection::Left
                                    dropdown_text=String::from("Add Override")
                                    dropdown_icon=String::from("ri-add-line")
                                    dropdown_options=default_config.get_value()
                                    on_select=handle_config_key_select
                                />
                            </div>
                        </Show>

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
                            each=move || {
                                overrides.get().into_iter().collect::<Vec<(String, Value)>>()
                            }

                            key=|(config_key, _)| config_key.to_string()
                            children=move |(config_key, config_value)| {
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
                                let key_type = get_key_type(&schema);
                                logging::log!("config value {}", config_value.clone());
                                view! {
                                    <div>
                                        <div class="flex items-center gap-4">
                                            <div class="form-control">
                                                <label class="label font-medium font-mono text-sm">
                                                    <span class="label-text">
                                                        {config_key_label.clone()} ":"
                                                    </span>
                                                </label>
                                            </div>
                                            <div class="form-control">
                                                {match key_type.as_str() {
                                                    "ENUM" => {
                                                        view! {
                                                            <EnumDropdown
                                                                schema
                                                                config_value=config_value
                                                                handle_change=Callback::new(move |selected_enum: String| {
                                                                    update_overrides(&config_key_value, selected_enum)
                                                                })

                                                                class=String::from("mt-2")
                                                            />
                                                        }
                                                            .into_view()
                                                    }
                                                    "BOOLEAN" => {
                                                        if config_value.is_empty() {
                                                            update_overrides_untracked(
                                                                &config_key_value,
                                                                config_value
                                                                    .clone()
                                                                    .parse::<bool>()
                                                                    .unwrap_or(false)
                                                                    .to_string(),
                                                            );
                                                        }
                                                        view! {
                                                            <BooleanToggle
                                                                config_value={config_value.parse::<bool>().unwrap_or(false)}
                                                                update_value=Callback::new(move |flag: bool| {
                                                                    update_overrides(&config_key_value, flag.to_string());
                                                                })
                                                            />
                                                        }
                                                            .into_view()
                                                    }
                                                    _ => {
                                                        view! {
                                                            <div class="w-2/5">
                                                                <textarea
                                                                    type="text"
                                                                    placeholder="Enter override here"
                                                                    name="override"
                                                                    class="input input-bordered w-[450px] flex items-center bg-white text-gray-700 shadow-md pt-3"
                                                                    on:change=move |event| {
                                                                        let input_value = event_target_value(&event);
                                                                        update_overrides(&config_key_value, input_value);
                                                                    }
                                                                >

                                                                    {config_value}
                                                                </textarea>
                                                            </div>
                                                        }
                                                            .into_view()
                                                    }
                                                }}

                                            </div>
                                            <div class="w-1/5">

                                                {if !disable_remove {
                                                    view! {
                                                        <button
                                                            class="btn btn-ghost btn-circle btn-sm"
                                                            on:click=move |ev| {
                                                                ev.prevent_default();
                                                                match handle_key_remove {
                                                                    Some(f) => f.call(config_key_label.clone()),
                                                                    None => {
                                                                        set_overrides
                                                                            .update(|value| {
                                                                                let position = value
                                                                                    .iter()
                                                                                    .position(|(k, _)| k.to_owned() == config_key.clone());
                                                                                if let Some(idx) = position {
                                                                                    value.remove(idx);
                                                                                }
                                                                            });
                                                                        set_override_keys
                                                                            .update(|keys| {
                                                                                keys.remove(&config_key);
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

                        <Show when=move || { !overrides.get().is_empty() && show_add_override }>
                            <div class="mt-4">

                                {move || {
                                    let unused_config_keys = default_config
                                        .get_value()
                                        .into_iter()
                                        .filter(|config| !override_keys.get().contains(&config.key))
                                        .collect::<Vec<DefaultConfig>>();
                                    view! {
                                        <Dropdown
                                            dropdown_direction=DropdownDirection::Down
                                            dropdown_text=String::from("Add Override")
                                            dropdown_icon=String::from("ri-add-line")
                                            dropdown_options=unused_config_keys.clone()
                                            on_select=handle_config_key_select
                                        />
                                    }
                                }}

                            </div>
                        </Show>
                    </div>
                </div>
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
