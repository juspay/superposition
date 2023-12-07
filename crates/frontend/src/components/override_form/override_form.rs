use crate::pages::ExperimentList::types::DefaultConfig;
use leptos::*;
use serde_json::{Map, Value, json};
use std::collections::HashSet;
use wasm_bindgen::JsCast;
use web_sys::{HtmlInputElement, HtmlSelectElement};

#[component]
pub fn OverrideForm(
    overrides: Map<String, Value>,
    default_config: Vec<DefaultConfig>,
) -> impl IntoView {
    let has_default_config = default_config.len() != 0;
    let (default_overrides, default_used_config_keys): (Map<String, Value>, Vec<String>) = if overrides.len() == 0 && has_default_config {
        (
            Map::from_iter([(default_config[0].key.to_string(), json!(""))]),
            vec![default_config[0].key.to_string()]
        )
    } else {
        (
            overrides.clone(),
            overrides.keys().map(String::from).collect::<Vec<String>>()
        )
    };
    let (overrides, set_overrides) = create_signal(default_overrides);
    let (used_config_keys, set_used_config_keys) = create_signal(HashSet::from_iter(default_used_config_keys));

    let override_signal = use_context::<RwSignal<(String, Map<String, Value>)>>();

    create_effect(move |_| {
        let overrides_vec = overrides.get().clone();
        let overrides_map: Map<String, Value> = overrides_vec.into_iter().collect();

        if let Some(override_context) = override_signal {
            override_context.set((
                "SomeName".to_string(), // Adjust according to your needs
                overrides_map,
            ));
        }
    });

    view! {
        <div class="space-y-4">
            <div class="flex items-center justify-between gap-4">
                <label class="label">
                    <span class="label-text font-semibold text-base">Override</span>
                </label>
                <div>
                    <div class="dropdown dropdown-left">
                        <label tabindex="0" class="btn btn-outline btn-sm text-xs m-1">
                            <i class="ri-add-line"></i>
                            Add Config Key
                        </label>
                        <ul
                            tabindex="0"
                            class="dropdown-content z-[1] menu p-2 shadow bg-base-100 rounded-box w-52"
                        >
                            <Show
                                when = move || !has_default_config
                            >
                                No default config
                            </Show>
                            <For
                                each=move || {
                                    default_config
                                        .clone()
                                        .into_iter()
                                        .filter(|item| {
                                            !used_config_keys.get().contains(&item.key)
                                        })
                                        .collect::<Vec<DefaultConfig>>()
                                }

                                key=|item: &DefaultConfig| item.key.to_string()
                                children=move |item: DefaultConfig| {
                                    let config_key = item.key.to_string();
                                    let label = config_key.to_string();
                                    view! {
                                        <li on:click=move |_| {
                                            set_overrides
                                                .update(|value| {
                                                    value
                                                        .insert(
                                                            config_key.to_string(),
                                                            json!("")
                                                        );
                                                });
                                            set_used_config_keys
                                                .update(|value: &mut HashSet<String>| {
                                                    value.insert(config_key.to_string());
                                                });
                                        }>

                                            <a>{label.to_string()}</a>
                                        </li>
                                    }
                                }
                            />

                        </ul>
                    </div>
                </div>
            </div>
            <For
                each=move || { overrides.get().into_iter().collect::<Vec<(String, Value)>>() }
                key=|(config_key, _)| config_key.to_string()
                children=move |(config_key, config_value)| {
                    let config_key_label = config_key.to_string();
                    let config_key_value = config_key.to_string();
                    view! {
                        <div>
                            <div class="flex items-center gap-4">
                                <div class="form-control">
                                    <label class="label font-medium font-mono text-sm">
                                        <span class="label-text">{config_key_label}":"</span>
                                    </label>
                                </div>
                                <div class="form-control w-2/5">
                                        <input
                                            type="text"
                                            placeholder="Enter override here"
                                            name="override"
                                            class="input input-bordered w-full bg-white text-gray-700 shadow-md"
                                            bind:value=config_value.to_string()
                                            on:input=move |event| {
                                                let input_value = event_target_value(&event);
                                                // TODO: validations
                                                set_overrides.update(|curr_overrides| {
                                                    curr_overrides.insert(config_key_value.to_string(), json!(input_value));
                                                });
                                            }
                                        />
                                </div>
                                <div class="w-1/5">
                                    <button
                                        class="btn btn-ghost btn-circle btn-sm"
                                        on:click=move |ev| {
                                            ev.prevent_default();
                                            set_overrides
                                                .update(|value| {
                                                    value.remove(&config_key);
                                                });
                                            set_used_config_keys.update(|value| {
                                                value.remove(&config_key);
                                            });
                                        }
                                    >
                                        <i class="ri-delete-bin-2-line text-xl text-2xl font-bold"></i>
                                    </button>
                                </div>
                            </div>
                        </div>
                    }
                }
            />

        </div>
    }
}