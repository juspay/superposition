use crate::pages::ExperimentList::types::DefaultConfig;
use leptos::*;
use serde_json::{Map, Value};
use std::collections::HashSet;

#[component]
pub fn OverrideForm(
    overrides: Map<String, Value>,
    default_config: Vec<DefaultConfig>,
) -> impl IntoView {
    let (overrides, set_overrides) = create_signal(overrides);
    view! {
        <div class="space-y-4 ">
            <label class="label">
                <span class="label-text font-semibold text-lg">Override</span>
            </label>
            <For
                each=move || { overrides.get().into_iter().collect::<Vec<(String, Value)>>() }
                key=|(config_key, _)| config_key.to_string()
                children=move |(config_key, config_value)| {
                    let configs = default_config.clone();
                    let config_key_copy = config_value.clone();
                    view! {
                        <div class="card bg-base-100 shadow-xl">
                            <div class="card-body">
                                <div class="flex items-center gap-4">
                                    <select class="select select-bordered w-full bg-gray-200 text-white-400/50">
                                        <option disabled selected class="text-sky-400/50 font-mono">
                                            Select Config
                                        </option>
                                        <For
                                            each=move || configs.clone()
                                            key=|item: &DefaultConfig| item.key.to_string()
                                            children=move |item: DefaultConfig| {
                                                view! {
                                                    <option
                                                        value=item.key.to_string()
                                                        selected=item.key.to_string() == config_key_copy
                                                    >
                                                        {item.key}
                                                    </option>
                                                }
                                            }
                                        />

                                    </select>
                                </div>
                                <div class="form-control flex-grow">
                                    <div class="flex gap-x-6 items-center">
                                        <input
                                            type="text"
                                            placeholder="Type here"
                                            value=config_value.to_string()
                                            class="input input-bordered w-full bg-white text-gray-700 shadow-md"
                                        />
                                        <button
                                            class="btn btn-error btn-circle"
                                            on:click=move |_| {
                                                set_overrides
                                                    .update(|value| {
                                                        value.remove(&config_key);
                                                    });
                                            }
                                        >

                                            <i class="ri-delete-bin-2-line"></i>
                                        </button>
                                    </div>
                                </div>
                            </div>
                        </div>
                    }
                }
            />

        </div>
    }
}
