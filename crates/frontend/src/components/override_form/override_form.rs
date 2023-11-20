use crate::{pages::ExperimentList::types::DefaultConfig, types::InputVector};
use leptos::*;
use serde_json::{Map, Value};

#[component]
pub fn OverrideForm(
    overrides: Map<String, Value>,
    default_config: Vec<DefaultConfig>,
    iv_rs: ReadSignal<InputVector>,
    iv_ws: WriteSignal<InputVector>,
) -> impl IntoView {
    let (overrides, set_overrides) = create_signal(overrides);
    view! {
        <For
            each=move || { overrides.get().into_iter().collect::<Vec<(String, Value)>>() }
            key=|(config_key, _)| config_key.to_string()
            children=move |(config_key, config_value)| {
                let configs = default_config.clone();
                let config_key_copy = config_value.clone();
                view! {
                    <div>
                        <div class="flex gap-x-6">
                            <div class="form-control w-20">
                                <select class="select select-bordered">
                                    <option disabled selected>
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
                            <div class="form-control">
                                <div class="flex gap-x-6 items-center">
                                    <input
                                        type="text"
                                        placeholder="Type here"
                                        name="override[]"
                                        value=config_value.to_string()
                                        class="input input-bordered w-full max-w-xs"
                                    />
                                    <button
                                        class="text-error text-xl font-light font-thin"
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
    }
}
