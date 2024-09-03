use std::collections::{HashMap, HashSet};

use crate::{
    components::{
        dropdown::{Dropdown, DropdownDirection},
        input_components::{Input, InputType},
    },
    form_types::{EnumVariants, SchemaType},
    types::DefaultConfig,
};
use leptos::*;
use serde_json::Value;
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

        if let Ok(config_type) = SchemaType::try_from(default_config.schema) {
            let def_value = config_type.default_value();
            set_overrides.update(|value| {
                value.push((config_key.clone(), def_value));
            });
            set_override_keys.update(|keys| {
                keys.insert(config_key);
            })
        }
    });

    let update_overrides = move |config_key_value: &str, value: Value| {
        set_overrides.update(|curr_overrides| {
            let position = curr_overrides
                .iter()
                .position(|(k, _)| k.to_owned() == config_key_value);
            if let Some(idx) = position {
                curr_overrides[idx].1 = value;
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
                    <div class="card-body gap-4">
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
                                let config_key = store_value(config_key);
                                let schema_type = SchemaType::try_from(
                                    default_config_map
                                        .get(&config_key.get_value())
                                        .unwrap()
                                        .schema
                                        .clone(),
                                );
                                let enum_variants = EnumVariants::try_from(
                                    default_config_map
                                        .get(&config_key.get_value())
                                        .unwrap()
                                        .schema
                                        .clone(),
                                );
                                let input_type = match (schema_type.clone(), enum_variants) {
                                    (Ok(type_), Ok(variants)) => {
                                        Some(InputType::from((type_, variants)))
                                    }
                                    _ => None,
                                };
                                let input_class = match input_type {
                                    Some(InputType::Toggle) | None => "",
                                    Some(_) => "w-[450px] text-gray-700",
                                };
                                let type_labels = match schema_type.clone() {
                                    Ok(SchemaType::Single(ref r#type)) => {
                                        view! {
                                            <div class="badge badge-outline text-gray-400 font-medium text-xs">
                                                {r#type.to_string()}
                                            </div>
                                        }
                                            .into_view()
                                    }
                                    Ok(SchemaType::Multiple(types)) => {
                                        types
                                            .iter()
                                            .map(|r#type| {
                                                view! {
                                                    <div class="badge badge-outline text-gray-400 font-medium text-xs">
                                                        {r#type.to_string()}
                                                    </div>
                                                }
                                            })
                                            .collect_view()
                                    }
                                    Err(_) => view! {}.into_view(),
                                };
                                view! {
                                    // TODO: give a width to the form container and remove it from the fields
                                    <div class="flex flex-col">
                                        <div class="form-control">
                                            <label class="label justify-start font-mono text-sm gap-2">
                                                <span class="label-text font-bold text-gray-500">
                                                    // add the type as a label here for each key
                                                    {config_key.get_value()} ":"
                                                </span>
                                                <div class="flex gap-1">{type_labels}</div>
                                            </label>
                                        </div>

                                        <div class="flex gap-4">
                                            {if input_type.is_some() {
                                                view! {
                                                    <Input
                                                        id=config_key.get_value()
                                                        class=input_class
                                                        r#type=input_type.unwrap()
                                                        value=config_value
                                                        schema_type=schema_type.unwrap()
                                                        on_change=Callback::new(move |value| {
                                                            update_overrides(&config_key.get_value(), value);
                                                        })
                                                    />
                                                }
                                                    .into_view()
                                            } else {
                                                view! { <p>"An Error Occured"</p> }.into_view()
                                            }}
                                            <div class="w-1/5">

                                                {if !disable_remove {
                                                    view! {
                                                        <button
                                                            class="btn btn-ghost btn-circle btn-sm"
                                                            on:click=move |ev| {
                                                                ev.prevent_default();
                                                                match handle_key_remove {
                                                                    Some(f) => f.call(config_key.get_value()),
                                                                    None => {
                                                                        set_overrides
                                                                            .update(|value| {
                                                                                let position = value
                                                                                    .iter()
                                                                                    .position(|(k, _)| k.to_owned() == config_key.get_value());
                                                                                if let Some(idx) = position {
                                                                                    value.remove(idx);
                                                                                }
                                                                            });
                                                                        set_override_keys
                                                                            .update(|keys| {
                                                                                keys.remove(&config_key.get_value());
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
