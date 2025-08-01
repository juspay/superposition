use std::collections::{HashMap, HashSet};

use leptos::*;
use serde_json::Value;
use superposition_types::database::models::cac::DefaultConfig;

use crate::{
    components::{
        dropdown::{Dropdown, DropdownDirection},
        input::{Input, InputType},
    },
    schema::{EnumVariants, SchemaType},
    types::{AutoCompleteCallbacks, OrganisationId, Tenant},
    utils::autocomplete_fn_generator,
};

#[component]
fn type_badge(r#type: Option<SchemaType>) -> impl IntoView {
    r#type.map(|t| match t {
        SchemaType::Single(ref r#type) => view! {
            <div class="badge badge-outline text-gray-400 font-medium text-xs">
                {r#type.to_string()}
            </div>
        }
        .into_view(),
        SchemaType::Multiple(types) => types
            .iter()
            .map(|r#type| {
                view! {
                    <div class="badge badge-outline text-gray-400 font-medium text-xs">
                        {r#type.to_string()}
                    </div>
                }
            })
            .collect_view(),
    })
}

#[component]
fn override_input(
    id: String,
    key: String,
    value: Value,
    r#type: Option<SchemaType>,
    variants: Option<EnumVariants>,
    on_change: Callback<(String, Value), ()>,
    on_remove: Callback<String, ()>,
    allow_remove: bool,
    disabled: bool,
    autocomplete_callbacks: AutoCompleteCallbacks,
) -> impl IntoView {
    let autocomplete_callback = autocomplete_callbacks.get(&key).cloned();
    let key = store_value(key);

    let input_type = match (r#type.clone(), variants) {
        (Some(type_), Some(variants)) => Some(InputType::from((type_, variants))),
        _ => None,
    };
    let input_class = match input_type {
        Some(InputType::Toggle) | None => "",
        Some(_) => "w-[450px] text-gray-700",
    };

    view! {
        <div class="flex flex-col">
            <div class="form-control">
                <label class="label justify-start text-sm gap-2">
                    <span class="label-text font-bold text-gray-500">{key.get_value()} ":"</span>
                    <div class="flex gap-1">
                        <TypeBadge r#type=r#type.clone() />
                    </div>
                </label>
            </div>

            <div class="flex gap-4">
                {if let Some(input_type) = input_type {
                    view! {
                        <Input
                            id=id
                            class=input_class
                            r#type=input_type
                            value=value
                            schema_type=r#type.unwrap_or_default()
                            on_change=Callback::new(move |value| {
                                on_change.call((key.get_value(), value));
                            })
                            disabled
                            autocomplete_function=autocomplete_callback
                        />
                    }
                        .into_view()
                } else {
                    view! { <p>"An Error Occured"</p> }.into_view()
                }} <Show when=move || { allow_remove }>
                    <div class="w-1/5">
                        <button
                            class="btn btn-ghost btn-circle btn-sm"
                            on:click=move |ev| {
                                ev.prevent_default();
                                on_remove.call(key.get_value());
                            }
                        >

                            <i class="ri-delete-bin-2-line text-2xl font-bold"></i>
                        </button>

                    </div>
                </Show>
            </div>

        </div>
    }
}

#[component]
pub fn override_form(
    overrides: Vec<(String, Value)>,
    default_config: Vec<DefaultConfig>,
    #[prop(into)] handle_change: Callback<Vec<(String, Value)>, ()>,
    #[prop(default = false)] auto_fill_from_default: bool,
    #[prop(into, default=String::new())] id: String,
    #[prop(default = false)] disable_remove: bool,
    #[prop(default = true)] show_add_override: bool,
    #[prop(into, optional)] handle_key_remove: Option<Callback<String, ()>>,
    #[prop(default = false)] disabled: bool,
    fn_environment: Memo<Value>,
) -> impl IntoView {
    let id = store_value(id);
    let default_config = store_value(default_config);
    let (override_keys, set_override_keys) = create_signal(HashSet::<String>::from_iter(
        overrides.clone().iter().map(|(k, _)| String::from(k)),
    ));
    let (overrides, set_overrides) = create_signal(overrides);

    let default_config_map: HashMap<String, DefaultConfig> = default_config
        .get_value()
        .into_iter()
        .map(|ele| (ele.key.clone(), ele))
        .collect();

    let handle_config_key_select = Callback::new(move |default_config: DefaultConfig| {
        let config_key = default_config.key;

        if let Ok(config_type) = SchemaType::try_from(default_config.schema) {
            let def_value = if auto_fill_from_default {
                default_config.value
            } else {
                config_type.default_value()
            };
            set_overrides.update(|value| {
                value.push((config_key.clone(), def_value));
            });
            set_override_keys.update(|keys| {
                keys.insert(config_key);
            })
        }
    });

    let on_change = Callback::new(move |(config_key_value, value): (String, Value)| {
        set_overrides.update(|curr_overrides| {
            let position = curr_overrides
                .iter()
                .position(|(k, _)| *k == config_key_value);
            if let Some(idx) = position {
                curr_overrides[idx].1 = value;
            }
        });
    });

    let on_remove = Callback::new(move |key: String| {
        match handle_key_remove {
            Some(f) => f.call(key),
            None => {
                set_overrides.update(|value| {
                    let position = value.iter().position(|(k, _)| *k == key.clone());
                    if let Some(idx) = position {
                        value.remove(idx);
                    }
                });
                set_override_keys.update(|keys| {
                    keys.remove(&key.clone());
                })
            }
        };
    });

    let workspace = use_context::<Signal<Tenant>>().unwrap();
    let org_id = use_context::<Signal<OrganisationId>>().unwrap();

    let autocomplete_callbacks = default_config
        .get_value()
        .iter()
        .filter_map(|d| {
            autocomplete_fn_generator(
                d.key.clone(),
                d.autocomplete_function_name.clone(),
                fn_environment,
                workspace.get_untracked().0,
                org_id.get_untracked().0,
            )
        })
        .collect::<AutoCompleteCallbacks>();

    create_effect(move |_| {
        let f_override = overrides.get();
        handle_change.call(f_override.clone());
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
                                    dropdown_direction=DropdownDirection::Down
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
                                let schema = default_config_map
                                    .get(&config_key)
                                    .map(|config| config.schema.clone())
                                    .unwrap_or_default();
                                let schema_type = SchemaType::try_from(schema.clone());
                                let enum_variants = EnumVariants::try_from(schema);
                                view! {
                                    <OverrideInput
                                        id=format!("{}-{}", id.get_value(), config_key)
                                        key=config_key
                                        value=config_value
                                        r#type=schema_type.ok()
                                        variants=enum_variants.ok()
                                        on_change=on_change
                                        on_remove=on_remove
                                        allow_remove=!disable_remove
                                        disabled
                                        autocomplete_callbacks=autocomplete_callbacks.clone()
                                    />
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
        </div>
    }
}
