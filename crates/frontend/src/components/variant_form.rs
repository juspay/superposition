use std::collections::{HashMap, HashSet};

use chrono::Local;
use leptos::*;
use serde_json::Value;

use crate::{
    components::{
        dropdown::{Dropdown, DropdownBtnType, DropdownDirection},
        override_form::OverrideForm,
    },
    schema::SchemaType,
    types::{DefaultConfig, VariantFormT, VariantType},
};

fn get_override_keys_from_variants(
    variants: &[(String, VariantFormT)],
) -> HashSet<String> {
    variants
        .iter()
        .flat_map(|(_, variant)| {
            variant
                .overrides
                .iter()
                .map(|(k, _)| String::from(k))
                .collect::<Vec<String>>()
        })
        .collect::<HashSet<String>>()
}

fn get_init_state(variants: &[(String, VariantFormT)]) -> HashSet<String> {
    get_override_keys_from_variants(variants)
}

#[component]
pub fn variant_form<HC>(
    edit: bool,
    variants: Vec<(String, VariantFormT)>,
    default_config: Vec<DefaultConfig>,
    handle_change: HC,
) -> impl IntoView
where
    HC: Fn(Vec<(String, VariantFormT)>) + 'static + Clone,
{
    let init_override_keys = get_init_state(&variants);
    let (f_variants, set_variants) = create_signal(variants);
    let (override_keys, set_override_keys) = create_signal(init_override_keys);

    let key_to_type = StoredValue::new(
        default_config
            .iter()
            .map(|d| (d.key.clone(), SchemaType::try_from(d.schema.clone()).ok()))
            .collect::<HashMap<String, Option<SchemaType>>>(),
    );
    let default_config = StoredValue::new(default_config);
    let handle_change = StoredValue::new(handle_change);
    let unused_config_keys = Signal::derive(move || {
        default_config
            .get_value()
            .into_iter()
            .filter(|config| !override_keys.get().contains(&config.key))
            .collect::<Vec<DefaultConfig>>()
    });

    let on_key_remove = move |removed_key: String| {
        logging::log!("Removing key {:?}", removed_key);

        set_override_keys.update(|current_override_keys| {
            current_override_keys.remove(&removed_key);
        });
        set_variants.update(|current_variants| {
            for variant in current_variants.iter_mut() {
                let position = variant
                    .1
                    .overrides
                    .iter()
                    .position(|(k, _)| k.to_owned() == removed_key);
                if let Some(idx) = position {
                    variant.1.overrides.remove(idx);
                }
            }
        });
    };

    let on_override_change = move |variant_idx: usize| {
        move |updated_overrides: Vec<(String, Value)>| {
            set_variants.update_untracked(|curr_variants| {
                curr_variants[variant_idx]
                    .1
                    .overrides
                    .clone_from(&updated_overrides);
                handle_change.get_value()(curr_variants.clone());
            });
        }
    };

    let on_key_select = Callback::new(move |default_config: DefaultConfig| {
        let config_key = default_config.key;
        if let Ok(config_type) = SchemaType::try_from(default_config.schema) {
            let def_value = config_type.default_value();

            set_variants.update(|current_variants: &mut Vec<(String, VariantFormT)>| {
                for (_, variant) in current_variants.iter_mut() {
                    variant
                        .overrides
                        .push((config_key.clone(), def_value.clone()));
                }
            });
            set_override_keys.update(|value: &mut HashSet<String>| {
                value.insert(config_key.clone());
            });
        }
    });

    let on_add_variant = move |_: web_sys::MouseEvent| {
        logging::log!("add new variant");
        set_variants.update(|curr_variants| {
            let key = Local::now().timestamp().to_string();
            let overrides = override_keys
                .get()
                .iter()
                .filter_map(|key| {
                    key_to_type
                        .with_value(|v| v.get(key).cloned())
                        .flatten()
                        .map(|r#type| (key.clone(), r#type.default_value()))
                })
                .collect::<Vec<(String, Value)>>();
            curr_variants.push((
                key,
                VariantFormT {
                    id: String::new(),
                    variant_type: VariantType::EXPERIMENTAL,
                    overrides,
                },
            ))
        });
    };

    create_effect(move |_| {
        let f_variants = f_variants.get();
        handle_change.get_value()(f_variants.clone());
    });

    view! {
        <div class="form-control w-full">
            <div class="flex items-center justify-between gap-4">
                <label class="label flex-col justify-center items-start">
                    <span class="label-text font-semibold text-base">Experiment Variants</span>
                    <span class="label-text text-slate-400">
                        "These are the override sets that would apply on the above context"
                    </span>
                </label>

            </div>
            <For
                each=move || {
                    f_variants
                        .get()
                        .into_iter()
                        .enumerate()
                        .collect::<Vec<(usize, (String, VariantFormT))>>()
                }

                key=|(idx, (key, _))| format!("{}-{}", key, idx)
                children=move |(idx, (key, variant))| {
                    let is_control_variant = variant.variant_type == VariantType::CONTROL;
                    let handle_change = on_override_change(idx);
                    let variant_type_label = match variant.variant_type {
                        VariantType::CONTROL => "Control".to_string(),
                        VariantType::EXPERIMENTAL => format!("Variant {idx}"),
                    };
                    let show_remove_btn = key != "control-variant" && key != "experimental-variant"
                        && !is_control_variant;
                    let key = StoredValue::new(key);
                    view! {
                        <div class="my-2 p-4 rounded bg-gray-50">
                            <div class="flex items-center justify-between">
                                <label class="label">
                                    <span class="label-text font-semibold text-base">
                                        {variant_type_label}
                                    </span>
                                </label>
                                <Show when=move || {
                                    is_control_variant && !override_keys.get().is_empty()
                                }>
                                    <Dropdown
                                        dropdown_btn_type=DropdownBtnType::Link
                                        dropdown_direction=DropdownDirection::Left
                                        dropdown_text=String::from("Add Override")
                                        dropdown_icon=String::from("ri-add-line")
                                        dropdown_options=unused_config_keys.get()
                                        on_select=on_key_select
                                    />
                                </Show>
                                <Show when=move || show_remove_btn>
                                    <button
                                        class="btn btn-sm btn-circle btn-ghost"
                                        on:click=move |_| {
                                            set_variants
                                                .update(|cvariants| {
                                                    let position = cvariants
                                                        .iter()
                                                        .position(|(k, _)| k.as_str() == key.get_value().as_str());
                                                    if let Some(idx) = position {
                                                        cvariants.remove(idx);
                                                    }
                                                })
                                        }
                                    >

                                        <i class="ri-close-line"></i>
                                    </button>
                                </Show>
                            </div>
                            <div class="flex items-center gap-4 my-4">
                                <div class="form-control">
                                    <label class="label">
                                        <span class="label-text">ID</span>
                                    </label>
                                </div>
                                <div class="form-control w-2/5">
                                    <input
                                        name="variantId"
                                        value=move || variant.id.to_string()
                                        disabled=edit
                                        type="text"
                                        placeholder="Type a unique name here"
                                        class="input input-bordered w-full max-w-xs h-10"
                                        on:input=move |event| {
                                            let variant_id = event_target_value(&event);
                                            set_variants
                                                .update(|
                                                    current_variants: &mut Vec<(String, VariantFormT)>|
                                                {
                                                    let variant_to_be_updated = current_variants.get_mut(idx);
                                                    match variant_to_be_updated {
                                                        Some((_, ref mut variant)) => {
                                                            variant.id = variant_id;
                                                        }
                                                        None => {
                                                            logging::log!(
                                                                "variant not found to update with id: {:?}", variant_id
                                                            )
                                                        }
                                                    }
                                                });
                                        }
                                    />

                                </div>
                            </div>
                            <div class="mt-2">
                                <Show when=move || {
                                    is_control_variant && override_keys.get().is_empty()
                                }>
                                    <div class="my-4 flex flex-col justify-between items-center">
                                        <Dropdown
                                            dropdown_btn_type=DropdownBtnType::Link
                                            dropdown_direction=DropdownDirection::Left
                                            dropdown_text=String::from("Add Override")
                                            dropdown_icon=String::from("ri-add-line")
                                            dropdown_options=unused_config_keys.get()
                                            on_select=on_key_select
                                        />
                                        <div>
                                            <span class="label-text text-slate-400 text-sm">
                                                "Add keys from your config that you want to override in this experiment"
                                            </span>
                                        </div>
                                    </div>
                                </Show>

                                <Show when=move || {
                                    !is_control_variant && override_keys.get().is_empty()
                                }>
                                    <div class="my-4 flex flex-col justify-between items-center">
                                        <span class="label-text text-slate-400 text-sm">
                                            "Keys added in CONTROL will appear here as well for override"
                                        </span>
                                    </div>
                                </Show>

                                <Show when=move || {
                                    !override_keys.get().is_empty()
                                }>
                                    {move || {
                                        let variant = f_variants.get().get(idx).unwrap().clone();
                                        let overrides = variant.1.overrides;
                                        if is_control_variant {
                                            view! {
                                                <OverrideForm
                                                    id=key.get_value()
                                                    overrides=overrides
                                                    default_config=default_config.get_value()
                                                    handle_change=handle_change
                                                    show_add_override=false
                                                    handle_key_remove=Some(Callback::new(on_key_remove))
                                                />
                                            }
                                        } else {
                                            view! {
                                                <OverrideForm
                                                    id=key.get_value()
                                                    overrides=overrides
                                                    default_config=default_config.get_value()
                                                    handle_change=handle_change
                                                    show_add_override=false
                                                    disable_remove=true
                                                />
                                            }
                                        }
                                    }}

                                </Show>

                            </div>
                        </div>
                    }
                }
            />

            <div>
                <button
                    class="btn btn-purple-outline btn-sm text-xs m-1"
                    disabled=edit
                    on:click:undelegated=on_add_variant
                >

                    <i class="ri-add-line"></i>
                    Add Variant
                </button>
            </div>

        </div>
    }
}
