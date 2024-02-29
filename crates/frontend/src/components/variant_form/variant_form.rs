use std::collections::HashSet;

use chrono::Local;
use leptos::*;
use serde_json::{json, Map, Value};

use crate::{
    components::{
        dropdown::dropdown::{Dropdown, DropdownBtnType, DropdownDirection},
        override_form::override_form::OverrideForm,
    },
    types::{DefaultConfig, Variant, VariantType},
};

fn get_override_keys_from_variants(variants: &[(String, Variant)]) -> HashSet<String> {
    variants
        .iter()
        .map(|(_, variant)| {
            variant
                .overrides
                .keys()
                .map(String::from)
                .collect::<Vec<String>>()
        })
        .flatten()
        .collect::<HashSet<String>>()
}

fn get_init_state(variants: &[(String, Variant)]) -> HashSet<String> {
    let init_override_keys = get_override_keys_from_variants(variants);

    init_override_keys
}

#[component]
pub fn variant_form<HC>(
    edit: bool,
    variants: Vec<(String, Variant)>,
    default_config: Vec<DefaultConfig>,
    handle_change: HC,
) -> impl IntoView
where
    HC: Fn(Vec<(String, Variant)>) + 'static + Clone,
{
    let init_override_keys = get_init_state(&variants);
    let (f_variants, set_variants) = create_signal(variants);
    let (override_keys, set_override_keys) = create_signal(init_override_keys);

    let default_config = StoredValue::new(default_config);
    let handle_change = StoredValue::new(handle_change);
    let unused_config_keys = Signal::derive(move || {
        default_config
            .get_value()
            .into_iter()
            .filter(|config| !override_keys.get().contains(&config.key))
            .collect::<Vec<DefaultConfig>>()
    });

    let handle_control_override_key_remove = move |removed_key: String| {
        logging::log!("Removing key {:?}", removed_key);

        set_override_keys.update(|current_override_keys| {
            current_override_keys.remove(&removed_key);
        });
        set_variants.update(|current_variants| {
            for variant in current_variants.iter_mut() {
                variant.1.overrides.remove(&removed_key);
            }
        });
    };

    let handle_override_form_change = move |variant_idx: usize| {
        let callback = move |updated_overrides: Map<String, Value>| {
            set_variants.update_untracked(|curr_variants| {
                curr_variants[variant_idx].1.overrides = updated_overrides.clone();
                handle_change.get_value()(curr_variants.clone());
            });
        };
        callback
    };

    create_effect(move |_| {
        let f_variants = f_variants.get();
        handle_change.get_value()(f_variants.clone());
    });

    let handle_config_key_select = move |default_config: DefaultConfig| {
        let config_key = default_config.key;
        set_variants.update(|current_variants: &mut Vec<(String, Variant)>| {
            for (_, variant) in current_variants.iter_mut() {
                variant.overrides.insert(config_key.clone(), json!(""));
            }
        });
        set_override_keys.update(|value: &mut HashSet<String>| {
            value.insert(config_key.clone());
        });
    };

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
                        .collect::<Vec<(usize, (String, Variant))>>()
                }

                key=|(_, (key, _))| key.to_string()
                children=move |(idx, (_, variant))| {
                    let is_control_variant = variant.variant_type == VariantType::CONTROL;
                    let handle_change = handle_override_form_change(idx);
                    let variant_type_label = match variant.variant_type {
                        VariantType::CONTROL => "Control".to_string(),
                        VariantType::EXPERIMENTAL => format!("Variant {idx}"),
                    };
                    view! {
                        <div class="my-2 p-4 rounded bg-gray-50">
                            <div class="flex items-center justify-between">
                                <label class="label">
                                    <span class="label-text font-semibold text-base">
                                        {variant_type_label}
                                    </span>
                                </label>
                                <Show when=move || {
                                    is_control_variant && override_keys.get().len() > 0
                                }>
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
                                                .update(|current_variants: &mut Vec<(String, Variant)>| {
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
                                    is_control_variant && override_keys.get().len() == 0
                                }>
                                    <div class="my-4 flex flex-col justify-between items-center">
                                        <Dropdown
                                            dropdown_btn_type=DropdownBtnType::Link
                                            dropdown_direction=DropdownDirection::Left
                                            dropdown_text=String::from("Add Override")
                                            dropdown_icon=String::from("ri-add-line")
                                            dropdown_options=unused_config_keys.get()
                                            on_select=Box::new(handle_config_key_select)
                                        />
                                        <div>
                                            <span class="label-text text-slate-400 text-sm">
                                                "Add keys from your config that you want to override in this experiment"
                                            </span>
                                        </div>
                                    </div>
                                </Show>

                                <Show when=move || {
                                    !is_control_variant && override_keys.get().len() == 0
                                }>
                                    <div class="my-4 flex flex-col justify-between items-center">
                                        <span class="label-text text-slate-400 text-sm">
                                            "Keys added in CONTROL will appear here as well for override"
                                        </span>
                                    </div>
                                </Show>

                                <Show when=move || {
                                    override_keys.get().len() > 0
                                }>
                                    {move || {
                                        let variant = f_variants.get().get(idx).unwrap().clone();
                                        let overrides = variant.1.overrides;
                                        if is_control_variant {
                                            view! {
                                                <OverrideForm
                                                    overrides=overrides
                                                    default_config=default_config.get_value()
                                                    handle_change=handle_change
                                                    is_standalone=false
                                                    show_add_override=false
                                                    handle_key_remove=Some(
                                                        Callback::new(handle_control_override_key_remove),
                                                    )
                                                />
                                            }
                                        } else {
                                            view! {
                                                <OverrideForm
                                                    overrides=overrides
                                                    default_config=default_config.get_value()
                                                    handle_change=handle_change
                                                    is_standalone=false
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
                    on:click:undelegated=move |_| {
                        leptos::logging::log!("add new variant");
                        set_variants
                            .update(|curr_variants| {
                                let total_variants = curr_variants.len();
                                let key = Local::now().timestamp().to_string();
                                let overrides = Map::from_iter(
                                    override_keys.get().into_iter().map(|key| { (key, json!("")) }),
                                );
                                curr_variants
                                    .push((
                                        key,
                                        Variant {
                                            id: format!("variant-{}", total_variants),
                                            variant_type: VariantType::EXPERIMENTAL,
                                            context_id: None,
                                            override_id: None,
                                            overrides: overrides,
                                        },
                                    ))
                            });
                    }
                >

                    <i class="ri-add-line"></i>
                    Add Variant
                </button>
            </div>

        </div>
    }
}
