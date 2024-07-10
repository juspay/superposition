use std::collections::HashSet;

use chrono::Local;
use leptos::*;
use serde_json::{json, Value};

use crate::{
    components::{
        dropdown::{Dropdown, DropdownBtnType, DropdownDirection},
        override_form::OverrideForm,
    },
    types::{DefaultConfig, VariantFormT, VariantFormTSignal, VariantType, Override},
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
    f_variants: ReadSignal<Vec<(String, VariantFormTSignal)>>,
    set_variants: WriteSignal<Vec<(String, VariantFormTSignal)>>,
    default_config: Vec<DefaultConfig>,
    handle_change: HC,
) -> impl IntoView
where
    HC: Fn(Vec<(String, VariantFormT)>) + 'static + Clone,
{
    let init_override_keys = get_init_state(
        &(f_variants.get()
        .into_iter()
        .map(|(key, ele)| {
        (key, ele.to_variant_form_t())
    }).collect::<Vec<(String,VariantFormT)>>()));
    // let (f_variants, set_variants) = create_signal(variants);
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
                let position = variant
                    .1
                    .overrides
                    .get()
                    .iter()
                    .position(|ele| ele.key.get().to_owned() == removed_key);
                if let Some(idx) = position {
                    variant.1.overrides.get().remove(idx);
                }
            }
        });
    };

    let handle_override_form_change = move |variant_idx: usize| {
        move |updated_overrides: Vec<(String, Value)>| {
            set_variants.update_untracked(|curr_variants| {
                curr_variants[variant_idx]
                    .1
                    .overrides.get()
                    .clone_from(&Override::from_vec(updated_overrides));
                handle_change.get_value()(curr_variants.clone().into_iter().map(|(key, ele)| {
                    (key, VariantFormTSignal::to_variant_form_t(&ele))
                }).collect());
            });
        }
    };

    create_effect(move |_| {
        let f_variants = f_variants.get();
        handle_change.get_value()(f_variants.clone().into_iter().map(|(key, ele)| {
            (key, ele.to_variant_form_t())
        }).collect());
    });

    let handle_config_key_select = Callback::new(move |default_config: DefaultConfig| {
        let config_key = default_config.key;
        set_variants.update(|current_variants: &mut Vec<(String, VariantFormTSignal)>| {
            for (_, variant) in current_variants.iter_mut() {
                variant.set_overrides.update(|overrides| overrides.push(Override::new(config_key.clone(), json!(""))));
            }
        });
        set_override_keys.update(|value: &mut HashSet<String>| {
            value.insert(config_key.clone());
        });
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
                        .collect::<Vec<(usize, (String, VariantFormTSignal))>>()
                }

                key=|(_, (key, _))| key.to_string()
                children=move |(idx, (_, variant))| {
                    logging::log!("<<>> variant {:?}", variant);
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
                                    is_control_variant && !override_keys.get().is_empty()
                                }>
                                    <Dropdown
                                        dropdown_btn_type=DropdownBtnType::Link
                                        dropdown_direction=DropdownDirection::Left
                                        dropdown_text=String::from("Add Override")
                                        dropdown_icon=String::from("ri-add-line")
                                        dropdown_options=unused_config_keys.get()
                                        on_select=handle_config_key_select
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
                                                .update(|
                                                    current_variants: &mut Vec<(String, VariantFormTSignal)>|
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
                                            on_select=handle_config_key_select
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
                                        let set_overrides = variant.1.set_overrides;
                                        if is_control_variant {
                                            view! {
                                                <OverrideForm
                                                    overrides=overrides
                                                    set_overrides
                                                    default_config=default_config.get_value()
                                                    // handle_change=handle_change
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
                                                    set_overrides
                                                    default_config=default_config.get_value()
                                                    // handle_change=handle_change
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
                                let overrides = override_keys
                                    .get()
                                    .into_iter()
                                    .map(|key| { (key, json!("")) })
                                    .collect();
                                curr_variants
                                    .push((
                                        key,
                                        VariantFormTSignal::new( 
                                            format!("variant-{}", total_variants),
                                            VariantType::EXPERIMENTAL,
                                            overrides,
                                        ),
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
