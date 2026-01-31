use std::collections::{HashMap, HashSet};

use chrono::Local;
use leptos::*;
use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};
use superposition_types::{
    api::{
        config::ResolveConfigQuery, functions::FunctionEnvironment,
        workspace::WorkspaceResponse,
    },
    custom_query::DimensionQuery,
    database::models::{cac::DefaultConfig, experimentation::VariantType},
};

use crate::{
    api::{get_context_from_condition, resolve_config},
    components::{
        dropdown::{Dropdown, DropdownBtnType, DropdownDirection},
        override_form::OverrideForm,
        skeleton::{Skeleton, SkeletonVariant},
    },
    logic::Conditions,
    schema::SchemaType,
    types::{OrganisationId, VariantFormT, Workspace},
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
pub fn VariantForm<HC>(
    edit: bool,
    #[prop(into)] context: Signal<Conditions>,
    variants: Vec<(String, VariantFormT)>,
    default_config: Vec<DefaultConfig>,
    handle_change: HC,
    fn_environment: Memo<FunctionEnvironment>,
) -> impl IntoView
where
    HC: Fn(Vec<(String, VariantFormT)>) + 'static + Clone,
{
    let workspace_settings = use_context::<StoredValue<WorkspaceResponse>>().unwrap();
    let workspace_rws = use_context::<Signal<Workspace>>().unwrap();
    let org_rws = use_context::<Signal<OrganisationId>>().unwrap();
    let init_override_keys = get_init_state(&variants);
    let (f_variants, set_variants) = create_signal(variants);
    let (override_keys, set_override_keys) = create_signal(init_override_keys);

    let key_to_type = StoredValue::new(
        default_config
            .iter()
            .map(|d| {
                (
                    d.key.clone(),
                    SchemaType::try_from(&d.schema as &Map<String, Value>).ok(),
                )
            })
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

    let resolved_config_resource = create_blocking_resource(
        move || {
            (
                context.get(),
                workspace_rws.get_untracked().0,
                org_rws.get_untracked().0,
                workspace_settings.with_value(|w| w.auto_populate_control),
            )
        },
        |(context, workspace, org_id, auto_populate_control)| async move {
            if auto_populate_control {
                let context = DimensionQuery::from(context.as_resolve_context());
                resolve_config(
                    &context,
                    &ResolveConfigQuery {
                        resolve_remote: Some(true),
                        ..Default::default()
                    },
                    &workspace,
                    &org_id,
                )
                .await
                .unwrap_or_default()
            } else {
                Map::new()
            }
        },
    );

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
                    .position(|(k, _)| *k == removed_key);
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

    let on_key_select = Callback::new(
        move |(resolved_config, default_config): (Map<String, Value>, DefaultConfig)| {
            let config_key = default_config.key;

            if let Ok(config_type) =
                SchemaType::try_from(&default_config.schema as &Map<String, Value>)
            {
                let def_value = config_type.default_value();
                let resolved_val = workspace_settings
                    .with_value(|w| w.auto_populate_control)
                    .then(|| resolved_config.get(&config_key).cloned())
                    .flatten()
                    .unwrap_or(def_value.clone());

                set_variants.update(
                    |current_variants: &mut Vec<(String, VariantFormT)>| {
                        for (_, variant) in current_variants.iter_mut() {
                            let value = if variant.variant_type == VariantType::CONTROL {
                                resolved_val.clone()
                            } else {
                                def_value.clone()
                            };
                            variant.overrides.push((config_key.clone(), value));
                        }
                    },
                );
                set_override_keys.update(|value: &mut HashSet<String>| {
                    value.insert(config_key.clone());
                });
            }
        },
    );

    let auto_populate_control =
        Callback::new(move |(resolved_config, idx): (Map<String, Value>, usize)| {
            let control_overrides = override_keys
                .get()
                .iter()
                .filter_map(|k| {
                    resolved_config
                        .get(k)
                        .map(|default_value| (k.clone(), default_value.clone()))
                })
                .collect::<Vec<_>>();

            if control_overrides != f_variants.get()[idx].1.overrides {
                set_variants.update(|curr_variants: &mut Vec<(String, VariantFormT)>| {
                    curr_variants[idx]
                        .1
                        .overrides
                        .clone_from(&control_overrides);
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
            <Suspense fallback=move || {
                view! { <Skeleton variant=SkeletonVariant::Block /> }
            }>
                {move || {
                    f_variants
                        .get()
                        .iter()
                        .cloned()
                        .enumerate()
                        .map(move |(idx, (key, variant))| {
                            let is_control_variant = variant.variant_type == VariantType::CONTROL;
                            let handle_change = on_override_change(idx);
                            let variant_type_label = match variant.variant_type {
                                VariantType::CONTROL => "Control".to_string(),
                                VariantType::EXPERIMENTAL => format!("Variant {idx}"),
                            };
                            let show_remove_btn = key != "control-variant"
                                && key != "experimental-variant" && !is_control_variant;
                            let key = StoredValue::new(key);
                            let overrides = StoredValue::new(variant.overrides);
                            let resolved_config = StoredValue::new(
                                if is_control_variant {
                                    resolved_config_resource.get().unwrap_or_default()
                                } else {
                                    Map::new()
                                },
                            );
                            Effect::new(move |_| {
                                let resolved_config = resolved_config_resource
                                    .get()
                                    .unwrap_or_default();
                                if !edit && is_control_variant && !resolved_config.is_empty() {
                                    auto_populate_control.call((resolved_config, idx));
                                }
                            });

                            view! {
                                <div class="my-2 p-4 rounded bg-gray-50">
                                    <div class="flex items-center justify-between">
                                        <label class="label label-text font-semibold text-base">
                                            {variant_type_label}
                                        </label>
                                        <div class="flex items-center gap-4">
                                            <Show when=move || {
                                                workspace_settings.with_value(|w| w.auto_populate_control)
                                                    && edit && is_control_variant
                                            }>
                                                <button
                                                    class="btn btn-sm text-xs btn-purple-link cursor-pointer flex items-center justify-center"
                                                    on:click=move |_| {
                                                        auto_populate_control
                                                            .call((resolved_config.get_value(), idx));
                                                    }
                                                >
                                                    <i class="ri-paint-line"></i>
                                                    Auto Populate
                                                </button>
                                            </Show>
                                            <Show when=move || {
                                                is_control_variant && !override_keys.get().is_empty()
                                            }>
                                                <Dropdown
                                                    dropdown_btn_type=DropdownBtnType::Link
                                                    dropdown_direction=DropdownDirection::Left
                                                    dropdown_text=String::from("Add Override")
                                                    dropdown_icon=String::from("ri-add-line")
                                                    dropdown_options=unused_config_keys.get()
                                                    on_select=move |config_key| {
                                                        on_key_select
                                                            .call((resolved_config.get_value(), config_key))
                                                    }
                                                />
                                            </Show>
                                        </div>
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
                                                <i class="ri-close-line" />
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
                                                                Some((_, variant)) => {
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
                                                    dropdown_direction=DropdownDirection::Down
                                                    dropdown_text=String::from("Add Override")
                                                    dropdown_icon=String::from("ri-add-line")
                                                    dropdown_options=unused_config_keys.get()
                                                    on_select=move |config_key| {
                                                        on_key_select
                                                            .call((resolved_config.get_value(), config_key))
                                                    }
                                                />
                                                <span class="label-text text-slate-400 text-sm text-center">
                                                    "Add keys from your config that you want to override in this experiment"
                                                </span>
                                            </div>
                                        </Show>

                                        <Show when=move || {
                                            !is_control_variant && override_keys.get().is_empty()
                                        }>
                                            <span class="my-4 label-text text-slate-400 text-sm text-center">
                                                "Keys added in CONTROL will appear here as well for override"
                                            </span>
                                        </Show>

                                        <Show when=move || {
                                            !override_keys.get().is_empty()
                                        }>
                                            {move || {
                                                if is_control_variant {
                                                    view! {
                                                        <OverrideForm
                                                            id=key.get_value()
                                                            overrides=overrides.get_value()
                                                            default_config=default_config.get_value()
                                                            handle_change=handle_change
                                                            show_add_override=false
                                                            handle_key_remove=on_key_remove
                                                            fn_environment
                                                            disabled=workspace_settings
                                                                .with_value(|w| w.auto_populate_control)
                                                        />
                                                    }
                                                } else {
                                                    view! {
                                                        <OverrideForm
                                                            id=key.get_value()
                                                            overrides=overrides.get_value()
                                                            default_config=default_config.get_value()
                                                            handle_change=handle_change
                                                            show_add_override=false
                                                            disable_remove=true
                                                            fn_environment
                                                        />
                                                    }
                                                }
                                            }}
                                        </Show>

                                    </div>
                                </div>
                            }
                        })
                        .collect_view()
                }}
            </Suspense>

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

#[component]
pub fn DeleteVariant(
    edit: bool,
    key: String,
    variant: VariantFormT,
    default_config: Vec<DefaultConfig>,
    label: String,
    #[prop(default = true)] show_remove_btn: bool,
    #[prop(into)] on_variant_change: Callback<VariantFormT, ()>,
    #[prop(into)] on_override_change: Callback<Vec<(String, Value)>, ()>,
    #[prop(into)] on_delete_variant: Callback<(), ()>,
    fn_environment: Memo<FunctionEnvironment>,
) -> impl IntoView {
    let variant_rws = RwSignal::new(variant);
    let override_keys = Signal::derive(move || {
        variant_rws
            .get()
            .overrides
            .iter()
            .map(|(k, _)| k.clone())
            .collect::<HashSet<String>>()
    });

    let key = StoredValue::new(key);
    let default_config = StoredValue::new(default_config);

    let unused_config_keys = Signal::derive(move || {
        default_config
            .get_value()
            .into_iter()
            .filter(|config| !override_keys.get().contains(&config.key))
            .collect::<Vec<DefaultConfig>>()
    });

    Effect::new(move |_| on_variant_change.call(variant_rws.get()));

    view! {
        <div class="my-2 p-4 rounded bg-gray-50">
            <div class="flex items-center justify-between">
                <label class="label">
                    <span class="label-text font-semibold text-base">{label}</span>
                </label>
                <div class="flex items-center gap-4">
                    <Show when=move || !override_keys.get().is_empty()>
                        <Dropdown
                            dropdown_btn_type=DropdownBtnType::Link
                            dropdown_direction=DropdownDirection::Left
                            dropdown_text=String::from("Add Keys to Remove")
                            dropdown_icon=String::from("ri-add-line")
                            dropdown_options=unused_config_keys.get()
                            on_select=move |default_config: DefaultConfig| {
                                variant_rws
                                    .update(|variant| {
                                        variant
                                            .overrides
                                            .push((default_config.key, default_config.value));
                                    });
                            }
                        />
                    </Show>
                    <Show when=move || show_remove_btn>
                        <button
                            class="btn btn-sm btn-circle btn-ghost"
                            on:click=move |_| on_delete_variant.call(())
                        >
                            <i class="ri-close-line" />
                        </button>
                    </Show>
                </div>
            </div>
            <div class="flex items-center gap-4 my-4">
                <div class="form-control">
                    <label class="label">
                        <span class="label-text">ID</span>
                    </label>
                    <input
                        name="variantId"
                        value=move || variant_rws.get_untracked().id.to_string()
                        disabled=edit
                        type="text"
                        placeholder="Type a unique name here"
                        class="input input-bordered w-full max-w-xs h-10"
                        on:input=move |event| {
                            let variant_id = event_target_value(&event);
                            variant_rws
                                .update(|variant| {
                                    variant.id = variant_id;
                                });
                        }
                    />
                </div>
            </div>
            <div class="mt-2">
                <Show when=move || { override_keys.get().is_empty() }>
                    <div class="my-4 flex flex-col justify-between items-center">
                        <Dropdown
                            dropdown_btn_type=DropdownBtnType::Link
                            dropdown_direction=DropdownDirection::Down
                            dropdown_text=String::from("Add Keys to Remove")
                            dropdown_icon=String::from("ri-add-line")
                            dropdown_options=unused_config_keys.get()
                            on_select=move |default_config: DefaultConfig| {
                                variant_rws
                                    .update(|variant| {
                                        variant
                                            .overrides
                                            .push((default_config.key, default_config.value));
                                    });
                            }
                        />
                        <span class="label-text text-slate-400 text-sm text-center">
                            "Add keys from your override that you want to delete from the context's overrides"
                        </span>
                    </div>
                </Show>

                <Show when=move || {
                    !override_keys.get().is_empty()
                }>
                    {move || {
                        view! {
                            <OverrideForm
                                disabled=true
                                id=key.get_value()
                                overrides=variant_rws.get().overrides
                                default_config=default_config.get_value()
                                handle_change=on_override_change
                                show_add_override=false
                                handle_key_remove=move |removed_key| {
                                    variant_rws
                                        .update(|variant| {
                                            variant.overrides.retain(|(k, _)| *k != removed_key);
                                        });
                                }
                                fn_environment
                            />
                        }
                    }}
                </Show>
            </div>
        </div>
    }
            .into_view()
}

#[derive(Serialize, Deserialize, Clone, Default)]
struct CombinedResource {
    overrides: Map<String, Value>,
    default_config: Vec<DefaultConfig>,
}

#[component]
pub fn DeleteVariantForm<HC>(
    edit: bool,
    context: Conditions,
    #[prop(default = None)] context_data: Option<(String, Map<String, Value>)>,
    variants: Vec<(String, VariantFormT)>,
    default_config: Vec<DefaultConfig>,
    handle_change: HC,
    fn_environment: Memo<FunctionEnvironment>,
) -> impl IntoView
where
    HC: Fn(Vec<(String, VariantFormT)>) + 'static + Clone,
{
    let variants_rws = RwSignal::new(variants);
    let workspace = use_context::<Signal<Workspace>>().unwrap();
    let org = use_context::<Signal<OrganisationId>>().unwrap();

    let combined_resource = create_blocking_resource(
        move || {
            (
                context.clone(),
                context_data.clone(),
                default_config.clone(),
                workspace.get_untracked().0,
                org.get_untracked().0,
            )
        },
        |(context, context_data, default_config, workspace, org_id)| async move {
            let context_data = match context_data {
                Some(data) => Ok(data),
                None => get_context_from_condition(
                    &context.as_context_json(),
                    &workspace,
                    &org_id,
                )
                .await
                .map(|context| (context.id, context.override_.into())),
            };
            match context_data {
                Ok((context_id, overrides)) => {
                    let context = DimensionQuery::from(context.as_resolve_context());
                    let resolved_config = resolve_config(
                        &context,
                        &ResolveConfigQuery {
                            context_id: Some(context_id),
                            ..Default::default()
                        },
                        &workspace,
                        &org_id,
                    )
                    .await
                    .unwrap_or_default();

                    let override_keys = overrides.keys().cloned().collect::<HashSet<_>>();
                    let default_config = default_config
                        .iter()
                        .filter(|&d| override_keys.contains(&d.key))
                        .filter_map(|d| {
                            resolved_config.get(&d.key).map(|v| DefaultConfig {
                                value: v.clone(),
                                ..d.clone()
                            })
                        })
                        .collect::<Vec<_>>();

                    Ok(CombinedResource {
                        overrides,
                        default_config,
                    })
                }
                Err(e) => Err(e),
            }
        },
    );

    let handle_change = StoredValue::new(handle_change);

    let on_add_variant = move |_: web_sys::MouseEvent| {
        variants_rws.update(|curr_variants| {
            let key = Local::now().timestamp().to_string();
            curr_variants.push((
                key,
                VariantFormT {
                    id: String::new(),
                    variant_type: VariantType::EXPERIMENTAL,
                    overrides: vec![],
                },
            ))
        });
    };

    Effect::new(move |_| handle_change.get_value()(variants_rws.get()));

    let update_control_variant =
        move |curr_variants: &mut Vec<(String, VariantFormT)>| {
            let control_keys = curr_variants
                .iter()
                .filter_map(|(_, v)| {
                    (v.variant_type != VariantType::CONTROL).then_some(
                        v.overrides
                            .iter()
                            .map(|(k, _)| k.clone())
                            .collect::<Vec<_>>(),
                    )
                })
                .flatten()
                .collect::<HashSet<_>>();

            let overrides = combined_resource
                .get()
                .and_then(|r| r.map(|c| c.overrides).ok())
                .unwrap_or_default();

            if let Some(idx) = curr_variants
                .iter()
                .position(|(_, v)| v.variant_type == VariantType::CONTROL)
            {
                if let Some((_, control_variant)) = curr_variants.get_mut(idx) {
                    control_variant.overrides = control_keys
                        .iter()
                        .filter_map(|k| overrides.get(k).map(|v| (k.clone(), v.clone())))
                        .collect();
                }
            }
        };

    view! {
        <Suspense fallback=move || view! { <Skeleton variant=SkeletonVariant::Block /> }>
            <div class="form-control w-full">
                <div class="label flex flex-col items-start">
                    <span class="label-text font-semibold text-base">Experiment Variants</span>
                    <span class="label-text text-slate-400">
                        "These are the override sets that would apply on the above context, for deleting the keys from the existing context's overrides. The value of the keys will be picked from the immediate-parent context."
                    </span>
                </div>
                {move || {
                    let default_config = match combined_resource.get() {
                        None => {
                            return view! { <div>Loading...</div> }.into_view();
                        }
                        Some(Err(_)) => {
                            return view! {
                                <div>
                                    "An error occured while fetching data for the existing context. Check if the context still exists. Discard the experiment if the context does not exist any more."
                                </div>
                            }
                                .into_view();
                        }
                        Some(Ok(combined_resource)) => combined_resource.default_config,
                    };
                    view! {
                        <For
                            each=move || {
                                variants_rws
                                    .get()
                                    .into_iter()
                                    .enumerate()
                                    .collect::<Vec<(usize, (String, VariantFormT))>>()
                            }

                            key=|(idx, (key, _))| format!("{}-{}", key, idx)
                            children=move |(idx, (key, variant))| {
                                if variant.variant_type == VariantType::CONTROL {
                                    return ().into_view();
                                }
                                let on_override_change = move |
                                    updated_overrides: Vec<(String, Value)>|
                                {
                                    variants_rws
                                        .update_untracked(|curr_variants| {
                                            curr_variants[idx]
                                                .1
                                                .overrides
                                                .clone_from(&updated_overrides);
                                            update_control_variant(curr_variants);
                                            handle_change.get_value()(curr_variants.clone());
                                        });
                                };
                                let on_variant_change = move |variant| {
                                    variants_rws
                                        .update(|current_variants| {
                                            current_variants[idx].1 = variant;
                                            update_control_variant(current_variants);
                                        });
                                };
                                let on_delete_variant = move |_| {
                                    variants_rws
                                        .update(|current_variants| {
                                            current_variants.remove(idx);
                                            update_control_variant(current_variants);
                                        });
                                };
                                view! {
                                    <DeleteVariant
                                        edit=edit
                                        key=key
                                        variant=variant.clone()
                                        show_remove_btn=idx != 1 && !edit
                                        default_config=default_config.clone()
                                        label=format!("Variant {idx}")
                                        on_variant_change
                                        on_override_change
                                        on_delete_variant
                                        fn_environment
                                    />
                                }
                                    .into_view()
                            }
                        />
                    }
                }}

                <button
                    class="w-fit btn btn-purple-outline btn-sm text-xs m-1"
                    disabled=edit
                    on:click:undelegated=on_add_variant
                >
                    <i class="ri-add-line" />
                    Add Variant
                </button>

            </div>
        </Suspense>
    }
}
