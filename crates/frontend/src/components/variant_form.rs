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
    #[prop(default = false)] vertical_split: bool,
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
    let (show_add_override_menu, set_show_add_override_menu) = create_signal(false);
    let (add_override_search_term, set_add_override_search_term) =
        create_signal(String::new());

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
    let filtered_unused_config_keys = Signal::derive(move || {
        let term = add_override_search_term.get().to_lowercase();
        unused_config_keys
            .get()
            .into_iter()
            .filter(|config| config.key.to_lowercase().contains(&term))
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
                let context = DimensionQuery::from(Map::from(context));
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
                            variant.overrides.insert(0, (config_key.clone(), value));
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
            let override_keys = override_keys.get();
            let existing_control_overrides = f_variants
                .get()
                .get(idx)
                .map(|(_, variant)| variant.overrides.clone())
                .unwrap_or_default();
            let mut control_overrides = Vec::new();

            for (key, _) in existing_control_overrides {
                if override_keys.contains(&key) {
                    if let Some(default_value) = resolved_config.get(&key) {
                        control_overrides.push((key, default_value.clone()));
                    }
                }
            }

            for key in &override_keys {
                if control_overrides
                    .iter()
                    .any(|(existing_key, _)| existing_key == key)
                {
                    continue;
                }
                if let Some(default_value) = resolved_config.get(key) {
                    control_overrides.push((key.clone(), default_value.clone()));
                }
            }

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

    let vertical_split = StoredValue::new(vertical_split);

    view! {
        <div class="form-control w-full">
            <div class="flex items-end justify-between gap-4">
                <label class="label flex-col justify-center items-start">
                    <span class="label-text font-semibold text-base">Experiment Variants</span>
                    <span class="label-text text-slate-400">
                        "These are the override sets that would apply on the above context"
                    </span>
                </label>
                <div class="relative">
                    <button
                        class="btn btn-sm text-xs m-1 w-fit btn-purple-outline"
                        disabled=move || unused_config_keys.get().is_empty()
                        on:click=move |_| {
                            set_show_add_override_menu.update(|show| *show = !*show);
                        }
                    >
                        <i class="ri-add-line"></i>
                        "Add Override"
                    </button>
                    <Show when=move || show_add_override_menu.get()>
                        <div class="w-96 absolute right-0 top-full mt-1 z-[1000] p-2 shadow bg-base-100 rounded-box border border-slate-200">
                            <div class="mb-3">
                                <label class="input input-bordered flex items-center gap-2 h-10">
                                    <i class="ri-search-line"></i>
                                    <input
                                        type="text"
                                        class="grow"
                                        placeholder="Search"
                                        value=add_override_search_term.get()
                                        on:input=move |event| {
                                            set_add_override_search_term
                                                .set(event_target_value(&event));
                                        }
                                    />
                                </label>
                            </div>
                            <ul class="menu flex-nowrap max-h-96 overflow-y-scroll overflow-x-hidden p-0">
                                <For
                                    each=move || filtered_unused_config_keys.get()
                                    key=|option: &DefaultConfig| option.key.clone()
                                    children=move |config_key: DefaultConfig| {
                                        let option_label = config_key.key.clone();
                                        let selected_option = config_key.clone();
                                        view! {
                                            <li class="w-full">
                                                <a
                                                    class="w-full word-break-break"
                                                    on:click=move |_| {
                                                        on_key_select
                                                            .call((
                                                                resolved_config_resource.get().unwrap_or_default(),
                                                                selected_option.clone(),
                                                            ));
                                                        set_add_override_search_term.set(String::new());
                                                        set_show_add_override_menu.set(false);
                                                    }
                                                >
                                                    {option_label.clone()}
                                                </a>
                                            </li>
                                        }
                                    }
                                />
                            </ul>
                        </div>
                    </Show>
                </div>
            </div>
            <Suspense fallback=move || {
                view! { <Skeleton variant=SkeletonVariant::Block /> }
            }>
                <div
                    class=move || {
                        if vertical_split.get_value() {
                            "flex flex-row items-start gap-4 overflow-x-auto overflow-y-visible pb-9"
                                .to_string()
                        } else {
                            String::new()
                        }
                    }
                    style:min-height=move || {
                        if !vertical_split.get_value() {
                            "auto".to_string()
                        } else {
                            let max_override_count = f_variants
                                .get()
                                .iter()
                                .map(|(_, variant)| variant.overrides.len())
                                .max()
                                .unwrap_or(0);
                            let base_height = 360usize;
                            let additional_height = max_override_count.saturating_sub(3) * 56;
                            format!("{}px", base_height + additional_height)
                        }
                    }
                >
                    {move || {
                        f_variants
                            .get()
                            .iter()
                            .cloned()
                            .enumerate()
                            .map(move |(idx, (key, variant))| {
                                let is_control_variant = variant.variant_type
                                    == VariantType::CONTROL;
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
                                let card_class = move || {
                                    if vertical_split.get_value() {
                                        "my-1 p-2 rounded bg-gray-50 flex-shrink-0 w-[calc(50%-2rem)] overflow-visible"
                                            .to_string()
                                    } else {
                                        "my-1 p-2 rounded bg-gray-50 overflow-visible".to_string()
                                    }
                                };

                                view! {
                                    <div class=card_class>
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
                                                <span class="my-4 label-text text-slate-400 text-sm text-center">
                                                    "Add keys from your config using the Add Override button above"
                                                </span>
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
                </div>
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
    original_override_keys: HashSet<String>,
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
        let keys = original_override_keys
            .difference(&override_keys.get())
            .map(String::to_owned)
            .collect::<Vec<String>>();
        default_config
            .get_value()
            .into_iter()
            .filter(|d| keys.contains(&d.key))
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
                    <Show when=move || !unused_config_keys.get().is_empty()>
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
    delete_override_default_config: Vec<DefaultConfig>,
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
                    &context.clone().into(),
                    &workspace,
                    &org_id,
                )
                .await
                .map(|context| (context.id, context.override_.into())),
            };
            match context_data {
                Ok((context_id, overrides)) => {
                    let context = DimensionQuery::from(Map::from(context));
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
                    let delete_override_default_config = default_config
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
                        delete_override_default_config,
                    })
                }
                Err(e) => Err(e),
            }
        },
    );

    let handle_change = StoredValue::new(handle_change);

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
                <div class="flex items-end justify-between gap-4">
                    <div class="label flex flex-col items-start">
                        <span class="label-text font-semibold text-base">Experiment Variants</span>
                        <span class="label-text text-slate-400">
                            "These are the override sets that would apply on the above context, for deleting the keys from the existing context's overrides. The value of the keys will be picked from the immediate-parent context."
                        </span>
                    </div>
                </div>
                {move || {
                    let (original_override_keys, default_config) = match combined_resource.get() {
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
                        Some(Ok(combined_resource)) => {
                            (
                                HashSet::from_iter(
                                    combined_resource.overrides.keys().map(String::to_owned),
                                ),
                                combined_resource.delete_override_default_config,
                            )
                        }
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
                                        original_override_keys=original_override_keys.clone()
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

            </div>
        </Suspense>
    }
}
