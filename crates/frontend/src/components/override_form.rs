use std::collections::{HashMap, HashSet};

use leptos::*;
use serde_json::{Map, Value};
use superposition_types::{
    api::{
        default_config::DefaultConfigFilters,
        functions::{FunctionEnvironment, KeyType},
    },
    custom_query::PaginationParams,
    database::models::cac::DefaultConfig,
};

use crate::{
    api::default_configs,
    components::{
        dropdown::{Dropdown, DropdownDirection, utils::DropdownOption},
        input::{Input, InputType},
    },
    schema::{EnumVariants, SchemaType},
    types::{OrganisationId, ValueComputeCallbacks, Workspace},
    utils::value_compute_fn_generator,
};

/// Fetches the default-config list once and derives a map from each
/// symlinked key to its target. A page may render more than one
/// `OverrideForm` (e.g. one per experiment variant); call this once near
/// the top of that page or its top-level form component - not once per
/// `OverrideForm` instance - and pass the resulting `Signal` into every
/// instance's `symlink_map` prop, so the fetch happens once per page
/// rather than multiplying per form.
///
/// Yields `None` while the fetch is pending *and* if it fails - both cases
/// mean "not known yet," which `OverrideForm` treats as a reason to hold
/// off on asserting there's no collision, not as "zero symlinks."
pub fn use_symlink_map(
    workspace: Signal<Workspace>,
    org_id: Signal<OrganisationId>,
) -> Signal<Option<HashMap<String, String>>> {
    let resource = create_resource(
        move || (workspace.get().0, org_id.get().0),
        |(workspace, org_id)| async move {
            default_configs::list_resolved(
                &PaginationParams::all_entries(),
                &DefaultConfigFilters::default(),
                &workspace,
                &org_id,
            )
            .await
            .ok()
            .map(|r| {
                r.data
                    .into_iter()
                    .filter_map(|d| d.symlink_to.map(|target| (d.config.key, target)))
                    .collect::<HashMap<String, String>>()
            })
        },
    );
    Signal::derive(move || resource.get().flatten())
}

/// The key an override entry will actually be stored under: a symlinked key
/// is redirected to its target, same as the server-side rewrite this mirrors
/// (see `crates/context_aware_config/src/symlinks.rs::apply_symlink_map`).
fn effective_key(key: &str, links: &HashMap<String, String>) -> String {
    links.get(key).cloned().unwrap_or_else(|| key.to_string())
}

/// The state of the symlink-collision check against `symlink_map`.
///
/// `symlink_map` is `None` both while its fetch is still pending *and* if it
/// failed - either way, the caller doesn't yet know which keys are
/// symlinks, so it would be wrong to report `Clear`: that reads as "checked,
/// no collision," when the honest answer is "not checked yet." Only
/// `Clear` permits propagating a change outward.
#[derive(Clone, PartialEq)]
enum SymlinkCheck {
    Pending,
    Clear,
    Collision { first: String, second: String, target: String },
}

/// A default-config key as it appears in the "Add Override" picker. A
/// symlink's label also carries the `-> target` badge text so the
/// write-through is visible before the override is committed, not as a
/// surprise on reload - a symlink stays selectable like any other key.
#[derive(Clone, PartialEq)]
struct OverrideKeyOption {
    config: DefaultConfig,
    symlink_target: Option<String>,
}

impl DropdownOption for OverrideKeyOption {
    fn key(&self) -> String {
        self.config.key.clone()
    }
    fn label(&self) -> String {
        match &self.symlink_target {
            Some(target) => format!("{}  →  {target}", self.config.key),
            None => self.config.key.clone(),
        }
    }
}

#[component]
fn TypeBadge(r#type: Option<SchemaType>) -> impl IntoView {
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
        SchemaType::Any => view! { <div class="badge badge-outline text-gray-400 font-medium text-xs">"any"</div> }
        .into_view(),
    })
}

#[component]
fn OverrideInput(
    id: String,
    key: String,
    value: Value,
    r#type: Option<SchemaType>,
    variants: Option<EnumVariants>,
    on_change: Callback<(String, Value), ()>,
    on_remove: Callback<String, ()>,
    allow_remove: bool,
    disabled: bool,
    value_compute_callbacks: ValueComputeCallbacks,
    #[prop(default = None)] symlink_target: Option<String>,
) -> impl IntoView {
    let value_compute_callback = value_compute_callbacks.get(&key).cloned();
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
                        {symlink_target.clone().map(|target| view! {
                            <span class="badge badge-sm badge-ghost" title="symlink">
                                <i class="ri-links-line mr-1" />
                                {format!("→ {target}")}
                            </span>
                        })}
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
                            value_compute_function=value_compute_callback
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
pub fn OverrideForm(
    overrides: Vec<(String, Value)>,
    default_config: Vec<DefaultConfig>,
    #[prop(into)] handle_change: Callback<Vec<(String, Value)>, ()>,
    #[prop(default = false)] auto_fill_from_default: bool,
    #[prop(into, default=String::new())] id: String,
    #[prop(default = false)] disable_remove: bool,
    #[prop(default = true)] show_add_override: bool,
    #[prop(into, optional)] handle_key_remove: Option<Callback<String, ()>>,
    #[prop(default = false)] disabled: bool,
    /// Maps a symlinked key to the target it resolves to; `None` while the
    /// caller's fetch for this is still pending (or failed), `Some(map)`
    /// once it's known - `map` may itself be empty if there happen to be no
    /// symlinks, which is a different thing from not knowing yet. One fetch
    /// per page, done by the real caller (which may render more than one
    /// `OverrideForm`, e.g. one per variant) and passed down here as a
    /// `Signal` so this component's own state isn't torn down and rebuilt
    /// when the fetch resolves.
    #[prop(into)] symlink_map: Signal<Option<HashMap<String, String>>>,
    fn_environment: Memo<FunctionEnvironment>,
) -> impl IntoView {
    let id = store_value(id);
    let default_config = store_value(default_config);
    let (override_keys, set_override_keys) = create_signal(HashSet::<String>::from_iter(
        overrides.clone().iter().map(|(k, _)| String::from(k)),
    ));
    let (overrides, set_overrides) = create_signal(overrides);

    let workspace = use_context::<Signal<Workspace>>().unwrap();
    let org_id = use_context::<Signal<OrganisationId>>().unwrap();

    let symlink_check = Signal::derive(move || match symlink_map.get() {
        None => SymlinkCheck::Pending,
        Some(links) => {
            let mut seen: HashMap<String, String> = HashMap::new();
            override_keys
                .get()
                .into_iter()
                .find_map(|key| {
                    let target = effective_key(&key, &links);
                    match seen.insert(target.clone(), key.clone()) {
                        Some(first) if first != key => Some((first, key, target)),
                        _ => None,
                    }
                })
                .map(|(first, second, target)| SymlinkCheck::Collision { first, second, target })
                .unwrap_or(SymlinkCheck::Clear)
        }
    });

    let default_config_map: HashMap<String, DefaultConfig> = default_config
        .get_value()
        .into_iter()
        .map(|ele| (ele.key.clone(), ele))
        .collect();

    let handle_config_key_select = Callback::new(move |option: OverrideKeyOption| {
        let default_config = option.config;
        let config_key = default_config.key;

        if let Ok(config_type) =
            SchemaType::try_from(&default_config.schema as &Map<String, Value>)
        {
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

    let value_compute_callbacks = default_config
        .get_value()
        .iter()
        .filter_map(|d| {
            value_compute_fn_generator(
                d.key.clone(),
                d.value_compute_function_name.clone(),
                fn_environment,
                &KeyType::ConfigKey,
                workspace.get_untracked().0,
                org_id.get_untracked().0,
            )
        })
        .collect::<ValueComputeCallbacks>();

    create_effect(move |_| {
        let f_override = overrides.get();
        // Only propagate once the check is actually `Clear`: a `Collision`
        // is exactly what the server's symlink rewrite refuses
        // (`crates/context_aware_config/src/symlinks.rs::apply_symlink_map`),
        // and `Pending` means the check hasn't run yet - asserting "no
        // collision" by default during that window would be a false
        // negative, not a safe one.
        if matches!(symlink_check.get(), SymlinkCheck::Clear) {
            handle_change.call(f_override.clone());
        }
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
                        {move || match symlink_check.get() {
                            SymlinkCheck::Pending => Some(view! {
                                <div class="alert text-sm flex items-center gap-2">
                                    <span class="loading loading-spinner loading-xs"></span>
                                    <span>"Checking for symlink collisions…"</span>
                                </div>
                            }),
                            SymlinkCheck::Clear => None,
                            SymlinkCheck::Collision { first, second, target } => Some(view! {
                                <div class="alert alert-warning text-sm flex items-start gap-2">
                                    <i class="ri-alert-line text-lg"></i>
                                    <span>
                                        {
                                            format!(
                                                "override names both `{first}` and `{second}`, which resolve to the \
                                                 same config key `{target}`, with different values; keep one of them",
                                            )
                                        }
                                    </span>
                                </div>
                            }),
                        }}
                        <Show when=move || { overrides.get().is_empty() && show_add_override }>
                            <div class="flex justify-center">
                                {move || {
                                    let add_override_options = default_config
                                        .get_value()
                                        .into_iter()
                                        .map(|config| {
                                            let symlink_target = symlink_map
                                                .with(|opt| opt.as_ref().and_then(|m| m.get(&config.key).cloned()));
                                            OverrideKeyOption { config, symlink_target }
                                        })
                                        .collect::<Vec<OverrideKeyOption>>();
                                    view! {
                                        <Dropdown
                                            dropdown_direction=DropdownDirection::Down
                                            dropdown_text=String::from("Add Override")
                                            dropdown_icon=String::from("ri-add-line")
                                            dropdown_options=add_override_options
                                            on_select=handle_config_key_select
                                        />
                                    }
                                }}
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
                                let schema: &Map<String, Value> = &default_config_map
                                    .get(&config_key)
                                    .map(|config| config.schema.clone())
                                    .unwrap_or_default();
                                let schema_type = SchemaType::try_from(schema);
                                let enum_variants = EnumVariants::try_from(schema);
                                let symlink_target = symlink_map
                                    .with(|opt| opt.as_ref().and_then(|m| m.get(&config_key).cloned()));
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
                                        value_compute_callbacks=value_compute_callbacks.clone()
                                        symlink_target=symlink_target
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
                                        .map(|config| {
                                            let symlink_target = symlink_map
                                                .with(|opt| opt.as_ref().and_then(|m| m.get(&config.key).cloned()));
                                            OverrideKeyOption { config, symlink_target }
                                        })
                                        .collect::<Vec<OverrideKeyOption>>();
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
