use std::collections::{HashMap, HashSet};

use leptos::*;
use serde::{Deserialize, Serialize};
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

/// Which keys are symlinks, as far as the browser knows.
///
/// A fetch *failure* is deliberately not the same thing as a fetch still in
/// flight. Collapsing the two into `None` meant one failed API call turned
/// every override edit on the page into a silent no-op, because the form
/// withheld its changes while "not known yet" and the submit button never
/// knew. Pending holds the submit back for the moment it takes to resolve;
/// `Unavailable` lets the edit through with a visible warning and leaves the
/// server - which performs the same check authoritatively in
/// `apply_symlink_map` - to refuse it if it really collides.
#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub enum SymlinkMapState {
    /// The fetch is still in flight.
    Pending,
    /// Known. May be empty, which means "there are no symlinks" - a different
    /// thing from not knowing yet.
    Ready(HashMap<String, String>),
    /// The fetch failed. The browser cannot check; the server still can.
    Unavailable,
}

impl SymlinkMapState {
    /// The target `key` resolves to, if it is a known symlink. Pending and
    /// failed both answer `None`: there is no badge to show for a map that
    /// hasn't arrived.
    pub fn target_of(&self, key: &str) -> Option<String> {
        match self {
            Self::Ready(links) => links.get(key).cloned(),
            Self::Pending | Self::Unavailable => None,
        }
    }
}

/// Fetches the default-config list once and derives a map from each
/// symlinked key to its target. A page may render more than one
/// `OverrideForm` (e.g. one per experiment variant); call this once near
/// the top of that page or its top-level form component - not once per
/// `OverrideForm` instance - and pass the resulting `Signal` into every
/// instance's `symlink_map` prop, so the fetch happens once per page
/// rather than multiplying per form.
pub fn use_symlink_map(
    workspace: Signal<Workspace>,
    org_id: Signal<OrganisationId>,
) -> Signal<SymlinkMapState> {
    let resource = create_resource(
        move || (workspace.get().0, org_id.get().0),
        |(workspace, org_id)| async move {
            match default_configs::list_resolved(
                &PaginationParams::all_entries(),
                &DefaultConfigFilters::default(),
                &workspace,
                &org_id,
            )
            .await
            {
                Ok(r) => SymlinkMapState::Ready(
                    r.data
                        .into_iter()
                        .filter_map(|d| d.symlink_to.map(|target| (d.config.key, target)))
                        .collect::<HashMap<String, String>>(),
                ),
                Err(e) => {
                    logging::error!("failed to fetch the symlink map: {e}");
                    SymlinkMapState::Unavailable
                }
            }
        },
    );
    Signal::derive(move || resource.get().unwrap_or(SymlinkMapState::Pending))
}

/// The key an override entry will actually be stored under: a symlinked key
/// is redirected to its target, same as the server-side rewrite this mirrors
/// (see `crates/context_aware_config/src/symlinks.rs::apply_symlink_map`).
fn effective_key(key: &str, links: &HashMap<String, String>) -> String {
    links.get(key).cloned().unwrap_or_else(|| key.to_string())
}

/// The state of the symlink-collision check for one override map.
///
/// The submitting parent gates on this: `OverrideForm` propagates every edit
/// outward unconditionally, so the parent always holds what the user typed,
/// and refuses to *send* it while the check says so. Withholding the edit
/// instead is what let a user see the warning, press Submit, and have the
/// last clear payload saved - losing the colliding key and every edit made
/// after it, with a success response.
#[derive(Clone, PartialEq, Debug)]
pub enum SymlinkCheck {
    /// The symlink map hasn't arrived; nothing can be asserted yet.
    Pending,
    /// Checked: no two keys resolve to the same target.
    Clear,
    /// The map couldn't be fetched, so the server is the only authority.
    Unavailable,
    Collision {
        first: String,
        second: String,
        target: String,
    },
}

impl SymlinkCheck {
    /// Whether a submit carrying this override map must be held back.
    pub fn blocks_submit(&self) -> bool {
        self.blocking_reason().is_some()
    }

    /// Why a submit is held back, phrased for the user.
    pub fn blocking_reason(&self) -> Option<String> {
        match self {
            Self::Pending => Some(
                "Still checking the overrides for symlink collisions; try again in \
                 a moment."
                    .to_string(),
            ),
            Self::Collision { .. } => self.warning(),
            Self::Clear | Self::Unavailable => None,
        }
    }

    /// The banner this check should show in the form, if any.
    pub fn warning(&self) -> Option<String> {
        match self {
            Self::Clear | Self::Pending => None,
            Self::Unavailable => Some(
                "Couldn't load the symlink list, so overrides can't be checked for \
                 collisions here; the server will still refuse a colliding one."
                    .to_string(),
            ),
            Self::Collision {
                first,
                second,
                target,
            } => Some(format!(
                "override names both `{first}` and `{second}`, which resolve to the \
                 same config key `{target}`, with different values; keep one of them"
            )),
        }
    }

    /// How loudly this reads, so several override maps can be reduced to the
    /// one worth reporting: a collision beats pending beats unavailable.
    fn severity(&self) -> u8 {
        match self {
            Self::Clear => 0,
            Self::Unavailable => 1,
            Self::Pending => 2,
            Self::Collision { .. } => 3,
        }
    }
}

/// Checks one override map's keys for two names that resolve to the same
/// config key - the collision the server refuses in `apply_symlink_map`.
///
/// Public because the gate belongs to whoever submits the payload, not to the
/// form that collects it.
pub fn check_symlink_collisions<'a>(
    state: &SymlinkMapState,
    keys: impl Iterator<Item = &'a String>,
) -> SymlinkCheck {
    let links = match state {
        SymlinkMapState::Pending => return SymlinkCheck::Pending,
        SymlinkMapState::Unavailable => return SymlinkCheck::Unavailable,
        SymlinkMapState::Ready(links) => links,
    };

    let mut seen: HashMap<String, String> = HashMap::new();
    for key in keys {
        let target = effective_key(key, links);
        if let Some(first) = seen.insert(target.clone(), key.clone()) {
            if first != *key {
                return SymlinkCheck::Collision {
                    first,
                    second: key.clone(),
                    target,
                };
            }
        }
    }
    SymlinkCheck::Clear
}

/// The check worth reporting out of several override maps (one per experiment
/// variant, say).
pub fn worst_check(checks: impl Iterator<Item = SymlinkCheck>) -> SymlinkCheck {
    checks
        .max_by_key(|check| check.severity())
        .unwrap_or(SymlinkCheck::Clear)
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
    /// Maps a symlinked key to the target it resolves to, with pending and
    /// failed kept apart (see [`SymlinkMapState`]). One fetch per page, done
    /// by the real caller (which may render more than one `OverrideForm`,
    /// e.g. one per variant) and passed down here as a `Signal` so this
    /// component's own state isn't torn down and rebuilt when the fetch
    /// resolves.
    #[prop(into)]
    symlink_map: Signal<SymlinkMapState>,
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

    // Shown here, but enforced by whoever submits: see `check_symlink_collisions`.
    let symlink_check = Signal::derive(move || {
        check_symlink_collisions(&symlink_map.get(), override_keys.get().iter())
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
        // Always propagate. The parent must hold exactly what the user typed,
        // or a gate on its side would be gating a stale payload - which is how
        // a colliding key and every edit after it used to be dropped with a
        // success response. The parent gates the *submit* on its own
        // `check_symlink_collisions` over the overrides it holds.
        handle_change.call(overrides.get());
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
                            SymlinkCheck::Clear => None,
                            SymlinkCheck::Pending => Some(view! {
                                <div class="alert text-sm flex items-center gap-2">
                                    <span class="loading loading-spinner loading-xs"></span>
                                    <span>"Checking for symlink collisions…"</span>
                                </div>
                            }),
                            check => check.warning().map(|message| view! {
                                <div class="alert alert-warning text-sm flex items-start gap-2">
                                    <i class="ri-alert-line text-lg"></i>
                                    <span>{message}</span>
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
                                                .with(|state| state.target_of(&config.key));
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
                                    .with(|state| state.target_of(&config_key));
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
                                                .with(|state| state.target_of(&config.key));
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

#[cfg(test)]
mod tests {
    use super::*;

    fn keys(keys: &[&str]) -> Vec<String> {
        keys.iter().map(|k| k.to_string()).collect()
    }

    fn ready(pairs: &[(&str, &str)]) -> SymlinkMapState {
        SymlinkMapState::Ready(
            pairs
                .iter()
                .map(|(k, v)| (k.to_string(), v.to_string()))
                .collect(),
        )
    }

    #[test]
    fn two_names_for_one_target_collide() {
        let state = ready(&[("payments.retry_count", "payments.retry.count")]);
        let keys = keys(&["payments.retry_count", "payments.retry.count"]);

        let check = check_symlink_collisions(&state, keys.iter());

        match check {
            SymlinkCheck::Collision { ref target, .. } => {
                assert_eq!(target, "payments.retry.count")
            }
            other => panic!("expected a collision, got {other:?}"),
        }
        assert!(check.blocks_submit());
        assert!(check.blocking_reason().is_some());
    }

    #[test]
    fn unrelated_keys_are_clear() {
        let state = ready(&[("payments.retry_count", "payments.retry.count")]);
        let keys = keys(&["payments.retry_count", "other.key"]);

        let check = check_symlink_collisions(&state, keys.iter());

        assert_eq!(check, SymlinkCheck::Clear);
        assert!(!check.blocks_submit());
        assert!(check.warning().is_none());
    }

    #[test]
    fn an_empty_map_is_known_and_clear() {
        // "Ready but empty" means there are no symlinks - not "not known yet".
        let check = check_symlink_collisions(&ready(&[]), keys(&["a.b"]).iter());
        assert_eq!(check, SymlinkCheck::Clear);
    }

    #[test]
    fn pending_blocks_the_submit_and_a_failed_fetch_does_not() {
        // The distinction the bug turned on: collapsing both into "not known"
        // made one failed API call silently swallow every override edit on the
        // page. A failure warns and defers to the server; pending just waits.
        let pending =
            check_symlink_collisions(&SymlinkMapState::Pending, keys(&["a.b"]).iter());
        assert_eq!(pending, SymlinkCheck::Pending);
        assert!(pending.blocks_submit());

        let unavailable = check_symlink_collisions(
            &SymlinkMapState::Unavailable,
            keys(&["a.b"]).iter(),
        );
        assert_eq!(unavailable, SymlinkCheck::Unavailable);
        assert!(
            !unavailable.blocks_submit(),
            "a failed fetch must not drop the edit; the server still checks"
        );
        assert!(
            unavailable.warning().is_some(),
            "a failed fetch must be visible rather than silent"
        );
    }

    #[test]
    fn target_of_only_answers_from_a_known_map() {
        let state = ready(&[("alias", "real.key")]);
        assert_eq!(state.target_of("alias").as_deref(), Some("real.key"));
        assert_eq!(state.target_of("real.key"), None);
        assert_eq!(SymlinkMapState::Pending.target_of("alias"), None);
        assert_eq!(SymlinkMapState::Unavailable.target_of("alias"), None);
    }

    #[test]
    fn the_worst_of_several_variants_is_what_gates_the_submit() {
        let collision = SymlinkCheck::Collision {
            first: "a".to_string(),
            second: "b".to_string(),
            target: "c".to_string(),
        };

        assert_eq!(
            worst_check([SymlinkCheck::Clear, SymlinkCheck::Unavailable].into_iter()),
            SymlinkCheck::Unavailable
        );
        assert_eq!(
            worst_check([SymlinkCheck::Unavailable, SymlinkCheck::Pending].into_iter()),
            SymlinkCheck::Pending
        );
        assert_eq!(
            worst_check([SymlinkCheck::Pending, collision.clone()].into_iter()),
            collision
        );
        assert_eq!(worst_check(std::iter::empty()), SymlinkCheck::Clear);
    }
}
