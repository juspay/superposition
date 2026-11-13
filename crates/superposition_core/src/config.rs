#[cfg(test)]
mod tests;

use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
};

use serde_json::{json, Map, Value};
pub use superposition_types::api::config::MergeStrategy;
use superposition_types::{
    logic::evaluate_local_cohorts, Config, ConfigFilter, Context, DimensionInfo,
    ExtendedMap, Overrides, PrefixList,
};

struct ConfigRef<'a> {
    pub contexts: Vec<&'a Context>,
    pub overrides: Cow<'a, HashMap<String, Overrides>>,
    pub default_configs: ExtendedMap,
    pub dimensions: &'a HashMap<String, DimensionInfo>,
}

impl<'a> ConfigFilter<'a> for ConfigRef<'a> {
    type Context = &'a Context;
    type Dimensions = &'a HashMap<String, DimensionInfo>;

    fn into_parts(
        self,
    ) -> (
        Vec<Self::Context>,
        Cow<'a, HashMap<String, Overrides>>,
        ExtendedMap,
        Self::Dimensions,
    ) {
        (
            self.contexts,
            self.overrides,
            self.default_configs,
            self.dimensions,
        )
    }

    fn from_parts(
        contexts: Vec<Self::Context>,
        overrides: Cow<'a, HashMap<String, Overrides>>,
        default_configs: ExtendedMap,
        dimensions: Self::Dimensions,
    ) -> Self {
        Self {
            contexts,
            overrides,
            default_configs,
            dimensions,
        }
    }
}

/// To be used when you have full ownership of the `Config` struct and
/// want to evaluate it with a given set of dimensions.
pub fn eval_config(
    mut config: Config,
    query_data: Map<String, Value>,
    merge_strategy: MergeStrategy,
    filter_prefixes: Option<Vec<String>>,
    filter_exclude_prefixes: Option<Vec<String>>,
) -> Map<String, Value> {
    let filter_prefixes = PrefixList::from(filter_prefixes);
    let filter_exclude_prefixes = PrefixList::from(filter_exclude_prefixes);

    // Apply prefix filtering if keys are provided (using existing superposition_types logic)
    if !filter_prefixes.is_empty() || !filter_exclude_prefixes.is_empty() {
        config = config.filter_by_prefix(&filter_prefixes, &filter_exclude_prefixes);
    }

    let modified_query_data = evaluate_local_cohorts(&config.dimensions, query_data);

    let overrides_map: Map<String, Value> = get_overrides(
        &modified_query_data,
        config.contexts.iter().collect(),
        Cow::Owned(config.overrides),
        &merge_strategy,
    );

    // Apply overrides to default config
    let result = apply_overrides_on_default_config(
        config.default_configs,
        overrides_map,
        merge_strategy,
    );

    result.into_inner()
}

/// To be used when the ownership of the `Config` struct is `not available`
/// and you want to evaluate it with a given set of dimensions.
#[allow(clippy::too_many_arguments)]
pub fn eval(
    default_configs: ExtendedMap,
    contexts: &[Context],
    overrides: &HashMap<String, Overrides>,
    dimensions: &HashMap<String, DimensionInfo>,
    query_data: Map<String, Value>,
    merge_strategy: MergeStrategy,
    filter_prefixes: Option<Vec<String>>,
    filter_exclude_prefixes: Option<Vec<String>>,
) -> Map<String, Value> {
    eval_inner(
        default_configs,
        contexts,
        overrides,
        dimensions,
        query_data,
        merge_strategy,
        filter_prefixes,
        filter_exclude_prefixes,
        false,
    )
    .0
}

/// Same as [`eval`], but also returns, for every resolved key that a context
/// overrode, the context that set it last and so decided its value. Keys left at
/// their default value have no entry.
///
/// Both come out of the same pass over the contexts, so this costs one evaluation.
#[allow(clippy::too_many_arguments)]
pub fn eval_with_key_sources<'a>(
    default_configs: ExtendedMap,
    contexts: &'a [Context],
    overrides: &'a HashMap<String, Overrides>,
    dimensions: &'a HashMap<String, DimensionInfo>,
    query_data: Map<String, Value>,
    merge_strategy: MergeStrategy,
    filter_prefixes: Option<Vec<String>>,
    filter_exclude_prefixes: Option<Vec<String>>,
) -> (Map<String, Value>, HashMap<String, &'a Context>) {
    eval_inner(
        default_configs,
        contexts,
        overrides,
        dimensions,
        query_data,
        merge_strategy,
        filter_prefixes,
        filter_exclude_prefixes,
        true,
    )
}

/// Same as [`eval`], but also returns the variants that actually took effect.
///
/// `query_data["variantIds"]` lists every variant the caller is bucketed into, but a
/// variant whose context lost every key to a later context never reaches the resolved
/// config. Only the variants whose context is the source of at least one resolved key
/// (see [`eval_with_key_sources`]) are returned, in the order they appear in
/// `query_data["variantIds"]`.
#[allow(clippy::too_many_arguments)]
pub fn eval_with_applied_variants(
    default_configs: ExtendedMap,
    contexts: &[Context],
    overrides: &HashMap<String, Overrides>,
    dimensions: &HashMap<String, DimensionInfo>,
    query_data: Map<String, Value>,
    merge_strategy: MergeStrategy,
    filter_prefixes: Option<Vec<String>>,
    filter_exclude_prefixes: Option<Vec<String>>,
) -> (Map<String, Value>, Vec<String>) {
    let variant_ids = query_data.get("variantIds").cloned();
    let (config, key_sources) = eval_with_key_sources(
        default_configs,
        contexts,
        overrides,
        dimensions,
        query_data,
        merge_strategy,
        filter_prefixes,
        filter_exclude_prefixes,
    );
    (config, applied_variant_ids(variant_ids, &key_sources))
}

#[allow(clippy::too_many_arguments)]
fn eval_inner<'a>(
    default_configs: ExtendedMap,
    contexts: &'a [Context],
    overrides: &'a HashMap<String, Overrides>,
    dimensions: &'a HashMap<String, DimensionInfo>,
    query_data: Map<String, Value>,
    merge_strategy: MergeStrategy,
    filter_prefixes: Option<Vec<String>>,
    filter_exclude_prefixes: Option<Vec<String>>,
    track_sources: bool,
) -> (Map<String, Value>, HashMap<String, &'a Context>) {
    // Create Config struct to use existing filtering logic
    let mut config = ConfigRef {
        default_configs,
        contexts: contexts.iter().collect(),
        overrides: Cow::Borrowed(overrides),
        dimensions,
    };

    let filter_prefixes = PrefixList::from(filter_prefixes);
    let filter_exclude_prefixes = PrefixList::from(filter_exclude_prefixes);

    // Apply prefix filtering if keys are provided (using existing superposition_types logic)
    if !filter_prefixes.is_empty() || !filter_exclude_prefixes.is_empty() {
        config = config.filter_by_prefix(&filter_prefixes, &filter_exclude_prefixes);
    }

    let modified_query_data = evaluate_local_cohorts(config.dimensions, query_data);

    let mut key_sources = HashMap::new();
    let overrides_map: Map<String, Value> = merge_matching_overrides(
        &modified_query_data,
        config.contexts,
        config.overrides,
        &merge_strategy,
        track_sources.then_some(&mut key_sources),
    );

    // overrides for keys missing from the default config are dropped while applying,
    // so those keys have no source
    key_sources.retain(|key, _| config.default_configs.contains_key(key));

    // Apply overrides to default config
    let result = apply_overrides_on_default_config(
        config.default_configs,
        overrides_map,
        merge_strategy,
    );

    (result.into_inner(), key_sources)
}

/// The ids in `variant_ids` whose context is the source of at least one key.
fn applied_variant_ids(
    variant_ids: Option<Value>,
    key_sources: &HashMap<String, &Context>,
) -> Vec<String> {
    let applied: HashSet<&str> = key_sources
        .values()
        .filter_map(|context| context.condition.get("variantIds"))
        .filter_map(Value::as_str)
        .collect();

    match variant_ids {
        Some(Value::Array(ids)) => ids
            .into_iter()
            .filter_map(|id| match id {
                Value::String(id) if applied.contains(id.as_str()) => Some(id),
                _ => None,
            })
            .collect(),
        _ => Vec::new(),
    }
}

// TODO: Move explain function to this file to avoid making this funcition public
pub fn merge(doc: &mut Value, patch: Value) {
    match (doc, patch) {
        (Value::Object(map), Value::Object(obj)) => {
            for (key, value) in obj {
                merge(map.entry(key).or_insert(Value::Null), value);
            }
        }
        (doc, patch) => *doc = patch,
    }
}

/// Merges the overrides of every context matching `query_data` into a single map,
/// in context order, so that later contexts win.
///
/// Override ids are derived from the override's contents, so several contexts can
/// share one and each must apply it at its own position. An override is therefore
/// cloned only when more than one matching context needs it, and the last of those
/// contexts moves it out of `overrides` rather than cloning.
fn get_overrides(
    query_data: &Map<String, Value>,
    contexts: Vec<&Context>,
    overrides: Cow<'_, HashMap<String, Overrides>>,
    merge_strategy: &MergeStrategy,
) -> Map<String, Value> {
    merge_matching_overrides(query_data, contexts, overrides, merge_strategy, None)
}

/// [`get_overrides`], optionally recording in `key_sources` the context that last
/// wrote each key.
fn merge_matching_overrides<'c>(
    query_data: &Map<String, Value>,
    mut contexts: Vec<&'c Context>,
    mut overrides: Cow<'_, HashMap<String, Overrides>>,
    merge_strategy: &MergeStrategy,
    mut key_sources: Option<&mut HashMap<String, &'c Context>>,
) -> Map<String, Value> {
    // borrow the keys instead of allocating two Strings per matching context
    let mut final_consumer_context: HashMap<&str, &str> = HashMap::new();

    contexts.retain(|&context| {
        if !superposition_types::apply(&context.condition, query_data) {
            return false;
        }
        final_consumer_context.insert(
            context.override_with_keys.get_key().as_str(),
            context.id.as_str(),
        );
        true
    });

    let mut required_overrides: Value = json!({});

    for context in contexts {
        let override_key = context.override_with_keys.get_key();
        let is_last = final_consumer_context.get(override_key.as_str())
            == Some(&context.id.as_str());

        // move it out only when we own the map *and* this is its last consumer
        let overriden_value = match (&mut overrides, is_last) {
            (Cow::Owned(map), true) => map.remove(override_key).map(Cow::Owned),
            (map, _) => map.get(override_key).map(Cow::Borrowed),
        };

        let Some(overriden_value) = overriden_value else {
            continue;
        };

        if let Some(sources) = key_sources.as_deref_mut() {
            for key in overriden_value.keys() {
                sources.insert(key.clone(), context);
            }
        }

        match merge_strategy {
            MergeStrategy::REPLACE => {
                if let Some(doc) = required_overrides.as_object_mut() {
                    for (key, value) in overriden_value.into_owned().into_inner() {
                        doc.insert(key, value);
                    }
                }
            }
            MergeStrategy::MERGE => merge(
                &mut required_overrides,
                Value::Object(overriden_value.into_owned().into_inner()),
            ),
        }
    }

    match required_overrides {
        Value::Object(map) => map,
        _ => Map::new(),
    }
}

fn apply_overrides_on_default_config(
    mut default_config: ExtendedMap,
    overrides: Map<String, Value>,
    merge_strategy: MergeStrategy,
) -> ExtendedMap {
    overrides.into_iter().for_each(|(key, val)| {
        if let Some(og_val) = default_config.get_mut(&key) {
            match merge_strategy {
                MergeStrategy::REPLACE => {
                    default_config.insert(key, val);
                }
                MergeStrategy::MERGE => merge(og_val, val),
            }
        } else {
            log::error!("Config: found non-default_config key: {key} in overrides");
        }
    });
    default_config
}
