use serde_json::{Map, Value};
use service_utils::service::types::{SchemaName, WorkspaceContext};
use superposition_core::{config::eval, helpers::hash};
use superposition_macros::unexpected_error;
use superposition_types::{
    Cac, Condition, Config, DBConnection, Overrides, api::config::MergeStrategy,
    database::models::cac::Context as DbContext, result as superposition,
};

use crate::helpers::generate_cac;

const VARIANT_IDS_DIMENSION: &str = "variantIds";

/// Config snapshot to check redundancy against. Loads the whole workspace, so build it once per request.
pub struct AutoReducer {
    config: Config,
}

pub struct Reduction {
    pub kept: Map<String, Value>,
    pub dropped: Vec<String>,
}

impl Reduction {
    pub fn is_fully_redundant(&self) -> bool {
        self.kept.is_empty() && !self.dropped.is_empty()
    }
}

impl AutoReducer {
    pub fn from_config(config: Config) -> Self {
        Self { config }
    }

    pub fn new(
        conn: &mut DBConnection,
        schema_name: &SchemaName,
    ) -> superposition::Result<Self> {
        Ok(Self {
            config: generate_cac(conn, schema_name)?,
        })
    }

    /// Drops keys `condition` already resolves to. `context_id` is excluded, else it would match itself.
    pub fn reduce(
        &self,
        context_id: &str,
        condition: &Condition,
        overrides: &Overrides,
    ) -> Reduction {
        let query_data: Map<String, Value> = condition.clone().into_inner();

        let contexts: Vec<_> = self
            .config
            .contexts
            .iter()
            .filter(|ctx| ctx.id != context_id)
            .cloned()
            .collect();

        let resolve = |strategy: MergeStrategy| {
            eval(
                self.config.default_configs.clone(),
                &contexts,
                &self.config.overrides,
                &self.config.dimensions,
                query_data.clone(),
                strategy,
                None,
                None,
            )
        };

        // Both strategies must agree: callers pick one at resolve time.
        let merged = resolve(MergeStrategy::MERGE);
        let replaced = resolve(MergeStrategy::REPLACE);

        let mut kept = Map::new();
        let mut dropped = Vec::new();

        for (key, value) in overrides.clone().into_inner() {
            let redundant =
                merged.get(&key) == Some(&value) && replaced.get(&key) == Some(&value);
            if redundant {
                dropped.push(key);
            } else {
                kept.insert(key, value);
            }
        }

        dropped.sort();
        Reduction { kept, dropped }
    }
}

/// The `x-auto-reduce` header wins; without it, the workspace setting applies.
pub fn is_enabled(header: Option<bool>, workspace_context: &WorkspaceContext) -> bool {
    header.unwrap_or(workspace_context.settings.enable_auto_reduce)
}

pub fn build_if_enabled(
    enabled: bool,
    conn: &mut DBConnection,
    schema_name: &SchemaName,
) -> superposition::Result<Option<AutoReducer>> {
    if !enabled {
        return Ok(None);
    }
    AutoReducer::new(conn, schema_name).map(Some)
}

pub enum ReducedContext {
    Unchanged(DbContext),
    Trimmed {
        context: DbContext,
        dropped: Vec<String>,
    },
    /// Every key was redundant, so there is nothing to write.
    FullyRedundant {
        context: DbContext,
        dropped: Vec<String>,
    },
}

/// Run auto-reduce over a fully built context that is about to be written.
pub fn apply(
    reducer: Option<&AutoReducer>,
    context: DbContext,
    schema_name: &SchemaName,
) -> superposition::Result<ReducedContext> {
    let Some(reducer) = reducer else {
        return Ok(ReducedContext::Unchanged(context));
    };

    // Experiments pin these by id and override_id; trimming one breaks them.
    if context.value.contains_key(VARIANT_IDS_DIMENSION) {
        return Ok(ReducedContext::Unchanged(context));
    }

    let reduction = reducer.reduce(&context.id, &context.value, &context.override_);
    if reduction.dropped.is_empty() {
        return Ok(ReducedContext::Unchanged(context));
    }

    if reduction.is_fully_redundant() {
        log::info!(
            "auto_reduce[{}]: context {} is fully redundant, skipping write; dropped keys: {:?}",
            schema_name.0,
            context.id,
            reduction.dropped
        );
        return Ok(ReducedContext::FullyRedundant {
            context,
            dropped: reduction.dropped,
        });
    }

    log::info!(
        "auto_reduce[{}]: dropping redundant keys {:?} from context {}",
        schema_name.0,
        reduction.dropped,
        context.id
    );

    let mut context = context;
    // override_id is what resolution looks the override up by.
    context.override_id = hash(&Value::Object(reduction.kept.clone()));
    context.override_ = Cac::<Overrides>::try_from(reduction.kept)
        .map_err(|err| {
            log::error!("auto_reduce: reduced overrides rejected: {err}");
            unexpected_error!(err)
        })?
        .into_inner();

    Ok(ReducedContext::Trimmed {
        context,
        dropped: reduction.dropped,
    })
}

#[cfg(test)]
mod tests;
