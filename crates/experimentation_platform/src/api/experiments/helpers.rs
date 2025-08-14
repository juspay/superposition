use std::collections::HashSet;

use actix_http::header;
use actix_web::web::Data;
use cac_client::utils::json_to_sorted_string;
use chrono::Utc;
use diesel::{
    pg::PgConnection,
    r2d2::{ConnectionManager, PooledConnection},
    BoolExpressionMethods, ExpressionMethods, QueryDsl, RunQueryDsl, SelectableHelper,
};
use serde_json::{Map, Value};
#[cfg(feature = "jsonlogic")]
use service_utils::helpers::extract_dimensions;
use service_utils::service::types::{
    AppState, ExperimentationFlags, SchemaName, WorkspaceContext,
};
use superposition_macros::{bad_argument, unexpected_error};
use superposition_types::database::models::experimentation::GroupType;
use superposition_types::{
    api::{experiment_groups::ExpGroupMemberRequest, I64Update},
    database::{
        models::{
            experimentation::{Experiment, ExperimentStatusType, Variant, VariantType},
            others::{Webhook, WebhookEvent},
            ChangeReason, Workspace,
        },
        schema::experiments::dsl as experiments,
        superposition_schema::superposition::workspaces,
    },
    result as superposition, Condition, Config, DBConnection, Exp, Overrides, User,
};

pub fn get_workspace(
    workspace_schema_name: &SchemaName,
    db_conn: &mut DBConnection,
) -> superposition::Result<Workspace> {
    let workspace = workspaces::dsl::workspaces
        .filter(workspaces::workspace_schema_name.eq(workspace_schema_name.to_string()))
        .get_result::<Workspace>(db_conn)?;
    Ok(workspace)
}
use crate::api::experiment_groups::helpers::{
    add_members, create_system_generated_experiment_group, fetch_experiment_group,
    remove_members,
};

use super::cac_api::{
    construct_header_map, get_context_override, get_partial_resolve_config,
    get_resolved_config,
};

pub fn check_variant_types(variants: &Vec<Variant>) -> superposition::Result<()> {
    let mut experimental_variant_cnt = 0;
    let mut control_variant_cnt = 0;

    for variant in variants {
        match variant.variant_type {
            VariantType::CONTROL => {
                control_variant_cnt += 1;
            }
            VariantType::EXPERIMENTAL => {
                experimental_variant_cnt += 1;
            }
        }
    }

    if control_variant_cnt > 1 || control_variant_cnt == 0 {
        return Err(bad_argument!(
            "Experiment should have exactly 1 control variant. Ensure only one control variant is present"
        ));
    } else if experimental_variant_cnt < 1 {
        return Err(bad_argument!(
            "Experiment should have at least 1 experimental variant. Ensure only one control variant is present"
        ));
    }

    Ok(())
}

pub fn validate_override_keys(override_keys: &Vec<String>) -> superposition::Result<()> {
    let mut key_set: HashSet<&str> = HashSet::new();
    for key in override_keys {
        if !key_set.insert(key) {
            return Err(bad_argument!(
                "override_keys are not unique. Remove duplicate entries in override_keys"
            ));
        }
    }

    Ok(())
}

pub fn hash(val: &Value) -> String {
    let sorted_str: String = json_to_sorted_string(val);
    blake3::hash(sorted_str.as_bytes()).to_string()
}

pub fn are_overlapping_contexts(
    context_a: &Condition,
    context_b: &Condition,
) -> superposition::Result<bool> {
    cfg_if::cfg_if! {
        if #[cfg(feature = "jsonlogic")] {
            let dimensions_a = extract_dimensions(context_a)?;
            let dimensions_b = extract_dimensions(context_b)?;
        } else {
            let dimensions_a = context_a;
            let dimensions_b = context_b;
        }
    }

    let dim_a_keys = dimensions_a.keys();
    let dim_b_keys = dimensions_b.keys();

    let ref_keys = if dim_a_keys.len() > dim_b_keys.len() {
        dim_b_keys
    } else {
        dim_a_keys
    };

    let mut is_overlapping = true;
    for key in ref_keys {
        let test = (dimensions_a.contains_key(key) && dimensions_b.contains_key(key))
            && (dimensions_a[key] == dimensions_b[key]);
        is_overlapping = is_overlapping && test;

        if !test {
            break;
        }
    }

    Ok(is_overlapping)
}

pub fn check_variant_override_coverage(
    variant_override: &Overrides,
    override_keys: &Vec<String>,
) -> bool {
    if variant_override.keys().len() != override_keys.len() {
        return false;
    }

    for override_key in override_keys {
        if variant_override.get(override_key).is_none() {
            return false;
        }
    }
    true
}

pub fn check_variants_override_coverage(
    variant_overrides: &Vec<Overrides>,
    override_keys: &Vec<String>,
) -> bool {
    for variant_override in variant_overrides {
        if !check_variant_override_coverage(variant_override, override_keys) {
            return false;
        }
    }

    true
}

fn validate_variants_delete_keys(
    control_variant_overrides: &Overrides,
    variant_delete_overrides: &[Overrides],
    override_keys: &[String],
) -> bool {
    let mut delete_keys = HashSet::new();

    for delete_variant in variant_delete_overrides.iter() {
        for key in delete_variant.keys() {
            if !override_keys.contains(key) {
                return false;
            }
            if !control_variant_overrides.contains_key(key) {
                return false;
            }

            delete_keys.insert(key);
        }
    }
    if delete_keys.len() != control_variant_overrides.len() {
        return false;
    }
    true
}

fn validate_keys_from_source(
    variant_override: &Overrides,
    source: &Map<String, Value>,
) -> bool {
    for (override_key, value) in variant_override.iter() {
        if let Some(val) = source.get(override_key) {
            if val != value {
                return false;
            }
        } else {
            return false;
        }
    }
    true
}

fn validate_variants_delete_override_value(
    delete_variant_overrides: &Vec<Overrides>,
    resolved_config: &Map<String, Value>,
) -> bool {
    for override_ in delete_variant_overrides {
        if !validate_keys_from_source(override_, resolved_config) {
            return false;
        }
    }
    true
}

fn validate_variants_control_override_value(
    control_variant_overrides: &Overrides,
    current_context_overrides: &Map<String, Value>,
) -> bool {
    validate_keys_from_source(control_variant_overrides, current_context_overrides)
}

pub async fn validate_delete_experiment_variants(
    user: &User,
    state: &Data<AppState>,
    exp_context: &Condition,
    context_id: &str,
    workspace_request: &WorkspaceContext,
    variants: &[Variant],
) -> superposition::Result<()> {
    let current_context =
        get_context_override(user, state, workspace_request, context_id.to_owned())
            .await?;

    let partial_resolved_config = get_partial_resolve_config(
        user,
        state,
        exp_context,
        context_id,
        workspace_request,
    )
    .await?;

    let control_variant_override = variants
        .iter()
        .find(|variant| variant.variant_type == VariantType::CONTROL)
        .map(|variant| variant.overrides.clone().into_inner())
        .ok_or_else(|| {
            log::error!("validate_delete_experiment : No control variant found");
            bad_argument!("No control variant found")
        })?;

    let other_variants_overrides = variants
        .iter()
        .filter(|variant| variant.variant_type != VariantType::CONTROL)
        .map(|variant| variant.overrides.clone().into_inner())
        .collect::<Vec<_>>();

    let are_valid_variants = validate_variants_delete_keys(
        &control_variant_override,
        &other_variants_overrides,
        &current_context
            .override_
            .keys()
            .cloned()
            .collect::<Vec<String>>(),
    );
    if !are_valid_variants {
        log::error!("validate_delete_experiment : Variant delete keys are not valid");
        return Err(bad_argument!(
                "Variant delete keys are not valid. Ensure the keys are present in the context"
            ));
    }

    if !(validate_variants_delete_override_value(
        &other_variants_overrides,
        &partial_resolved_config,
    )) {
        log::error!("validate_delete_experiment: Inconsistent value for variant's overrides delete keys");
        return Err(bad_argument!(
            "Inconsistent value for variant's overrides delete keys"
        ));
    }

    if !(validate_variants_control_override_value(
        &control_variant_override,
        &current_context.override_,
    )) {
        log::error!(
            "validate_delete_experiment: Inconsistent value for variant's overrides keys"
        );
        return Err(bad_argument!(
            "Inconsistent value for variant's overrides keys"
        ));
    }
    Ok(())
}
pub fn is_valid_experiment(
    context: &Condition,
    override_keys: &[String],
    flags: &ExperimentationFlags,
    active_experiments: &[Experiment],
) -> superposition::Result<(bool, String)> {
    let mut valid_experiment = true;
    let mut invalid_reason = String::new();
    if !flags.allow_same_keys_overlapping_ctx
        || !flags.allow_diff_keys_overlapping_ctx
        || !flags.allow_same_keys_non_overlapping_ctx
    {
        let override_keys_set = HashSet::<&String>::from_iter(override_keys);
        for active_experiment in active_experiments {
            let active_exp_context = Exp::<Condition>::validate_db_data(
                active_experiment.context.clone().into()
            )
            .map_err(|err| {
                log::error!("is_valid_experiment : failed to decode overrides from db with error {}", err);
                unexpected_error!(err)
            })?
            .into_inner();
            let are_overlapping =
                are_overlapping_contexts(context, &active_exp_context)
                    .map_err(|e| {
                        log::info!("experiment validation failed with error: {e}");
                        bad_argument!(
                            "Context overlap validation failed, given context overlaps with a running experiment's context. Overlapping contexts are not allowed currently as per your configuration"
                        )
                    })?;

            let have_intersecting_key_set = active_experiment
                .override_keys
                .iter()
                .any(|key| override_keys_set.contains(key));

            let same_key_set = active_experiment
                .override_keys
                .iter()
                .all(|key| override_keys_set.contains(key));

            if !flags.allow_diff_keys_overlapping_ctx {
                valid_experiment = valid_experiment && (!are_overlapping || same_key_set);
            }
            if !flags.allow_same_keys_overlapping_ctx {
                valid_experiment =
                    valid_experiment && !(are_overlapping && have_intersecting_key_set);
            }
            if !flags.allow_same_keys_non_overlapping_ctx {
                valid_experiment =
                    valid_experiment && (are_overlapping || !have_intersecting_key_set);
            }

            if !valid_experiment {
                invalid_reason.push_str("This current context overlaps with an existing experiment or the keys in the context are overlapping");
                break;
            }
        }
    }

    Ok((valid_experiment, invalid_reason))
}

pub fn validate_experiment(
    context: &Condition,
    override_keys: &[String],
    experiment_id: Option<i64>,
    flags: &ExperimentationFlags,
    schema_name: &SchemaName,
    conn: &mut PgConnection,
) -> superposition::Result<(bool, String)> {
    use superposition_types::database::schema::experiments::dsl as experiments_dsl;

    let active_experiments: Vec<Experiment> = experiments_dsl::experiments
        .filter(
            diesel::dsl::not(experiments_dsl::id.eq(experiment_id.unwrap_or_default()))
                .and(
                    experiments_dsl::status
                        .eq(ExperimentStatusType::CREATED)
                        .or(experiments_dsl::status.eq(ExperimentStatusType::INPROGRESS)),
                ),
        )
        .schema_name(schema_name)
        .load(conn)?;

    is_valid_experiment(context, override_keys, flags, &active_experiments)
}

#[cfg(feature = "jsonlogic")]
pub fn add_variant_dimension_to_ctx(
    context: &Condition,
    variant: String,
) -> superposition::Result<Value> {
    let variant_condition = serde_json::json!({
        "in" : [
            variant,
            { "var": "variantIds" }
        ]
    });

    if context.is_empty() {
        Ok(serde_json::json!({"and" : [variant_condition]}))
    } else {
        let mut conditions = match context.get("and") {
            Some(conditions_json) => conditions_json
                .as_array()
                .ok_or(bad_argument!(
                    "Failed parsing conditions as an array. Ensure the context provided obeys the rules of JSON logic"
                ))?
                .clone(),
            None => vec![Value::Object(context.clone().into())],
        };

        conditions.push(variant_condition);

        let mut updated_ctx = Map::new();
        updated_ctx.insert(String::from("and"), serde_json::Value::Array(conditions));

        match serde_json::to_value(updated_ctx) {
            Ok(value) => Ok(value),
            Err(_) => Err(bad_argument!(
                "Failed to convert context to a valid JSON object. Check the request sent for correctness"
            )),
        }
    }
}

#[cfg(not(feature = "jsonlogic"))]
pub fn add_variant_dimension_to_ctx(
    context: &Condition,
    variant: String,
) -> superposition::Result<Value> {
    let mut context_map: Map<String, Value> = context.clone().into();
    context_map.insert("variantIds".to_string(), Value::String(variant));
    Ok(Value::Object(context_map))
}

pub fn extract_override_keys(overrides: &Map<String, Value>) -> HashSet<String> {
    overrides.keys().map(String::from).collect()
}

pub async fn fetch_cac_config(
    state: &Data<AppState>,
    workspace_request: &WorkspaceContext,
) -> superposition::Result<(Config, Option<String>)> {
    let http_client = reqwest::Client::new();
    let url = state.cac_host.clone() + "/config";
    let headers_map = construct_header_map(
        &workspace_request.workspace_id,
        &workspace_request.organisation_id,
        vec![],
    )?;

    let response = http_client
        .get(&url)
        .headers(headers_map.into())
        .header(
            header::AUTHORIZATION,
            format!("Internal {}", state.superposition_token),
        )
        .send()
        .await;

    match response {
        Ok(res) => {
            let config_version = res
                .headers()
                .get("x-config-version")
                .and_then(|val| val.to_str().ok().map(String::from));
            let config = res.json::<Config>().await.map_err(|err| {
                log::error!("failed to parse cac config response with error: {}", err);
                unexpected_error!("Failed to parse cac config.")
            })?;
            Ok((config, config_version))
        }
        Err(error) => {
            log::error!("Failed to fetch cac config with error: {:?}", error);
            Err(unexpected_error!(error))
        }
    }
}

pub async fn fetch_webhook_by_event(
    state: &Data<AppState>,
    user: &User,
    event: &WebhookEvent,
    workspace_request: &WorkspaceContext,
) -> superposition::Result<Webhook> {
    let http_client = reqwest::Client::new();
    let url = format!("{}/webhook/event/{event}", state.cac_host);
    let user_str = serde_json::to_string(user).map_err(|err| {
        log::error!("Something went wrong, failed to stringify user data {err}");
        unexpected_error!(
            "Something went wrong, failed to stringify user data {}",
            err
        )
    })?;

    let headers_map = construct_header_map(
        &workspace_request.workspace_id,
        &workspace_request.organisation_id,
        vec![("x-user", user_str)],
    )?;

    let response = http_client
        .get(&url)
        .headers(headers_map.into())
        .header(
            header::AUTHORIZATION,
            format!("Internal {}", state.superposition_token),
        )
        .send()
        .await;

    match response {
        Ok(res) => {
            if res.status() == 404 {
                log::info!("No Webhook found for event: {}", event);
                return Ok(Webhook::default());
            }
            let webhook = res.json::<Webhook>().await.map_err(|err| {
                log::error!("failed to parse Webhook response with error: {}", err);
                unexpected_error!("Failed to parse Webhook.")
            })?;
            Ok(webhook)
        }
        Err(error) => {
            log::error!("Failed to fetch Webhook with error: {:?}", error);
            Err(unexpected_error!(error))
        }
    }
}

pub fn handle_experiment_group_membership(
    experiment: &Experiment,
    new_group_id: &Option<I64Update>,
    current_group_id: &Option<i64>,
    state: &Data<AppState>,
    transaction_conn: &mut PooledConnection<ConnectionManager<PgConnection>>,
    schema_name: &SchemaName,
    user: &User,
) -> superposition::Result<I64Update> {
    let experiment_id = experiment.id;

    fn create_member_request(
        action: &str,
        experiment_id: i64,
    ) -> superposition::Result<ExpGroupMemberRequest> {
        Ok(ExpGroupMemberRequest {
            change_reason: ChangeReason::try_from(format!(
                "{} experiment {} to/from the group, while updating the experiment.",
                action, experiment_id
            ))
            .map_err(|e| unexpected_error!(e))?,
            member_experiment_ids: vec![experiment_id],
        })
    }

    match (new_group_id, current_group_id) {
        // Case 1: Adding to a new group (group specified and experiment not currently in any group)
        (Some(I64Update::Add(experiment_group_id)), None) => {
            add_members(
                experiment_group_id,
                &[experiment.clone()],
                create_member_request("Adding", experiment_id)?,
                transaction_conn,
                schema_name,
                user,
            )?;
            Ok(I64Update::Add(*experiment_group_id))
        }

        // Case 2: Moving to a different group
        (Some(I64Update::Add(experiment_group_id)), Some(current_group_id))
            if experiment_group_id != current_group_id =>
        {
            // Remove from current group
            remove_members(
                current_group_id,
                create_member_request("Removing", experiment_id)?,
                transaction_conn,
                schema_name,
                user,
            )?;

            // Add to new group
            add_members(
                experiment_group_id,
                &[experiment.clone()],
                create_member_request("Adding", experiment_id)?,
                transaction_conn,
                schema_name,
                user,
            )?;
            Ok(I64Update::Add(*experiment_group_id))
        }

        // Case 3: Removing from group (explicitly set to None)
        (Some(I64Update::Remove), Some(current_group_id)) => {
            remove_members(
                current_group_id,
                create_member_request("Removing", experiment_id)?,
                transaction_conn,
                schema_name,
                user,
            )?;

            // Make a new group if Inprogress or Paused
            if experiment.status == ExperimentStatusType::INPROGRESS
                || experiment.status == ExperimentStatusType::PAUSED
            {
                let new_experiment_group = create_system_generated_experiment_group(
                    experiment,
                    &experiment.traffic_percentage,
                    state,
                    transaction_conn,
                    schema_name,
                    user,
                )?;
                Ok(I64Update::Add(new_experiment_group.id))
            } else {
                Ok(I64Update::Remove)
            }
        }
        // Case 4: All other cases (no change needed)
        _ => Ok(current_group_id
            .map(I64Update::Add)
            .unwrap_or(I64Update::Remove)),
    }
}

pub fn validate_and_add_experiment_group_id(
    member_experiments: &[Experiment],
    new_experiment_group_id: &i64,
    schema_name: &SchemaName,
    transaction_conn: &mut PooledConnection<ConnectionManager<PgConnection>>,
    user: &User,
) -> superposition::Result<()> {
    let now = Utc::now();

    for experiment in member_experiments {
        let experiment_id = experiment.id;

        if let Some(existing_group_id) = experiment.experiment_group_id {
            if existing_group_id != *new_experiment_group_id {
                return Err(bad_argument!(
                    "Experiment {} is already a part of a different experiment group {}",
                    experiment_id,
                    existing_group_id
                ));
            }
        }

        let change_reason = ChangeReason::try_from(format!(
            "Adding experiment {} to group {}",
            experiment_id, new_experiment_group_id
        ))
        .map_err(|e| unexpected_error!(e))?;

        // Update experiment
        diesel::update(experiments::experiments.find(experiment_id))
            .set((
                experiments::experiment_group_id
                    .eq(I64Update::Add(*new_experiment_group_id)),
                experiments::last_modified_by.eq(&user.get_email()),
                experiments::last_modified.eq(now),
                experiments::change_reason.eq(change_reason),
            ))
            .returning(Experiment::as_returning())
            .schema_name(schema_name)
            .execute(transaction_conn)?;
    }

    Ok(())
}

pub fn validate_and_remove_experiment_group_id(
    member_experiment_ids: &[i64],
    experiment_group_id: &i64,
    schema_name: &SchemaName,
    state: &Data<AppState>,
    conn: &mut PooledConnection<ConnectionManager<PgConnection>>,
    user: &User,
) -> superposition::Result<()> {
    let experiment_group =
        fetch_experiment_group(experiment_group_id, conn, schema_name)?;

    if experiment_group.group_type == GroupType::SystemGenerated {
        return Err(bad_argument!(
            "Cannot remove experiments from a system-generated experiment group"
        ));
    }

    let member_experiments: Vec<Experiment> = experiments::experiments
        .filter(experiments::id.eq_any(member_experiment_ids))
        .schema_name(schema_name)
        .get_results::<Experiment>(conn)?;

    ensure_experiments_exist(
        &HashSet::from_iter(member_experiment_ids.to_owned()),
        &member_experiments,
        "The following experiment IDs are not present in the database",
    )?;
    let now = Utc::now();

    for experiment in member_experiments {
        let new_experiment_group_id = match experiment.experiment_group_id {
            None => {
                return Err(bad_argument!(
                    "Experiment with id {} is not part of any experiment group",
                    experiment.id
                ));
            }
            Some(existing_group_id) if existing_group_id != *experiment_group_id => {
                return Err(bad_argument!(
                    "Experiment with id {} is part of a different experiment group: {}. Cannot remove from group {}",
                    experiment.id,
                    existing_group_id,
                    experiment_group_id
                ));
            }
            _ => {
                // Make a new group if Inprogress or Paused
                if experiment.status == ExperimentStatusType::INPROGRESS
                    || experiment.status == ExperimentStatusType::PAUSED
                {
                    let new_experiment_group = create_system_generated_experiment_group(
                        &experiment,
                        &experiment.traffic_percentage,
                        state,
                        conn,
                        schema_name,
                        user,
                    )?;
                    I64Update::Add(new_experiment_group.id)
                } else {
                    I64Update::Remove
                }
            }
        };

        let change_reason = ChangeReason::try_from(format!(
            "Removing experiment {} from group {}",
            experiment.id, experiment_group_id
        ))
        .map_err(|e| unexpected_error!(e))?;

        diesel::update(experiments::experiments.find(experiment.id))
            .set((
                experiments::experiment_group_id.eq(new_experiment_group_id),
                experiments::last_modified_by.eq(&user.get_email()),
                experiments::last_modified.eq(now),
                experiments::change_reason.eq(change_reason),
            ))
            .returning(Experiment::as_returning())
            .schema_name(schema_name)
            .execute(conn)?;
    }

    Ok(())
}

pub fn ensure_experiments_exist(
    requested_ids: &HashSet<i64>,
    found_experiments: &[Experiment],
    error_message: &str,
) -> superposition::Result<()> {
    let found_ids: HashSet<i64> = found_experiments.iter().map(|e| e.id).collect();
    let requested_ids: HashSet<i64> = requested_ids.iter().copied().collect();

    let missing_experiment_ids: Vec<i64> =
        requested_ids.difference(&found_ids).copied().collect();
    if !missing_experiment_ids.is_empty() {
        return Err(bad_argument!(
            "{}: {}",
            error_message,
            missing_experiment_ids
                .iter()
                .map(|id| id.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        ));
    }
    Ok(())
}

pub fn fetch_experiment(
    experiment_id: &i64,
    conn: &mut DBConnection,
    schema_name: &SchemaName,
) -> superposition::Result<Experiment> {
    use superposition_types::database::schema::experiments::dsl::*;
    let result: Experiment = experiments
        .find(experiment_id)
        .schema_name(schema_name)
        .get_result::<Experiment>(conn)?;

    Ok(result)
}

pub async fn validate_control_overrides(
    control_overrides: &Exp<Overrides>,
    exp_context: &Condition,
    workspace_request: &WorkspaceContext,
    user: &User,
    state: &Data<AppState>,
) -> superposition::Result<()> {
    cfg_if::cfg_if! {
        if #[cfg(feature = "jsonlogic")] {
            let context = &extract_dimensions(exp_context)?;
        } else {
            let context = &exp_context;
        }
    }

    let resolved_config =
        get_resolved_config(user, state, context, workspace_request).await?;
    let control_variant_overrides = control_overrides.clone().into_inner();
    let mismatched_overrides: Map<_, _> = control_variant_overrides
        .into_iter()
        .filter_map(|(key, value)| match resolved_config.get(&key) {
            Some(resolved_value) if *resolved_value == value => None,
            Some(resolved_value) => Some(Ok((key, resolved_value.clone()))),
            None => Some(Err(bad_argument!(
                "Control variant overrides do not match resolved config for key: {key}"
            ))),
        })
        .collect::<superposition::Result<_>>()?;

    if !mismatched_overrides.is_empty() {
        return Err(bad_argument!(
            "Outdated control variant overrides. Please update the control variant's overrides with: {}.",
            serde_json::to_string(&mismatched_overrides).unwrap_or_default()
        ));
    }

    Ok(())
}
