use std::{collections::HashMap, str::FromStr};

use actix_http::header::HeaderValue;
#[cfg(feature = "high-performance-mode")]
use actix_http::StatusCode;
#[cfg(feature = "high-performance-mode")]
use actix_web::http::header::ContentType;
#[cfg(feature = "high-performance-mode")]
use actix_web::web::Data;
use actix_web::{
    get, put, route,
    web::{Json, Path, Query},
    HttpRequest, HttpResponse, HttpResponseBuilder, Scope,
};
use cac_client::{eval_cac, eval_cac_with_reasoning, MergeStrategy};
use chrono::{DateTime, Timelike, Utc};
use diesel::{
    dsl::max, BoolExpressionMethods, ExpressionMethods, QueryDsl, RunQueryDsl,
    SelectableHelper,
};
#[cfg(feature = "high-performance-mode")]
use fred::interfaces::KeysInterface;
use itertools::Itertools;
use serde_json::{json, Map, Value};
#[cfg(feature = "jsonlogic")]
use service_utils::helpers::extract_dimensions;
#[cfg(feature = "high-performance-mode")]
use service_utils::service::types::AppState;
use service_utils::service::types::{
    AppHeader, DbConnection, SchemaName, WorkspaceContext,
};
#[cfg(feature = "high-performance-mode")]
use superposition_macros::response_error;
use superposition_macros::{bad_argument, db_error, unexpected_error};
use superposition_types::{
    api::{
        config::{ConfigVersionResponse, ContextPayload},
        context::PutRequest,
    },
    custom_query::{
        self as superposition_query, CustomQuery, PaginationParams, QueryMap,
    },
    database::{
        models::{
            cac::{ConfigVersion, ConfigVersionListItem},
            ChangeReason, Description,
        },
        schema::{config_versions::dsl as config_versions, event_log::dsl as event_log},
        superposition_schema::superposition::workspaces,
    },
    result as superposition, Cac, Condition, Config, Context, DBConnection,
    OverrideWithKeys, Overrides, PaginatedResponse, User,
};
use uuid::Uuid;

use crate::helpers::generate_cac;
use crate::{
    api::context::{self, helpers::query_description},
    helpers::DimensionData,
};
use crate::{
    api::dimension::{get_dimension_data, get_dimension_data_map},
    helpers::calculate_context_weight,
};

use super::helpers::apply_prefix_filter_to_config;

#[allow(clippy::let_and_return)]
pub fn endpoints() -> Scope {
    let scope = Scope::new("")
        .service(get_config)
        .service(get_resolved_config)
        .service(reduce_config)
        .service(list_config_versions)
        .service(fetch_config_version);
    #[cfg(feature = "high-performance-mode")]
    let scope = scope.service(get_config_fast);
    scope
}

fn get_config_version_from_workspace(
    workspace_context: &WorkspaceContext,
    conn: &mut DBConnection,
) -> Option<i64> {
    match workspaces::dsl::workspaces
        .select(workspaces::config_version)
        .filter(
            workspaces::organisation_id
                .eq(&workspace_context.organisation_id.0)
                .and(workspaces::workspace_name.eq(&workspace_context.workspace_id.0)),
        )
        .get_result::<Option<i64>>(conn)
    {
        Ok(version) => version,
        Err(e) => {
            log::error!(
                "Failed to get config_version for org_id: {}, workspace_name: {} — {:?}",
                workspace_context.organisation_id.0,
                workspace_context.workspace_id.0,
                e
            );
            None
        }
    }
}
fn get_config_version(
    query_params_map: &mut Map<String, Value>,
    workspace_context: &WorkspaceContext,
    conn: &mut DBConnection,
) -> superposition::Result<Option<i64>> {
    query_params_map.remove("version").map_or_else(
        || Ok(get_config_version_from_workspace(workspace_context, conn)),
        |version| {
            if version == Value::String("latest".to_string()) {
                log::info!("latest config request");
                return Ok(None);
            }
            version.as_i64().map_or_else(
                || {
                    log::error!("failed to decode version as integer: {}", version);
                    Err(bad_argument!("version is not of type integer"))
                },
                |v| Ok(Some(v)),
            )
        },
    )
}

pub fn add_audit_id_to_header(
    conn: &mut DBConnection,
    resp_builder: &mut HttpResponseBuilder,
    schema_name: &SchemaName,
) {
    if let Ok(uuid) = event_log::event_log
        .select(event_log::id)
        .filter(event_log::table_name.eq("contexts"))
        .order_by(event_log::timestamp.desc())
        .schema_name(schema_name)
        .first::<Uuid>(conn)
    {
        resp_builder.insert_header((AppHeader::XAuditId.to_string(), uuid.to_string()));
    } else {
        log::error!("Failed to fetch contexts from event_log");
    }
}

fn add_last_modified_to_header(
    max_created_at: Option<DateTime<Utc>>,
    is_smithy: bool,
    resp_builder: &mut HttpResponseBuilder,
) {
    if let Some(date) = max_created_at {
        let value = if is_smithy {
            // Smithy needs to be in this format otherwise they can't
            // deserialize it.
            HeaderValue::from_str(date.to_rfc3339().as_str())
        } else {
            HeaderValue::from_str(date.to_rfc2822().as_str())
        };
        if let Ok(header_value) = value {
            resp_builder
                .insert_header((AppHeader::LastModified.to_string(), header_value));
        } else {
            log::error!("failed parsing datetime_utc {:?}", value);
        }
    }
}

fn add_config_version_to_header(
    config_version: &Option<i64>,
    resp_builder: &mut HttpResponseBuilder,
) {
    if let Some(val) = config_version {
        resp_builder.insert_header((
            AppHeader::XConfigVersion.to_string(),
            val.clone().to_string(),
        ));
    }
}

fn get_max_created_at(
    conn: &mut DBConnection,
    schema_name: &SchemaName,
) -> Result<DateTime<Utc>, diesel::result::Error> {
    event_log::event_log
        .select(max(event_log::timestamp))
        .filter(event_log::table_name.eq_any(vec!["contexts", "default_configs"]))
        .schema_name(schema_name)
        .first::<Option<DateTime<Utc>>>(conn)
        .and_then(|res| res.ok_or(diesel::result::Error::NotFound))
}

fn is_not_modified(max_created_at: Option<DateTime<Utc>>, req: &HttpRequest) -> bool {
    let nanosecond_erasure = |t: DateTime<Utc>| t.with_nanosecond(0);
    let last_modified = req
        .headers()
        .get("If-Modified-Since")
        .and_then(|header_val| {
            let header_str = header_val.to_str().ok()?;
            DateTime::parse_from_rfc2822(header_str)
                .map(|datetime| datetime.with_timezone(&Utc))
                .ok()
        })
        .and_then(nanosecond_erasure);
    log::info!("last modified {last_modified:?}");
    let parsed_max: Option<DateTime<Utc>> = max_created_at.and_then(nanosecond_erasure);
    max_created_at.is_some() && parsed_max <= last_modified
}

pub fn generate_config_from_version(
    version: &mut Option<i64>,
    conn: &mut DBConnection,
    schema_name: &SchemaName,
) -> superposition::Result<Config> {
    if let Some(val) = version {
        let config = config_versions::config_versions
            .select(config_versions::config)
            .filter(config_versions::id.eq(*val))
            .schema_name(schema_name)
            .get_result::<Value>(conn)
            .map_err(|err| {
                log::error!("failed to fetch config with error: {}", err);
                db_error!(err)
            })?;
        serde_json::from_value::<Config>(config).map_err(|err| {
            log::error!("failed to decode config: {}", err);
            unexpected_error!("failed to decode config")
        })
    } else {
        match config_versions::config_versions
            .select((config_versions::id, config_versions::config))
            .order(config_versions::created_at.desc())
            .schema_name(schema_name)
            .first::<(i64, Value)>(conn)
        {
            Ok((latest_version, config)) => {
                *version = Some(latest_version);
                serde_json::from_value::<Config>(config).or_else(|err| {
                    log::error!("failed to decode config: {}", err);
                    generate_cac(conn, schema_name)
                })
            }
            Err(err) => {
                log::error!("failed to find latest config: {err}");
                generate_cac(conn, schema_name)
            }
        }
    }
}

fn generate_subsets(map: &Map<String, Value>) -> Vec<Map<String, Value>> {
    let mut subsets = Vec::new();
    let keys: Vec<String> = map.keys().cloned().collect_vec();
    let all_subsets_keys = generate_subsets_keys(keys);

    for subset_keys in &all_subsets_keys {
        let mut subset_map = Map::new();

        for key in subset_keys {
            if let Some(value) = map.get(key) {
                subset_map.insert(key.to_string(), value.clone());
            }
        }

        subsets.push(subset_map);
    }

    subsets
}

fn generate_subsets_keys(keys: Vec<String>) -> Vec<Vec<String>> {
    let mut res = vec![[].to_vec()];
    for element in keys {
        let len = res.len();
        for ind in 0..len {
            let mut sub = res[ind].clone();
            sub.push(element.clone());
            res.push(sub);
        }
    }
    res
}

fn reduce(
    contexts_overrides_values: Vec<(Context, Map<String, Value>, Value, String)>,
    default_config_val: &Value,
) -> superposition::Result<Vec<Map<String, Value>>> {
    let mut dimensions: Vec<Map<String, Value>> = Vec::new();
    for (context, overrides, key_val, override_id) in contexts_overrides_values {
        cfg_if::cfg_if! {
            if #[cfg(feature = "jsonlogic")] {
                let mut ct_dimensions = extract_dimensions(&context.condition)?;
            } else {
                let mut ct_dimensions: Map<String, Value> = context.condition.clone().into();
            }
        }

        ct_dimensions.insert("key_val".to_string(), key_val);
        let request_payload = json!({
            "override": overrides,
            "context": context.condition,
            "id": context.id,
            "to_be_deleted": overrides.is_empty(),
            "override_id": override_id,
        });
        ct_dimensions.insert("req_payload".to_string(), request_payload);
        dimensions.push(ct_dimensions);
    }

    //adding default config value
    let mut default_config_map = Map::new();
    default_config_map.insert("key_val".to_string(), default_config_val.to_owned());
    dimensions.push(default_config_map);

    /*
    We now have dimensions array, which is a vector of elements representing each context present where each element is a type of Map<String,Value> which contains the following
        1. all the dimensions and value of those dimensions in the context
        2. key_val, which is the value of the override key for which we are trying to reduce
        3. A req_payload which contains the details of the context like, context_id, override_id, the context_condition, new overrides (without containing the key that has to be reduced)
        {
            dimension1_in_context : value_of_dimension1_in_context,
            dimension2_in_context : value_of_dimension2_in_context,
            .
            .
            key_val: value of the override key that we are trying to reduce
            req_payload : {
                override : new_overrides(without the key that is to be reduced)
                context : context_condition
                id : context_id
                to_be_deleted : if new_overrides is empty then delete this context
            }
        }

    We have also sorted this dimensions vector in descending order based on the weight of the dimensions in that context
    and in this vector the default config will be at the end of the list as it has no dimensions and it's weight is the least

    Now we iterate from start and then pick an element and generate all subsets of that element keys excluding the req_payload and key_val
    i.e we only generate different subsets of dimensions of that context along with the value of those dimensions in that context

    Next we check if in the vector we find any other element c2 whose dimensions is part of the subsets of the parent element c1
    if dimensions_subsets_of_c1 contains dimensions_of_c2

        if the value of the override key is same in both c1 and c2 then we can reduce or remove that key in the override of c1
        so we mark the can_be_reduce to be true, and then update the dimensions vector.

        but if we find any other element c3 whose dimensions is a subset of c1_dimensions but the value is not the same
        then that means we can't reduce this key from c1, because in resolve if we remove it from c1 it will pick the value form c3 which is different.
        So if we find this element c3 before any other element which is a subset of c1 with the same value, then we can't reduce this key for c1 so we break
        and continue with the next element.
        Here "before" means the element with higher weight comes first with a subset of c1 but differnt override value for the key
     */
    for (c1_index, dimensions_of_c1_with_payload) in dimensions.clone().iter().enumerate()
    {
        let mut dimensions_of_c1 = dimensions_of_c1_with_payload.clone();
        dimensions_of_c1.remove("req_payload");
        let override_val_of_key_in_c1 = dimensions_of_c1.remove("key_val");
        let dimensions_subsets_of_c1 = generate_subsets(&dimensions_of_c1);
        for (c2_index, dimensions_in_c2_with_payload) in dimensions.iter().enumerate() {
            let mut dimensions_of_c2 = dimensions_in_c2_with_payload.clone();
            dimensions_of_c2.remove("req_payload");
            let override_val_of_key_in_c2 = dimensions_of_c2.remove("key_val");
            if c2_index != c1_index
                && dimensions_subsets_of_c1.contains(&dimensions_of_c2)
            {
                if override_val_of_key_in_c1 == override_val_of_key_in_c2 {
                    let mut temp_c1 = dimensions_of_c1_with_payload.to_owned();
                    temp_c1.insert("can_be_reduced".to_string(), Value::Bool(true));
                    dimensions[c1_index] = temp_c1;
                    break;
                } else if override_val_of_key_in_c2.is_some() {
                    break;
                }
            }
        }
    }
    Ok(dimensions)
}

#[allow(clippy::type_complexity)]
fn get_contextids_from_overrideid(
    contexts: Vec<Context>,
    overrides: Map<String, Value>,
    key_val: Value,
    override_id: &str,
) -> superposition::Result<Vec<(Context, Map<String, Value>, Value, String)>> {
    let mut res: Vec<(Context, Map<String, Value>, Value, String)> = Vec::new();
    for ct in contexts {
        cfg_if::cfg_if! {
            if #[cfg(feature = "jsonlogic")] {
                let ct_dimensions = extract_dimensions(&ct.condition)?;
            } else {
                let ct_dimensions = &ct.condition;
            }
        }

        if ct_dimensions.contains_key("variantIds") {
            continue;
        }
        let override_keys = &ct.override_with_keys;
        if override_keys.contains(&override_id.to_owned()) {
            res.push((
                ct,
                overrides.clone(),
                key_val.clone(),
                override_id.to_string(),
            ));
        }
    }
    Ok(res)
}

fn construct_new_payload(
    req_payload: &Map<String, Value>,
) -> superposition::Result<PutRequest> {
    let mut res = req_payload.clone();
    res.remove("to_be_deleted");
    res.remove("override_id");
    res.remove("id");

    let context = res
        .get("context")
        .and_then(|val| val.as_object())
        .map_or_else(
            || {
                log::error!("construct new payload: Context not present");
                Err(bad_argument!("Context not present"))
            },
            |val| {
                Cac::<Condition>::try_from(val.to_owned()).map_err(|err| {
                    log::error!("failed to decode condition with error : {}", err);
                    bad_argument!(err)
                })
            },
        )?;

    let override_ = res
        .get("override")
        .and_then(|val| val.as_object())
        .map_or_else(
            || {
                log::error!("construct new payload Override not present");
                Err(bad_argument!("Override not present"))
            },
            |val| {
                Cac::<Overrides>::try_from(val.to_owned()).map_err(|err| {
                    log::error!("failed to decode override with error : {}", err);
                    bad_argument!(err)
                })
            },
        )?;

    let description = match res.get("description") {
        Some(Value::String(s)) => Some(s.clone()),
        Some(_) => {
            log::error!("construct new payload: Description is not a valid string");
            return Err(bad_argument!("Description must be a string"));
        }
        None => None,
    }
    .map(Description::try_from)
    .transpose()
    .map_err(|e| unexpected_error!(e))?;

    let change_reason = ChangeReason::try_from(
        res.get("change_reason")
            .and_then(|val| val.as_str())
            .map(|s| s.to_string())
            .ok_or_else(|| {
                log::error!(
                    "construct new payload: Change reason not present or invalid"
                );
                bad_argument!("Change reason is required and must be a string")
            })?,
    )
    .map_err(|e| unexpected_error!(e))?;

    Ok(PutRequest {
        context,
        r#override: override_,
        description,
        change_reason,
    })
}

#[allow(clippy::too_many_arguments)]
async fn reduce_config_key(
    user: &User,
    conn: &mut DBConnection,
    mut og_contexts: Vec<Context>,
    mut og_overrides: HashMap<String, Overrides>,
    check_key: &str,
    dimension_schema_map: &HashMap<String, DimensionData>,
    default_config: Map<String, Value>,
    is_approve: bool,
    schema_name: &SchemaName,
) -> superposition::Result<Config> {
    let default_config_val =
        default_config
            .get(check_key)
            .ok_or(superposition::AppError::BadArgument(format!(
                "{} not found in default config",
                check_key
            )))?;
    let mut contexts_overrides_values = Vec::new();

    for (override_id, mut override_value) in og_overrides.clone() {
        if let Some(value_of_check_key) = override_value.remove(check_key) {
            let context_arr = get_contextids_from_overrideid(
                og_contexts.clone(),
                override_value.into(),
                value_of_check_key.clone(),
                &override_id,
            )?;
            contexts_overrides_values.extend(context_arr);
        }
    }

    let mut weights = Vec::new();

    for (index, ctx) in contexts_overrides_values.iter().enumerate() {
        let weight =
            calculate_context_weight(&json!((ctx.0).condition), dimension_schema_map)
                .map_err(|err| bad_argument!(err))?;
        weights.push((index, weight))
    }

    // Sort the collected results based on weight
    weights.sort_by(|a, b| b.1.cmp(&a.1));

    // Use the sorted indices to reorder the original vector
    let sorted_weight_contexts = weights
        .into_iter()
        .map(|(index, _)| contexts_overrides_values[index].clone())
        .collect();

    let resolved_dimensions = reduce(sorted_weight_contexts, default_config_val)?;
    for rd in resolved_dimensions {
        match (
            rd.get("can_be_reduced"),
            rd.get("req_payload"),
            rd.get("req_payload").and_then(|v| v.get("id")),
            rd.get("req_payload").and_then(|v| v.get("override_id")),
            rd.get("req_payload").and_then(|v| v.get("to_be_deleted")),
            rd.get("req_payload").and_then(|v| v.get("override")),
        ) {
            (
                Some(Value::Bool(true)),
                Some(Value::Object(request_payload)),
                Some(Value::String(cid)),
                Some(Value::String(oid)),
                Some(Value::Bool(to_be_deleted)),
                Some(Value::Object(override_val)),
            ) => {
                if *to_be_deleted {
                    if is_approve {
                        let _ = context::delete(cid.clone(), user, conn, schema_name);
                    }
                    og_contexts.retain(|x| x.id != *cid);
                } else {
                    if is_approve {
                        let _ = context::delete(cid.clone(), user, conn, schema_name);
                        if let Ok(put_req) = construct_new_payload(request_payload) {
                            let description = match put_req.description.clone() {
                                Some(val) => val,
                                None => query_description(
                                    Value::Object(
                                        put_req.context.clone().into_inner().into(),
                                    ),
                                    conn,
                                    schema_name,
                                )?,
                            };
                            let _ = context::upsert(
                                put_req,
                                description,
                                conn,
                                false,
                                user,
                                schema_name,
                                false,
                            );
                        }
                    }

                    let override_val =
                        Cac::<Overrides>::validate_db_data(override_val.clone())
                            .map_err(|err| {
                                log::error!(
                            "reduce_config_key: failed to decode overrides from db {err}"
                        );
                                unexpected_error!(err)
                            })?
                            .into_inner();

                    let new_id =
                        context::hash(&Value::Object(override_val.clone().into()));
                    og_overrides.insert(new_id.clone(), override_val);

                    let mut ctx_index = 0;
                    let mut delete_old_oid = true;

                    for (ind, ctx) in og_contexts.iter().enumerate() {
                        if ctx.id == *cid {
                            ctx_index = ind;
                        } else if ctx.override_with_keys.contains(oid) {
                            delete_old_oid = false;
                        }
                    }

                    let mut elem = og_contexts[ctx_index].clone();
                    elem.override_with_keys = OverrideWithKeys::new(new_id);
                    og_contexts[ctx_index] = elem;

                    if delete_old_oid {
                        og_overrides.remove(oid);
                    }
                }
            }
            _ => continue,
        }
    }

    Ok(Config {
        contexts: og_contexts,
        overrides: og_overrides,
        default_configs: default_config,
    })
}

#[put("/reduce")]
async fn reduce_config(
    req: HttpRequest,
    user: User,
    db_conn: DbConnection,
    schema_name: SchemaName,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;
    let is_approve = req
        .headers()
        .get("x-approve")
        .and_then(|value| value.to_str().ok().and_then(|s| s.parse::<bool>().ok()))
        .unwrap_or(false);

    let dimensions_vec = get_dimension_data(&mut conn, &schema_name)?;
    let dimensions_data_map = get_dimension_data_map(&dimensions_vec)?;
    let mut config = generate_cac(&mut conn, &schema_name)?;
    let default_config = (config.default_configs).clone();
    for (key, _) in default_config {
        let contexts = config.contexts;
        let overrides = config.overrides;
        let default_config = config.default_configs;
        config = reduce_config_key(
            &user,
            &mut conn,
            contexts.clone(),
            overrides.clone(),
            key.as_str(),
            &dimensions_data_map,
            default_config.clone(),
            is_approve,
            &schema_name,
        )
        .await?;
        if is_approve {
            config = generate_cac(&mut conn, &schema_name)?;
        }
    }

    Ok(HttpResponse::Ok().json(config))
}

#[cfg(feature = "high-performance-mode")]
#[get("/fast")]
async fn get_config_fast(
    schema_name: SchemaName,
    state: Data<AppState>,
) -> superposition::Result<HttpResponse> {
    use fred::interfaces::MetricsInterface;

    log::debug!("Started redis fetch");
    let config_key = format!("{}::cac_config", *schema_name);
    let last_modified_at_key = format!("{}::cac_config::last_modified_at", *schema_name);
    let audit_id_key = format!("{}::cac_config::audit_id", *schema_name);
    let config_version_key = format!("{}::cac_config::config_version", *schema_name);
    let client = state.redis.next_connected();
    let config = client.get::<String, String>(config_key).await;
    let metrics = client.take_latency_metrics();
    let network_metrics = client.take_network_latency_metrics();
    log::trace!(
        "Network metrics for config fetch in milliseconds :: max: {}, min: {}, avg: {}; Latency metrics :: max: {}, min: {}, avg: {}",
        network_metrics.max,
        network_metrics.min,
        network_metrics.avg,
        metrics.max,
        metrics.min,
        metrics.avg
    );
    match config {
        Ok(config) => {
            let mut response = HttpResponse::Ok();
            if let Ok(max_created_at) =
                client.get::<String, String>(last_modified_at_key).await
            {
                let metrics = client.take_latency_metrics();
                let network_metrics = client.take_network_latency_metrics();
                log::trace!(
                    "Network metrics max-created-by fetch in milliseconds :: max: {}, min: {}, avg: {}; Latency metrics :: max: {}, min: {}, avg: {}",
                    network_metrics.max,
                    network_metrics.min,
                    network_metrics.avg,
                    metrics.max,
                    metrics.min,
                    metrics.avg
                );
                response
                    .insert_header((AppHeader::LastModified.to_string(), max_created_at));
            }
            if let Ok(audit_id) = client.get::<String, String>(audit_id_key).await {
                let metrics = client.take_latency_metrics();
                let network_metrics = client.take_network_latency_metrics();
                log::trace!(
                    "Network metrics for audit ID in milliseconds :: max: {}, min: {}, avg: {}; Latency metrics :: max: {}, min: {}, avg: {}",
                    network_metrics.max,
                    network_metrics.min,
                    network_metrics.avg,
                    metrics.max,
                    metrics.min,
                    metrics.avg
                );
                response.insert_header((AppHeader::XAuditId.to_string(), audit_id));
            }
            if let Ok(config_version) =
                client.get::<Option<i64>, String>(config_version_key).await
            {
                let metrics = client.take_latency_metrics();
                let network_metrics = client.take_network_latency_metrics();
                log::trace!(
                    "Network metrics for version ID in milliseconds :: max: {}, min: {}, avg: {}; Latency metrics :: max: {}, min: {}, avg: {}",
                    network_metrics.max,
                    network_metrics.min,
                    network_metrics.avg,
                    metrics.max,
                    metrics.min,
                    metrics.avg
                );
                add_config_version_to_header(&config_version, &mut response);
            }
            response.insert_header(ContentType::json());
            Ok(response.body(config))
        }
        Err(err) => {
            log::error!("Could not get config in redis due to {}", err);
            Err(response_error!(
                StatusCode::INTERNAL_SERVER_ERROR,
                "could not fetch config, please try /config API"
            ))
        }
    }
}

#[route("", method = "GET", method = "POST")]
async fn get_config(
    req: HttpRequest,
    body: Option<Json<ContextPayload>>,
    db_conn: DbConnection,
    query_map: superposition_query::Query<QueryMap>,
    workspace_context: WorkspaceContext,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;

    let max_created_at = get_max_created_at(&mut conn, &workspace_context.schema_name)
        .map_err(|e| log::error!("failed to fetch max timestamp from event_log: {e}"))
        .ok();

    log::info!("Max created at: {max_created_at:?}");

    let is_not_modified = is_not_modified(max_created_at, &req);

    if is_not_modified {
        return Ok(HttpResponse::NotModified().finish());
    }

    let mut query_params_map = query_map.into_inner();
    let mut version =
        get_config_version(&mut query_params_map, &workspace_context, &mut conn)?;

    let mut config = generate_config_from_version(
        &mut version,
        &mut conn,
        &workspace_context.schema_name,
    )?;

    config = apply_prefix_filter_to_config(&mut query_params_map, config)?;
    let is_smithy: bool;
    let context = if req.method() == actix_web::http::Method::GET {
        is_smithy = false;
        query_params_map
    } else {
        // Assuming smithy.
        is_smithy = true;
        body.map_or_else(QueryMap::default, |body| body.into_inner().context.into())
    };
    if !context.is_empty() {
        config = config.filter_by_dimensions(&context)
    }

    let mut response = HttpResponse::Ok();
    add_last_modified_to_header(max_created_at, is_smithy, &mut response);
    add_audit_id_to_header(&mut conn, &mut response, &workspace_context.schema_name);
    add_config_version_to_header(&version, &mut response);
    Ok(response.json(config))
}

#[route("/resolve", method = "GET", method = "POST")]
async fn get_resolved_config(
    req: HttpRequest,
    body: Option<Json<ContextPayload>>,
    db_conn: DbConnection,
    query_map: superposition_query::Query<QueryMap>,
    workspace_context: WorkspaceContext,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;
    let mut query_params_map = query_map.into_inner();

    let max_created_at = get_max_created_at(&mut conn, &workspace_context.schema_name)
        .map_err(|e| log::error!("failed to fetch max timestamp from event_log : {e}"))
        .ok();

    let is_not_modified = is_not_modified(max_created_at, &req);

    if is_not_modified {
        return Ok(HttpResponse::NotModified().finish());
    }

    let mut config_version =
        get_config_version(&mut query_params_map, &workspace_context, &mut conn)?;
    let mut config = generate_config_from_version(
        &mut config_version,
        &mut conn,
        &workspace_context.schema_name,
    )?;

    config = apply_prefix_filter_to_config(&mut query_params_map, config)?;

    let merge_strategy = req
        .headers()
        .get("x-merge-strategy")
        .and_then(|header_value: &HeaderValue| header_value.to_str().ok())
        .and_then(|val| MergeStrategy::from_str(val).ok())
        .unwrap_or_default();

    let mut override_map = HashMap::new();
    for (key, val) in config.overrides {
        override_map.insert(key, val);
    }
    let show_reason = matches!(
        query_params_map.get("show_reasoning"),
        Some(Value::String(_))
    );

    if let Some(context_id) = query_params_map.get("context_id") {
        let c_id = context_id
            .as_str()
            .ok_or_else(|| bad_argument!("context_id is not a string"))?
            .to_string();

        config.contexts =
            if let Some(index) = config.contexts.iter().position(|ctx| ctx.id == c_id) {
                config.contexts[..index].to_vec()
            } else {
                return Err(bad_argument!(
                    "context with id {} not found in CAC",
                    context_id
                ));
            };
    }

    let is_smithy: bool;
    let context = if req.method() == actix_web::http::Method::GET {
        is_smithy = false;
        query_params_map
    } else {
        // Must be smithy calling w/ POST :D
        is_smithy = true;
        body.ok_or(bad_argument!(
            "When using POST, context needs to be provided in the body."
        ))?
        .into_inner()
        .context
        .into()
    };
    let response = if show_reason {
        eval_cac_with_reasoning(
            config.default_configs,
            &config.contexts,
            &override_map,
            &context,
            merge_strategy,
        )
        .map_err(|err| {
            log::error!("failed to eval cac with err: {}", err);
            unexpected_error!("cac eval failed")
        })?
    } else {
        eval_cac(
            config.default_configs,
            &config.contexts,
            &override_map,
            &context,
            merge_strategy,
        )
        .map_err(|err| {
            log::error!("failed to eval cac with err: {}", err);
            unexpected_error!("cac eval failed")
        })?
    };
    let mut resp = HttpResponse::Ok();
    add_last_modified_to_header(max_created_at, is_smithy, &mut resp);
    add_audit_id_to_header(&mut conn, &mut resp, &workspace_context.schema_name);
    add_config_version_to_header(&config_version, &mut resp);

    Ok(resp.json(response))
}

#[get("/versions")]
async fn list_config_versions(
    db_conn: DbConnection,
    filters: Query<PaginationParams>,
    schema_name: SchemaName,
) -> superposition::Result<Json<PaginatedResponse<ConfigVersionListItem>>> {
    let DbConnection(mut conn) = db_conn;

    if let Some(true) = filters.all {
        let config_versions = config_versions::config_versions
            .schema_name(&schema_name)
            .select(ConfigVersionListItem::as_select())
            .get_results(&mut conn)?;
        return Ok(Json(PaginatedResponse::all(config_versions)));
    }

    let n_version: i64 = config_versions::config_versions
        .count()
        .schema_name(&schema_name)
        .get_result(&mut conn)?;

    let limit = filters.count.unwrap_or(10);
    let mut builder = config_versions::config_versions
        .schema_name(&schema_name)
        .into_boxed()
        .order(config_versions::created_at.desc())
        .limit(limit);
    if let Some(page) = filters.page {
        let offset = (page - 1) * limit;
        builder = builder.offset(offset);
    }
    let config_versions = builder
        .select(ConfigVersionListItem::as_select())
        .load(&mut conn)?;
    let total_pages = (n_version as f64 / limit as f64).ceil() as i64;
    Ok(Json(PaginatedResponse {
        total_pages,
        total_items: n_version,
        data: config_versions,
    }))
}

#[get("/version/{version}")]
async fn fetch_config_version(
    db_conn: DbConnection,
    version: Path<i64>,
    schema_name: SchemaName,
) -> superposition::Result<Json<ConfigVersionResponse>> {
    let DbConnection(mut conn) = db_conn;

    let config_version = config_versions::config_versions
        .schema_name(&schema_name)
        .find(version.into_inner())
        .get_result::<ConfigVersion>(&mut conn)?;

    Ok(Json(config_version.into()))
}
