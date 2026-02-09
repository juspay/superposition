use std::collections::HashMap;

use actix_http::header::HeaderValue;
use actix_web::{
    HttpRequest, HttpResponse, HttpResponseBuilder, Scope, get, put, routes,
    web::{Data, Header, Json, Path, Query},
};
use chrono::{DateTime, Timelike, Utc};
use diesel::{ExpressionMethods, QueryDsl, RunQueryDsl, SelectableHelper, dsl::max};
use itertools::Itertools;
use serde_json::{Map, Value, json};
use service_utils::{
    helpers::fetch_dimensions_info_map,
    redis::{
        AUDIT_ID_KEY_SUFFIX, CONFIG_KEY_SUFFIX, CONFIG_VERSION_KEY_SUFFIX,
        LAST_MODIFIED_KEY_SUFFIX, fetch_from_redis_else_writeback,
    },
    service::{
        get_db_connection,
        types::{AppHeader, AppState, DbConnection, SchemaName, WorkspaceContext},
    },
};
use superposition_core::{
    helpers::{calculate_context_weight, hash},
    serialize_to_toml,
};
use superposition_derives::authorized;
use superposition_macros::{bad_argument, db_error, not_found, unexpected_error};
use superposition_types::{
    Cac, Condition, Config, Context, DBConnection, DimensionInfo, OverrideWithKeys,
    Overrides, PaginatedResponse, User,
    api::{
        config::{ConfigQuery, ContextPayload, MergeStrategy, ResolveConfigQuery},
        context::PutRequest,
    },
    custom_query::{
        self as superposition_query, CustomQuery, DimensionQuery, PaginationParams,
        QueryMap,
    },
    database::{
        models::{
            ChangeReason,
            cac::{ConfigVersion, ConfigVersionListItem},
        },
        schema::{config_versions::dsl as config_versions, event_log::dsl as event_log},
    },
    result as superposition,
};
use uuid::Uuid;

use crate::api::context::{self, helpers::query_description};
use crate::{
    api::{
        context::{self, helpers::query_description},
        dimension::fetch_dimensions_info_map,
    },
    helpers::{generate_cac, generate_detailed_cac, get_config_from_redis},
use crate::api::{
    config::helpers::get_config_version,
    context::{self, helpers::query_description},
};
use crate::helpers::{calculate_context_weight, generate_cac};

use super::helpers::{apply_prefix_filter_to_config, resolve, setup_query_data};

#[allow(clippy::let_and_return)]
pub fn endpoints() -> Scope {
    let scope = Scope::new("")
        .service(get_handler)
        .service(get_toml_handler)
        .service(resolve_handler)
        .service(reduce_handler)
        .service(list_version_handler)
        .service(get_version_handler)
}

pub fn fetch_audit_id(
    conn: &mut DBConnection,
    schema_name: &SchemaName,
) -> Option<String> {
    event_log::event_log
        .select(event_log::id)
        .filter(event_log::table_name.eq("contexts"))
        .order_by(event_log::timestamp.desc())
        .schema_name(schema_name)
        .first::<Uuid>(conn)
        .map(|uuid| uuid.to_string())
        .ok()
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
    config_versions::config_versions
        .select(max(config_versions::created_at))
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
>>>>>>> 269cf29d (feat: introduce writeback methods for redis)
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
        let mut ct_dimensions: Map<String, Value> = context.condition.clone().into();

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
        if ct.condition.contains_key("variantIds") {
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

    let change_reason =
        ChangeReason::try_from("Context updated during reduce operation".to_string())
            .map_err(|e| unexpected_error!(e))?;

    Ok(PutRequest {
        context,
        r#override: override_,
        description: None,
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
    dimension_schema_map: &HashMap<String, DimensionInfo>,
    default_config: Map<String, Value>,
    is_approve: bool,
    workspace_context: &WorkspaceContext,
    state: &AppState,
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

    for (index, (ctx, _, _, _)) in contexts_overrides_values.iter().enumerate() {
        let weight = calculate_context_weight(&ctx.condition, dimension_schema_map)
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
                        let _ = context::delete(
                            cid.clone(),
                            user,
                            conn,
                            &workspace_context.schema_name,
                        );
                    }
                    og_contexts.retain(|x| x.id != *cid);
                } else {
                    if is_approve {
                        let _ = context::delete(
                            cid.clone(),
                            user,
                            conn,
                            &workspace_context.schema_name,
                        );
                        if let Ok(put_req) = construct_new_payload(request_payload) {
                            let description = match put_req.description.clone() {
                                Some(val) => val,
                                None => query_description(
                                    Value::Object(
                                        put_req.context.clone().into_inner().into(),
                                    ),
                                    conn,
                                    &workspace_context.schema_name,
                                )?,
                            };

                            let _ = context::upsert(
                                put_req,
                                description,
                                conn,
                                false,
                                user,
                                workspace_context,
                                false,
                                &state.master_encryption_key,
                            );
                        }
                    }

                    let override_val = Cac::<Overrides>::validate_db_data(
                        override_val.clone(),
                    )
                    .map_err(|err| {
                        log::error!(
                            "reduce_config_key: failed to decode overrides from db {err}"
                        );
                        unexpected_error!(err)
                    })?
                    .into_inner();

                    let new_id = hash(&Value::Object(override_val.clone().into()));
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
        default_configs: default_config.into(),
        dimensions: dimension_schema_map.clone(),
    })
}

#[authorized]
#[put("/reduce")]
async fn reduce_handler(
    workspace_context: WorkspaceContext,
    req: HttpRequest,
    user: User,
    db_conn: DbConnection,
    state: Data<AppState>,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;
    let is_approve = req
        .headers()
        .get("x-approve")
        .and_then(|value| value.to_str().ok().and_then(|s| s.parse::<bool>().ok()))
        .unwrap_or(false);

    let dimensions_info_map =
        fetch_dimensions_info_map(&mut conn, &workspace_context.schema_name)?;
    let mut config = generate_cac(&mut conn, &workspace_context.schema_name)?;
    let default_config = (*config.default_configs).clone();
    for (key, _) in default_config {
        let contexts = config.contexts;
        let overrides = config.overrides;
        let default_config = config.default_configs.into_inner();
        config = reduce_config_key(
            &user,
            &mut conn,
            contexts.clone(),
            overrides.clone(),
            key.as_str(),
            &dimensions_info_map,
            default_config.clone(),
            is_approve,
            &workspace_context,
            &state,
        )
        .await?;
        if is_approve {
            config = generate_cac(&mut conn, &workspace_context.schema_name)?;
        }
    }

    Ok(HttpResponse::Ok().json(config))
}

#[authorized]
#[routes]
#[get("")]
#[post("")]
async fn get_handler(
    req: HttpRequest,
    body: Option<Json<ContextPayload>>,
    dimension_params: DimensionQuery<QueryMap>,
    query_filters: superposition_query::Query<ConfigQuery>,
    workspace_context: WorkspaceContext,
    state: Data<AppState>,
) -> superposition::Result<HttpResponse> {
    let mut response = HttpResponse::Ok();
    let is_smithy = req.method() != actix_web::http::Method::GET;
    let schema_name = workspace_context.schema_name.clone();
    let max_created_at = fetch_from_redis_else_writeback::<DateTime<Utc>>(
        format!("{}{LAST_MODIFIED_KEY_SUFFIX}", schema_name.0),
        &schema_name,
        state.redis.clone(),
        state.db_pool.clone(),
        |db_pool| {
            let DbConnection(mut conn) = get_db_connection(db_pool)?;
            get_max_created_at(&mut conn, &schema_name).map_err(|e| {
                log::error!("failed to fetch max timestamp from event_log: {e}");
                db_error!(e)
            })
        },
    )
    .await
    .ok();

    log::info!("Max created at: {max_created_at:?}");

    let is_not_modified = is_not_modified(max_created_at, &req);

    if is_not_modified {
        return Ok(HttpResponse::NotModified().finish());
    }

    let query_filters = query_filters.into_inner();
    let version = fetch_from_redis_else_writeback::<i64>(
        format!("{}{CONFIG_VERSION_KEY_SUFFIX}", schema_name.0),
        &schema_name,
        state.redis.clone(),
        state.db_pool.clone(),
        |db_pool| {
            let DbConnection(mut conn) = get_db_connection(db_pool)?;
            get_config_version(&query_filters.version, &workspace_context, &mut conn)
        },
    )
    .await
    .map_err(|e| unexpected_error!("Config version not found due to: {}", e))?;

    let mut config = fetch_from_redis_else_writeback::<Config>(
        format!("{}::{}{CONFIG_KEY_SUFFIX}", schema_name.0, version,),
        &schema_name,
        state.redis.clone(),
        state.db_pool.clone(),
        |db_pool| {
            let DbConnection(mut conn) = get_db_connection(db_pool)?;
            generate_config_from_version(
                &mut Some(version),
                &mut conn,
                &workspace_context.schema_name,
            )
        },
    )
    .await
    .map_err(|e| unexpected_error!("failed to generate config: {}", e))?;
    config = apply_prefix_filter_to_config(&query_filters.prefix, config)?;
    let context = if req.method() == actix_web::http::Method::GET {
        dimension_params.into_inner()
    } else {
        body.map_or_else(QueryMap::default, |body| body.into_inner().context.into())
    };
    if !context.is_empty() {
        config = config.filter_by_dimensions(&context);
    }
    add_last_modified_to_header(max_created_at, is_smithy, &mut response);
    if let Ok(audit_id) = fetch_from_redis_else_writeback::<String>(
        format!("{}{AUDIT_ID_KEY_SUFFIX}", schema_name.0),
        &schema_name,
        state.redis.clone(),
        state.db_pool.clone(),
        |db_pool| {
            let DbConnection(mut conn) = get_db_connection(db_pool)?;
            fetch_audit_id(&mut conn, &workspace_context.schema_name)
                .ok_or(not_found!("Audit ID not found"))
        },
    )
    .await
    {
        response.insert_header((AppHeader::XAuditId.to_string(), audit_id));
    }
    add_config_version_to_header(&Some(version), &mut response);
    Ok(response.json(config))
}

/// Handler that returns config in TOML format with schema information.
/// This uses generate_detailed_cac to fetch schemas from the database.
#[authorized]
#[get("/toml")]
async fn get_toml_handler(
    req: HttpRequest,
    db_conn: DbConnection,
    workspace_context: WorkspaceContext,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;

    let max_created_at = get_max_created_at(&mut conn, &workspace_context.schema_name)
        .map_err(|e| log::error!("failed to fetch max timestamp from event_log: {e}"))
        .ok();

    log::info!("Max created at: {max_created_at:?}");

    if is_not_modified(max_created_at, &req) {
        return Ok(HttpResponse::NotModified().finish());
    }

    let detailed_config =
        generate_detailed_cac(&mut conn, &workspace_context.schema_name)?;

    let toml_str = serialize_to_toml(detailed_config)
        .map_err(|e| unexpected_error!("Failed to serialize config to TOML: {}", e))?;

    let mut response = HttpResponse::Ok();
    add_last_modified_to_header(max_created_at, false, &mut response);
    add_audit_id_to_header(&mut conn, &mut response, &workspace_context.schema_name);
    response.insert_header(("Content-Type", "application/toml"));

    Ok(response.body(toml_str))
}

#[allow(clippy::too_many_arguments)]
#[authorized]
#[routes]
#[get("/resolve")]
#[post("/resolve")]
async fn resolve_handler(
    req: HttpRequest,
    body: Option<Json<ContextPayload>>,
    merge_strategy: Header<MergeStrategy>,
    dimension_params: DimensionQuery<QueryMap>,
    query_filters: superposition_query::Query<ResolveConfigQuery>,
    workspace_context: WorkspaceContext,
    state: Data<AppState>,
) -> superposition::Result<HttpResponse> {
    let query_filters = query_filters.into_inner();
    let schema_name = workspace_context.schema_name.clone();

    let max_created_at = fetch_from_redis_else_writeback::<DateTime<Utc>>(
        format!("{}{LAST_MODIFIED_KEY_SUFFIX}", schema_name.0),
        &schema_name,
        state.redis.clone(),
        state.db_pool.clone(),
        |db_pool| {
            let DbConnection(mut conn) = get_db_connection(db_pool)?;
            get_max_created_at(&mut conn, &schema_name).map_err(|e| {
                log::error!("failed to fetch max timestamp from event_log: {e}");
                db_error!(e)
            })
        },
    )
    .await
    .ok();

    if is_not_modified(max_created_at, &req) {
        return Ok(HttpResponse::NotModified().finish());
    }

    let config_version = fetch_from_redis_else_writeback::<i64>(
        format!("{}{CONFIG_VERSION_KEY_SUFFIX}", schema_name.0),
        &schema_name,
        state.redis.clone(),
        state.db_pool.clone(),
        |db_pool| {
            let DbConnection(mut conn) = get_db_connection(db_pool)?;
            get_config_version(&query_filters.version, &workspace_context, &mut conn)
        },
    )
    .await
    .map_err(|e| unexpected_error!("Config version not found due to: {}", e))?;

    let mut config = fetch_from_redis_else_writeback::<Config>(
        format!("{}::{}{CONFIG_KEY_SUFFIX}", schema_name.0, config_version,),
        &schema_name,
        state.redis.clone(),
        state.db_pool.clone(),
        |db_pool| {
            let DbConnection(mut conn) = get_db_connection(db_pool)?;
            generate_config_from_version(
                &mut Some(config_version),
                &mut conn,
                &workspace_context.schema_name,
            )
        },
    )
    .await
    .map_err(|e| unexpected_error!("failed to generate config: {}", e))?;

    let (is_smithy, query_data) = setup_query_data(&req, &body, &dimension_params)?;

    let resolved_config = {
        let DbConnection(mut conn) = get_db_connection(state.db_pool.clone())?;
        resolve(
            &mut config,
            query_data,
            merge_strategy,
            &mut conn,
            &query_filters,
            &workspace_context,
            &state.master_encryption_key,
        )?
    };

    let mut resp = HttpResponse::Ok();
    add_last_modified_to_header(max_created_at, is_smithy, &mut resp);
    if let Ok(audit_id) = fetch_from_redis_else_writeback::<String>(
        format!("{}{AUDIT_ID_KEY_SUFFIX}", schema_name.0),
        &schema_name,
        state.redis.clone(),
        state.db_pool.clone(),
        |db_pool| {
            let DbConnection(mut conn) = get_db_connection(db_pool)?;
            fetch_audit_id(&mut conn, &workspace_context.schema_name)
                .ok_or(not_found!("Audit ID not found"))
        },
    )
    .await
    {
        resp.insert_header((AppHeader::XAuditId.to_string(), audit_id));
    }
    add_config_version_to_header(&Some(config_version), &mut resp);
    Ok(resp.json(resolved_config))
}

#[authorized]
#[get("/versions")]
async fn list_version_handler(
    workspace_context: WorkspaceContext,
    db_conn: DbConnection,
    filters: Query<PaginationParams>,
) -> superposition::Result<Json<PaginatedResponse<ConfigVersionListItem>>> {
    let DbConnection(mut conn) = db_conn;

    let n_version: i64 = config_versions::config_versions
        .count()
        .schema_name(&workspace_context.schema_name)
        .get_result(&mut conn)?;

    let limit = filters.count.unwrap_or(10);
    let mut builder = config_versions::config_versions
        .schema_name(&workspace_context.schema_name)
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

#[authorized]
#[get("/version/{version}")]
async fn get_version_handler(
    workspace_context: WorkspaceContext,
    db_conn: DbConnection,
    version: Path<i64>,
) -> superposition::Result<Json<ConfigVersion>> {
    let DbConnection(mut conn) = db_conn;

    let config_version = config_versions::config_versions
        .schema_name(&workspace_context.schema_name)
        .find(version.into_inner())
        .get_result::<ConfigVersion>(&mut conn)?;

    Ok(Json(config_version))
}
