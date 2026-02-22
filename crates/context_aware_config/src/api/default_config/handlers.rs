use std::{
    cmp::{Ordering, min},
    collections::{HashMap, HashSet},
};

use actix_web::{
    Either, HttpResponse, Scope, delete, get, post, routes,
    web::{Data, Json, Path, Query},
};
use chrono::{DateTime, Utc};
use diesel::{
    Connection, ExpressionMethods, PgTextExpressionMethods, QueryDsl, RunQueryDsl,
    SelectableHelper,
};
use jsonschema::ValidationError;
use serde_json::Value;
use service_utils::{
    helpers::parse_config_tags,
    service::types::{
        AppHeader, AppState, CustomHeaders, DbConnection, EncryptionKey, SchemaName,
        WorkspaceContext,
    },
};
use superposition_core::validations::{try_into_jsonschema, validation_err_to_str};
use superposition_derives::authorized;
use superposition_macros::{
    bad_argument, db_error, not_found, unexpected_error, validation_error,
};
use superposition_types::{
    DBConnection, PaginatedResponse, SortBy, User,
    api::{
        default_config::{
            DefaultConfigCreateRequest, DefaultConfigFilters, DefaultConfigKey,
            DefaultConfigUpdateRequest, ListDefaultConfigResponse, SortOn,
        },
        functions::{FunctionEnvironment, FunctionExecutionRequest, KeyType},
    },
    custom_query::PaginationParams,
    database::{
        models::{
            Description,
            cac::{self as models, Context, DefaultConfig, FunctionType},
        },
        schema::{self, contexts::dsl::contexts, default_configs::dsl},
    },
    result as superposition,
};

#[cfg(feature = "high-performance-mode")]
use crate::helpers::put_config_in_redis;
use crate::{
    api::{
        context::helpers::validation_function_executor,
        default_config::helpers::{
            InternalStructure, get_from_unflattened_map, unflatten_map,
        },
        functions::{
            helpers::{check_fn_published, get_published_function_code},
            types::FunctionInfo,
        },
    },
    helpers::{add_config_version, validate_change_reason},
};

pub fn endpoints() -> Scope {
    Scope::new("")
        .service(create_handler)
        .service(update_handler)
        .service(get_handler)
        .service(list_handler)
        .service(delete_handler)
}

#[authorized]
#[post("")]
async fn create_handler(
    workspace_context: WorkspaceContext,
    state: Data<AppState>,
    custom_headers: CustomHeaders,
    request: Json<DefaultConfigCreateRequest>,
    db_conn: DbConnection,
    user: User,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;
    let req = request.into_inner();
    let key = req.key;
    let tags = parse_config_tags(custom_headers.config_tags)?;

    if req.schema.is_empty() {
        return Err(bad_argument!("Schema cannot be empty."));
    }

    validate_change_reason(
        &workspace_context,
        &req.change_reason,
        &mut conn,
        &state.master_encryption_key,
    )?;

    let value = req.value;

    let default_config = DefaultConfig {
        key: key.to_owned(),
        value,
        schema: req.schema,
        value_validation_function_name: req.value_validation_function_name,
        created_by: user.get_email(),
        created_at: Utc::now(),
        last_modified_at: Utc::now(),
        last_modified_by: user.get_email(),
        description: req.description,
        change_reason: req.change_reason.clone(),
        value_compute_function_name: req.value_compute_function_name,
    };

    let schema = Value::from(&default_config.schema);

    let schema_compile_result = try_into_jsonschema(&schema);
    let jschema = match schema_compile_result {
        Ok(jschema) => jschema,
        Err(e) => {
            log::info!("Failed to compile as a Draft-7 JSON schema: {e}");
            return Err(bad_argument!("Invalid JSON schema (failed to compile)"));
        }
    };

    if let Err(e) = jschema.validate(&default_config.value) {
        let verrors = e.collect::<Vec<ValidationError>>();
        log::info!(
            "Validation for value with given JSON schema failed: {:?}",
            verrors
        );
        return Err(validation_error!(
            "Schema validation failed: {}",
            &validation_err_to_str(verrors)
                .first()
                .unwrap_or(&String::new())
        ));
    }

    validate_default_config_with_function(
        &workspace_context,
        &mut conn,
        &default_config.value_validation_function_name,
        &default_config.key,
        &default_config.value,
        &state.master_encryption_key,
    )?;

    validate_fn_published(
        &default_config.value_compute_function_name,
        FunctionType::ValueCompute,
        &mut conn,
        &workspace_context.schema_name,
    )?;

    let version_id =
        conn.transaction::<_, superposition::AppError, _>(|transaction_conn| {
            diesel::insert_into(dsl::default_configs)
                .values(&default_config)
                .returning(DefaultConfig::as_returning())
                .schema_name(&workspace_context.schema_name)
                .execute(transaction_conn)?;

            let version_id = add_config_version(
                &state,
                tags,
                req.change_reason.into(),
                transaction_conn,
                &workspace_context.schema_name,
            )?;
            Ok(version_id)
        })?;

    #[cfg(feature = "high-performance-mode")]
    put_config_in_redis(version_id, state, &workspace_context.schema_name, &mut conn)
        .await?;
    let mut http_resp = HttpResponse::Ok();

    http_resp.insert_header((
        AppHeader::XConfigVersion.to_string(),
        version_id.to_string(),
    ));

    Ok(http_resp.json(default_config))
}

#[authorized]
#[get("/{key}")]
async fn get_handler(
    workspace_context: WorkspaceContext,
    key: Path<DefaultConfigKey>,
    db_conn: DbConnection,
) -> superposition::Result<Json<DefaultConfig>> {
    let DbConnection(mut conn) = db_conn;
    let res = fetch_default_key(&key, &mut conn, &workspace_context.schema_name)?;
    Ok(Json(res))
}

#[allow(clippy::too_many_arguments)]
#[authorized]
#[routes]
#[put("/{key}")]
#[patch("/{key}")]
async fn update_handler(
    workspace_context: WorkspaceContext,
    state: Data<AppState>,
    key: Path<DefaultConfigKey>,
    custom_headers: CustomHeaders,
    request: Json<DefaultConfigUpdateRequest>,
    db_conn: DbConnection,
    user: User,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;
    let req = request.into_inner();
    let key_str = key.into_inner().into();
    let tags = parse_config_tags(custom_headers.config_tags)?;

    let existing = fetch_default_key(&key_str, &mut conn, &workspace_context.schema_name)
        .map_err(|e| match e {
            superposition::AppError::DbError(diesel::NotFound) => {
                bad_argument!(
                    "No record found for {}. Use create endpoint instead.",
                    key_str
                )
            }
            _ => {
                log::error!("Failed to fetch {key_str}: {e}");
                unexpected_error!("Something went wrong.")
            }
        })?;

    validate_change_reason(
        &workspace_context,
        &req.change_reason,
        &mut conn,
        &state.master_encryption_key,
    )?;

    let value = req.value.clone().unwrap_or_else(|| existing.value.clone());

    if let Some(ref schema) = req.schema {
        let schema = Value::from(schema);

        let jschema = try_into_jsonschema(&schema).map_err(|e| {
            log::info!("Failed to compile JSON schema: {e}");
            bad_argument!("Invalid JSON schema.")
        })?;

        jschema.validate(&value).map_err(|e| {
            let verrors = e.collect::<Vec<ValidationError>>();
            validation_error!(
                "Schema validation failed: {}",
                &validation_err_to_str(verrors)
                    .first()
                    .unwrap_or(&String::new())
            )
        })?;
    }

    if let Some(ref validation_function_name) = req.value_validation_function_name {
        let value = req.value.clone().unwrap_or_else(|| existing.value.clone());

        validate_default_config_with_function(
            &workspace_context,
            &mut conn,
            validation_function_name,
            &key_str,
            &value,
            &state.master_encryption_key,
        )?
    }

    if let Some(ref value_compute_function_name) = req.value_compute_function_name {
        validate_fn_published(
            value_compute_function_name,
            FunctionType::ValueCompute,
            &mut conn,
            &workspace_context.schema_name,
        )?;
    }

    let (db_row, version_id) =
        conn.transaction::<_, superposition::AppError, _>(|transaction_conn| {
            let change_reason = req.change_reason.clone();
            let val = diesel::update(dsl::default_configs)
                .filter(dsl::key.eq(key_str.clone()))
                .set((
                    req,
                    dsl::last_modified_at.eq(Utc::now()),
                    dsl::last_modified_by.eq(user.get_email()),
                ))
                .schema_name(&workspace_context.schema_name)
                .get_result::<DefaultConfig>(transaction_conn)?;

            let version_id = add_config_version(
                &state,
                tags.clone(),
                change_reason.into(),
                transaction_conn,
                &workspace_context.schema_name,
            )?;

            Ok((val, version_id))
        })?;

    #[cfg(feature = "high-performance-mode")]
    put_config_in_redis(version_id, state, &workspace_context.schema_name, &mut conn)
        .await?;

    let mut http_resp = HttpResponse::Ok();
    http_resp.insert_header((
        AppHeader::XConfigVersion.to_string(),
        version_id.to_string(),
    ));
    Ok(http_resp.json(db_row))
}

fn validate_fn_published(
    function: &Option<String>,
    f_type: FunctionType,
    conn: &mut DBConnection,
    schema_name: &SchemaName,
) -> superposition::Result<()> {
    let Some(func_name) = function else {
        return Ok(());
    };
    check_fn_published(func_name, f_type, conn, schema_name)
}

fn validate_default_config_with_function(
    workspace_context: &WorkspaceContext,
    conn: &mut DBConnection,
    function_name: &Option<String>,
    key: &str,
    value: &Value,
    master_encryption_key: &Option<EncryptionKey>,
) -> superposition::Result<()> {
    if let Some(f_name) = function_name {
        let FunctionInfo {
            published_code: function_code,
            published_runtime_version: function_version,
            ..
        } = get_published_function_code(
            conn,
            f_name,
            FunctionType::ValueValidation,
            &workspace_context.schema_name,
        )
        .map_err(|_| {
            bad_argument!("Function {}'s published code does not exist.", f_name)
        })?;
        if let (Some(f_code), Some(f_version)) = (function_code, function_version) {
            validation_function_executor(
                workspace_context,
                f_name.as_str(),
                &f_code,
                &FunctionExecutionRequest::ValueValidationFunctionRequest {
                    key: key.to_string(),
                    value: value.clone(),
                    r#type: KeyType::ConfigKey,
                    environment: FunctionEnvironment::default(),
                },
                f_version,
                conn,
                master_encryption_key,
            )?;
        }
    };
    Ok(())
}

fn fetch_default_key(
    key: &String,
    conn: &mut DBConnection,
    schema_name: &SchemaName,
) -> superposition::Result<models::DefaultConfig> {
    let res = dsl::default_configs
        .filter(schema::default_configs::key.eq(key))
        .select(models::DefaultConfig::as_select())
        .schema_name(schema_name)
        .get_result(conn)?;
    Ok(res)
}

fn get_extreme_date<F>(
    entry: &InternalStructure,
    field_extractor: F,
    comparator: fn(DateTime<Utc>, DateTime<Utc>) -> DateTime<Utc>,
) -> Option<DateTime<Utc>>
where
    F: Fn(&DefaultConfig) -> DateTime<Utc> + Copy,
{
    fn inner<F>(
        value: &InternalStructure,
        field_extractor: F,
        comparator: fn(DateTime<Utc>, DateTime<Utc>) -> DateTime<Utc>,
    ) -> Option<DateTime<Utc>>
    where
        F: Fn(&DefaultConfig) -> DateTime<Utc> + Copy,
    {
        let mut result = value.value.as_ref().map(field_extractor);
        for child in value.sub_keys.values() {
            if let Some(child_date) = inner(child, field_extractor, comparator) {
                result = Some(match result {
                    Some(current) => comparator(current, child_date),
                    None => child_date,
                });
            }
        }
        result
    }
    inner(entry, field_extractor, comparator)
}

fn earliest_created_at(entry: &InternalStructure) -> Option<DateTime<Utc>> {
    get_extreme_date(entry, |config| config.created_at, DateTime::min)
}

fn latest_created_at(entry: &InternalStructure) -> Option<DateTime<Utc>> {
    get_extreme_date(entry, |config| config.created_at, DateTime::max)
}

fn earliest_modified_at(entry: &InternalStructure) -> Option<DateTime<Utc>> {
    get_extreme_date(entry, |config| config.last_modified_at, DateTime::min)
}

fn latest_modified_at(entry: &InternalStructure) -> Option<DateTime<Utc>> {
    get_extreme_date(entry, |config| config.last_modified_at, DateTime::max)
}

fn apply_sort_order(sort_by: SortBy, ord: Ordering) -> Ordering {
    match sort_by {
        SortBy::Asc => ord,
        SortBy::Desc => ord.reverse(),
    }
}

fn sort_keys(mut keys: Vec<String>, sort_by: SortBy) -> Vec<String> {
    keys.sort_by(|a, b| apply_sort_order(sort_by, a.cmp(b)));
    keys
}

fn sort_groups_by_date(
    filtered: &InternalStructure,
    sort_by: SortBy,
    date_for: fn(&InternalStructure) -> Option<DateTime<Utc>>,
) -> Vec<ListDefaultConfigResponse> {
    let mut group_entries = filtered
        .sub_keys
        .iter()
        .filter(|(_, v)| !v.sub_keys.is_empty())
        .map(|(k, v)| (k.clone(), date_for(v)))
        .collect::<Vec<_>>();

    group_entries.sort_by(|(a_key, a_date), (b_key, b_date)| {
        let ord = match (a_date, b_date) {
            (Some(a_dt), Some(b_dt)) => a_dt.cmp(b_dt),
            (None, Some(_)) => Ordering::Less,
            (Some(_), None) => Ordering::Greater,
            (None, None) => a_key.cmp(b_key),
        };
        apply_sort_order(sort_by, ord)
    });

    group_entries
        .into_iter()
        .map(|(k, _)| ListDefaultConfigResponse::Group(k))
        .collect::<Vec<_>>()
}

fn build_group_data(
    filtered: &InternalStructure,
    sort_on: SortOn,
    sort_by: SortBy,
) -> Vec<ListDefaultConfigResponse> {
    let date_fn = match (sort_on, sort_by) {
        (SortOn::Key, _) => {
            let keys = filtered
                .sub_keys
                .iter()
                .filter(|(_, v)| !v.sub_keys.is_empty())
                .map(|(k, _)| k.clone())
                .collect::<Vec<_>>();

            return sort_keys(keys, sort_by)
                .into_iter()
                .map(ListDefaultConfigResponse::Group)
                .collect();
        }
        (SortOn::CreatedAt, SortBy::Asc) => earliest_created_at,
        (SortOn::CreatedAt, SortBy::Desc) => latest_created_at,
        (SortOn::LastModifiedAt, SortBy::Asc) => earliest_modified_at,
        (SortOn::LastModifiedAt, SortBy::Desc) => latest_modified_at,
    };

    sort_groups_by_date(filtered, sort_by, date_fn)
}

fn build_config_data(
    filtered: &InternalStructure,
    sort_on: SortOn,
    sort_by: SortBy,
) -> Vec<ListDefaultConfigResponse> {
    let mut configs = filtered
        .sub_keys
        .values()
        .filter_map(|v| v.value.clone())
        .collect::<Vec<_>>();

    configs.sort_by(|a, b| {
        let ord = match sort_on {
            SortOn::Key => a.key.cmp(&b.key),
            SortOn::CreatedAt => a.created_at.cmp(&b.created_at),
            SortOn::LastModifiedAt => a.last_modified_at.cmp(&b.last_modified_at),
        };
        apply_sort_order(sort_by, ord)
    });

    configs
        .into_iter()
        .map(ListDefaultConfigResponse::Config)
        .collect()
}

fn list_grouped_configs(
    schema_name: &SchemaName,
    conn: &mut DBConnection,
    filters: &DefaultConfigFilters,
    offset: i64,
    count: i64,
    show_all: bool,
) -> superposition::Result<PaginatedResponse<ListDefaultConfigResponse>> {
    let configs = dsl::default_configs
        .schema_name(schema_name)
        .get_results::<DefaultConfig>(conn)?;

    let unflattened_config_map = unflatten_map(configs)
        .map_err(|e| unexpected_error!("Failed to group configs: {}", e))?;

    let prefix = filters.prefix.clone().unwrap_or_default();
    let prefix_filtered = get_from_unflattened_map(unflattened_config_map, &prefix);

    let name_filtered = match (prefix_filtered, filters.name.as_ref()) {
        (Some(filtered), Some(name_filters)) => {
            let name_set = name_filters.iter().cloned().collect::<HashSet<_>>();
            let filtered_sub_keys = filtered
                .sub_keys
                .into_iter()
                .filter(|(k, _)| name_set.contains(k))
                .collect::<HashMap<String, InternalStructure>>();

            Some(InternalStructure {
                value: filtered.value,
                sub_keys: filtered_sub_keys,
            })
        }
        (Some(filtered), None) => Some(filtered),
        (None, _) => None,
    };

    let sort_on = filters.sort_on.unwrap_or_default();
    let sort_by = filters.sort_by.unwrap_or_default();

    let (mut group_data, mut config_data) = match name_filtered.as_ref() {
        Some(filtered) => (
            build_group_data(filtered, sort_on, sort_by),
            build_config_data(filtered, sort_on, sort_by),
        ),
        None => (Vec::new(), Vec::new()),
    };

    let mut data: Vec<ListDefaultConfigResponse> =
        Vec::with_capacity(group_data.len() + config_data.len());
    data.append(&mut group_data);
    data.append(&mut config_data);

    let resp = if show_all {
        PaginatedResponse::all(data)
    } else {
        let total_items = data.len();
        let start = offset as usize;
        let end = min((offset + count) as usize, total_items);
        let data = data
            .get(start..end)
            .map(|slice| slice.to_vec())
            .unwrap_or_default();

        PaginatedResponse {
            total_pages: (total_items as f64 / count as f64).ceil() as i64,
            total_items: total_items as i64,
            data,
        }
    };

    Ok(resp)
}

#[authorized]
#[get("")]
async fn list_handler(
    workspace_context: WorkspaceContext,
    db_conn: DbConnection,
    pagination: Query<PaginationParams>,
    filters: Query<DefaultConfigFilters>,
) -> superposition::Result<
    Either<
        Json<PaginatedResponse<DefaultConfig>>,
        Json<PaginatedResponse<ListDefaultConfigResponse>>,
    >,
> {
    let DbConnection(mut conn) = db_conn;

    let filters = filters.into_inner();

    let page = pagination.page.unwrap_or(1);
    let count = pagination.count.unwrap_or(10);
    let show_all = pagination.all.unwrap_or_default();
    let offset = count * (page - 1);

    let grouped_config = filters.search.is_none()
        && (filters.grouped.unwrap_or_default() || filters.prefix.is_some());

    if grouped_config {
        let resp = list_grouped_configs(
            &workspace_context.schema_name,
            &mut conn,
            &filters,
            offset,
            count,
            show_all,
        )?;
        return Ok(Either::Right(Json(resp)));
    }

    let query_builder = |filters: &DefaultConfigFilters| {
        let mut builder = dsl::default_configs
            .schema_name(&workspace_context.schema_name)
            .into_boxed();
        if let Some(ref config_names) = filters.name {
            builder = builder.filter(dsl::key.eq_any(config_names.0.clone()));
        } else if let Some(ref search) = filters.search {
            let pattern = format!("%{}%", search);
            builder = builder.filter(dsl::key.ilike(pattern));
        }

        builder
    };

    let sort_on = filters.sort_on.unwrap_or_default();
    let sort_by = filters.sort_by.unwrap_or_default();

    let base_query = match (sort_on, sort_by) {
        (SortOn::Key, SortBy::Asc) => query_builder(&filters).order(dsl::key.asc()),
        (SortOn::Key, SortBy::Desc) => query_builder(&filters).order(dsl::key.desc()),
        (SortOn::CreatedAt, SortBy::Asc) => {
            query_builder(&filters).order(dsl::created_at.asc())
        }
        (SortOn::CreatedAt, SortBy::Desc) => {
            query_builder(&filters).order(dsl::created_at.desc())
        }
        (SortOn::LastModifiedAt, SortBy::Asc) => {
            query_builder(&filters).order(dsl::last_modified_at.asc())
        }
        (SortOn::LastModifiedAt, SortBy::Desc) => {
            query_builder(&filters).order(dsl::last_modified_at.desc())
        }
    };

    if show_all {
        let result = base_query.get_results::<DefaultConfig>(&mut conn)?;
        return Ok(Either::Left(Json(PaginatedResponse::all(result))));
    }

    let count_query = query_builder(&filters);

    let n_default_configs: i64 = count_query.count().get_result(&mut conn)?;
    let result = base_query
        .limit(count)
        .offset(offset)
        .load::<DefaultConfig>(&mut conn)?;

    let total_pages = (n_default_configs as f64 / count as f64).ceil() as i64;

    Ok(Either::Left(Json(PaginatedResponse {
        total_pages,
        total_items: n_default_configs,
        data: result,
    })))
}

pub fn get_key_usage_context_ids(
    key: &str,
    conn: &mut DBConnection,
    schema_name: &SchemaName,
) -> superposition::Result<Vec<String>> {
    let result: Vec<Context> =
        contexts
            .schema_name(schema_name)
            .load(conn)
            .map_err(|err| {
                log::error!("failed to fetch contexts with error: {}", err);
                db_error!(err)
            })?;

    let mut context_ids = vec![];
    for context in result.iter() {
        context
            .override_
            .get(key)
            .map_or((), |_| context_ids.push(context.id.to_owned()))
    }
    Ok(context_ids)
}

#[authorized]
#[delete("/{key}")]
async fn delete_handler(
    workspace_context: WorkspaceContext,
    state: Data<AppState>,
    path: Path<DefaultConfigKey>,
    custom_headers: CustomHeaders,
    db_conn: DbConnection,
    user: User,
) -> superposition::Result<HttpResponse> {
    let DbConnection(mut conn) = db_conn;
    let tags = parse_config_tags(custom_headers.config_tags)?;

    let key: String = path.into_inner().into();
    let mut version_id = 0;

    let context_ids =
        get_key_usage_context_ids(&key, &mut conn, &workspace_context.schema_name)
            .map_err(|_| unexpected_error!("Something went wrong"))?;
    if context_ids.is_empty() {
        let resp: Result<HttpResponse, superposition::AppError> =
            conn.transaction::<_, superposition::AppError, _>(|transaction_conn| {
                diesel::update(dsl::default_configs)
                    .filter(dsl::key.eq(&key))
                    .set((
                        dsl::last_modified_at.eq(Utc::now()),
                        dsl::last_modified_by.eq(user.get_email()),
                    ))
                    .schema_name(&workspace_context.schema_name)
                    .execute(transaction_conn)?;

                let deleted_row =
                    diesel::delete(dsl::default_configs.filter(dsl::key.eq(&key)))
                        .schema_name(&workspace_context.schema_name)
                        .execute(transaction_conn);
                match deleted_row {
                    Ok(0) => {
                        Err(not_found!("default config key `{}` doesn't exists", key))
                    }
                    Ok(_) => {
                        let config_version_desc = Description::try_from(format!(
                            "Context Deleted by {}",
                            user.get_email()
                        ))
                        .map_err(|e| unexpected_error!(e))?;
                        version_id = add_config_version(
                            &state,
                            tags,
                            config_version_desc,
                            transaction_conn,
                            &workspace_context.schema_name,
                        )?;
                        log::info!(
                            "default config key: {key} deleted by {}",
                            user.get_email()
                        );
                        Ok(HttpResponse::NoContent()
                            .insert_header((
                                AppHeader::XConfigVersion.to_string(),
                                version_id.to_string(),
                            ))
                            .finish())
                    }
                    Err(e) => {
                        log::error!("default config delete query failed with error: {e}");
                        Err(unexpected_error!("Something went wrong."))
                    }
                }
            });

        if resp.is_ok() {
            #[cfg(feature = "high-performance-mode")]
            put_config_in_redis(
                version_id,
                state,
                &workspace_context.schema_name,
                &mut conn,
            )
            .await?;
        }
        resp
    } else {
        Err(bad_argument!(
            "Given key already in use in contexts: {}",
            context_ids.join(",")
        ))
    }
}
