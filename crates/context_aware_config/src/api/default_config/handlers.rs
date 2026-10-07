use std::ops::Deref;

use actix_web::{
    HttpResponse, Scope, delete, get, post, routes,
    web::{Data, Json, Path, Query},
};
use chrono::Utc;
use diesel::{
    Connection, ExpressionMethods, OptionalExtension, QueryDsl, RunQueryDsl,
    SelectableHelper, TextExpressionMethods,
};
use jsonschema::ValidationError;
use serde_json::Value;
use service_utils::{
    helpers::{WebhookData, execute_webhook_call, parse_config_tags},
    service::types::{
        AppHeader, AppState, CustomHeaders, DbConnection, EncryptionKey, SchemaName,
        WorkspaceContext, WorkspaceWritePermit,
    },
};
use superposition_core::validations::{try_into_jsonschema, validation_err_to_str};
use superposition_derives::{authorized, declare_resource};
use superposition_macros::{
    bad_argument, db_error, not_found, unexpected_error, validation_error,
};
use superposition_types::{
    DBConnection, ExtendedMap, PaginatedResponse, Resource, User,
    api::{
        default_config::{
            DefaultConfigCreateRequest, DefaultConfigFilters, DefaultConfigKey,
            DefaultConfigResponse, DefaultConfigUpdateRequest,
        },
        functions::{FunctionEnvironment, FunctionExecutionRequest, KeyType},
        webhook::Action,
    },
    custom_query::PaginationParams,
    database::{
        models::{
            Description,
            cac::{self as models, Context, DefaultConfig, FunctionType},
            others::WebhookEvent,
        },
        schema::{self, contexts::dsl::contexts, default_configs::dsl},
    },
    result as superposition,
    symlink::{carries_symlink_marker, normalize_symlink_write, symlink_target},
};

use crate::{
    api::{
        context::helpers::validation_function_executor,
        functions::{
            helpers::{check_fn_published, get_published_function_code},
            types::FunctionInfo,
        },
    },
    helpers::{add_config_version, put_config_in_redis, validate_change_reason},
    symlinks::{
        experiment_dependents, flatten_target, resolve_for_response,
        resolve_many_for_response, symlink_conversion_refusal, symlink_dependents,
    },
};

declare_resource!(DefaultConfig);

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
    mut write_permit: WorkspaceWritePermit,
    user: User,
) -> superposition::Result<HttpResponse> {
    let req = request.into_inner();
    let conn = write_permit.connection();

    // Authorize the key the caller named before anything is looked up.
    //
    // `#[authorized]` only proves the caller may perform *this action on this
    // resource* at all - its extractor runs `is_allowed` with no attributes - so it
    // says nothing about this particular key. `flatten_target` below reports
    // whether an arbitrary caller-supplied key exists, which would make it an
    // existence oracle for a caller with no authority over the key they are
    // writing. One ordering rule, shared with `update_handler`: authorize the named
    // key, then fetch or flatten, then additionally authorize the redirected
    // target.
    _auth_z.authorized(&[req.key.deref()]).await?;

    // A create whose schema carries the symlink marker is a link, not an ordinary
    // key: its value names a target, which must already exist and must not be
    // another link (flatten_target keeps stored links at depth 1).
    //
    // The gate is marker *presence*, not `is_symlink_schema`: a marker that is
    // there but isn't the boolean `true` must reach `normalize_symlink_write` and
    // be rejected. Gating on `is_symlink_schema` instead let such a row through as
    // an ordinary key — skipping the target authorization below — while the read
    // path's SQL predicate still saw a link and published the target's value under
    // this name.
    let symlink = if carries_symlink_marker(req.schema.inner()) {
        let (requested, canonical) =
            normalize_symlink_write(req.schema.inner(), &req.value)
                .map_err(|e| bad_argument!("{}", e))?;

        if req.value_validation_function_name.is_some()
            || req.value_compute_function_name.is_some()
        {
            return Err(bad_argument!(
                "a symlink cannot carry validation or compute functions; \
                 they belong to its target `{requested}`"
            ));
        }

        let target = flatten_target(conn, &workspace_context.schema_name, &requested)?;
        if target == *req.key {
            return Err(bad_argument!("a symlink cannot point at itself"));
        }

        // Creating a link needs authority over the target too: otherwise a
        // principal could create `allowed.alias -> restricted.key` and surface a
        // restricted key's value under a name a prefix-scoped reader is permitted
        // to see.
        _auth_z.authorized(&[req.key.deref(), &target]).await?;

        Some((target, canonical))
    } else {
        None
    };

    let key = req.key;
    let tags = parse_config_tags(custom_headers.config_tags)?;

    if req.schema.is_empty() {
        return Err(bad_argument!("Schema cannot be empty."));
    }

    validate_change_reason(
        &workspace_context,
        &req.change_reason,
        conn,
        &state.master_encryption_key,
    )
    .await?;

    let (value, schema) = match symlink {
        Some((target, canonical)) => {
            (Value::String(target), ExtendedMap::from(canonical))
        }
        None => (req.value, req.schema),
    };

    let default_config = DefaultConfig {
        key: key.to_owned(),
        value,
        schema,
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
        conn,
        &default_config.value_validation_function_name,
        &default_config.key,
        &default_config.value,
        &state.master_encryption_key,
    )
    .await?;

    validate_fn_published(
        &default_config.value_compute_function_name,
        FunctionType::ValueCompute,
        conn,
        &workspace_context.schema_name,
    )?;

    let config_version =
        conn.transaction::<_, superposition::AppError, _>(|transaction_conn| {
            diesel::insert_into(dsl::default_configs)
                .values(&default_config)
                .returning(DefaultConfig::as_returning())
                .schema_name(&workspace_context.schema_name)
                .execute(transaction_conn)?;

            let config_version = add_config_version(
                &state,
                tags,
                req.change_reason.into(),
                transaction_conn,
                &workspace_context.schema_name,
            )?;
            Ok(config_version)
        })?;

    let _ = put_config_in_redis(
        &config_version,
        &state,
        &workspace_context.schema_name,
        conn,
    )
    .await;

    let data = WebhookData {
        payload: &default_config,
        resource: Resource::DefaultConfig,
        event: WebhookEvent::ConfigChanged,
        config_version_opt: Some(config_version.id.to_string()),
        action: Action::Create,
    };

    let webhook_status =
        execute_webhook_call(data, &workspace_context, &state, conn).await;

    let mut http_resp = if webhook_status {
        HttpResponse::Ok()
    } else {
        HttpResponse::build(
            actix_web::http::StatusCode::from_u16(512)
                .unwrap_or(actix_web::http::StatusCode::INTERNAL_SERVER_ERROR),
        )
    };

    http_resp.insert_header((
        AppHeader::XConfigVersion.to_string(),
        config_version.id.to_string(),
    ));

    let response =
        resolve_for_response(conn, &workspace_context.schema_name, default_config)?;
    Ok(http_resp.json(response))
}

#[authorized]
#[get("/{key}")]
async fn get_handler(
    workspace_context: WorkspaceContext,
    key: Path<DefaultConfigKey>,
    db_conn: DbConnection,
) -> superposition::Result<Json<DefaultConfigResponse>> {
    let DbConnection(mut conn) = db_conn;
    let res = fetch_default_key(&key, &mut conn, &workspace_context.schema_name)?;
    let resolved = resolve_for_response(&mut conn, &workspace_context.schema_name, res)?;
    Ok(Json(resolved))
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
    mut write_permit: WorkspaceWritePermit,
    user: User,
) -> superposition::Result<HttpResponse> {
    let key = key.into_inner();
    let mut req = request.into_inner();
    let key_str: String = key.into();
    let tags = parse_config_tags(custom_headers.config_tags)?;

    let conn = write_permit.connection();

    // Authorize the key the caller named before the row is fetched.
    //
    // `#[authorized]`'s extractor checks only the action against the resource,
    // with no attributes, so without this an unauthorized caller got
    // "No record found for X" (or the symlink-target probe further down) instead
    // of a 403 - an existence oracle for any key they cannot write. The
    // redirected target is authorized additionally, below, once it is known.
    _auth_z.authorized(&[&key_str]).await?;

    let existing = fetch_default_key(&key_str, conn, &workspace_context.schema_name)
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

    // The update discriminator. Exactly one of these holds:
    // - the schema carries the symlink marker: the write repoints the link
    //   itself, regardless of what else is present;
    // - it doesn't, but the write carries `value`, `schema` or a function name:
    //   that addresses the *value*, so it redirects to the target when `existing`
    //   is a link;
    // - neither: a description-only write (plus the mandatory `change_reason`)
    //   stays on the link's own row.
    let repoint = req
        .schema
        .as_ref()
        .map(|schema| carries_symlink_marker(schema.inner()))
        .unwrap_or(false);

    if repoint
        && (req.value_validation_function_name.is_some()
            || req.value_compute_function_name.is_some())
    {
        return Err(bad_argument!(
            "a symlink cannot carry validation or compute functions; they belong to its target"
        ));
    }

    let addresses_value = req.value.is_some()
        || req.schema.is_some()
        || req.value_validation_function_name.is_some()
        || req.value_compute_function_name.is_some();

    let existing_target =
        symlink_target(existing.schema.inner(), &existing.value).map(str::to_string);

    let addressed_key = match (repoint, addresses_value, &existing_target) {
        (true, _, _) => key_str.clone(),
        (false, true, Some(target)) => target.clone(),
        (false, _, _) => key_str.clone(),
    };

    if repoint {
        let value = req.value.clone().ok_or_else(|| {
            bad_argument!("repointing a symlink requires its new target as value")
        })?;
        let (requested, canonical) = normalize_symlink_write(
            req.schema
                .as_ref()
                .expect("repoint is true only when schema carries the marker")
                .inner(),
            &value,
        )
        .map_err(|e| bad_argument!("{}", e))?;
        let target = flatten_target(conn, &workspace_context.schema_name, &requested)?;
        if target == key_str {
            return Err(bad_argument!("a symlink cannot point at itself"));
        }

        // Repointing surfaces a (possibly different) target's value under this
        // existing name, exactly like creating a link, so it needs authority
        // over both.
        _auth_z.authorized(&[&key_str, &target]).await?;

        // Converting an existing key into a symlink is the feature's primary use
        // case, and it is the write that empties the key of its own value: from
        // here on `generate_cac` excludes the row and `expand_symlinks` fills the
        // name in from the target. Anything that still depends on this key holding
        // its own value must be dealt with first, or the invariant
        // `eval(config)[link] == eval(config)[target]` breaks in silence. This
        // mirrors `delete_handler`, which refuses for the same reasons.
        //
        // Repointing a key that is *already* a symlink necessarily finds all three
        // lists empty - overrides naming a link are rewritten to its target on
        // write, and a link is never another link's target - so this is a no-op
        // there, and only bites on the conversion.
        let dependents =
            symlink_dependents(conn, &workspace_context.schema_name, &key_str)?;
        let context_ids =
            get_key_usage_context_ids(&key_str, conn, &workspace_context.schema_name)?;
        let experiment_ids =
            experiment_dependents(conn, &workspace_context.schema_name, &key_str)?;
        if let Some(refusal) = symlink_conversion_refusal(
            &key_str,
            &target,
            &dependents,
            &context_ids,
            &experiment_ids,
        ) {
            return Err(bad_argument!("{}", refusal));
        }

        req.value = Some(Value::String(target));
        req.schema = Some(ExtendedMap::from(canonical));
    } else if addressed_key != key_str {
        // A value write against a link lands on its target, so the target needs
        // authorizing too - additionally, not instead: `key_str` was authorized
        // before the fetch above.
        _auth_z.authorized(&[&key_str, &addressed_key]).await?;
    }

    // The row the patch actually lands on. For a redirect this is the target's
    // own current row, not the link's, so an omitted field falls back to what
    // the target already holds rather than to the link's pointer value.
    let target_row = if addressed_key == key_str {
        existing.clone()
    } else {
        fetch_default_key(&addressed_key, conn, &workspace_context.schema_name).map_err(
            |e| match e {
                superposition::AppError::DbError(diesel::NotFound) => {
                    unexpected_error!(
                        "`{key_str}` points at `{addressed_key}`, which no longer exists"
                    )
                }
                _ => {
                    log::error!("Failed to fetch {addressed_key}: {e}");
                    unexpected_error!("Something went wrong.")
                }
            },
        )?
    };

    validate_change_reason(
        &workspace_context,
        &req.change_reason,
        conn,
        &state.master_encryption_key,
    )
    .await?;

    let value = req
        .value
        .clone()
        .unwrap_or_else(|| target_row.value.clone());

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
        let value = req
            .value
            .clone()
            .unwrap_or_else(|| target_row.value.clone());

        validate_default_config_with_function(
            &workspace_context,
            conn,
            validation_function_name,
            &addressed_key,
            &value,
            &state.master_encryption_key,
        )
        .await?
    }

    if let Some(ref value_compute_function_name) = req.value_compute_function_name {
        validate_fn_published(
            value_compute_function_name,
            FunctionType::ValueCompute,
            conn,
            &workspace_context.schema_name,
        )?;
    }

    let (db_row, config_version) =
        conn.transaction::<_, superposition::AppError, _>(|transaction_conn| {
            let change_reason = req.change_reason.clone();
            let val = diesel::update(dsl::default_configs)
                .filter(dsl::key.eq(addressed_key.clone()))
                .set((
                    req,
                    dsl::last_modified_at.eq(Utc::now()),
                    dsl::last_modified_by.eq(user.get_email()),
                ))
                .schema_name(&workspace_context.schema_name)
                .get_result::<DefaultConfig>(transaction_conn)?;

            let config_version = add_config_version(
                &state,
                tags.clone(),
                change_reason.into(),
                transaction_conn,
                &workspace_context.schema_name,
            )?;

            Ok((val, config_version))
        })?;

    let _ = put_config_in_redis(
        &config_version,
        &state,
        &workspace_context.schema_name,
        conn,
    )
    .await;

    let data = WebhookData {
        payload: &db_row,
        resource: Resource::DefaultConfig,
        event: WebhookEvent::ConfigChanged,
        config_version_opt: Some(config_version.id.to_string()),
        action: Action::Update,
    };

    let webhook_status =
        execute_webhook_call(data, &workspace_context, &state, conn).await;

    let mut http_resp = if webhook_status {
        HttpResponse::Ok()
    } else {
        HttpResponse::build(
            actix_web::http::StatusCode::from_u16(512)
                .unwrap_or(actix_web::http::StatusCode::INTERNAL_SERVER_ERROR),
        )
    };
    http_resp.insert_header((
        AppHeader::XConfigVersion.to_string(),
        config_version.id.to_string(),
    ));
    let response = resolve_for_response(conn, &workspace_context.schema_name, db_row)?;
    Ok(http_resp.json(response))
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

async fn validate_default_config_with_function(
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
            )
            .await?;
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

#[authorized]
#[get("")]
async fn list_handler(
    workspace_context: WorkspaceContext,
    db_conn: DbConnection,
    pagination: Query<PaginationParams>,
    filters: Query<DefaultConfigFilters>,
) -> superposition::Result<Json<PaginatedResponse<DefaultConfigResponse>>> {
    let DbConnection(mut conn) = db_conn;

    let filters = filters.into_inner();

    let query_builder = |filters: &DefaultConfigFilters| {
        let mut builder = dsl::default_configs
            .schema_name(&workspace_context.schema_name)
            .into_boxed();
        if let Some(ref config_name) = filters.name {
            builder = builder
                .filter(schema::default_configs::key.like(format!["%{}%", config_name]));
        }
        builder
    };

    if let Some(true) = pagination.all {
        let result: Vec<DefaultConfig> =
            query_builder(&filters).get_results(&mut conn)?;
        let resolved =
            resolve_many_for_response(&mut conn, &workspace_context.schema_name, result)?;
        return Ok(Json(PaginatedResponse::all(resolved)));
    }

    let base_query = query_builder(&filters);
    let count_query = query_builder(&filters);

    let n_default_configs: i64 = count_query.count().get_result(&mut conn)?;
    let limit = pagination.count.unwrap_or(10);
    let mut builder = base_query.order(dsl::created_at.desc()).limit(limit);
    if let Some(page) = pagination.page {
        let offset = (page - 1) * limit;
        builder = builder.offset(offset);
    }
    let result: Vec<DefaultConfig> = builder.load(&mut conn)?;
    let total_pages = (n_default_configs as f64 / limit as f64).ceil() as i64;
    let resolved =
        resolve_many_for_response(&mut conn, &workspace_context.schema_name, result)?;
    Ok(Json(PaginatedResponse {
        total_pages,
        total_items: n_default_configs,
        data: resolved,
    }))
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
    mut write_permit: WorkspaceWritePermit,
    user: User,
) -> superposition::Result<HttpResponse> {
    let key = path.into_inner();
    _auth_z.authorized(&[key.deref()]).await?;

    let tags = parse_config_tags(custom_headers.config_tags)?;

    let key: String = key.into();

    let conn = write_permit.connection();

    let dependents = symlink_dependents(conn, &workspace_context.schema_name, &key)?;
    if !dependents.is_empty() {
        return Err(bad_argument!(
            "cannot delete `{key}`: it is the target of symlink(s) {}. \
             Delete or repoint them first.",
            dependents.join(", ")
        ));
    }

    let context_ids =
        get_key_usage_context_ids(&key, conn, &workspace_context.schema_name)
            .map_err(|_| unexpected_error!("Something went wrong"))?;
    if context_ids.is_empty() {
        let (config_version, default_config) = conn
            .transaction::<_, superposition::AppError, _>(|transaction_conn| {
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
                        .get_result::<DefaultConfig>(transaction_conn)
                        .optional()?;
                match deleted_row {
                    None => {
                        Err(not_found!("default config key `{}` doesn't exists", key))?
                    }
                    Some(default_config) => {
                        let config_version_desc = Description::try_from(format!(
                            "Context Deleted by {}",
                            user.get_email()
                        ))
                        .map_err(|e| unexpected_error!(e))?;
                        let config_version = add_config_version(
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
                        Ok((config_version, default_config))
                    }
                }
            })?;

        let _ = put_config_in_redis(
            &config_version,
            &state,
            &workspace_context.schema_name,
            conn,
        )
        .await;

        let data = WebhookData {
            payload: &default_config,
            resource: Resource::DefaultConfig,
            event: WebhookEvent::ConfigChanged,
            config_version_opt: Some(config_version.id.to_string()),
            action: Action::Delete,
        };

        let webhook_status =
            execute_webhook_call(data, &workspace_context, &state, conn).await;

        let mut http_resp = if webhook_status {
            HttpResponse::Ok()
        } else {
            HttpResponse::build(
                actix_web::http::StatusCode::from_u16(512)
                    .unwrap_or(actix_web::http::StatusCode::INTERNAL_SERVER_ERROR),
            )
        };
        http_resp.insert_header((
            AppHeader::XConfigVersion.to_string(),
            config_version.id.to_string(),
        ));

        Ok(http_resp.finish())
    } else {
        Err(bad_argument!(
            "Given key already in use in contexts: {}",
            context_ids.join(",")
        ))
    }
}
