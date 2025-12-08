use actix_web::web::Json;
use chrono::Utc;
use diesel::{
    Connection, ExpressionMethods, PgConnection, QueryDsl, RunQueryDsl, SelectableHelper,
    r2d2::{ConnectionManager, PooledConnection},
    result::{DatabaseErrorKind::*, Error::DatabaseError},
};
use serde_json::{Map, Value};
use service_utils::service::types::{SchemaName, WorkspaceContext};
use superposition_macros::{db_error, not_found, unexpected_error};
use superposition_types::{
    DBConnection, Overrides, User,
    api::context::{Identifier, MoveRequest, PutRequest, UpdateRequest},
    database::{
        models::{Description, cac::Context},
        schema::contexts::{self, dsl},
    },
    result,
};

use crate::{
    api::context::helpers::{
        create_ctx_from_put_req, hash, replace_override_of_existing_ctx,
        update_override_of_existing_ctx, validate_ctx,
    },
    helpers::calculate_context_weight,
};

use super::{
    helpers::validate_override_with_functions, types::UpdateContextOverridesChangeset,
    validations::validate_override_with_default_configs,
};

#[allow(clippy::too_many_arguments)]
pub fn upsert(
    req: PutRequest,
    description: Description,
    conn: &mut PooledConnection<ConnectionManager<PgConnection>>,
    already_under_txn: bool,
    user: &User,
    workspace_context: &WorkspaceContext,
    replace: bool,
    master_key: Option<&secrecy::SecretString>,
) -> result::Result<Context> {
    use contexts::dsl::contexts;
    let new_ctx = create_ctx_from_put_req(
        req,
        description,
        conn,
        user,
        workspace_context,
        master_key,
    )?;

    if already_under_txn {
        diesel::sql_query("SAVEPOINT put_ctx_savepoint").execute(conn)?;
    }
    let insert = diesel::insert_into(contexts)
        .values(&new_ctx)
        .returning(Context::as_returning())
        .schema_name(&workspace_context.schema_name)
        .execute(conn);

    match insert {
        Ok(_) => Ok(new_ctx),
        Err(DatabaseError(UniqueViolation, _)) => {
            if already_under_txn {
                diesel::sql_query("ROLLBACK TO put_ctx_savepoint").execute(conn)?;
            }
            if replace {
                replace_override_of_existing_ctx(
                    conn,
                    new_ctx,
                    user,
                    &workspace_context.schema_name,
                )
            } else {
                update_override_of_existing_ctx(
                    conn,
                    new_ctx,
                    user,
                    &workspace_context.schema_name,
                )
            }
        }
        Err(e) => {
            log::error!("failed to update context with db error: {:?}", e);
            Err(db_error!(e))
        }
    }
}

pub fn update(
    workspace_context: &WorkspaceContext,
    req: UpdateRequest,
    conn: &mut PooledConnection<ConnectionManager<PgConnection>>,
    user: &User,
    master_key: Option<&secrecy::SecretString>,
) -> result::Result<Context> {
    let (context_id, context) = match req.context {
        Identifier::Context(context) => {
            let ctx_value: Map<String, Value> = context.into_inner().into();
            (hash(&Value::Object(ctx_value.clone())), ctx_value)
        }
        Identifier::Id(id) => {
            let ctx_value: Context = dsl::contexts
                .filter(dsl::id.eq(id.clone()))
                .schema_name(&workspace_context.schema_name)
                .get_result::<Context>(conn)?;
            (id.clone(), ctx_value.value.into())
        }
    };

    let r_override = req.override_.clone().into_inner();
    let ctx_override = Value::Object(r_override.clone().into());

    validate_override_with_default_configs(
        conn,
        &r_override,
        &workspace_context.schema_name,
    )?;
    validate_override_with_functions(
        workspace_context,
        conn,
        &r_override,
        &context,
        master_key,
    )?;

    let update_request = UpdateContextOverridesChangeset {
        override_id: hash(&ctx_override),
        override_: r_override,
        last_modified_at: Utc::now(),
        last_modified_by: user.get_email(),
        description: req.description.clone(),
        change_reason: req.change_reason.clone(),
    };

    diesel::update(dsl::contexts)
        .filter(dsl::id.eq(context_id))
        .set(update_request)
        .schema_name(&workspace_context.schema_name)
        .returning(Context::as_returning())
        .get_result(conn)
        .map_err(|e| db_error!(e))
}

#[allow(clippy::too_many_arguments)]
pub fn r#move(
    workspace_context: &WorkspaceContext,
    old_ctx_id: String,
    req: Json<MoveRequest>,
    req_description: Description,
    conn: &mut PooledConnection<ConnectionManager<PgConnection>>,
    already_under_txn: bool,
    user: &User,
    master_key: Option<&secrecy::SecretString>,
) -> result::Result<Context> {
    use contexts::dsl;
    let req = req.into_inner();
    let ctx_condition = req.context.to_owned().into_inner();
    let ctx_condition_value = Value::Object(ctx_condition.clone().into());
    let change_reason = req.change_reason.clone();

    let new_ctx_id = hash(&ctx_condition_value);

    let dimension_data_map = validate_ctx(
        conn,
        workspace_context,
        ctx_condition.clone(),
        Overrides::default(),
        master_key,
    )?;
    let weight = calculate_context_weight(&ctx_condition_value, &dimension_data_map)
        .map_err(|_| unexpected_error!("Something Went Wrong"))?;

    if already_under_txn {
        diesel::sql_query("SAVEPOINT update_ctx_savepoint").execute(conn)?;
    }

    let context = diesel::update(dsl::contexts)
        .filter(dsl::id.eq(&old_ctx_id))
        .set((
            dsl::id.eq(&new_ctx_id),
            dsl::value.eq(&ctx_condition_value),
            dsl::weight.eq(&weight),
            dsl::last_modified_at.eq(Utc::now()),
            dsl::last_modified_by.eq(user.get_email()),
            dsl::description.eq(req_description.clone()),
            dsl::change_reason.eq(change_reason.clone()),
        ))
        .returning(Context::as_returning())
        .schema_name(&workspace_context.schema_name)
        .get_result::<Context>(conn);

    let contruct_new_ctx_with_old_overrides = |ctx: Context| Context {
        id: new_ctx_id,
        value: ctx_condition,
        created_at: Utc::now(),
        created_by: user.get_email(),
        override_id: ctx.override_id,
        override_: ctx.override_,
        last_modified_at: Utc::now(),
        last_modified_by: user.get_email(),
        weight,
        description: req_description,
        change_reason,
    };

    let handle_unique_violation =
        |db_conn: &mut DBConnection, already_under_txn: bool| {
            if already_under_txn {
                let deleted_ctxt = diesel::delete(dsl::contexts)
                    .filter(dsl::id.eq(&old_ctx_id))
                    .schema_name(&workspace_context.schema_name)
                    .get_result(db_conn)?;

                let ctx = contruct_new_ctx_with_old_overrides(deleted_ctxt);
                update_override_of_existing_ctx(
                    db_conn,
                    ctx,
                    user,
                    &workspace_context.schema_name,
                )
            } else {
                db_conn.transaction(|conn| {
                    let deleted_ctxt = diesel::delete(dsl::contexts)
                        .filter(dsl::id.eq(&old_ctx_id))
                        .schema_name(&workspace_context.schema_name)
                        .get_result(conn)?;
                    let ctx = contruct_new_ctx_with_old_overrides(deleted_ctxt);
                    update_override_of_existing_ctx(
                        conn,
                        ctx,
                        user,
                        &workspace_context.schema_name,
                    )
                })
            }
        };

    match context {
        Ok(ctx) => Ok(ctx),
        Err(DatabaseError(UniqueViolation, _)) => {
            if already_under_txn {
                diesel::sql_query("ROLLBACK TO update_ctx_savepoint").execute(conn)?;
            }
            handle_unique_violation(conn, already_under_txn)
        }
        Err(e) => {
            log::error!("failed to move context with db error: {:?}", e);
            Err(db_error!(e))
        }
    }
}

pub fn delete(
    ctx_id: String,
    user: &User,
    conn: &mut DBConnection,
    schema_name: &SchemaName,
) -> result::Result<()> {
    use contexts::dsl;
    diesel::update(dsl::contexts)
        .filter(dsl::id.eq(&ctx_id))
        .set((
            dsl::last_modified_at.eq(Utc::now()),
            dsl::last_modified_by.eq(user.get_email()),
        ))
        .returning(Context::as_returning())
        .schema_name(schema_name)
        .execute(conn)?;
    let deleted_row = diesel::delete(dsl::contexts.filter(dsl::id.eq(&ctx_id)))
        .schema_name(schema_name)
        .execute(conn);
    match deleted_row {
        Ok(0) => Err(not_found!("Context Id `{}` doesn't exists", ctx_id)),
        Ok(_) => {
            log::info!("{ctx_id} context deleted by {}", user.get_email());
            Ok(())
        }
        Err(e) => {
            log::error!("context delete query failed with error: {e}");
            Err(unexpected_error!("Something went wrong."))
        }
    }
}
