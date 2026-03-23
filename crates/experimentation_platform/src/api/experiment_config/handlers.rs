use std::collections::HashSet;

use actix_web::{
    HttpRequest, HttpResponse, Scope, routes,
    web::{Data, Json},
};
use chrono::{DateTime, Utc};
use diesel::{ExpressionMethods, QueryDsl, RunQueryDsl};
use service_utils::{
    db::run_query,
    helpers::is_not_modified,
    redis::{
        EXPERIMENT_CONFIG_LAST_MODIFIED_KEY_SUFFIX, EXPERIMENT_GROUPS_LIST_KEY_SUFFIX,
        EXPERIMENTS_LIST_KEY_SUFFIX, read_through_cache,
    },
    service::types::{AppHeader, AppState, WorkspaceContext},
};
use superposition_derives::{authorized, declare_resource};
use superposition_types::{
    DBConnection, IsEmpty, PaginatedResponse,
    api::{
        experiment_config::{
            ExperimentConfig, ExperimentConfigFilters, ExperimentConfigRequest,
        },
        experiments::ExperimentResponse,
    },
    custom_query::{self as superposition_query, CustomQuery, DimensionQuery, QueryMap},
    database::{
        models::experimentation::{Experiment, ExperimentGroup, ExperimentStatusType},
        schema::{
            event_log::dsl as event_log, experiment_groups::dsl as experiment_groups,
            experiments::dsl as experiments,
        },
    },
    experimental::{Experimental, ExperimentalVariants},
    result as superposition,
};

declare_resource!(ExperimentConfig);

pub fn endpoints(scope: Scope) -> Scope {
    scope.service(get_handler)
}

#[allow(clippy::too_many_arguments)]
#[authorized]
#[routes]
#[get("")]
#[post("")]
async fn get_handler(
    workspace_context: WorkspaceContext,
    req: HttpRequest,
    body: Option<Json<ExperimentConfigRequest>>,
    filters: superposition_query::Query<ExperimentConfigFilters>,
    dimension_params: DimensionQuery<QueryMap>,
    state: Data<AppState>,
) -> superposition::Result<HttpResponse> {
    let max_event_timestamp = read_through_cache::<Option<DateTime<Utc>>>(
        format!(
            "{}{EXPERIMENT_CONFIG_LAST_MODIFIED_KEY_SUFFIX}",
            *workspace_context.schema_name
        ),
        &workspace_context.schema_name,
        &state.redis,
        &state.db_pool,
        |conn| {
            event_log::event_log
                .filter(
                    event_log::table_name.eq_any(["experiments", "experiment_groups"]),
                )
                .select(diesel::dsl::max(event_log::timestamp))
                .schema_name(&workspace_context.schema_name)
                .first(conn)
        },
    )
    .await?;

    if is_not_modified(max_event_timestamp, &req) {
        return Ok(HttpResponse::NotModified().finish());
    };

    let is_smithy = matches!(req.method(), &actix_web::http::Method::POST);
    let dimension_params = if req.method() == actix_web::http::Method::GET {
        dimension_params.into_inner()
    } else {
        body.and_then(|b| b.into_inner().context)
            .map(Into::into)
            .unwrap_or_default()
    };

    let read_from_redis = dimension_params.is_empty() && filters.is_empty();

    let mut response = HttpResponse::Ok();
    AppHeader::add_last_modified(max_event_timestamp, is_smithy, &mut response);

    if read_from_redis {
        let exp_list = read_through_cache::<PaginatedResponse<ExperimentResponse>>(
            format!(
                "{}{EXPERIMENTS_LIST_KEY_SUFFIX}",
                *workspace_context.schema_name
            ),
            &workspace_context.schema_name,
            &state.redis,
            &state.db_pool,
            |conn| {
                let experiment_list: Vec<Experiment> = experiments::experiments
                    .filter(
                        experiments::status.eq_any(ExperimentStatusType::active_list()),
                    )
                    .order(experiments::last_modified.desc())
                    .schema_name(&workspace_context.schema_name)
                    .load::<Experiment>(conn)?;

                let experiment_responses: Vec<ExperimentResponse> = experiment_list
                    .into_iter()
                    .map(ExperimentResponse::from)
                    .collect();

                Ok(PaginatedResponse::all(experiment_responses))
            },
        )
        .await?;
        let exp_group_list = read_through_cache::<Vec<ExperimentGroup>>(
            format!(
                "{}{EXPERIMENT_GROUPS_LIST_KEY_SUFFIX}",
                *workspace_context.schema_name
            ),
            &workspace_context.schema_name,
            &state.redis,
            &state.db_pool,
            |conn| {
                experiment_groups::experiment_groups
                    .order(experiment_groups::last_modified_at.desc())
                    .schema_name(&workspace_context.schema_name)
                    .load::<ExperimentGroup>(conn)
            },
        )
        .await?;

        Ok(ExperimentConfig {
            experiments: exp_list.data,
            experiment_groups: exp_group_list,
        })
    } else {
        run_query(&state.db_pool, |conn| {
            get_experiment_config_db(filters, dimension_params, conn, &workspace_context)
        })
    }
    .map(|r| response.json(r))
}

fn get_experiment_config_db(
    filters: superposition_query::Query<ExperimentConfigFilters>,
    dimension_params: QueryMap,
    conn: &mut DBConnection,
    workspace_context: &WorkspaceContext,
) -> superposition::DieselResult<ExperimentConfig> {
    let filters = filters.into_inner();

    let exp_list = {
        let mut experiment_list: Vec<Experiment> = experiments::experiments
            .filter(experiments::status.eq_any(ExperimentStatusType::active_list()))
            .order(experiments::last_modified.desc())
            .schema_name(&workspace_context.schema_name)
            .load::<Experiment>(conn)?;

        if let Some(prefix_list) = filters.prefix {
            if !prefix_list.is_empty() {
                experiment_list = Experiment::filter_keys_by_prefix(
                    experiment_list,
                    &HashSet::from_iter(prefix_list.0),
                );
            }
        }

        if !dimension_params.is_empty() {
            experiment_list =
                Experiment::filter_by_eval(experiment_list, &dimension_params)
        }

        experiment_list
            .into_iter()
            .map(ExperimentResponse::from)
            .collect()
    };

    let exp_group_list = {
        let mut group_list = experiment_groups::experiment_groups
            .order(experiment_groups::last_modified_at.desc())
            .schema_name(&workspace_context.schema_name)
            .load::<ExperimentGroup>(conn)?;

        if !dimension_params.is_empty() {
            group_list = ExperimentGroup::filter_by_eval(group_list, &dimension_params);
        }

        group_list
    };

    Ok(ExperimentConfig {
        experiments: exp_list,
        experiment_groups: exp_group_list,
    })
}
