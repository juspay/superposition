// @generated automatically by Diesel CLI.

pub mod sql_types {
    #[derive(diesel::sql_types::SqlType)]
    #[diesel(postgres_type(name = "experiment_status_type"))]
    pub struct ExperimentStatusType;

    #[derive(diesel::sql_types::SqlType)]
    #[diesel(postgres_type(name = "experiment_type"))]
    pub struct ExperimentType;

    #[derive(diesel::sql_types::SqlType)]
    #[diesel(postgres_type(name = "function_types_new"))]
    pub struct FunctionTypesNew;

    #[derive(diesel::sql_types::SqlType)]
    #[diesel(postgres_type(name = "http_method"))]
    pub struct HttpMethod;

    #[derive(diesel::sql_types::SqlType)]
    #[diesel(postgres_type(name = "group_type"))]
    pub struct GroupType;
}

diesel::table! {
    config_versions (id) {
        id -> Int8,
        config -> Json,
        config_hash -> Text,
        tags -> Nullable<Array<Varchar>>,
        created_at -> Timestamptz,
        description -> Text,
    }
}

diesel::table! {
    contexts (id) {
        id -> Varchar,
        value -> Json,
        override_id -> Varchar,
        created_at -> Timestamptz,
        created_by -> Varchar,
        #[sql_name = "override"]
        override_ -> Json,
        last_modified_at -> Timestamptz,
        #[max_length = 200]
        last_modified_by -> Varchar,
        weight -> Numeric,
        description -> Text,
        change_reason -> Text,
    }
}

diesel::table! {
    default_configs (key) {
        key -> Varchar,
        value -> Json,
        created_at -> Timestamptz,
        created_by -> Varchar,
        schema -> Json,
        value_validation_function_name -> Nullable<Text>,
        last_modified_at -> Timestamptz,
        #[max_length = 200]
        last_modified_by -> Varchar,
        description -> Text,
        change_reason -> Text,
        value_compute_function_name -> Nullable<Text>,
    }
}

diesel::table! {
    dimensions (dimension) {
        dimension -> Varchar,
        created_at -> Timestamptz,
        created_by -> Varchar,
        schema -> Json,
        value_validation_function_name -> Nullable<Text>,
        last_modified_at -> Timestamptz,
        #[max_length = 200]
        last_modified_by -> Varchar,
        position -> Int4,
        description -> Text,
        change_reason -> Text,
        dependency_graph -> Json,
        value_compute_function_name -> Nullable<Text>,
        dimension_type -> Text,
    }
}

diesel::table! {
    event_log (id, timestamp) {
        id -> Uuid,
        table_name -> Text,
        user_name -> Text,
        timestamp -> Timestamptz,
        action -> Text,
        original_data -> Nullable<Json>,
        new_data -> Nullable<Json>,
        query -> Text,
    }
}

diesel::table! {
    use diesel::sql_types::*;
    use super::sql_types::{ExperimentStatusType, ExperimentType};

    experiments (id) {
        id -> Int8,
        created_at -> Timestamptz,
        created_by -> Text,
        last_modified -> Timestamptz,
        name -> Text,
        experiment_type -> ExperimentType,
        override_keys -> Array<Text>,
        status -> ExperimentStatusType,
        traffic_percentage -> Int4,
        started_at -> Nullable<Timestamptz>,
        started_by -> Nullable<Text>,
        context -> Json,
        variants -> Json,
        last_modified_by -> Text,
        chosen_variant -> Nullable<Text>,
        description -> Text,
        change_reason -> Text,
        metrics -> Json,
        experiment_group_id -> Nullable<Int8>,
        idempotency_key -> Nullable<Text>,
    }
}

diesel::table! {
    use diesel::sql_types::*;
    use super::sql_types::FunctionTypesNew;

    functions (function_name) {
        function_name -> Text,
        published_code -> Nullable<Text>,
        draft_code -> Text,
        description -> Text,
        #[max_length = 16]
        published_runtime_version -> Nullable<Varchar>,
        #[max_length = 16]
        draft_runtime_version -> Varchar,
        published_at -> Nullable<Timestamptz>,
        draft_edited_at -> Timestamptz,
        published_by -> Nullable<Text>,
        draft_edited_by -> Text,
        last_modified_at -> Timestamptz,
        #[max_length = 200]
        last_modified_by -> Varchar,
        change_reason -> Text,
        function_type -> FunctionTypesNew,
        created_by -> Text,
        created_at -> Timestamptz,
    }
}

diesel::table! {
    type_templates (type_name) {
        type_name -> Text,
        type_schema -> Json,
        created_by -> Text,
        created_at -> Timestamptz,
        last_modified_at -> Timestamptz,
        #[max_length = 200]
        last_modified_by -> Varchar,
        description -> Text,
        change_reason -> Text,
    }
}

diesel::table! {
    use diesel::sql_types::*;
    use super::sql_types::HttpMethod;

    webhooks (name) {
        name -> Text,
        description -> Text,
        enabled -> Bool,
        url -> Text,
        method -> HttpMethod,
        payload_version -> Text,
        custom_headers -> Json,
        events -> Array<Text>,
        max_retries -> Int4,
        last_triggered_at -> Nullable<Timestamptz>,
        change_reason -> Text,
        created_by -> Text,
        created_at -> Timestamptz,
        last_modified_by -> Text,
        last_modified_at -> Timestamptz,
    }
}

diesel::table! {
    use diesel::sql_types::*;
    use super::sql_types::GroupType;

    experiment_groups (id) {
        id -> Int8,
        context_hash -> Text,
        name -> Text,
        description -> Text,
        change_reason -> Text,
        context -> Json,
        traffic_percentage -> Int4,
        member_experiment_ids -> Array<Int8>,
        created_at -> Timestamptz,
        created_by -> Text,
        last_modified_at -> Timestamptz,
        last_modified_by -> Text,
        buckets -> Array<Nullable<Json>>,
        group_type -> GroupType
    }
}

diesel::table! {
    use diesel::sql_types::*;
    use super::sql_types::HttpMethod;

    variables (name) {
        name -> Varchar,
        value -> Text,
        description -> Text,
        change_reason -> Text,
        created_at -> Timestamptz,
        last_modified_at -> Timestamptz,
        created_by -> Varchar,
        last_modified_by -> Varchar,
    }
}

diesel::table! {
    secrets (name) {
        name -> Varchar,
        encrypted_value -> Text,
        description -> Text,
        change_reason -> Text,
        created_at -> Timestamptz,
        last_modified_at -> Timestamptz,
        created_by -> Varchar,
        last_modified_by -> Varchar,
    }
}

diesel::table! {
    use diesel::sql_types::*;
    use super::super::superposition_schema::superposition::sql_types::{BackgroundJobType, BackgroundJobStatus};
    job_manager (id) {
        id -> Int8,
        kronos_job_id -> Text,
        description -> Text,
        job_type -> BackgroundJobType,
        status -> BackgroundJobStatus,
        name -> Text,
        progress -> Int4,
        created_at -> Timestamptz,
        logs -> Text,
    }
}

diesel::joinable!(default_configs -> functions (value_validation_function_name));
diesel::joinable!(dimensions -> functions (value_validation_function_name));

diesel::allow_tables_to_appear_in_same_query!(
    config_versions,
    contexts,
    default_configs,
    dimensions,
    event_log,
    experiments,
    experiment_groups,
    functions,
    type_templates,
    webhooks,
    variables,
    secrets,
    job_manager,
);
