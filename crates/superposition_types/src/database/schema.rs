// @generated automatically by Diesel CLI.

pub mod sql_types {
    #[derive(diesel::sql_types::SqlType)]
    #[diesel(postgres_type(name = "experiment_status_type"))]
    pub struct ExperimentStatusType;

    #[derive(diesel::sql_types::SqlType)]
    #[diesel(postgres_type(name = "function_types"))]
    pub struct FunctionTypes;
}

diesel::table! {
    config_versions (id) {
        id -> Int8,
        config -> Json,
        config_hash -> Text,
        tags -> Nullable<Array<Varchar>>,
        created_at -> Timestamp,
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
        last_modified_at -> Timestamp,
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
        function_name -> Nullable<Text>,
        last_modified_at -> Timestamp,
        #[max_length = 200]
        last_modified_by -> Varchar,
        description -> Text,
        change_reason -> Text,
    }
}

diesel::table! {
    dimensions (dimension) {
        dimension -> Varchar,
        created_at -> Timestamptz,
        created_by -> Varchar,
        schema -> Json,
        function_name -> Nullable<Text>,
        last_modified_at -> Timestamp,
        #[max_length = 200]
        last_modified_by -> Varchar,
        position -> Int4,
        description -> Text,
        change_reason -> Text,
        dependency_graph -> Json,
        dependents -> Array<Text>,
        dependencies -> Array<Text>,
    }
}

diesel::table! {
    event_log (id, timestamp) {
        id -> Uuid,
        table_name -> Text,
        user_name -> Text,
        timestamp -> Timestamp,
        action -> Text,
        original_data -> Nullable<Json>,
        new_data -> Nullable<Json>,
        query -> Text,
    }
}

diesel::table! {
    event_log_y2023m08 (id, timestamp) {
        id -> Uuid,
        table_name -> Text,
        user_name -> Text,
        timestamp -> Timestamp,
        action -> Text,
        original_data -> Nullable<Json>,
        new_data -> Nullable<Json>,
        query -> Text,
    }
}

diesel::table! {
    event_log_y2023m09 (id, timestamp) {
        id -> Uuid,
        table_name -> Text,
        user_name -> Text,
        timestamp -> Timestamp,
        action -> Text,
        original_data -> Nullable<Json>,
        new_data -> Nullable<Json>,
        query -> Text,
    }
}

diesel::table! {
    event_log_y2023m10 (id, timestamp) {
        id -> Uuid,
        table_name -> Text,
        user_name -> Text,
        timestamp -> Timestamp,
        action -> Text,
        original_data -> Nullable<Json>,
        new_data -> Nullable<Json>,
        query -> Text,
    }
}

diesel::table! {
    event_log_y2023m11 (id, timestamp) {
        id -> Uuid,
        table_name -> Text,
        user_name -> Text,
        timestamp -> Timestamp,
        action -> Text,
        original_data -> Nullable<Json>,
        new_data -> Nullable<Json>,
        query -> Text,
    }
}

diesel::table! {
    event_log_y2023m12 (id, timestamp) {
        id -> Uuid,
        table_name -> Text,
        user_name -> Text,
        timestamp -> Timestamp,
        action -> Text,
        original_data -> Nullable<Json>,
        new_data -> Nullable<Json>,
        query -> Text,
    }
}

diesel::table! {
    event_log_y2024m01 (id, timestamp) {
        id -> Uuid,
        table_name -> Text,
        user_name -> Text,
        timestamp -> Timestamp,
        action -> Text,
        original_data -> Nullable<Json>,
        new_data -> Nullable<Json>,
        query -> Text,
    }
}

diesel::table! {
    event_log_y2024m02 (id, timestamp) {
        id -> Uuid,
        table_name -> Text,
        user_name -> Text,
        timestamp -> Timestamp,
        action -> Text,
        original_data -> Nullable<Json>,
        new_data -> Nullable<Json>,
        query -> Text,
    }
}

diesel::table! {
    event_log_y2024m03 (id, timestamp) {
        id -> Uuid,
        table_name -> Text,
        user_name -> Text,
        timestamp -> Timestamp,
        action -> Text,
        original_data -> Nullable<Json>,
        new_data -> Nullable<Json>,
        query -> Text,
    }
}

diesel::table! {
    event_log_y2024m04 (id, timestamp) {
        id -> Uuid,
        table_name -> Text,
        user_name -> Text,
        timestamp -> Timestamp,
        action -> Text,
        original_data -> Nullable<Json>,
        new_data -> Nullable<Json>,
        query -> Text,
    }
}

diesel::table! {
    event_log_y2024m05 (id, timestamp) {
        id -> Uuid,
        table_name -> Text,
        user_name -> Text,
        timestamp -> Timestamp,
        action -> Text,
        original_data -> Nullable<Json>,
        new_data -> Nullable<Json>,
        query -> Text,
    }
}

diesel::table! {
    event_log_y2024m06 (id, timestamp) {
        id -> Uuid,
        table_name -> Text,
        user_name -> Text,
        timestamp -> Timestamp,
        action -> Text,
        original_data -> Nullable<Json>,
        new_data -> Nullable<Json>,
        query -> Text,
    }
}

diesel::table! {
    event_log_y2024m07 (id, timestamp) {
        id -> Uuid,
        table_name -> Text,
        user_name -> Text,
        timestamp -> Timestamp,
        action -> Text,
        original_data -> Nullable<Json>,
        new_data -> Nullable<Json>,
        query -> Text,
    }
}

diesel::table! {
    event_log_y2024m08 (id, timestamp) {
        id -> Uuid,
        table_name -> Text,
        user_name -> Text,
        timestamp -> Timestamp,
        action -> Text,
        original_data -> Nullable<Json>,
        new_data -> Nullable<Json>,
        query -> Text,
    }
}

diesel::table! {
    event_log_y2024m09 (id, timestamp) {
        id -> Uuid,
        table_name -> Text,
        user_name -> Text,
        timestamp -> Timestamp,
        action -> Text,
        original_data -> Nullable<Json>,
        new_data -> Nullable<Json>,
        query -> Text,
    }
}

diesel::table! {
    event_log_y2024m10 (id, timestamp) {
        id -> Uuid,
        table_name -> Text,
        user_name -> Text,
        timestamp -> Timestamp,
        action -> Text,
        original_data -> Nullable<Json>,
        new_data -> Nullable<Json>,
        query -> Text,
    }
}

diesel::table! {
    event_log_y2024m11 (id, timestamp) {
        id -> Uuid,
        table_name -> Text,
        user_name -> Text,
        timestamp -> Timestamp,
        action -> Text,
        original_data -> Nullable<Json>,
        new_data -> Nullable<Json>,
        query -> Text,
    }
}

diesel::table! {
    event_log_y2024m12 (id, timestamp) {
        id -> Uuid,
        table_name -> Text,
        user_name -> Text,
        timestamp -> Timestamp,
        action -> Text,
        original_data -> Nullable<Json>,
        new_data -> Nullable<Json>,
        query -> Text,
    }
}

diesel::table! {
    event_log_y2025m01 (id, timestamp) {
        id -> Uuid,
        table_name -> Text,
        user_name -> Text,
        timestamp -> Timestamp,
        action -> Text,
        original_data -> Nullable<Json>,
        new_data -> Nullable<Json>,
        query -> Text,
    }
}

diesel::table! {
    event_log_y2025m02 (id, timestamp) {
        id -> Uuid,
        table_name -> Text,
        user_name -> Text,
        timestamp -> Timestamp,
        action -> Text,
        original_data -> Nullable<Json>,
        new_data -> Nullable<Json>,
        query -> Text,
    }
}

diesel::table! {
    event_log_y2025m03 (id, timestamp) {
        id -> Uuid,
        table_name -> Text,
        user_name -> Text,
        timestamp -> Timestamp,
        action -> Text,
        original_data -> Nullable<Json>,
        new_data -> Nullable<Json>,
        query -> Text,
    }
}

diesel::table! {
    event_log_y2025m04 (id, timestamp) {
        id -> Uuid,
        table_name -> Text,
        user_name -> Text,
        timestamp -> Timestamp,
        action -> Text,
        original_data -> Nullable<Json>,
        new_data -> Nullable<Json>,
        query -> Text,
    }
}

diesel::table! {
    event_log_y2025m05 (id, timestamp) {
        id -> Uuid,
        table_name -> Text,
        user_name -> Text,
        timestamp -> Timestamp,
        action -> Text,
        original_data -> Nullable<Json>,
        new_data -> Nullable<Json>,
        query -> Text,
    }
}

diesel::table! {
    event_log_y2025m06 (id, timestamp) {
        id -> Uuid,
        table_name -> Text,
        user_name -> Text,
        timestamp -> Timestamp,
        action -> Text,
        original_data -> Nullable<Json>,
        new_data -> Nullable<Json>,
        query -> Text,
    }
}

diesel::table! {
    event_log_y2025m07 (id, timestamp) {
        id -> Uuid,
        table_name -> Text,
        user_name -> Text,
        timestamp -> Timestamp,
        action -> Text,
        original_data -> Nullable<Json>,
        new_data -> Nullable<Json>,
        query -> Text,
    }
}

diesel::table! {
    event_log_y2025m08 (id, timestamp) {
        id -> Uuid,
        table_name -> Text,
        user_name -> Text,
        timestamp -> Timestamp,
        action -> Text,
        original_data -> Nullable<Json>,
        new_data -> Nullable<Json>,
        query -> Text,
    }
}

diesel::table! {
    event_log_y2025m09 (id, timestamp) {
        id -> Uuid,
        table_name -> Text,
        user_name -> Text,
        timestamp -> Timestamp,
        action -> Text,
        original_data -> Nullable<Json>,
        new_data -> Nullable<Json>,
        query -> Text,
    }
}

diesel::table! {
    event_log_y2025m10 (id, timestamp) {
        id -> Uuid,
        table_name -> Text,
        user_name -> Text,
        timestamp -> Timestamp,
        action -> Text,
        original_data -> Nullable<Json>,
        new_data -> Nullable<Json>,
        query -> Text,
    }
}

diesel::table! {
    event_log_y2025m11 (id, timestamp) {
        id -> Uuid,
        table_name -> Text,
        user_name -> Text,
        timestamp -> Timestamp,
        action -> Text,
        original_data -> Nullable<Json>,
        new_data -> Nullable<Json>,
        query -> Text,
    }
}

diesel::table! {
    event_log_y2025m12 (id, timestamp) {
        id -> Uuid,
        table_name -> Text,
        user_name -> Text,
        timestamp -> Timestamp,
        action -> Text,
        original_data -> Nullable<Json>,
        new_data -> Nullable<Json>,
        query -> Text,
    }
}

diesel::table! {
    event_log_y2026m01 (id, timestamp) {
        id -> Uuid,
        table_name -> Text,
        user_name -> Text,
        timestamp -> Timestamp,
        action -> Text,
        original_data -> Nullable<Json>,
        new_data -> Nullable<Json>,
        query -> Text,
    }
}

diesel::table! {
    event_log_y2026m02 (id, timestamp) {
        id -> Uuid,
        table_name -> Text,
        user_name -> Text,
        timestamp -> Timestamp,
        action -> Text,
        original_data -> Nullable<Json>,
        new_data -> Nullable<Json>,
        query -> Text,
    }
}

diesel::table! {
    event_log_y2026m03 (id, timestamp) {
        id -> Uuid,
        table_name -> Text,
        user_name -> Text,
        timestamp -> Timestamp,
        action -> Text,
        original_data -> Nullable<Json>,
        new_data -> Nullable<Json>,
        query -> Text,
    }
}

diesel::table! {
    event_log_y2026m04 (id, timestamp) {
        id -> Uuid,
        table_name -> Text,
        user_name -> Text,
        timestamp -> Timestamp,
        action -> Text,
        original_data -> Nullable<Json>,
        new_data -> Nullable<Json>,
        query -> Text,
    }
}

diesel::table! {
    event_log_y2026m05 (id, timestamp) {
        id -> Uuid,
        table_name -> Text,
        user_name -> Text,
        timestamp -> Timestamp,
        action -> Text,
        original_data -> Nullable<Json>,
        new_data -> Nullable<Json>,
        query -> Text,
    }
}

diesel::table! {
    event_log_y2026m06 (id, timestamp) {
        id -> Uuid,
        table_name -> Text,
        user_name -> Text,
        timestamp -> Timestamp,
        action -> Text,
        original_data -> Nullable<Json>,
        new_data -> Nullable<Json>,
        query -> Text,
    }
}

diesel::table! {
    event_log_y2026m07 (id, timestamp) {
        id -> Uuid,
        table_name -> Text,
        user_name -> Text,
        timestamp -> Timestamp,
        action -> Text,
        original_data -> Nullable<Json>,
        new_data -> Nullable<Json>,
        query -> Text,
    }
}

diesel::table! {
    event_log_y2026m08 (id, timestamp) {
        id -> Uuid,
        table_name -> Text,
        user_name -> Text,
        timestamp -> Timestamp,
        action -> Text,
        original_data -> Nullable<Json>,
        new_data -> Nullable<Json>,
        query -> Text,
    }
}

diesel::table! {
    event_log_y2026m09 (id, timestamp) {
        id -> Uuid,
        table_name -> Text,
        user_name -> Text,
        timestamp -> Timestamp,
        action -> Text,
        original_data -> Nullable<Json>,
        new_data -> Nullable<Json>,
        query -> Text,
    }
}

diesel::table! {
    event_log_y2026m10 (id, timestamp) {
        id -> Uuid,
        table_name -> Text,
        user_name -> Text,
        timestamp -> Timestamp,
        action -> Text,
        original_data -> Nullable<Json>,
        new_data -> Nullable<Json>,
        query -> Text,
    }
}

diesel::table! {
    event_log_y2026m11 (id, timestamp) {
        id -> Uuid,
        table_name -> Text,
        user_name -> Text,
        timestamp -> Timestamp,
        action -> Text,
        original_data -> Nullable<Json>,
        new_data -> Nullable<Json>,
        query -> Text,
    }
}

diesel::table! {
    event_log_y2026m12 (id, timestamp) {
        id -> Uuid,
        table_name -> Text,
        user_name -> Text,
        timestamp -> Timestamp,
        action -> Text,
        original_data -> Nullable<Json>,
        new_data -> Nullable<Json>,
        query -> Text,
    }
}

diesel::table! {
    use diesel::sql_types::*;
    use super::sql_types::ExperimentStatusType;

    experiments (id) {
        id -> Int8,
        created_at -> Timestamptz,
        created_by -> Text,
        last_modified -> Timestamptz,
        name -> Text,
        override_keys -> Array<Text>,
        status -> ExperimentStatusType,
        traffic_percentage -> Int4,
        context -> Json,
        variants -> Json,
        last_modified_by -> Text,
        chosen_variant -> Nullable<Text>,
        description -> Text,
        change_reason -> Text,
    }
}

diesel::table! {
    use diesel::sql_types::*;
    use super::sql_types::FunctionTypes;

    functions (function_name) {
        function_name -> Text,
        published_code -> Nullable<Text>,
        draft_code -> Text,
        description -> Text,
        #[max_length = 16]
        published_runtime_version -> Nullable<Varchar>,
        #[max_length = 16]
        draft_runtime_version -> Varchar,
        published_at -> Nullable<Timestamp>,
        draft_edited_at -> Timestamp,
        published_by -> Nullable<Text>,
        draft_edited_by -> Text,
        last_modified_at -> Timestamp,
        #[max_length = 200]
        last_modified_by -> Varchar,
        change_reason -> Text,
        function_type -> FunctionTypes,
    }
}

diesel::table! {
    type_templates (type_name) {
        type_name -> Text,
        type_schema -> Json,
        created_by -> Text,
        created_at -> Timestamp,
        last_modified_at -> Timestamp,
        #[max_length = 200]
        last_modified_by -> Varchar,
        description -> Text,
        change_reason -> Text,
    }
}

diesel::joinable!(default_configs -> functions (function_name));
diesel::joinable!(dimensions -> functions (function_name));

diesel::allow_tables_to_appear_in_same_query!(
    config_versions,
    contexts,
    default_configs,
    dimensions,
    event_log,
    event_log_y2023m08,
    event_log_y2023m09,
    event_log_y2023m10,
    event_log_y2023m11,
    event_log_y2023m12,
    event_log_y2024m01,
    event_log_y2024m02,
    event_log_y2024m03,
    event_log_y2024m04,
    event_log_y2024m05,
    event_log_y2024m06,
    event_log_y2024m07,
    event_log_y2024m08,
    event_log_y2024m09,
    event_log_y2024m10,
    event_log_y2024m11,
    event_log_y2024m12,
    event_log_y2025m01,
    event_log_y2025m02,
    event_log_y2025m03,
    event_log_y2025m04,
    event_log_y2025m05,
    event_log_y2025m06,
    event_log_y2025m07,
    event_log_y2025m08,
    event_log_y2025m09,
    event_log_y2025m10,
    event_log_y2025m11,
    event_log_y2025m12,
    event_log_y2026m01,
    event_log_y2026m02,
    event_log_y2026m03,
    event_log_y2026m04,
    event_log_y2026m05,
    event_log_y2026m06,
    event_log_y2026m07,
    event_log_y2026m08,
    event_log_y2026m09,
    event_log_y2026m10,
    event_log_y2026m11,
    event_log_y2026m12,
    experiments,
    functions,
    type_templates,
);
