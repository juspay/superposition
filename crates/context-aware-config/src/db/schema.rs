// @generated automatically by Diesel CLI.

diesel::table! {
    contexts (id) {
        id -> Varchar,
        value -> Json,
        override_id -> Varchar,
        created_at -> Timestamptz,
        created_by -> Varchar,
        priority -> Int4,
        #[sql_name = "override"]
        override_ -> Json,
    }
}

diesel::table! {
    default_configs (key) {
        key -> Varchar,
        value -> Json,
        created_at -> Timestamptz,
        created_by -> Varchar,
        schema -> Json,
    }
}

diesel::table! {
    dimensions (dimension) {
        dimension -> Varchar,
        priority -> Int4,
        created_at -> Timestamptz,
        created_by -> Varchar,
        schema -> Json,
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

diesel::allow_tables_to_appear_in_same_query!(
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
);
