// @generated automatically by Diesel CLI.

pub mod cac_v1 {
    pub mod sql_types {
        #[derive(diesel::sql_types::SqlType)]
        #[diesel(postgres_type(name = "dimension_type", schema = "cac_v1"))]
        pub struct DimensionType;

        #[derive(diesel::sql_types::SqlType)]
        #[diesel(postgres_type(name = "experiment_status_type", schema = "cac_v1"))]
        pub struct ExperimentStatusType;
    }

    diesel::table! {
        cac_v1.contexts (id) {
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
        cac_v1.default_configs (key) {
            key -> Varchar,
            value -> Json,
            created_at -> Timestamptz,
            created_by -> Varchar,
            schema -> Json,
        }
    }

    diesel::table! {
        use diesel::sql_types::*;
        use super::sql_types::DimensionType;

        cac_v1.dimensions (dimension) {
            dimension -> Varchar,
            priority -> Int4,
            #[sql_name = "type"]
            type_ -> DimensionType,
            created_at -> Timestamptz,
            created_by -> Varchar,
        }
    }

    diesel::table! {
        cac_v1.event_log (id, timestamp) {
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
        cac_v1.event_log_y2023m08 (id, timestamp) {
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
        cac_v1.event_log_y2023m09 (id, timestamp) {
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
        cac_v1.event_log_y2023m10 (id, timestamp) {
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
        cac_v1.event_log_y2023m11 (id, timestamp) {
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
        cac_v1.event_log_y2023m12 (id, timestamp) {
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
        cac_v1.event_log_y2024m01 (id, timestamp) {
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
        cac_v1.event_log_y2024m02 (id, timestamp) {
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
        cac_v1.event_log_y2024m03 (id, timestamp) {
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
        cac_v1.event_log_y2024m04 (id, timestamp) {
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
        cac_v1.event_log_y2024m05 (id, timestamp) {
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
        cac_v1.event_log_y2024m06 (id, timestamp) {
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
        cac_v1.event_log_y2024m07 (id, timestamp) {
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

        cac_v1.experiments (id) {
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
        experiments,
    );
}
