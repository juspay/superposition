// @generated automatically by Diesel CLI.

pub mod sql_types {
    #[derive(diesel::sql_types::SqlType)]
    #[diesel(postgres_type(name = "experiment_status_type"))]
    pub struct ExperimentStatusType;
}

diesel::table! {
    use diesel::sql_types::*;
    use super::sql_types::ExperimentStatusType;

    experiments (id) {
        id -> Int8,
        created_at -> Timestamptz,
        created_by -> Text,
        last_modified -> Nullable<Timestamptz>,
        name -> Text,
        override_keys -> Array<Text>,
        status -> ExperimentStatusType,
        traffic_percentage -> Int4,
        context -> Json,
        variants -> Json,
    }
}
