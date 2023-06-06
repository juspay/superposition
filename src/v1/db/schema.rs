// @generated automatically by Diesel CLI.

diesel::table! {
    contexts (id) {
        id -> Varchar,
        value -> Json,
        override_id -> Varchar,
        created_at -> Timestamptz,
        created_by -> Varchar,
    }
}

diesel::table! {
    overrides (id) {
        id -> Varchar,
        value -> Json,
        created_at -> Timestamptz,
        created_by -> Varchar,
    }
}

diesel::allow_tables_to_appear_in_same_query!(
    contexts,
    overrides,
);
