// @generated automatically by Diesel CLI.
diesel::table! {
    dimensions (dimension) {
        uuid -> Uuid,
        dimension -> Varchar,
        priority -> Int4,
        last_modified -> Timestamptz,
        created_on -> Timestamptz,
    }
}

diesel::table! {
    global_config (key) {
        uuid -> Uuid,
        key -> Varchar,
        value -> Json,
        last_modified -> Timestamptz,
        created_on -> Timestamptz,
    }
}


diesel::table! {
    overrides (key) {
        key -> Varchar,
        value -> Json,
        last_modified -> Timestamptz,
        created_on -> Timestamptz,
    }
}

diesel::allow_tables_to_appear_in_same_query!(
    dimensions,
    global_config,
    overrides,
);
