// @generated automatically by Diesel CLI.

diesel::table! {
    dimensions (dimension) {
        dimension -> Varchar,
        priority -> Int4,
        last_modified -> Timestamptz,
        created_on -> Timestamptz,
    }
}

diesel::table! {
    global_config (key) {
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

diesel::table! {
    contexts (key) {
        key -> Varchar,
        value -> Varchar,
        last_modified -> Timestamptz,
        created_on -> Timestamptz,
    }
}

diesel::table! {
    newcontexts (key) {
        key -> Varchar,
        value -> Json,
        column1 -> Nullable<Varchar>,
        column2 -> Nullable<Varchar>,
        column3 -> Nullable<Varchar>,
        column4 -> Nullable<Varchar>,
        last_modified -> Timestamptz,
        created_on -> Timestamptz,
    }
}

diesel::table! {
    ctxoverrides (context_id) {
        context_id -> Varchar,
        override_id -> Varchar,
        last_modified -> Timestamptz,
        created_on -> Timestamptz,
    }
}

diesel::allow_tables_to_appear_in_same_query!(
    contexts,
    ctxoverrides,
    dimensions,
    global_config,
    overrides,
    newcontexts
);
