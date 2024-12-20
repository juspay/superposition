// @generated automatically by Diesel CLI.

pub mod superposition {
    pub mod sql_types {
        #[derive(diesel::sql_types::SqlType)]
        #[diesel(postgres_type(name = "org_status", schema = "superposition"))]
        pub struct OrgStatus;

        #[derive(diesel::sql_types::SqlType)]
        #[diesel(postgres_type(name = "workspace_status", schema = "superposition"))]
        pub struct WorkspaceStatus;
    }

    diesel::table! {
        use diesel::sql_types::*;
        use super::sql_types::OrgStatus;

        superposition.organisations (id) {
            #[max_length = 30]
            id -> Varchar,
            name -> Text,
            #[max_length = 10]
            country_code -> Nullable<Varchar>,
            #[max_length = 255]
            contact_email -> Nullable<Varchar>,
            #[max_length = 15]
            contact_phone -> Nullable<Varchar>,
            created_by -> Text,
            admin_email -> Text,
            status -> OrgStatus,
            #[max_length = 100]
            sector -> Nullable<Varchar>,
            created_at -> Timestamp,
            updated_at -> Timestamp,
            updated_by -> Text,
        }
    }

    diesel::table! {
        use diesel::sql_types::*;
        use super::sql_types::WorkspaceStatus;

        superposition.workspaces (organisation_id, workspace_name) {
            #[max_length = 30]
            organisation_id -> Varchar,
            organisation_name -> Text,
            #[max_length = 25]
            workspace_name -> Varchar,
            workspace_schema_name -> Text,
            workspace_status -> WorkspaceStatus,
            workspace_admin_email -> Text,
            created_by -> Text,
            last_modified_by -> Text,
            last_modified_at -> Timestamp,
            created_at -> Timestamp,
            mandatory_dimensions -> Nullable<Array<Text>>,
        }
    }

    diesel::joinable!(workspaces -> organisations (organisation_id));

    diesel::allow_tables_to_appear_in_same_query!(organisations, workspaces,);
}
