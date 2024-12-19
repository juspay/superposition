// @generated automatically by Diesel CLI.

pub mod superposition {
    pub mod sql_types {
        #[derive(diesel::sql_types::SqlType)]
        #[diesel(postgres_type(name = "workspace_status", schema = "superposition"))]
        pub struct WorkspaceStatus;
    }

    diesel::table! {
        use diesel::sql_types::*;
        use super::sql_types::WorkspaceStatus;

        superposition.workspaces (organization_id, workspace_name) {
            organization_id -> Text,
            organization_name -> Text,
            workspace_name -> Text,
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
}
