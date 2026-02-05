$version: "2.0"

namespace io.superposition

resource Workspace {
    identifiers: {
        workspace_name: String
        org_id: String
    }
    properties: {
        organisation_id: String
        organisation_name: String
        workspace_schema_name: String
        workspace_status: WorkspaceStatus
        workspace_admin_email: String
        config_version: String
        created_by: String
        last_modified_by: String
        last_modified_at: DateTime
        created_at: DateTime
        mandatory_dimensions: ListMandatoryDimensions
        metrics: Document
        allow_experiment_self_approval: Boolean
        auto_populate_control: Boolean
        enable_context_validation: Boolean
        enable_change_reason_validation: Boolean
        change_reason: String
    }
    list: ListWorkspace
    update: UpdateWorkspace
    read: GetWorkspace
    operations: [
        CreateWorkspace
        MigrateWorkspaceSchema
        RotateWorkspaceEncryptionKey
    ]
}

enum WorkspaceStatus {
    ENABLED
    DISABLED
}

list ListMandatoryDimensions {
    member: String
}

structure CreateWorkspaceRequest for Workspace with [OrganisationMixin] {
    @required
    $workspace_admin_email

    @required
    $workspace_name

    $workspace_status

    $metrics

    $allow_experiment_self_approval

    $auto_populate_control

    $enable_context_validation

    $enable_change_reason_validation

    @required
    $change_reason
}

structure UpdateWorkspaceRequest for Workspace with [OrganisationMixin] {
    @httpLabel
    @required
    $workspace_name

    @required
    $workspace_admin_email

    @documentation("To unset config version, pass \"null\" string.")
    $config_version

    $mandatory_dimensions

    $workspace_status

    $metrics

    $allow_experiment_self_approval

    $auto_populate_control

    $enable_context_validation

    $enable_change_reason_validation

    $change_reason
}

structure WorkspaceSelectorRequest for Workspace with [OrganisationMixin] {
    @httpLabel
    @required
    $workspace_name
}

structure WorkspaceResponse for Workspace {
    @required
    $workspace_name

    @required
    $organisation_id

    @required
    $organisation_name

    @required
    $workspace_schema_name

    @required
    $workspace_status

    @required
    $workspace_admin_email

    $config_version

    @required
    $created_by

    @required
    $last_modified_by

    @required
    $last_modified_at

    @required
    $created_at

    $mandatory_dimensions

    @required
    $metrics

    @required
    $allow_experiment_self_approval

    @required
    $auto_populate_control

    @required
    $enable_context_validation

    @required
    $enable_change_reason_validation

    @required
    $change_reason
}

list WorkspaceList {
    member: WorkspaceResponse
}

@documentation("Retrieves detailed information about a specific workspace including its configuration and metadata.")
@readonly
@http(method: "GET", uri: "/workspaces/{workspace_name}")
@tags(["Workspace Management"])
operation GetWorkspace with [GetOperation] {
    input := for Workspace with [OrganisationMixin] {
        @httpLabel
        @required
        $workspace_name
    }

    output: WorkspaceResponse
}

// Operations
@documentation("Creates a new workspace within an organisation, including database schema setup and isolated environment for config management with specified admin and settings.")
@http(method: "POST", uri: "/workspaces")
@tags(["Workspace Management"])
operation CreateWorkspace {
    input: CreateWorkspaceRequest
    output: WorkspaceResponse
}

@documentation("Updates an existing workspace configuration, allowing modification of admin settings, mandatory dimensions, and workspace properties. Validates config version existence if provided.")
@idempotent
@http(method: "PATCH", uri: "/workspaces/{workspace_name}")
@tags(["Workspace Management"])
operation UpdateWorkspace with [GetOperation] {
    input: UpdateWorkspaceRequest
    output: WorkspaceResponse
}

@documentation("Retrieves a paginated list of all workspaces with optional filtering by workspace name, including their status, config details, and administrative information.")
@readonly
@http(method: "GET", uri: "/workspaces")
@tags(["Workspace Management"])
operation ListWorkspace {
    input := with [PaginationParams, OrganisationMixin] {}

    output := with [PaginatedResponse] {
        @required
        data: WorkspaceList
    }
}

@documentation("Migrates the workspace database schema to the new version of the template")
@idempotent
@http(method: "POST", uri: "/workspaces/{workspace_name}/db/migrate")
@tags(["Workspace Management"])
operation MigrateWorkspaceSchema with [GetOperation] {
    input: WorkspaceSelectorRequest
    output: WorkspaceResponse
}

@documentation("Rotates the workspace encryption key. Generates a new encryption key and re-encrypts all secrets with the new key. This is a critical operation that should be done during low-traffic periods.")
@http(method: "POST", uri: "/workspaces/{workspace_name}/rotate-encryption-key")
@idempotent
@tags(["Workspace Management"])
operation RotateWorkspaceEncryptionKey {
    input: WorkspaceSelectorRequest

    output := {
        @required
        @notProperty
        @documentation("Number of secrets that were re-encrypted with the new key.")
        total_secrets_re_encrypted: Long
    }
}
