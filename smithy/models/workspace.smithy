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
        workspace_strict_mode: Boolean
    }
    list: ListWorkspace
    put: UpdateWorkspace
    operations: [
        CreateWorkspace
    ]
}

enum WorkspaceStatus {
    ENABLED
    DISABLED
}

list ListMandatoryDimensions {
    member: String
}

structure CreateWorkspaceRequest for Workspace with [CreateWorkspaceMixin] {
    @required
    $workspace_admin_email

    @required
    $workspace_name

    $workspace_status
    
    @required
    $workspace_strict_mode
}

structure UpdateWorkspaceRequest for Workspace with [CreateWorkspaceMixin] {
    @httpLabel
    @required
    $workspace_name

    @required
    $workspace_admin_email

    $config_version

    $mandatory_dimensions

    $workspace_status
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
    $workspace_strict_mode
}

list WorkspaceList {
    member: WorkspaceResponse
}

structure WorkspaceListResponse for Workspace {
    @required
    total_pages: Long

    @required
    total_items: Long

    @required
    data: WorkspaceList
}

@httpError(404)
@error("client")
structure WorkspaceNotFound {}

// Operations
@http(method: "POST", uri: "/workspaces")
operation CreateWorkspace {
    input: CreateWorkspaceRequest
    output: WorkspaceResponse
}

@idempotent
@http(method: "PUT", uri: "/workspaces/{workspace_name}")
operation UpdateWorkspace {
    input: UpdateWorkspaceRequest
    output: WorkspaceResponse
    errors: [
        WorkspaceNotFound
    ]
}

@readonly
@http(method: "GET", uri: "/workspaces")
operation ListWorkspace {
    input := with [PaginationParams, CreateWorkspaceMixin] {}
    output: WorkspaceListResponse
}
