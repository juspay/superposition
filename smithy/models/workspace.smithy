
$version: "2.0"

namespace io.superposition

use aws.protocols#restJson1

resource Workspace {
    identifiers: {
        workspace_name: String
        workspace_id: String
        org_id: String
    }
    properties: {
        organisation_id: String
        organisation_name: String
        workspace_schema_name: String
        workspace_status: String
        workspace_admin_email: String
        created_by: String
        last_modified_by: String
        last_modified_at: Timestamp
        created_at: Timestamp
        mandatory_dimensions: ListMandatoryDimensions
    }

    list: ListWorkspace
    put: UpdateWorkspace
    operations: [CreateWorkspace]

}

list ListMandatoryDimensions {
    member: String
}


structure CreateWorkspaceRequest for Workspace with [WorkspaceMixin] {
    @required
    $workspace_admin_email

    @required
    $workspace_name

    $workspace_status

}


structure UpdateWorkspaceRequest for Workspace with [WorkspaceMixin] {

    @httpLabel
    @required
    $workspace_name

    @required
    $workspace_admin_email

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

    @required
    $created_by

    @required
    $last_modified_by

    @required
    $last_modified_at

    @required
    $created_at

    $mandatory_dimensions

}

list WorkspaceList {
    member: WorkspaceResponse
}

structure WorkspaceListResponse for Workspace{

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
    input : CreateWorkspaceRequest
    output: WorkspaceResponse
}


@idempotent
@http(method: "PUT", uri: "/workspaces/{workspace_name}")
operation UpdateWorkspace {
    input : UpdateWorkspaceRequest

    output: WorkspaceResponse

    errors: [
        WorkspaceNotFound
    ]
}

@readonly
@http(method: "GET", uri: "/workspaces")
operation ListWorkspace {
    input :=  for Workspace with [WorkspaceMixin]{
        @httpQuery("page")
        page: Long

        @httpQuery("count")
        count: Long

        @httpQuery("all")
        all: Boolean
    }

    output : WorkspaceListResponse
}
