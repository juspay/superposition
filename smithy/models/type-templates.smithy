// Smithy version, not API version.
$version: "2.0"

namespace io.superposition

resource TypeTemplates {
    identifiers: {
        type_name: String
        workspace_id: String
        org_id: String
    }
    properties: {
        type_schema: Object
        description: String
        change_reason: String
        created_by: String
        created_at: DateTime
        last_modified_at: DateTime
        last_modified_by: String
    }
    list: GetTypeTemplatesList
    update: UpdateTypeTemplates
    delete: DeleteTypeTemplates
    read: GetTypeTemplate
    operations: [
        CreateTypeTemplates
    ]
}

structure CreateTypeTemplatesRequest for TypeTemplates with [WorkspaceMixin] {
    @required
    $type_name

    @required
    $type_schema

    @required
    $description

    @required
    $change_reason
}

structure UpdateTypeTemplatesRequest for TypeTemplates with [WorkspaceMixin] {
    @httpLabel
    @required
    $type_name

    @required
    $type_schema

    $description

    @required
    $change_reason
}

structure TypeTemplatesResponse for TypeTemplates {
    @required
    $type_name

    @required
    $type_schema

    @required
    $description

    @required
    $change_reason

    @required
    $created_by

    @required
    $created_at

    @required
    $last_modified_at

    @required
    $last_modified_by
}

list TypeTemplatesList {
    member: TypeTemplatesResponse
}

@documentation("Retrieves detailed information about a specific type template including its schema and metadata.")
@readonly
@http(method: "GET", uri: "/types/{type_name}")
@tags(["Type Templates"])
operation GetTypeTemplate with [GetOperation] {
    input := for TypeTemplates with [WorkspaceMixin] {
        @httpLabel
        @required
        $type_name
    }

    output: TypeTemplatesResponse
}

// Operations
@documentation("Creates a new type template with specified schema definition, providing reusable type definitions for config validation.")
@http(method: "POST", uri: "/types")
@tags(["Type Templates"])
operation CreateTypeTemplates {
    input: CreateTypeTemplatesRequest
    output: TypeTemplatesResponse
}

@documentation("Retrieves a paginated list of all type templates in the workspace, including their schemas and metadata for type management.")
@readonly
@http(method: "GET", uri: "/types")
@tags(["Type Templates"])
operation GetTypeTemplatesList {
    input := with [PaginationParams, WorkspaceMixin] {}

    output := with [PaginatedResponse] {
        @required
        data: TypeTemplatesList
    }
}

@documentation("Updates an existing type template's schema definition and metadata while preserving its identifier and usage history.")
@idempotent
@http(method: "PATCH", uri: "/types/{type_name}")
@tags(["Type Templates"])
operation UpdateTypeTemplates with [GetOperation] {
    input: UpdateTypeTemplatesRequest
    output: TypeTemplatesResponse
}

@documentation("Permanently removes a type template from the workspace. No checks performed while deleting")
@idempotent
@http(method: "DELETE", uri: "/types/{type_name}")
@tags(["Type Templates"])
operation DeleteTypeTemplates with [GetOperation] {
    input := for TypeTemplates with [WorkspaceMixin] {
        @httpLabel
        @required
        $type_name
    }

    output: TypeTemplatesResponse
}
