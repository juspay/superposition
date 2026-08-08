// Smithy version, not API version.
$version: "2.0"

namespace io.superposition

union DimensionType {
    REGULAR: Unit
    LOCAL_COHORT: String
    REMOTE_COHORT: String
}

map DependencyGraph {
    key: String
    value: StringList
}

resource Dimension {
    identifiers: {
        dimension: String
        workspace_id: String
        org_id: String
    }
    properties: {
        position: Integer
        schema: Object
        value_validation_function_name: String
        description: String
        change_reason: String
        dependency_graph: DependencyGraph
        created_at: DateTime
        created_by: String
        last_modified_at: DateTime
        last_modified_by: String
        value_compute_function_name: String
        dimension_type: DimensionType
    }
    list: ListDimensions
    update: UpdateDimension
    delete: DeleteDimension
    read: GetDimension
    operations: [
        CreateDimension
        BulkDimensionOperation
    ]
}

structure DimensionResponse for Dimension {
    @required
    $dimension

    @required
    $position

    @required
    $schema

    $value_validation_function_name

    @required
    $description

    @required
    $change_reason

    @required
    $last_modified_at

    @required
    $last_modified_by

    @required
    $created_at

    @required
    $created_by

    @required
    $dependency_graph

    @required
    $dimension_type

    $value_compute_function_name

    @notProperty
    @required
    mandatory: Boolean
}

list DimensionList {
    member: DimensionResponse
}

@documentation("Creates a new dimension with the specified json schema. Dimensions define categorical attributes used for context-based config management.")
@http(method: "POST", uri: "/dimension")
@tags(["Dimensions"])
operation CreateDimension with [WebhookOperation, WorkspaceWriteOperation] {
    input := for Dimension with [WorkspaceMixin] {
        @required
        $dimension

        @required
        $position

        @required
        $schema

        $value_validation_function_name

        @required
        $description

        @required
        $change_reason

        $dimension_type

        $value_compute_function_name
    }

    output: DimensionResponse
}

structure DimensionBulkItem for Dimension {
    @required
    $dimension

    @required
    $position

    @required
    $schema

    $value_validation_function_name

    @required
    $description

    @required
    $change_reason

    $dimension_type

    $value_compute_function_name
}

@length(min: 1)
list DimensionBulkOperationList {
    member: DimensionAction
}

structure DimensionUpdateBulkRequest {
    @required
    dimension: String

    @required
    request: DimensionUpdateBulkPayload
}

structure DimensionUpdateBulkPayload for Dimension {
    $schema
    $position
    $value_validation_function_name
    $description

    @required
    $change_reason

    $value_compute_function_name
}

union DimensionAction {
    CREATE: DimensionBulkItem
    UPDATE: DimensionUpdateBulkRequest
    DELETE: String
}

union DimensionBulkOperationOutput {
    CREATE: DimensionResponse
    UPDATE: DimensionResponse
    DELETE: String
}

list DimensionBulkOperationOutputList {
    member: DimensionBulkOperationOutput
}

@documentation("Executes multiple dimension create, update, and delete operations atomically.")
@idempotent
@http(method: "PUT", uri: "/dimension/bulk-operations")
@tags(["Dimensions"])
operation BulkDimensionOperation with [WebhookOperation, WorkspaceWriteOperation] {
    input := with [WorkspaceMixin] {
        @required
        operations: DimensionBulkOperationList
    }

    output := {
        @required
        output: DimensionBulkOperationOutputList
    }
}

@documentation("Retrieves a paginated list of all dimensions in the workspace. Dimensions are returned with their details and metadata.")
@readonly
@http(method: "GET", uri: "/dimension")
@tags(["Dimensions"])
operation ListDimensions {
    input := with [PaginationParams, WorkspaceMixin] {}

    output := with [PaginatedResponse] {
        @required
        data: DimensionList
    }
}

@documentation("Retrieves detailed information about a specific dimension, including its schema, cohort dependency graph, and configuration metadata.")
@readonly
@http(method: "GET", uri: "/dimension/{dimension}")
@tags(["Dimensions"])
operation GetDimension with [GetOperation] {
    input := for Dimension with [WorkspaceMixin] {
        @httpLabel
        @required
        $dimension
    }

    output: DimensionResponse
}

@documentation("Updates an existing dimension's configuration. Allows modification of schema, position, function mappings, and other properties while maintaining dependency relationships.")
@idempotent
@http(method: "PATCH", uri: "/dimension/{dimension}")
@tags(["Dimensions"])
operation UpdateDimension with [GetOperation, WebhookOperation, WorkspaceWriteOperation] {
    input := for Dimension with [WorkspaceMixin] {
        @httpLabel
        @required
        $dimension

        $schema

        $position

        @documentation("To unset the function name, pass \"null\" string.")
        $value_validation_function_name

        $description

        @required
        $change_reason

        @documentation("To unset the function name, pass \"null\" string.")
        $value_compute_function_name
    }

    output: DimensionResponse
}

@documentation("Permanently removes a dimension from the workspace. This operation will fail if the dimension has active dependencies or is referenced by existing configurations.")
@idempotent
@http(method: "DELETE", uri: "/dimension/{dimension}", code: 204)
@tags(["Dimensions"])
operation DeleteDimension with [GetOperation, WebhookOperation, WorkspaceWriteOperation] {
    input := for Dimension with [WorkspaceMixin] {
        @httpLabel
        @required
        $dimension
    }
}
