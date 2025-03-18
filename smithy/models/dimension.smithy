// Smithy version, not API version.
$version: "2.0"

namespace io.superposition

resource Dimension {
    identifiers: {
        dimension: String
        workspace_id: String
        org_id: String
    }
    properties: {
        position: Integer
        schema: Document
        function_name: String
        description: String
        change_reason: String
        created_at: Timestamp
        created_by: String
        last_modified_at: Timestamp
        last_modified_by: String
    }
    list: ListDimensions
    put: UpdateDimension
    delete: DeleteDimension
    operations: [
        CreateDimension
    ]
}

@mixin
structure DimensionMixin for Dimension {
    @required
    $dimension

    @required
    $position

    @required
    $schema

    $function_name

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
}

structure DimensionExt with [DimensionMixin] {
    @notProperty
    mandatory: Boolean
}

list DimensionExtList {
    member: DimensionExt
}

@http(method: "POST", uri: "/dimension")
operation CreateDimension {
    input := for Dimension with [WorkspaceMixin] {
        @required
        $dimension

        @required
        $position

        @required
        $schema

        $function_name

        @required
        $description

        @required
        $change_reason
    }

    output: DimensionExt
}

@readonly
@http(method: "GET", uri: "/dimension")
operation ListDimensions {
    input := with [PaginationParams, WorkspaceMixin] {}
    output := with [PaginatedResponse] {
        data: DimensionExtList
    }
}

@idempotent
@http(method: "PUT", uri: "/dimension/{dimension}")
operation UpdateDimension {
    input := for Dimension with [WorkspaceMixin] {
        @httpLabel
        @required
        $dimension

        $schema

        $function_name

        $description

        @required
        $change_reason
    }

    output: DimensionExt

    errors: [
        ResourceNotFound
    ]
}

@idempotent
@http(method: "DELETE", uri: "/dimension/{dimension}", code: 201)
operation DeleteDimension {
    input := for Dimension with [WorkspaceMixin] {
        @httpLabel
        @required
        $dimension
    }

    output := {}

    errors: [
        ResourceNotFound
    ]
}
