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
        dependencies: Dependencies
        dependents: Dependents
        dependency_graph: Object
        created_at: DateTime
        created_by: String
        last_modified_at: DateTime
        last_modified_by: String
        autocomplete_function_name: String
    }
    list: ListDimensions
    put: UpdateDimension
    delete: DeleteDimension
    operations: [
        GetDimension
        CreateDimension
    ]
}

list Dependencies {
    member: String
}

list Dependents {
    member: String
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

    @required
    $dependencies

    @required
    $dependents

    @required
    $dependency_graph

    $autocomplete_function_name
}

structure DimensionExt with [DimensionMixin] {
    @notProperty
    mandatory: Boolean
}

list DimensionExtList {
    member: DimensionExt
}

@documentation("Creates a new dimension with the specified json schema. Dimensions define categorical attributes used for context-based config management.")
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

        $dependencies

        @required
        $description

        @required
        $change_reason

        $autocomplete_function_name
    }

    output: DimensionExt
}

@documentation("Retrieves a paginated list of all dimensions in the workspace. Dimensions are returned with their details and metadata.")
@readonly
@http(method: "GET", uri: "/dimension")
operation ListDimensions {
    input := with [PaginationParams, WorkspaceMixin] {}
    output := with [PaginatedResponse] {
        data: DimensionExtList
    }
}

@documentation("Retrieves detailed information about a specific dimension, including its schema, dependencies, and configuration metadata.")
@http(method: "GET", uri: "/dimension/{dimension}")
operation GetDimension {
    input := for Dimension with [WorkspaceMixin] {
        @httpLabel
        @required
        $dimension
    }

    output: DimensionExt

    errors: [
        ResourceNotFound
    ]
}

@documentation("Updates an existing dimension's configuration. Allows modification of schema, position, function mappings, and other properties while maintaining dependency relationships.")
@idempotent
@http(method: "PUT", uri: "/dimension/{dimension}")
operation UpdateDimension {
    input := for Dimension with [WorkspaceMixin] {
        @httpLabel
        @required
        $dimension

        $schema

        $position

        $function_name

        $description

        $dependencies

        @required
        $change_reason

        $autocomplete_function_name
    }

    output: DimensionExt

    errors: [
        ResourceNotFound
    ]
}

@documentation("Permanently removes a dimension from the workspace. This operation will fail if the dimension has active dependencies or is referenced by existing configurations.")
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
