$version: "2.0"

namespace io.superposition

@documentation("Variables are key-value pairs used to store configuration values that can be referenced in contexts, webhooks, and other parts of the system.")
resource Variable {
    identifiers: {
        workspace_id: String
        org_id: String
        name: String
    }
    properties: {
        value: String
        description: String
        change_reason: String
        created_by: String
        created_at: DateTime
        last_modified_by: String
        last_modified_at: DateTime
    }
    list: ListVariables
    update: UpdateVariable
    read: GetVariable
    delete: DeleteVariable
    operations: [
        CreateVariable
    ]
}

structure VariableResponse for Variable {
    @required
    $name

    @required
    $value

    @required
    $description

    @required
    $change_reason

    @required
    $created_by

    @required
    $created_at

    @required
    $last_modified_by

    @required
    $last_modified_at
}

list VariableList {
    member: VariableResponse
}

enum VariableSortOn {
    NAME = "name"
    CREATED_AT = "created_at"
    LAST_MODIFIED_AT = "last_modified_at"
}

@documentation("Creates a new variable with the specified name and value.")
@http(method: "POST", uri: "/variables")
@tags(["Variables"])
operation CreateVariable {
    input := for Variable with [WorkspaceMixin] {
        @required
        name: String

        @required
        value: String

        @required
        description: String

        @required
        change_reason: String
    }

    output: VariableResponse
}

@documentation("Updates an existing variable's value, description, or tags.")
@idempotent
@http(method: "PATCH", uri: "/variables/{name}")
@tags(["Variables"])
operation UpdateVariable with [GetOperation] {
    input := for Variable with [WorkspaceMixin] {
        @httpLabel
        @required
        $name

        value: String

        description: String

        @required
        change_reason: String
    }

    output: VariableResponse
}

@documentation("Retrieves a paginated list of all variables in the workspace with optional filtering and sorting.")
@readonly
@http(method: "GET", uri: "/variables")
@tags(["Variables"])
operation ListVariables {
    input := with [PaginationParams, WorkspaceMixin] {
        @httpQuery("name")
        @documentation("Filter by variable name (exact match or substring, depending on backend implementation).")
        @notProperty
        name: String

        @httpQuery("created_by")
        @documentation("Filter by the user who created the variable")
        created_by: String

        @httpQuery("last_modified_by")
        @documentation("Filter by the user who last modified the variable")
        last_modified_by: String

        @httpQuery("sort_on")
        @documentation("Field to sort the results by.")
        @notProperty
        sort_on: VariableSortOn

        @httpQuery("sort_by")
        @documentation("Sort order (ascending or descending).")
        @notProperty
        sort_by: SortBy
    }

    output := with [PaginatedResponse] {
        @required
        data: VariableList
    }
}

@documentation("Retrieves detailed information about a specific variable by its name.")
@readonly
@http(method: "GET", uri: "/variables/{name}")
@tags(["Variables"])
operation GetVariable with [GetOperation] {
    input := for Variable with [WorkspaceMixin] {
        @httpLabel
        @required
        $name
    }

    output: VariableResponse
}

@documentation("Permanently deletes a variable from the workspace.")
@idempotent
@http(method: "DELETE", uri: "/variables/{name}")
@tags(["Variables"])
operation DeleteVariable with [GetOperation] {
    input := for Variable with [WorkspaceMixin] {
        @httpLabel
        @required
        $name
    }

    output: VariableResponse
}
