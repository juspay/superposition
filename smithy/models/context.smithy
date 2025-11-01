$version: "2.0"

namespace io.superposition

list OverrideWithKeys {
    member: String
}

resource Context {
    identifiers: {
        workspace_id: String
        org_id: String
        id: String
    }
    properties: {
        value: Condition
        override: Overrides
        override_id: String
        weight: Weight
        description: String
        change_reason: String
        created_at: DateTime
        created_by: String
        last_modified_at: DateTime
        last_modified_by: String
    }
    delete: DeleteContext
    create: CreateContext
    read: GetContext
    list: ListContexts
    operations: [
        MoveContext
    ]
    collectionOperations: [
        UpdateOverride
        GetContextFromCondition
        WeightRecompute
        BulkOperation
        ValidateContext
    ]
}

structure ContextResponse for Context {
    @required
    $id

    $value

    $override

    $override_id

    $weight

    $description

    $change_reason

    $created_at

    $created_by

    $last_modified_at

    $last_modified_by
}

@documentation("Creates a new context with specified conditions and overrides. Contexts define conditional rules for config management.")
@idempotent
@http(method: "PUT", uri: "/context")
@tags(["Context Management"])
operation CreateContext with [GetOperation] {
    input := for Context with [WorkspaceMixin] {
        // TODO Find re-name functionality.
        @required
        @notProperty
        context: Condition

        @httpHeader("x-config-tags")
        @notProperty
        config_tags: String

        @required
        $override

        $description

        @required
        $change_reason
    }

    output: ContextResponse
}

@documentation("Validates if a given context condition is well-formed")
@idempotent
@http(method: "PUT", uri: "/context/validate")
@tags(["Context Management"])
operation ValidateContext {
    input := for Context with [WorkspaceMixin] {
        @required
        @notProperty
        context: Condition
    }
}

@documentation("Retrieves detailed information about a specific context by its unique identifier, including conditions, overrides, and metadata.")
@readonly
@http(method: "GET", uri: "/context/{id}")
@tags(["Context Management"])
operation GetContext with [GetOperation] {
    input := for Context with [WorkspaceMixin] {
        @httpLabel
        @required
        $id
    }

    output: ContextResponse
}

@documentation("Updates the condition of the mentioned context, if a context with the new condition already exists, it merges the override and effectively deleting the old context")
@http(method: "PUT", uri: "/context/move/{id}")
@tags(["Context Management"])
operation MoveContext with [GetOperation] {
    input := for Context with [WorkspaceMixin] {
        @httpLabel
        @required
        $id

        @required
        @notProperty
        context: Condition

        $description

        @required
        $change_reason
    }

    output: ContextResponse
}

union ContextIdentifier {
    id: String
    context: Condition
}

structure UpdateContextOverrideRequest for Context {
    @required
    context: ContextIdentifier

    @required
    $override

    $description

    @required
    $change_reason
}

@documentation("Updates the overrides for an existing context. Allows modification of override values while maintaining the context's conditions.")
@http(method: "PATCH", uri: "/context/overrides")
@tags(["Context Management"])
operation UpdateOverride with [GetOperation] {
    input := for Context with [WorkspaceMixin] {
        // REVIEW Should this be made a property?
        @httpHeader("x-config-tags")
        @notProperty
        config_tags: String

        @httpPayload
        @required
        @notProperty
        request: UpdateContextOverrideRequest
    }

    output: ContextResponse
}

@documentation("Retrieves context information by matching against provided conditions. Used to find contexts that would apply to specific scenarios.")
@http(method: "POST", uri: "/context/get")
@tags(["Context Management"])
operation GetContextFromCondition with [GetOperation] {
    input := for Context with [WorkspaceMixin] {
        @httpPayload
        @notProperty
        context: Document
    }

    output: ContextResponse
}

enum ContextFilterSortOn {
    LastModifiedAt = "last_modified_at"
    CreatedAt = "created_at"
    Weight = "weight"
}

list ListContextOut {
    member: ContextResponse
}

@documentation("Retrieves a paginated list of contexts with support for filtering by creation date, modification date, weight, and other criteria.")
@readonly
@http(method: "GET", uri: "/context")
@tags(["Context Management"])
operation ListContexts {
    input := with [PaginationParams, WorkspaceMixin] {
        @httpQuery("prefix")
        @notProperty
        prefix: String

        @httpQuery("sort_on")
        @notProperty
        sort_on: ContextFilterSortOn

        @httpQuery("sort_by")
        @notProperty
        sort_by: SortBy

        @httpQuery("created_by")
        @notProperty
        created_by: String

        @httpQuery("last_modified_by")
        @notProperty
        last_modified_by: String

        @httpQuery("plaintext")
        @notProperty
        plaintext: String

        @httpQuery("dimension_match_strategy")
        @notProperty
        dimension_match_strategy: DimensionMatchStrategy
    }

    output := with [PaginatedResponse] {
        data: ListContextOut
    }
}

@documentation("Permanently removes a context from the workspace. This operation cannot be undone and will affect config resolution.")
@idempotent
@http(method: "DELETE", uri: "/context/{id}", code: 201)
@tags(["Context Management"])
operation DeleteContext with [GetOperation] {
    input := for Context with [WorkspaceMixin] {
        @httpLabel
        @required
        $id

        @httpHeader("x-config-tags")
        @notProperty
        config_tags: String
    }
}

structure WeightRecomputeResponse for Context {
    $id
    condition: Condition
    old_weight: Weight
    new_weight: Weight
}

list WeightRecomputeResponses {
    member: WeightRecomputeResponse
}

@documentation("Recalculates and updates the priority weights for all contexts in the workspace based on their dimensions.")
@http(method: "PUT", uri: "/context/weight/recompute")
@tags(["Context Management"])
operation WeightRecompute {
    input := with [WorkspaceMixin] {
        @httpHeader("x-config-tags")
        @notProperty
        config_tags: String
    }

    output := {
        @notProperty
        data: WeightRecomputeResponses
    }
}

structure ContextPut for Context {
    @required
    context: Condition

    @required
    $override

    $description

    @required
    $change_reason
}

structure ContextMove for Context {
    $id

    @required
    context: Condition

    $description

    @required
    $change_reason
}

union ContextAction {
    PUT: ContextPut
    REPLACE: UpdateContextOverrideRequest
    DELETE: String
    MOVE: ContextMove
}

list BulkOperationList {
    member: ContextAction
}

structure BulkOperationReq {
    operations: BulkOperationList
}

union ContextActionOut for Context {
    PUT: ContextResponse
    REPLACE: ContextResponse
    DELETE: String
    MOVE: ContextResponse
}

list BulkOperationOutList {
    member: ContextActionOut
}

structure BulkOperationOut {
    output: BulkOperationOutList
}

@documentation("Executes multiple context operations (PUT, REPLACE, DELETE, MOVE) in a single atomic transaction for efficient batch processing.")
@http(method: "PUT", uri: "/context/bulk-operations")
@tags(["Context Management"])
operation BulkOperation with [GetOperation] {
    input := for Context with [WorkspaceMixin] {
        @httpHeader("x-config-tags")
        @notProperty
        config_tags: String

        @httpPayload
        @required
        @notProperty
        bulk_operation: BulkOperationReq
    }

    output := {
        @httpPayload
        @notProperty
        bulk_operation_output: BulkOperationOut
    }
}
