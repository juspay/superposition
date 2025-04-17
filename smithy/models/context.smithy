$version: "2.0"

namespace io.superposition

list OverrideWithKeys {
    member: String
}

resource Context {
    identifiers: {
        workspace_id: String
        org_id: String
    }
    properties: {
        // FIXME This should have been an identifier but
        // smithy does not allow PUT's w/o all identifiers
        // in the request, so due to our create API, we had
        // to move it here.
        id: String
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
    put: CreateContext
    operations: [
        MoveContext
        UpdateOverride
        GetContext
        GetContextFromCondition
        ListContexts
        WeightRecompute
        BulkOperation
    ]
}

structure ContextFull for Context {
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

structure ContextActionResponse for Context {
    @required
    @notProperty
    context_id: String

    @required
    @notProperty
    override_id: String

    @required
    $weight

    @required
    $description

    @required
    $change_reason
}

@idempotent
@http(method: "PUT", uri: "/context")
operation CreateContext {
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

    output: ContextActionResponse
}

@http(method: "GET", uri: "/context/{id}")
operation GetContext {
    input := for Context with [WorkspaceMixin] {
        @httpLabel
        @required
        $id
    }

    output: ContextFull

    errors: [
        ResourceNotFound
    ]
}

@http(method: "PUT", uri: "/context/move/{id}")
operation MoveContext {
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

    output: ContextActionResponse

    errors: [
        ResourceNotFound
    ]
}

@http(method: "PUT", uri: "/context/overrides")
operation UpdateOverride {
    input := for Context with [WorkspaceMixin] {
        // TODO Find re-name functionality.
        @required
        @notProperty
        context: Condition

        // REVIEW Should this be made a property?
        @httpHeader("x-config-tags")
        @notProperty
        config_tags: String

        @required
        $override

        $description

        @required
        $change_reason
    }

    output: ContextActionResponse

    errors: [
        ResourceNotFound
    ]
}

@http(method: "POST", uri: "/context/get")
operation GetContextFromCondition {
    input := for Context with [WorkspaceMixin] {
        @httpPayload
        @notProperty
        context: Document
    }

    output: ContextFull

    errors: [
        ResourceNotFound
    ]
}

enum ContextFilterSortOn {
    LastModifiedAt = "last_modified_at"
    CreatedAt = "created_at"
    Weight = "weight"
}

list ListContextOut {
    member: ContextFull
}

@readonly
@http(method: "GET", uri: "/context/list")
operation ListContexts {
    input := with [WorkspaceMixin] {
        @httpQuery("page")
        @notProperty
        page: Integer

        @httpQuery("count")
        @notProperty
        count: Integer

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
    }

    output := {
        @notProperty
        total_pages: Integer

        @notProperty
        total_items: Integer

        @notProperty
        data: ListContextOut
    }
}

@idempotent
@http(method: "DELETE", uri: "/context/{id}", code: 201)
operation DeleteContext {
    input := for Context with [WorkspaceMixin] {
        @httpLabel
        @required
        $id

        @httpHeader("x-config-tags")
        @notProperty
        config_tags: String
    }

    output := {}

    errors: [
        ResourceNotFound
    ]
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

@http(method: "PUT", uri: "/context/weight/recompute")
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
    REPLACE: ContextPut
    DELETE: String
    MOVE: ContextMove
}

list BulkOperationList {
    member: ContextAction
}

structure BulkOperationReq {
    operations: BulkOperationList
}

@mixin
structure ContextPutOutMixin for Context {
    context_id: String
    override_id: String
    weight: Weight
    description: String
    change_reason: String
}

structure ContextPutOut with [ContextPutOutMixin] {}

structure ContextMoveOut with [ContextPutOutMixin] {}

union ContextActionOut for Context {
    PUT: ContextPutOut
    REPLACE: ContextPutOut
    DELETE: String
    MOVE: ContextMoveOut
}

list BulkOperationOutList {
    member: ContextActionOut
}

structure BulkOperationOut {
    output: BulkOperationOutList
}

@http(method: "PUT", uri: "/context/bulk-operations")
operation BulkOperation {
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
