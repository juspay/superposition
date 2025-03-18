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
        // FIXME `BigDecimal`/`BigInteger` cannot be deserialized.
        // Until then using integer.
        weight: Integer
        override_with_keys: OverrideWithKeys
        description: String
        change_reason: String
        created_at: Timestamp
        created_by: String
        last_modified_at: Timestamp
        last_modified_by: String
    }
    operations: [
        CreateContext
        MoveContext
        UpdateOverride
        GetContext
        GetContextFromCondition
        ListContexts
    ]
}

structure ContextFull for Context {
    @required
    $id

    $value

    $override

    $override_id

    $weight

    $override_with_keys

    $description

    $change_reason

    @timestampFormat("date-time")
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

@http(method: "PUT", uri: "/context/")
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
}

@http(method: "POST", uri: "/context/get")
operation GetContextFromCondition {
    input := for Context with [WorkspaceMixin] {
        @httpPayload
        @notProperty
        context: Document
    }

    output: ContextFull
}

enum ContextFilterSortOn {
    CreatedAt
    Weight
}

enum SortBy {
    Desc
    Asc
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

        @httpQuery("size")
        @notProperty
        size: Integer

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
