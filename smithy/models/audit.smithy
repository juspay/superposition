$version: "2.0"

namespace io.superposition

resource AuditLog {
    identifiers: {
        id: String
        workspace_id: String
        org_id: String
    }
    properties: {}
    list: ListAuditLogs
}

structure AuditLogFull {
    // REVIEW Technically this should have been properties of the resource
    // but since we're not using these directly in an input or output, smithy
    // will complain about them and fail the build.
    @required
    id: String

    @required
    table_name: String

    @required
    user_name: String

    @required
    timestamp: DateTime

    @required
    action: AuditAction

    original_data: Document

    new_data: Document

    @required
    query: String
}

list AuditLogList {
    member: AuditLogFull
}

enum AuditAction {
    INSERT
    UPDATE
    DELETE
}

list AuditActionList {
    member: AuditAction
}

@documentation("Retrieves a paginated list of audit logs with support for filtering by date range, table names, actions, and usernames for compliance and monitoring purposes.")
@readonly
@http(method: "GET", uri: "/audit")
@tags(["Audit & Monitoring"])
operation ListAuditLogs {
    input := with [WorkspaceMixin, PaginationParams] {
        @httpQuery("from_date")
        @notProperty
        from_date: DateTime

        @httpQuery("to_date")
        @notProperty
        to_date: DateTime

        @httpQuery("table")
        @notProperty
        tables: StringList

        @httpQuery("action")
        @notProperty
        action: AuditActionList

        @httpQuery("username")
        @notProperty
        username: String

        @httpQuery("sort_by")
        @notProperty
        sort_by: SortBy
    }

    output := with [PaginatedResponse] {
        @required
        data: AuditLogList
    }
}
