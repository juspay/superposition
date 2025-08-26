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
    table_name: String

    user_name: String

    timestamp: DateTime

    action: String

    original_data: Document

    new_data: Document

    query: String
}

list AuditLogList {
    member: AuditLogFull
}

@documentation("Retrieves a paginated list of audit logs with support for filtering by date range, table names, actions, and usernames for compliance and monitoring purposes.")
@readonly
@http(method: "GET", uri: "/audit")
operation ListAuditLogs {
    input := with [WorkspaceMixin, PaginationParams] {
        @httpQuery("from_date")
        @notProperty
        from_date: DateTime

        @httpQuery("to_date")
        @notProperty
        to_date: DateTime

        /// Comma serparated list of tables.
        @httpQuery("table")
        @notProperty
        tables: String

        /// Comma serparated list of actions.
        @httpQuery("action")
        @notProperty
        action: String

        @httpQuery("username")
        @notProperty
        username: String
    }

    output := with [PaginatedResponse] {
        data: AuditLogList
    }
}
