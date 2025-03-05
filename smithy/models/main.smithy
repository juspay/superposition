// Smithy version, not API version.
$version: "2.0"

namespace io.superposition

use aws.protocols#restJson1

@title("Superposition")
@restJson1
@httpBearerAuth
service Superposition {
    version: "2025-03-05"
    resources: [
        DefaultConfig
        Dimension
        Context
        Config
        AuditLog
        Function
        Organisation
        Experiments
        TypeTemplates
        Workspace
    ]
    errors: [
        InternalServerError
    ]
}

@mixin
structure PaginationParams {
    @httpQuery("count")
    count: Integer

    @httpQuery("page")
    page: Integer
}

@mixin
structure WorkspaceMixin {
    @required
    @httpHeader("x-tenant")
    workspace_id: String

    @required
    @httpHeader("x-org-id")
    @default("juspay")
    org_id: String
}

@mixin
structure CreateWorkspaceMixin {
    @required
    @httpHeader("x-org-id")
    @default("juspay")
    org_id: String
}


@mixin
structure PaginatedResponse {
    total_pages: Integer
    total_items: Integer
}

// Errors
@httpError(500)
@error("server")
structure InternalServerError {
    message: String
}

@httpError(404)
@error("client")
structure ResourceNotFound {}

@timestampFormat("date-time")
timestamp DateTime

@timestampFormat("http-date")
timestamp HttpDate
