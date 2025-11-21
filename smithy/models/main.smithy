// Smithy version, not API version.
$version: "2.0"

namespace io.superposition

use aws.protocols#restJson1

@title("Superposition")
@restJson1
@httpBearerAuth
@httpBasicAuth
service Superposition {
    version: "2025-03-05"
    resources: [
        DefaultConfig
        Dimension
        Context
        Config
        ConfigVersion
        AuditLog
        Function
        Organisation
        Experiments
        TypeTemplates
        Workspace
        Webhook
        ExperimentGroup
    ]
    errors: [
        InternalServerError
    ]
}

@mixin
structure PaginationParams {
    @httpQuery("count")
    @documentation("Number of items to be returned in each page.")
    count: Integer

    @httpQuery("page")
    @documentation("Page number to retrieve, starting from 1.")
    page: Integer

    @httpQuery("all")
    @documentation("If true, returns all requested items, ignoring pagination parameters page and count.")
    all: Boolean
}

@mixin
structure WorkspaceMixin {
    @required
    @httpHeader("x-workspace")
    workspace_id: String

    @required
    @httpHeader("x-org-id")
    org_id: String
}

@mixin
structure OrganisationMixin {
    @required
    @httpHeader("x-org-id")
    org_id: String
}

@mixin
structure PaginatedResponse {
    @required
    total_pages: Integer

    @required
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

@mixin
operation GetOperation {
    errors: [
        ResourceNotFound
    ]
}

@timestampFormat("date-time")
timestamp DateTime

@timestampFormat("http-date")
timestamp HttpDate
