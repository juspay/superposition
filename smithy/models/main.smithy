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
        ExperimentConfig
        Variable
        Secret
        MasterKey
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

@documentation("Indicates that the operation succeeded but the webhook call failed. The response body contains the successful result, but the client should be aware that webhook notification did not complete.")
@httpError(512)
@error("server")
structure WebhookFailed {
    @required
    @documentation("The successful operation result that would have been returned with HTTP 200, serialized as an untyped/raw JSON document. The structure logically corresponds to the operation's normal output type, but is modeled as Document since this single error is shared across multiple operations with different output shapes.")
    data: Document
}

@mixin
operation GetOperation {
    errors: [
        ResourceNotFound
    ]
}

@mixin
operation WebhookOperation {
    errors: [
        WebhookFailed
    ]
}

@timestampFormat("date-time")
timestamp DateTime

@timestampFormat("http-date")
timestamp HttpDate
