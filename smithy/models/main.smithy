// Smithy version, not API version.
$version: "2.0"

namespace io.superposition

use aws.protocols#restJson1

@title("Superposition")
@restJson1
service Superposition {
    version: "2025-03-05"
    resources: [
        DefaultConfig
        Dimension
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
    @httpHeader("x-workspace-id")
    workspace_id: String
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
