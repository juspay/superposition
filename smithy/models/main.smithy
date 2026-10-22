// Smithy version, not API version.
$version: "2.0"

metadata suppressions = [
    {
        id: "HttpHeaderTrait"
        namespace: "io.superposition"
        reason: "Superposition allows SDK callers to set the user-agent header to identify their application."
    }
]

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

    @httpHeader("user-agent")
    user_agent: String
}

@mixin
structure OrganisationMixin {
    @required
    @httpHeader("x-org-id")
    org_id: String

    @httpHeader("user-agent")
    user_agent: String
}

// The user_agent header is a cross-cutting concern and is neither a resource
// identifier nor a resource property. Resource-bound operations require such
// members to be marked with @notProperty; since the trait cannot be applied on
// mixin members directly (TraitTarget constraint), it is applied here on each
// composed operation input member.
apply ConcludeExperimentInput$user_agent @notProperty
apply CreateContextInput$user_agent @notProperty
apply CreateDefaultConfigInput$user_agent @notProperty
apply CreateDimensionInput$user_agent @notProperty
apply CreateExperimentGroupRequest$user_agent @notProperty
apply CreateExperimentRequest$user_agent @notProperty
apply CreateFunctionRequest$user_agent @notProperty
apply CreateSecretInput$user_agent @notProperty
apply CreateTypeTemplatesRequest$user_agent @notProperty
apply CreateVariableInput$user_agent @notProperty
apply CreateWebhookInput$user_agent @notProperty
apply CreateWorkspaceRequest$user_agent @notProperty
apply DeleteContextInput$user_agent @notProperty
apply DeleteDefaultConfigInput$user_agent @notProperty
apply DeleteDimensionInput$user_agent @notProperty
apply DeleteExperimentGroupInput$user_agent @notProperty
apply DeleteFunctionInput$user_agent @notProperty
apply DeleteSecretInput$user_agent @notProperty
apply DeleteTypeTemplatesInput$user_agent @notProperty
apply DeleteVariableInput$user_agent @notProperty
apply DeleteWebhookInput$user_agent @notProperty
apply DiscardExperimentInput$user_agent @notProperty
apply GetConfigInput$user_agent @notProperty
apply GetConfigJsonInput$user_agent @notProperty
apply GetConfigTomlInput$user_agent @notProperty
apply GetContextInput$user_agent @notProperty
apply GetDefaultConfigInput$user_agent @notProperty
apply GetDetailedResolvedConfigInput$user_agent @notProperty
apply GetDimensionInput$user_agent @notProperty
apply GetExperimentConfigInput$user_agent @notProperty
apply GetExperimentGroupInput$user_agent @notProperty
apply GetExperimentInput$user_agent @notProperty
apply GetFunctionInput$user_agent @notProperty
apply GetResolvedConfigExplanationInput$user_agent @notProperty
apply GetResolvedConfigInput$user_agent @notProperty
apply GetResolvedConfigWithIdentifierInput$user_agent @notProperty
apply GetSecretInput$user_agent @notProperty
apply GetTypeTemplateInput$user_agent @notProperty
apply GetVariableInput$user_agent @notProperty
apply GetVersionInput$user_agent @notProperty
apply GetWebhookInput$user_agent @notProperty
apply GetWorkspaceInput$user_agent @notProperty
apply ModifyMembersToGroupRequest$user_agent @notProperty
apply MoveContextInput$user_agent @notProperty
apply PauseExperimentInput$user_agent @notProperty
apply PublishInput$user_agent @notProperty
apply RampExperimentInput$user_agent @notProperty
apply ResumeExperimentInput$user_agent @notProperty
apply TestInput$user_agent @notProperty
apply UpdateDefaultConfigInput$user_agent @notProperty
apply UpdateDimensionInput$user_agent @notProperty
apply UpdateExperimentGroupRequest$user_agent @notProperty
apply UpdateFunctionRequest$user_agent @notProperty
apply UpdateOverrideRequest$user_agent @notProperty
apply UpdateSecretInput$user_agent @notProperty
apply UpdateTypeTemplatesRequest$user_agent @notProperty
apply UpdateVariableInput$user_agent @notProperty
apply UpdateWebhookInput$user_agent @notProperty
apply UpdateWorkspaceRequest$user_agent @notProperty
apply WorkspaceSelectorRequest$user_agent @notProperty

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

@documentation("Returned when a workspace write operation cannot proceed because another write operation currently holds the workspace lock.")
@httpError(409)
@error("client")
structure WorkspaceLockConflict {
    @required
    message: String

    @required
    lock: WorkspaceLock
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

@mixin
operation WorkspaceWriteOperation {
    errors: [
        WorkspaceLockConflict
    ]
}

@timestampFormat("date-time")
timestamp DateTime

@timestampFormat("http-date")
timestamp HttpDate
