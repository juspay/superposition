$version: "2.0"

namespace io.superposition

resource Webhook {
    identifiers: {
        workspace_id: String
        org_id: String
        name: String
    }
    properties: {
        description: String
        enabled: Boolean
        url: String
        method: HttpMethod
        version: Version
        custom_headers: Object
        events: Events
        max_retries: Integer
        last_triggered_at: DateTime
        change_reason: String
        created_by: String
        created_at: DateTime
        last_modified_by: String
        last_modified_at: DateTime
    }
    list: ListWebhook
    update: UpdateWebhook
    read: GetWebhook
    delete: DeleteWebhook
    operations: [
        CreateWebhook
    ]
    collectionOperations: [
        GetWebhookByEvent
    ]
}

enum HttpMethod {
    GET
    POST
    PUT
    PATCH
    DELETE
    HEAD
}

enum Version {
    V1
}

list Events {
    member: String
}

structure WebhookResponse for Webhook {
    @required
    $name

    @required
    $description

    @required
    $enabled

    @required
    $url

    @required
    $method

    @required
    $version

    $custom_headers

    @required
    $events

    @required
    $max_retries

    $last_triggered_at

    @required
    $change_reason

    @required
    $created_by

    @required
    $created_at

    @required
    $last_modified_by

    @required
    $last_modified_at
}

list WebhookList {
    member: WebhookResponse
}

// Operations
@documentation("Creates a new webhook config to receive HTTP notifications when specified events occur in the system.")
@http(method: "POST", uri: "/webhook")
@tags(["Webhooks"])
operation CreateWebhook {
    input := for Webhook with [WorkspaceMixin] {
        @required
        name: String

        @required
        description: String

        @required
        enabled: Boolean

        @required
        url: String

        @required
        method: HttpMethod

        version: Version

        custom_headers: Object

        @required
        events: Events

        @required
        change_reason: String
    }

    output: WebhookResponse
}

@documentation("Updates an existing webhook config, allowing modification of URL, events, headers, and other webhook properties.")
@idempotent
@http(method: "PATCH", uri: "/webhook/{name}")
@tags(["Webhooks"])
operation UpdateWebhook with [GetOperation] {
    input := for Webhook with [WorkspaceMixin] {
        @httpLabel
        @required
        $name

        description: String

        enabled: Boolean

        url: String

        method: HttpMethod

        version: Version

        custom_headers: Object

        events: Events

        @required
        change_reason: String
    }

    output: WebhookResponse
}

@documentation("Retrieves a paginated list of all webhook configs in the workspace, including their status and config details.")
@readonly
@http(method: "GET", uri: "/webhook")
@tags(["Webhooks"])
operation ListWebhook {
    input := with [PaginationParams, WorkspaceMixin] {}

    output := with [PaginatedResponse] {
        @required
        data: WebhookList
    }
}

@documentation("Retrieves detailed information about a specific webhook config, including its events, headers, and trigger history.")
@readonly
@http(method: "GET", uri: "/webhook/{name}")
@tags(["Webhooks"])
operation GetWebhook with [GetOperation] {
    input := for Webhook with [WorkspaceMixin] {
        @httpLabel
        @required
        $name
    }

    output: WebhookResponse
}

@documentation("Retrieves a webhook configuration based on a specific event type, allowing users to find which webhook is set to trigger for that event.")
@http(method: "GET", uri: "/webhook/event/{event}")
@tags(["Webhooks"])
operation GetWebhookByEvent with [GetOperation] {
    input := with [WorkspaceMixin] {
        @httpLabel
        @required
        @notProperty
        event: String
    }

    output: WebhookResponse
}

@documentation("Permanently removes a webhook config from the workspace, stopping all future event notifications to that endpoint.")
@idempotent
@http(method: "DELETE", uri: "/webhook/{name}", code: 204)
@tags(["Webhooks"])
operation DeleteWebhook with [GetOperation] {
    input := for Webhook with [WorkspaceMixin] {
        @httpLabel
        @required
        $name
    }
}
