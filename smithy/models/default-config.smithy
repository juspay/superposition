// Smithy version, not API version.
$version: "2.0"

namespace io.superposition

resource DefaultConfig {
    identifiers: {
        key: String
        workspace_id: String
        org_id: String
    }
    properties: {
        value: Document
        schema: Object
        value_validation_function_name: String
        description: String
        change_reason: String
        created_at: DateTime
        created_by: String
        last_modified_at: DateTime
        last_modified_by: String
        value_compute_function_name: String
    }
    update: UpdateDefaultConfig
    delete: DeleteDefaultConfig
    read: GetDefaultConfig
    list: ListDefaultConfigs
    operations: [
        CreateDefaultConfig
        BulkDefaultConfigOperation
    ]
}

// Using in input for create API.
@mixin
structure DefaultConfigMixin for DefaultConfig {
    @required
    $key

    @required
    $value

    @required
    $schema

    @required
    $description

    @required
    $change_reason

    $value_validation_function_name

    $value_compute_function_name
}

structure DefaultConfigResponse for DefaultConfig with [DefaultConfigMixin] {
    @required
    $created_at

    @required
    $created_by

    @required
    $last_modified_at

    @required
    $last_modified_by
}

list ListDefaultConfigOut {
    member: DefaultConfigResponse
}

@documentation("Retrieves a specific default config entry by its key, including its value, schema, function mappings, and metadata.")
@readonly
@http(method: "GET", uri: "/default-config/{key}")
@tags(["Default Configuration"])
operation GetDefaultConfig with [GetOperation] {
    input := for DefaultConfig with [WorkspaceMixin] {
        @httpLabel
        @required
        $key
    }

    output: DefaultConfigResponse
}

// Operations
@documentation("Creates a new default config entry with specified key, value, schema, and metadata. Default configs serve as fallback values when no specific context matches.")
@http(method: "POST", uri: "/default-config")
@tags(["Default Configuration"])
operation CreateDefaultConfig with [WebhookOperation, WorkspaceWriteOperation] {
    input := with [DefaultConfigMixin, WorkspaceMixin] {}
    output: DefaultConfigResponse
}

structure DefaultConfigBulkItem with [DefaultConfigMixin] {}

@length(min: 1)
list DefaultConfigBulkOperationList {
    member: DefaultConfigAction
}

structure DefaultConfigUpdateBulkRequest {
    @required
    key: String

    @required
    request: DefaultConfigUpdateBulkPayload
}

structure DefaultConfigUpdateBulkPayload for DefaultConfig {
    $value
    $schema
    $value_validation_function_name
    $description

    @required
    $change_reason

    $value_compute_function_name
}

union DefaultConfigAction {
    CREATE: DefaultConfigBulkItem
    UPDATE: DefaultConfigUpdateBulkRequest
    DELETE: String
}

union DefaultConfigBulkOperationOutput {
    CREATE: DefaultConfigResponse
    UPDATE: DefaultConfigResponse
    DELETE: String
}

list DefaultConfigBulkOperationOutputList {
    member: DefaultConfigBulkOperationOutput
}

@documentation("Executes multiple default config create, update, and delete operations atomically.")
@idempotent
@http(method: "PUT", uri: "/default-config/bulk-operations")
@tags(["Default Configuration"])
operation BulkDefaultConfigOperation with [WebhookOperation, WorkspaceWriteOperation] {
    input := with [WorkspaceMixin] {
        @required
        operations: DefaultConfigBulkOperationList
    }

    output := {
        @required
        output: DefaultConfigBulkOperationOutputList
    }
}

@documentation("Retrieves a paginated list of all default config entries in the workspace, including their values, schemas, and metadata.")
@readonly
@http(method: "GET", uri: "/default-config")
@tags(["Default Configuration"])
operation ListDefaultConfigs {
    input := with [WorkspaceMixin, PaginationParams] {
        @httpQuery("name")
        @notProperty
        name: String
    }

    output := with [PaginatedResponse] {
        @required
        data: ListDefaultConfigOut
    }
}

@documentation("Updates an existing default config entry. Allows modification of value, schema, function mappings, and description while preserving the key identifier.")
@idempotent
@http(method: "PATCH", uri: "/default-config/{key}")
@tags(["Default Configuration"])
operation UpdateDefaultConfig with [GetOperation, WebhookOperation, WorkspaceWriteOperation] {
    input := for DefaultConfig with [WorkspaceMixin] {
        @httpLabel
        @required
        $key

        @required
        $change_reason

        $value

        $schema

        @documentation("To unset the function name, pass \"null\" string.")
        $value_validation_function_name

        $description

        @documentation("To unset the function name, pass \"null\" string.")
        $value_compute_function_name
    }

    output: DefaultConfigResponse
}

@documentation("Permanently removes a default config entry from the workspace. This operation cannot be performed if it affects config resolution for contexts that rely on this fallback value.")
@idempotent
@http(method: "DELETE", uri: "/default-config/{key}", code: 204)
@tags(["Default Configuration"])
operation DeleteDefaultConfig with [GetOperation, WebhookOperation, WorkspaceWriteOperation] {
    input := for DefaultConfig with [WorkspaceMixin] {
        @httpLabel
        @required
        $key
    }
}
