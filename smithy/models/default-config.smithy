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
        schema: Document
        function_name: String
        description: String
        change_reason: String
        created_at: DateTime
        created_by: String
        last_modified_at: DateTime
        last_modified_by: String
        autocomplete_function_name: String
    }
    list: ListDefaultConfigs
    put: UpdateDefaultConfig
    delete: DeleteDefaultConfig
    operations: [
        CreateDefaultConfig
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

    /// Optional
    $function_name

    $autocomplete_function_name
}

structure DefaultConfigFull for DefaultConfig with [DefaultConfigMixin] {
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
    member: DefaultConfigFull
}

// Operations
@documentation("Creates a new default config entry with specified key, value, schema, and metadata. Default configs serve as fallback values when no specific context matches.")
@http(method: "POST", uri: "/default-config")
operation CreateDefaultConfig {
    input := with [DefaultConfigMixin, WorkspaceMixin] {}
    output: DefaultConfigFull
}

@documentation("Retrieves a paginated list of all default config entries in the workspace, including their values, schemas, and metadata.")
@readonly
@http(method: "GET", uri: "/default-config")
operation ListDefaultConfigs {
    input := with [PaginationParams, WorkspaceMixin] {}
    output := with [PaginatedResponse] {
        data: ListDefaultConfigOut
    }
    errors: [
        ResourceNotFound
    ]
}

@documentation("Updates an existing default config entry. Allows modification of value, schema, function mappings, and description while preserving the key identifier.")
@idempotent
@http(method: "PUT", uri: "/default-config/{key}")
operation UpdateDefaultConfig {
    input := for DefaultConfig with [WorkspaceMixin] {
        @httpLabel
        @required
        $key

        @required
        $change_reason

        $value

        $schema

        $function_name

        $description

        $autocomplete_function_name
    }

    output: DefaultConfigFull

    errors: [
        ResourceNotFound
    ]
}

@documentation("Permanently removes a default config entry from the workspace. This operation cannot be performed if it affects config resolution for contexts that rely on this fallback value.")
@idempotent
@http(method: "DELETE", uri: "/default-config/{key}", code: 201)
operation DeleteDefaultConfig {
    input := for DefaultConfig with [WorkspaceMixin] {
        @httpLabel
        @required
        $key
    }

    output := {}

    errors: [
        ResourceNotFound
    ]
}
