// Smithy version, not API version.
$version: "2.0"

namespace io.superposition

resource DefaultConfig {
    identifiers: {
        key: String
    }
    properties: {
        value: Document
        schema: Document
        function_name: String
        description: String
        change_reason: String
        created_at: Timestamp
        created_by: String
        last_modified_at: Timestamp
        last_modified_by: String
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
@http(method: "POST", uri: "/default-config")
operation CreateDefaultConfig {
    input := with [DefaultConfigMixin] {}
    output: DefaultConfigFull
}

@readonly
@http(method: "GET", uri: "/default-config")
operation ListDefaultConfigs {
    input := with [PaginationParams] {}
    output := with [PaginatedResponse] {
        data: ListDefaultConfigOut
    }
    errors: [
        ResourceNotFound
    ]
}

@idempotent
@http(method: "PUT", uri: "/default-config/{key}")
operation UpdateDefaultConfig {
    input := for DefaultConfig {
        @httpLabel
        @required
        $key

        @required
        $change_reason

        $value

        $schema

        $function_name

        $description
    }

    output: DefaultConfigFull

    errors: [
        ResourceNotFound
    ]
}

@idempotent
@http(method: "DELETE", uri: "/default-config/{key}")
operation DeleteDefaultConfig {
    input := for DefaultConfig {
        @httpLabel
        @required
        $key
    }

    output := {}

    errors: [
        ResourceNotFound
    ]
}
