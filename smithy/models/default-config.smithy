// Smithy version, not API version.
$version: "2.0"

namespace cac.default_config

use aws.protocols#restJson1

@title("Default Config Service")
@restJson1
service DefaultConfigService {
    version: "2025-03-05"
    resources: [
        DefaultConfig
    ]
    errors: [
        InternalServerError
    ]
}

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
    read: GetDefaultConfig
    post: CreateDefaultConfig
    put: UpdateDefaultConfig
    delete: DeleteDefaultConfig
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

// Errors
@httpError(500)
@error("server")
structure InternalServerError {
    message: String
}

@httpError(404)
@error("client")
structure DefaultConfigNotFound {}

// Operations
@http(method: "POST", uri: "/default-config")
operation CreateDefaultConfig {
    input := with [DefaultConfigMixin] {}
    output: DefaultConfigFull
}

@readonly
@http(method: "GET", uri: "/default-config/{key}")
operation GetDefaultConfig {
    input := for DefaultConfig {
        @httpLabel
        @required
        $key
    }

    output: DefaultConfigFull

    errors: [
        DefaultConfigNotFound
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
        DefaultConfigNotFound
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
        DefaultConfigNotFound
    ]
}
