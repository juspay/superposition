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
    put: CreateDefaultConfig
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
@idempotent
@http(method: "PUT", uri: "/default-config")
operation CreateDefaultConfig {
    input := for DefaultConfig {
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

    output := for DefaultConfig {
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

        @required
        $created_at

        @required
        $created_by

        @required
        $last_modified_at

        @required
        $last_modified_by
    }

    errors: [
        InternalServerError
    ]
}

@readonly
@http(method: "GET", uri: "/default-config/{key}")
operation GetDefaultConfig {
    input := for DefaultConfig {
        @httpLabel
        @required
        $key
    }

    output := for DefaultConfig {
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

        @required
        $created_at

        @required
        $created_by

        @required
        $last_modified_at

        @required
        $last_modified_by
    }

    errors: [
        DefaultConfigNotFound
    ]
}
