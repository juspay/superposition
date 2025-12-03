$version: "2.0"

namespace io.superposition

@documentation("Secrets are encrypted key-value pairs stored at the workspace level. Secret values are encrypted with workspace-specific encryption keys and support key rotation.")
resource Secret {
    identifiers: {
        workspace_id: String
        org_id: String
        name: String
    }
    properties: {
        encrypted_value: String
        key_version: Integer
        description: String
        change_reason: String
        created_by: String
        created_at: DateTime
        last_modified_by: String
        last_modified_at: DateTime
    }
    list: ListSecrets
    update: UpdateSecret
    read: GetSecret
    delete: DeleteSecret
    operations: [
        CreateSecret
        RotateWorkspaceKey
    ]
}

@documentation("Response structure for secret operations. The value is always masked except during creation.")
structure SecretResponse for Secret {
    @required
    $name

    @required
    @documentation("Masked value shown as '••••••••' for security. Only shown unmasked during creation.")
    value: String

    @required
    $key_version

    @required
    $description

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

@documentation("Response structure for secret creation. The actual unmasked value is returned only once.")
structure SecretUnmaskedResponse for Secret {
    @required
    $name

    @required
    @documentation("Actual unmasked secret value. This is only returned during creation and will not be shown again.")
    value: String

    @required
    $key_version

    @required
    $description

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

list SecretList {
    member: SecretResponse
}

enum SecretSortOn {
    NAME = "name"
    CREATED_AT = "created_at"
    LAST_MODIFIED_AT = "last_modified_at"
}

@documentation("Status response for encryption key rotation operation.")
structure KeyRotationStatus {
    @required
    @documentation("Whether the key rotation completed successfully.")
    success: Boolean

    @required
    @documentation("Number of secrets that were re-encrypted with the new key.")
    secrets_re_encrypted: Long

    @required
    @documentation("Timestamp when the rotation was completed.")
    rotation_timestamp: DateTime

    @required
    @documentation("Human-readable message about the rotation operation.")
    message: String
}

@documentation("Creates a new encrypted secret with the specified name and value. The secret is encrypted with the workspace's current encryption key. Returns the unmasked value once - it will be masked in all subsequent operations.")
@http(method: "POST", uri: "/secrets")
@tags(["Secrets"])
operation CreateSecret {
    input := for Secret with [WorkspaceMixin] {
        @required
        name: String

        @required
        @documentation("Plaintext value to be encrypted and stored.")
        value: String

        @required
        description: String

        @required
        change_reason: String
    }

    output: SecretUnmaskedResponse
}

@documentation("Updates an existing secret's value or description. The value is re-encrypted with the current workspace encryption key. Returns masked value.")
@idempotent
@http(method: "PATCH", uri: "/secrets/{name}")
@tags(["Secrets"])
operation UpdateSecret with [GetOperation] {
    input := for Secret with [WorkspaceMixin] {
        @httpLabel
        @required
        $name

        @documentation("New plaintext value to encrypt and store. If provided, will be encrypted with current key.")
        value: String

        description: String

        @required
        change_reason: String
    }

    output: SecretResponse
}

@documentation("Retrieves a paginated list of all secrets in the workspace with optional filtering and sorting. All secret values are masked.")
@readonly
@http(method: "GET", uri: "/secrets")
@tags(["Secrets"])
operation ListSecrets {
    input := with [PaginationParams, WorkspaceMixin] {
        @httpQuery("name")
        @documentation("Filter by secret name.")
        @notProperty
        name: String

        @httpQuery("created_by")
        @documentation("Filter by the user who created the secret.")
        created_by: String

        @httpQuery("last_modified_by")
        @documentation("Filter by the user who last modified the secret.")
        last_modified_by: String

        @httpQuery("sort_on")
        @documentation("Field to sort the results by.")
        @notProperty
        sort_on: SecretSortOn

        @httpQuery("sort_by")
        @documentation("Sort order (ascending or descending).")
        @notProperty
        sort_by: SortBy
    }

    output := with [PaginatedResponse] {
        @required
        data: SecretList
    }
}

@documentation("Retrieves detailed information about a specific secret by its name. The value is masked for security.")
@readonly
@http(method: "GET", uri: "/secrets/{name}")
@tags(["Secrets"])
operation GetSecret with [GetOperation] {
    input := for Secret with [WorkspaceMixin] {
        @httpLabel
        @required
        $name
    }

    output: SecretResponse
}

@documentation("Permanently deletes a secret from the workspace. The encrypted value is removed and cannot be recovered.")
@idempotent
@http(method: "DELETE", uri: "/secrets/{name}")
@tags(["Secrets"])
operation DeleteSecret with [GetOperation] {
    input := for Secret with [WorkspaceMixin] {
        @httpLabel
        @required
        $name
    }

    output: SecretResponse
}

@documentation("Rotates the workspace encryption key. Generates a new encryption key, re-encrypts all secrets with the new key, and stores the old key as previous_encryption_key for graceful migration. This is a critical operation that should be done during low-traffic periods.")
@http(method: "POST", uri: "/secrets/rotate-key")
@tags(["Secrets"])
operation RotateWorkspaceKey {
    input := with [WorkspaceMixin] {
        @required
        @documentation("Reason for rotating the encryption key (e.g., 'Scheduled rotation', 'Security incident').")
        change_reason: String
    }

    output: KeyRotationStatus
}
