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
        value: String
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
    ]
}

@documentation("Response structure for secret operations. Secret values are never returned for security.")
structure SecretResponse {
    @required
    name: String

    @required
    description: String

    @required
    change_reason: String

    @required
    created_by: String

    @required
    created_at: DateTime

    @required
    last_modified_by: String

    @required
    last_modified_at: DateTime
}

list SecretList {
    member: SecretResponse
}

enum SecretSortOn {
    NAME = "name"
    CREATED_AT = "created_at"
    LAST_MODIFIED_AT = "last_modified_at"
}

@documentation("Creates a new encrypted secret with the specified name and value. The secret is encrypted with the workspace's current encryption key. Secret values are never returned in responses for security.")
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

    output: SecretResponse
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

@documentation("Secrets are encrypted key-value pairs stored at the workspace level. Secret values are encrypted with workspace-specific encryption keys and support key rotation.")
resource MasterKey {
    identifiers: {}
    properties: {}
    operations: [
        RotateMasterEncryptionKey
    ]
}

@documentation("Rotates the master encryption key across all workspaces")
@idempotent
@http(method: "POST", uri: "/master-encryption-key/rotate")
@tags(["Admin", "Encryption"])
operation RotateMasterEncryptionKey {
    output := {
        @required
        workspaces_rotated: Long

        @required
        total_secrets_re_encrypted: Long
    }
}
