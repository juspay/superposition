$version: "2.0"

namespace io.superposition

resource Config {
    identifiers: {
        workspace_id: String
        org_id: String
    }
    properties: {
        version: String
        config: Document
        last_modified: DateTime
    }
    operations: [
        GetConfigFast
        GetConfig
        GetResolvedConfig
    ]
}

@documentation("Retrieves the latest config with no processing for high-performance access.")
@http(method: "GET", uri: "/config/fast")
@tags(["Configuration Management"])
operation GetConfigFast {
    input := with [WorkspaceMixin] {}

    output := for Config {
        @httpPayload
        $config

        @httpHeader("x-config-version")
        @notProperty
        $version

        @httpHeader("last-modified")
        $last_modified

        @httpHeader("x-audit-id")
        @notProperty
        audit_id: String
    }
}

@length(max: 1, min: 1)
list OverrideWithKeys {
    member: String
}

structure ContextPartial {
    @required
    id: String

    @required
    condition: Condition

    @required
    priority: Integer

    @required
    weight: Integer

    @required
    override_with_keys: OverrideWithKeys
}

structure DimensionInfo {
    @required
    schema: Object

    @required
    position: Integer

    @required
    dimension_type: DimensionType

    @required
    dependency_graph: DependencyGraph

    autocomplete_function_name: String
}

map DimensionData {
    key: String
    value: DimensionInfo
}

list ContextList {
    member: ContextPartial
}

map OverridesMap {
    key: String
    value: Overrides
}

@documentation("Retrieves config data with context evaluation, including applicable contexts, overrides, and default values based on provided conditions.")
@http(method: "POST", uri: "/config")
@tags(["Configuration Management"])
operation GetConfig {
    input := with [WorkspaceMixin] {
        @httpQuery("prefix")
        @notProperty
        prefix: StringList

        @httpQuery("version")
        @notProperty
        version: String

        @notProperty
        context: ContextMap
    }

    output := for Config {
        @notProperty
        @required
        contexts: ContextList

        @notProperty
        @required
        overrides: OverridesMap

        @notProperty
        @required
        default_configs: Object

        @notProperty
        @required
        dimensions: DimensionData

        @httpHeader("x-config-version")
        @required
        $version

        @httpHeader("last-modified")
        @required
        $last_modified

        @httpHeader("x-audit-id")
        @notProperty
        audit_id: String
    }
}

enum MergeStrategy {
    MERGE
    REPLACE
}

@documentation("Resolves and merges config values based on context conditions, applying overrides and merge strategies to produce the final configuration.")
@http(method: "POST", uri: "/config/resolve")
@tags(["Configuration Management"])
operation GetResolvedConfig {
    input := with [WorkspaceMixin] {
        @httpQuery("prefix")
        @notProperty
        prefix: StringList

        @httpQuery("version")
        @notProperty
        version: String

        @httpQuery("show_reasoning")
        @notProperty
        show_reasoning: Boolean

        @httpHeader("x-merge-strategy")
        @notProperty
        merge_strategy: MergeStrategy

        @httpQuery("context_id")
        @notProperty
        context_id: String

        @httpQuery("resolve_remote")
        @notProperty
        @documentation("Intended for control resolution. If true, evaluates and includes remote cohort-based contexts during config resolution.")
        resolve_remote: Boolean

        @notProperty
        context: ContextMap
    }

    output := for Config {
        @httpPayload
        @required
        $config

        @httpHeader("x-config-version")
        @required
        $version

        @httpHeader("last-modified")
        @required
        $last_modified

        @httpHeader("x-audit-id")
        @notProperty
        audit_id: String
    }
}

resource ConfigVersion {
    identifiers: {
        workspace_id: String
        org_id: String
        id: String
    }
    properties: {
        config_hash: String
        created_at: DateTime
        description: String
        tags: StringList
        config: Document
    }
    read: GetVersion
    list: ListVersions
    operations: []
}

@documentation("Retrieves a specific config version along with its metadata for audit and rollback purposes.")
@readonly
@http(method: "GET", uri: "/version/{id}")
@tags(["Configuration Management"])
operation GetVersion with [GetOperation] {
    input := for ConfigVersion with [WorkspaceMixin] {
        @httpLabel
        @required
        $id
    }

    output: GetVersionResponse
}

structure ListVersionsMember for ConfigVersion {
    @required
    $id

    @required
    $config

    @required
    $created_at

    @required
    $description

    $tags
}

structure GetVersionResponse for ConfigVersion {
    @required
    $id

    @required
    $config

    @required
    $config_hash

    @required
    $created_at

    @required
    $description

    $tags
}

list ListVersionsOut {
    member: ListVersionsMember
}

@documentation("Retrieves a paginated list of config versions with their metadata, hash values, and creation timestamps for audit and rollback purposes.")
@readonly
@http(method: "GET", uri: "/config/versions")
@tags(["Configuration Management"])
operation ListVersions {
    input := with [WorkspaceMixin] {
        @httpQuery("count")
        @documentation("Number of items to be returned in each page.")
        @notProperty
        count: Integer

        @httpQuery("page")
        @documentation("Page number to retrieve, starting from 1.")
        @notProperty
        page: Integer
    }

    output := with [PaginatedResponse] {
        @required
        data: ListVersionsOut
    }
}
