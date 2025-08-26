$version: "2.0"

namespace io.superposition

resource Config {
    identifiers: {
        workspace_id: String
        org_id: String
    }
    properties: {
        // FIXME Cannot keep this in identifiers :(
        version: String
        config: Document
        last_modified: DateTime
    }
    operations: [
        ListVersions
        GetConfigFast
        GetConfig
        GetResolvedConfig
    ]
}

@documentation("Retrieves the latest config with no processing for high-performance access.")
@http(method: "GET", uri: "/config/fast")
operation GetConfigFast {
    input := with [WorkspaceMixin] {}

    output := for Config {
        @httpPayload
        $config

        @httpHeader("x-config-version")
        $version

        @httpHeader("last-modified")
        $last_modified

        @httpHeader("x-audit-id")
        @notProperty
        audit_id: String
    }
}

structure ListVersionsMember for Config {
    @required
    id: String

    @required
    $config

    @required
    config_hash: String

    @required
    created_at: DateTime

    @required
    description: String

    tags: StringList
}

list ListVersionsOut {
    member: ListVersionsMember
}

@documentation("Retrieves a paginated list of config versions with their metadata, hash values, and creation timestamps for audit and rollback purposes.")
@readonly
@http(method: "GET", uri: "/config/versions")
operation ListVersions {
    input := with [WorkspaceMixin] {
        @httpQuery("count")
        @notProperty
        count: Integer

        @httpQuery("page")
        @notProperty
        page: Integer
    }

    output := {
        @notProperty
        @required
        total_pages: Integer

        @notProperty
        @required
        total_items: Integer

        @notProperty
        @required
        data: ListVersionsOut
    }
}

@length(max: 1)
list OverrideWithKeys {
    member: String
}

structure ContextPartial {
    id: String
    condition: Condition
    priority: Integer
    weight: Integer
    override_with_keys: OverrideWithKeys
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
operation GetConfig {
    input := with [WorkspaceMixin] {
        @httpQuery("prefix")
        @notProperty
        prefix: String

        @httpQuery("version")
        @notProperty
        version: String

        @notProperty
        context: ContextMap
    }

    output := for Config {
        @notProperty
        contexts: ContextList

        @notProperty
        overrides: OverridesMap

        @notProperty
        default_configs: Object

        @httpHeader("x-config-version")
        $version

        @httpHeader("last-modified")
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
operation GetResolvedConfig {
    input := with [WorkspaceMixin] {
        @httpQuery("prefix")
        @notProperty
        prefix: String

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

        @notProperty
        context: ContextMap
    }

    output := for Config {
        @httpPayload
        $config

        @httpHeader("x-config-version")
        $version

        @httpHeader("last-modified")
        $last_modified

        @httpHeader("x-audit-id")
        @notProperty
        audit_id: String
    }
}
