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

list ListVersionsOut {
    member: String
}

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
        total_pages: Integer

        @notProperty
        total_items: Integer

        @notProperty
        data: ListVersionsOut
    }
}

structure ContextPartial {
    id: String
    condition: Condition
    priority: Integer
    weight: Integer
    override_with_keys: StringList
}

list ContextList {
    member: ContextPartial
}

map OverridesMap {
    key: String
    value: Overrides
}

@http(method: "POST", uri: "/config")
operation GetConfig {
    input := with [WorkspaceMixin] {
        @httpQuery("prefix")
        @notProperty
        prefix: String

        @httpQuery("version")
        @notProperty
        version: String

        @httpPayload
        @notProperty
        context: Document
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

@http(method: "POST", uri: "/config/resolved")
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

        @httpQuery("x-merge-strategy")
        @notProperty
        merge_strategy: MergeStrategy

        @httpPayload
        @notProperty
        context: Document
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
