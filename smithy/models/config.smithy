$version: "2.0"

namespace io.superposition

resource Config {
    identifiers: {
        workspace_id: String
        org_id: String
    }
    properties: {
        contexts: ContextList
        overrides: OverridesMap
        default_configs: Object
        dimensions: DimensionData
        version: String
        last_modified: DateTime
    }
    operations: [
        GetConfig
        GetResolvedConfig
        GetDetailedResolvedConfig
        GetResolvedConfigExplanation
        GetResolvedConfigWithIdentifier
        GetConfigToml
        GetConfigJson
    ]
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

    value_compute_function_name: String
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

structure ConfigData {
    @required
    contexts: ContextList

    @required
    overrides: OverridesMap

    @required
    default_configs: Object

    @required
    dimensions: DimensionData
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

        @documentation("While using this, 304 response is treated as error, which needs to be handled separately by checking the response code of the http response. This is required to make sure that clients can cache the response and avoid unnecessary calls when there are no updates.")
        @httpHeader("if-modified-since")
        @notProperty
        if_modified_since: DateTime

        @notProperty
        context: ContextMap
    }

    output := for Config {
        @required
        $contexts

        @required
        $overrides

        @required
        $default_configs

        @required
        $dimensions

        @httpHeader("x-config-version")
        @required
        $version

        @httpHeader("last-modified")
        @required
        $last_modified
    }
}

@documentation("Retrieves the full config in TOML format, including default configs with schemas, dimensions, and overrides. This endpoint is optimized for clients that prefer TOML format for configuration management.")
@readonly
@http(method: "POST", uri: "/config/toml")
@tags(["Configuration Management"])
operation GetConfigToml {
    input := with [WorkspaceMixin] {
        @documentation("While using this, 304 response is treated as error, which needs to be handled separately by checking the response code of the http response. This is required to make sure that clients can cache the response and avoid unnecessary calls when there are no updates.")
        @httpHeader("if-modified-since")
        @notProperty
        if_modified_since: DateTime
    }

    output := for Config {
        @httpPayload
        @required
        @notProperty
        toml_config: String

        @httpHeader("last-modified")
        $last_modified
    }
}

@documentation("Retrieves the full config in JSON format, including default configs with schemas, dimensions, and overrides. This endpoint is optimized for clients that prefer JSON format for configuration management.")
@readonly
@http(method: "POST", uri: "/config/json")
@tags(["Configuration Management"])
operation GetConfigJson {
    input := with [WorkspaceMixin] {
        @documentation("While using this, 304 response is treated as error, which needs to be handled separately by checking the response code of the http response. This is required to make sure that clients can cache the response and avoid unnecessary calls when there are no updates.")
        @httpHeader("if-modified-since")
        @notProperty
        if_modified_since: DateTime
    }

    output := for Config {
        @httpPayload
        @required
        @notProperty
        json_config: String

        @httpHeader("last-modified")
        $last_modified
    }
}

@documentation("How an import applies file entities to the workspace.")
enum ImportStrategy {
    @documentation("Create entities that are present in the file but missing from the workspace. Existing entities are skipped. Nothing is deleted.")
    CREATE_ONLY = "create_only"

    @documentation("Create missing entities and update existing entities from the file. Entities absent from the file are left untouched.")
    UPSERT = "upsert"

    @documentation("Mirror the file: create missing entities, update existing entities, and delete dimensions, default-configs and contexts that are absent from it.")
    REPLACE = "replace"
}

@documentation("How an import reacts when an individual entity fails to apply.")
enum ImportOnError {
    @documentation("Roll the whole import back on the first error.")
    ABORT = "abort"

    @documentation("Apply everything that is valid and report per-entity errors.")
    CONTINUE = "continue"
}

@documentation("Per-entity outcome counts for an import.")
structure ImportEntityReport {
    @required
    created: Integer

    @required
    updated: Integer

    @required
    skipped: Integer

    @required
    deleted: Integer

    errors: ImportErrorList
}

list ImportErrorList {
    member: ImportErrorItem
}

structure ImportErrorItem {
    @required
    id: String

    @required
    message: String
}

@documentation("Summary of what an import created, updated, skipped or deleted.")
structure ImportConfigOutput {
    @required
    @notProperty
    strategy: String

    @required
    @notProperty
    dry_run: Boolean

    @notProperty
    config_version: String

    @required
    @notProperty
    dimensions: ImportEntityReport

    @required
    @notProperty
    default_configs: ImportEntityReport

    @required
    @notProperty
    contexts: ImportEntityReport
}

@documentation("Imports a full config from a TOML document, persisting dimensions, default-configs and contexts in a single transaction after validating the document.")
@http(method: "POST", uri: "/config/toml/import")
@tags(["Configuration Management"])
operation ImportConfigToml {
    input := with [WorkspaceMixin] {
        @documentation("How the import applies file entities to the workspace. Defaults to upsert.")
        @httpHeader("x-import-strategy")
        @notProperty
        strategy: ImportStrategy

        @documentation("Whether to abort (default) or continue on per-entity errors.")
        @httpHeader("x-import-on-error")
        @notProperty
        on_error: ImportOnError

        @documentation("When true, validates and summarises the import without persisting anything. Defaults to false.")
        @httpHeader("x-import-dry-run")
        @notProperty
        dry_run: Boolean

        @httpHeader("x-config-tags")
        @notProperty
        config_tags: String

        @httpPayload
        @required
        @notProperty
        toml_config: String
    }

    output: ImportConfigOutput
}

@documentation("Imports a full config from a JSON document, persisting dimensions, default-configs and contexts in a single transaction after validating the document.")
@http(method: "POST", uri: "/config/json/import")
@tags(["Configuration Management"])
operation ImportConfigJson {
    input := with [WorkspaceMixin] {
        @documentation("How the import applies file entities to the workspace. Defaults to upsert.")
        @httpHeader("x-import-strategy")
        @notProperty
        strategy: ImportStrategy

        @documentation("Whether to abort (default) or continue on per-entity errors.")
        @httpHeader("x-import-on-error")
        @notProperty
        on_error: ImportOnError

        @documentation("When true, validates and summarises the import without persisting anything. Defaults to false.")
        @httpHeader("x-import-dry-run")
        @notProperty
        dry_run: Boolean

        @httpHeader("x-config-tags")
        @notProperty
        config_tags: String

        @httpPayload
        @required
        @notProperty
        json_config: String
    }

    output: ImportConfigOutput
}

enum MergeStrategy {
    MERGE
    REPLACE
}

map DetailedResolvedConfig {
    key: String
    value: DetailedResolvedConfigValue
}

structure DetailedResolvedConfigValue {
    @required
    description: String

    type: Document

    @required
    value: Document
}

structure ResolveExplanationTimelineItem {
    @required
    context_id: String

    @required
    condition: Condition

    @required
    override_id: String

    @required
    value_before: Document

    @required
    value_after: Document
}

list ResolveExplanationTimeline {
    member: ResolveExplanationTimelineItem
}

structure ResolveExplanation {
    @required
    key: String

    @required
    timeline: ResolveExplanationTimeline
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
        @notProperty
        @required
        config: Document

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

@documentation("Resolves config values and returns each key with default-config metadata.")
@http(method: "POST", uri: "/config/resolve/detailed")
@tags(["Configuration Management"])
operation GetDetailedResolvedConfig {
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
        @notProperty
        config: Document

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

@documentation("Explains how matching contexts affect a single resolved config key.")
@http(method: "POST", uri: "/config/resolve/explain/{key}")
@tags(["Configuration Management"])
operation GetResolvedConfigExplanation {
    input := with [WorkspaceMixin] {
        @httpLabel
        @notProperty
        @required
        key: String

        @httpQuery("version")
        @notProperty
        version: String

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
        @notProperty
        explanation: ResolveExplanation

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
        config: ConfigData
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

@documentation("Resolves and merges config values based on context conditions and identifier, applying overrides and merge strategies to produce the final configuration.")
@http(method: "POST", uri: "/resolve")
@tags(["Configuration Management"])
operation GetResolvedConfigWithIdentifier {
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

        @httpQuery("identifier")
        @notProperty
        identifier: String
    }

    output := for Config {
        @httpPayload
        @notProperty
        @required
        config: Document

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
