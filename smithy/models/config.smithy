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
    ]
}

@http(method: "GET", uri:"/config/fast")
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
@http(method: "GET", uri:"/config/versions")
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

// TODO Model `get_config` & `get_resolved_config`
