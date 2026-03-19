# Superposition MCP Server — Implementation Plan

## Overview

Build a new `superposition_mcp` crate that exposes Superposition's API as MCP (Model Context Protocol) tools. The server uses `rmcp` (the official Rust MCP SDK) and delegates all API calls to the `superposition_sdk` client. It supports stdio transport (for Claude Desktop, Cursor, etc.) and can optionally be mounted as an SSE endpoint on the main `superposition` actix-web app.

## Architecture

```
┌─────────────────────────────────────────────┐
│  MCP Client (Claude, Cursor, etc.)          │
│  ↕ stdio / SSE                              │
├─────────────────────────────────────────────┤
│  superposition_mcp crate                    │
│  ┌─────────────────────────────────────┐    │
│  │ SuperpositionMcpServer struct       │    │
│  │  - superposition_sdk::Client        │    │
│  │  - workspace_id, org_id (config)    │    │
│  │                                     │    │
│  │ #[tool_router] impl                 │    │
│  │  - tools derived from smithy ops    │    │
│  └─────────────────────────────────────┘    │
├─────────────────────────────────────────────┤
│  superposition_sdk (HTTP client)            │
│  → Superposition REST API                   │
└─────────────────────────────────────────────┘
```

## Crate: `crates/superposition_mcp`

### Cargo.toml

```toml
[package]
name = "superposition_mcp"
version.workspace = true
edition.workspace = true
rust-version.workspace = true

[dependencies]
rmcp = { version = "1.2.0", features = ["server", "transport-sse-server"] }
superposition_sdk = { workspace = true }
serde = { workspace = true }
serde_json = { workspace = true }
tokio = { workspace = true }
schemars = "0.8"
tracing = { workspace = true }
anyhow = { workspace = true }

[features]
default = ["stdio"]
stdio = []
sse = ["rmcp/transport-sse-server"]
```

### Tools Mapping (Smithy → MCP Tools)

Each Smithy operation becomes an MCP tool. The tool names follow a `resource.operation` convention. Arguments are derived from the Smithy input structures. The `workspace_id` and `org_id` are configured once on the server and injected into every SDK call (not exposed as tool arguments).

#### Configuration Management (7 tools)
| MCP Tool Name | Smithy Operation | Description |
|---|---|---|
| `config.get` | GetConfig | Get config with context evaluation |
| `config.resolve` | GetResolvedConfig | Resolve config for a context |
| `config.get_versions` | ListVersions | List config versions |
| `config.get_version` | GetVersion | Get specific config version |
| `config.get_toml` | GetConfigToml | Get full config in TOML |
| `config.get_json` | GetConfigJson | Get full config in JSON |
| `config.get_fast` | GetConfigFast | Get raw config fast |

#### Default Config (5 tools)
| MCP Tool Name | Smithy Operation | Description |
|---|---|---|
| `default_config.create` | CreateDefaultConfig | Create a default config key |
| `default_config.get` | GetDefaultConfig | Get a default config by key |
| `default_config.list` | ListDefaultConfigs | List all default configs |
| `default_config.update` | UpdateDefaultConfig | Update a default config |
| `default_config.delete` | DeleteDefaultConfig | Delete a default config |

#### Dimensions (5 tools)
| MCP Tool Name | Smithy Operation | Description |
|---|---|---|
| `dimension.create` | CreateDimension | Create a dimension |
| `dimension.get` | GetDimension | Get a dimension |
| `dimension.list` | ListDimensions | List dimensions |
| `dimension.update` | UpdateDimension | Update a dimension |
| `dimension.delete` | DeleteDimension | Delete a dimension |

#### Contexts (9 tools)
| MCP Tool Name | Smithy Operation | Description |
|---|---|---|
| `context.create` | CreateContext | Create a context with overrides |
| `context.get` | GetContext | Get context by ID |
| `context.list` | ListContexts | List contexts |
| `context.delete` | DeleteContext | Delete a context |
| `context.update_override` | UpdateOverride | Update context overrides |
| `context.move` | MoveContext | Move context to new condition |
| `context.get_by_condition` | GetContextFromCondition | Get context from condition |
| `context.bulk_operation` | BulkOperation | Bulk context operations |
| `context.weight_recompute` | WeightRecompute | Recompute context weights |

#### Experiments (10 tools)
| MCP Tool Name | Smithy Operation | Description |
|---|---|---|
| `experiment.create` | CreateExperiment | Create an experiment |
| `experiment.get` | GetExperiment | Get experiment details |
| `experiment.list` | ListExperiment | List experiments |
| `experiment.update_overrides` | UpdateOverridesExperiment | Update experiment variant overrides |
| `experiment.conclude` | ConcludeExperiment | Conclude with a winner |
| `experiment.discard` | DiscardExperiment | Discard experiment |
| `experiment.ramp` | RampExperiment | Adjust traffic percentage |
| `experiment.pause` | PauseExperiment | Pause experiment |
| `experiment.resume` | ResumeExperiment | Resume experiment |
| `experiment.applicable_variants` | ApplicableVariants | Get applicable variants for context |

#### Experiment Groups (7 tools)
| MCP Tool Name | Smithy Operation | Description |
|---|---|---|
| `experiment_group.create` | CreateExperimentGroup | Create experiment group |
| `experiment_group.get` | GetExperimentGroup | Get experiment group |
| `experiment_group.list` | ListExperimentGroups | List experiment groups |
| `experiment_group.update` | UpdateExperimentGroup | Update experiment group |
| `experiment_group.delete` | DeleteExperimentGroup | Delete experiment group |
| `experiment_group.add_members` | AddMembersToGroup | Add experiments to group |
| `experiment_group.remove_members` | RemoveMembersFromGroup | Remove experiments from group |

#### Functions (7 tools)
| MCP Tool Name | Smithy Operation | Description |
|---|---|---|
| `function.create` | CreateFunction | Create custom function |
| `function.get` | GetFunction | Get function details |
| `function.list` | ListFunction | List functions |
| `function.update` | UpdateFunction | Update function draft |
| `function.delete` | DeleteFunction | Delete function |
| `function.test` | Test | Test function execution |
| `function.publish` | Publish | Publish draft to active |

#### Type Templates (5 tools)
| MCP Tool Name | Smithy Operation | Description |
|---|---|---|
| `type_template.create` | CreateTypeTemplates | Create type template |
| `type_template.get` | GetTypeTemplate | Get type template |
| `type_template.list` | GetTypeTemplatesList | List type templates |
| `type_template.update` | UpdateTypeTemplates | Update type template |
| `type_template.delete` | DeleteTypeTemplates | Delete type template |

#### Organisations (4 tools)
| MCP Tool Name | Smithy Operation | Description |
|---|---|---|
| `organisation.create` | CreateOrganisation | Create organisation |
| `organisation.get` | GetOrganisation | Get organisation |
| `organisation.list` | ListOrganisation | List organisations |
| `organisation.update` | UpdateOrganisation | Update organisation |

#### Workspaces (4 tools)
| MCP Tool Name | Smithy Operation | Description |
|---|---|---|
| `workspace.create` | CreateWorkspace | Create workspace |
| `workspace.get` | GetWorkspace | Get workspace |
| `workspace.list` | ListWorkspace | List workspaces |
| `workspace.update` | UpdateWorkspace | Update workspace |

#### Audit Logs (1 tool)
| MCP Tool Name | Smithy Operation | Description |
|---|---|---|
| `audit_log.list` | ListAuditLogs | List audit logs |

#### Variables (5 tools)
| MCP Tool Name | Smithy Operation | Description |
|---|---|---|
| `variable.create` | CreateVariable | Create variable |
| `variable.get` | GetVariable | Get variable |
| `variable.list` | ListVariables | List variables |
| `variable.update` | UpdateVariable | Update variable |
| `variable.delete` | DeleteVariable | Delete variable |

#### Webhooks (6 tools)
| MCP Tool Name | Smithy Operation | Description |
|---|---|---|
| `webhook.create` | CreateWebhook | Create webhook |
| `webhook.get` | GetWebhook | Get webhook |
| `webhook.list` | ListWebhook | List webhooks |
| `webhook.update` | UpdateWebhook | Update webhook |
| `webhook.delete` | DeleteWebhook | Delete webhook |
| `webhook.get_by_event` | GetWebhookByEvent | Get webhook by event |

#### Secrets (5 tools)
| MCP Tool Name | Smithy Operation | Description |
|---|---|---|
| `secret.create` | CreateSecret | Create encrypted secret |
| `secret.get` | GetSecret | Get secret metadata |
| `secret.list` | ListSecrets | List secrets |
| `secret.update` | UpdateSecret | Update secret |
| `secret.delete` | DeleteSecret | Delete secret |

**Total: ~80 tools** covering all Smithy operations.

### File Structure

```
crates/superposition_mcp/
├── Cargo.toml
├── src/
│   ├── lib.rs              — Public API: SuperpositionMcpServer, config
│   ├── server.rs           — Server struct, #[tool_router] impl
│   ├── config.rs           — McpServerConfig (endpoint_url, auth, workspace, org)
│   ├── tools/
│   │   ├── mod.rs
│   │   ├── config.rs       — Config tools
│   │   ├── default_config.rs
│   │   ├── dimension.rs
│   │   ├── context.rs
│   │   ├── experiment.rs
│   │   ├── experiment_group.rs
│   │   ├── function.rs
│   │   ├── type_template.rs
│   │   ├── organisation.rs
│   │   ├── workspace.rs
│   │   ├── audit_log.rs
│   │   ├── variable.rs
│   │   ├── webhook.rs
│   │   └── secret.rs
│   ├── types.rs            — Schemars-annotated parameter structs for each tool
│   └── main.rs             — Standalone stdio binary entry point
```

### Implementation Pattern

Each tool module defines parameter structs with `schemars::JsonSchema` + `serde::Deserialize` derives, and an async handler function that:

1. Receives `Parameters<ToolArgs>` (rmcp extracts and validates JSON)
2. Calls the `superposition_sdk::Client` fluent builder
3. Serializes the response to JSON
4. Returns `CallToolResult` with text content

Example (pseudocode):
```rust
#[derive(JsonSchema, Deserialize)]
struct ListContextsParams {
    /// Number of items per page
    count: Option<i32>,
    /// Page number
    page: Option<i32>,
    /// Sort field
    sort_on: Option<String>,
    /// Sort order (asc/desc)
    sort_by: Option<String>,
}

#[tool(name = "context.list", description = "List contexts with filtering and pagination")]
async fn list_contexts(&self, #[tool(params)] args: ListContextsParams) -> Result<CallToolResult, McpError> {
    let mut req = self.client.list_contexts()
        .workspace_id(&self.config.workspace_id)
        .org_id(&self.config.org_id);
    if let Some(count) = args.count { req = req.count(count); }
    if let Some(page) = args.page { req = req.page(page); }
    // ...
    let resp = req.send().await.map_err(|e| McpError::internal(e.to_string()))?;
    let json = serde_json::to_string_pretty(&resp).map_err(|e| McpError::internal(e.to_string()))?;
    Ok(CallToolResult::success(vec![Content::text(json)]))
}
```

### Configuration

The MCP server is configured via environment variables or a config struct:

| Env Var | Description | Required |
|---|---|---|
| `SUPERPOSITION_URL` | Base URL of Superposition API | Yes |
| `SUPERPOSITION_WORKSPACE` | Default workspace ID | Yes |
| `SUPERPOSITION_ORG_ID` | Default org ID | Yes |
| `SUPERPOSITION_AUTH_TOKEN` | Bearer token for auth | No |

### Standalone Binary (stdio)

`main.rs` reads config from env, constructs the SDK client and MCP server, and runs over stdio:

```rust
#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let config = McpServerConfig::from_env()?;
    let server = SuperpositionMcpServer::new(config);
    let service = server.serve(tokio::io::stdin(), tokio::io::stdout()).await?;
    service.waiting().await?;
    Ok(())
}
```

## Integration with Main Superposition App

### Feature Flag

Add an optional `mcp` feature to `crates/superposition/Cargo.toml`:

```toml
[features]
mcp = ["superposition_mcp"]

[dependencies]
superposition_mcp = { path = "../superposition_mcp", optional = true }
```

### SSE Mount

When the `mcp` feature is enabled, mount an SSE endpoint in `main.rs`:

```rust
#[cfg(feature = "mcp")]
{
    // Mount MCP SSE endpoint at /mcp
    let mcp_config = superposition_mcp::McpServerConfig::from_env_with_defaults(&app_state);
    app = app.service(
        scope("/mcp")
            .configure(superposition_mcp::configure_sse_routes(mcp_config))
    );
}
```

This allows the MCP server to be reached over HTTP SSE when running as part of the main app, while the standalone binary uses stdio for local tool integrations.

## Workspace Changes

1. Add `"crates/superposition_mcp"` to workspace members in root `Cargo.toml`
2. Add `superposition_mcp` to workspace dependencies

## Implementation Steps

1. **Create crate scaffold** — `Cargo.toml`, `lib.rs`, `config.rs`, `server.rs`
2. **Define parameter types** — `types.rs` with JsonSchema derives for all tool inputs
3. **Implement tool modules** — One file per resource domain, each with `#[tool]` handlers
4. **Wire up `#[tool_router]`** — Central `server.rs` delegates to tool modules
5. **Implement stdio binary** — `main.rs`
6. **Add workspace integration** — Feature flag in `superposition` crate, SSE mount
7. **Test** — Build the crate, verify tool listing with MCP inspector
