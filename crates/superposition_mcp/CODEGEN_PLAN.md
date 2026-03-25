# Smithy-to-MCP Deterministic Code Generator Plan

## Problem

Each MCP tool file manually declares parameter structs, SDK builder calls, and
response formatting that can be derived from the Smithy model. When a new
operation or service is added to Smithy, an engineer must manually write the
corresponding MCP tool code — ~200 lines of boilerplate per resource.

## Approach: Smithy Codegen Plugin

Write a **custom Smithy codegen plugin** (Java/Kotlin) that reads the same
`.smithy` models and emits Rust source files for the MCP crate. This runs
alongside the existing `rust-client-codegen` plugin in `smithy-build.json`.

### Why a Smithy Plugin (vs. Parsing Rust SDK Output)

- Smithy's Java model API gives structured access to operations, shapes,
  traits (`@http`, `@httpQuery`, `@required`, `@documentation`, etc.)
- No fragile regex/AST parsing of generated Rust code
- Runs as part of the existing `smithy build` pipeline
- Same approach used by the 6 existing codegen plugins

## Generated Artifacts

For each Smithy resource (e.g., `Context`, `Dimension`), generate:

### 1. `tools/{resource}.rs` — Parameter Structs + Impl

```
// AUTO-GENERATED — DO NOT EDIT
// Source: smithy/models/context.smithy

use serde::Deserialize;
use schemars::JsonSchema;

/// Params for CreateContext
#[derive(Debug, Deserialize, JsonSchema)]
pub struct CreateContextParams {
    /// The context conditions (required)
    pub context: serde_json::Value,
    /// Override values (required)
    pub r#override: serde_json::Value,
    /// Reason for the change (required)
    pub change_reason: String,
    /// Optional description
    pub description: Option<String>,
    /// Config tags header
    pub config_tags: Option<String>,
}

// ... more param structs for Get, List, Update, Delete ...

impl SuperpositionMcpServer {
    pub async fn create_context_impl(
        &self,
        args: CreateContextParams,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        let ctx_map = json_to_doc_map(args.context).map_err(mcp_err)?;
        let ovr_map = json_to_doc_map(args.r#override).map_err(mcp_err)?;
        let mut put_builder = superposition_sdk::types::ContextPut::builder()
            .set_context(Some(ctx_map))
            .set_override(Some(ovr_map))
            .change_reason(args.change_reason);
        if let Some(d) = args.description {
            put_builder = put_builder.description(d);
        }
        let put = put_builder.build().map_err(mcp_err)?;
        let mut req = self.client
            .create_context()
            .workspace_id(&self.config.workspace_id)
            .org_id(&self.config.org_id)
            .request(put);
        if let Some(tags) = args.config_tags {
            req = req.config_tags(tags);
        }
        let resp = req.send().await.map_err(mcp_err)?;
        let json = serde_json::to_string_pretty(
            &context_to_json!(resp)
        ).map_err(mcp_err)?;
        Ok(CallToolResult::success(vec![Content::text(json)]))
    }
}
```

### 2. `server_tools.rs` — Tool Registration

```
// AUTO-GENERATED — DO NOT EDIT

#[tool_router]
impl SuperpositionMcpServer {
    #[tool(
        name = "context.create",
        description = "Creates a new context with specified conditions and overrides."
    )]
    async fn context_create(
        &self,
        Parameters(args): Parameters<CreateContextParams>,
    ) -> Result<CallToolResult, rmcp::ErrorData> {
        self.create_context_impl(args).await
    }
    // ... all other tools ...
}
```

### 3. `helpers_gen.rs` — Response Macros

```
// AUTO-GENERATED — DO NOT EDIT

macro_rules! context_to_json {
    ($r:expr) => {{
        serde_json::json!({
            "id": $r.id,
            "value": $crate::helpers::doc_map_to_json(&$r.value),
            // ... derived from output shape fields ...
        })
    }}
}
```

## Smithy Model → Rust Mapping Rules

These are deterministic rules the codegen plugin applies:

### Tool Naming

```
Resource "Context" + Operation "Create" → "context.create"
Resource "DefaultConfig" + Operation "List" → "default_config.list"
Resource "ExperimentGroup" + Operation "Delete" → "experiment_group.delete"
```

Rule: `snake_case(resource) + "." + snake_case(verb)`

### Parameter Struct Generation

For each operation input shape, emit a Rust struct field per member:

| Smithy Trait               | Rust Type                    | Builder Call Pattern                        |
|----------------------------|------------------------------|---------------------------------------------|
| `@required` String         | `pub field: String`          | `.field(args.field)`                        |
| Optional String            | `pub field: Option<String>`  | `if let Some(x) = args.field { req = req.field(x); }` |
| `@required` Document/Map   | `pub field: serde_json::Value` | `.set_field(Some(json_to_doc_map(args.field)?))` |
| Optional Document/Map      | `pub field: Option<serde_json::Value>` | conversion + conditional set     |
| `@required` Integer        | `pub field: i32`             | `.field(args.field)`                        |
| Optional Integer           | `pub field: Option<i32>`     | conditional                                 |
| `@required` List<String>   | `pub field: Vec<String>`     | `for x in args.field { req = req.field(x); }` |
| Optional List<String>      | `pub field: Option<Vec<String>>` | conditional iteration                   |
| `@httpHeader("x-config-tags")` | `pub config_tags: Option<String>` | `req = req.config_tags(tags);`       |
| `@httpQuery("prefix")`     | `pub prefix: Option<Vec<String>>` | iteration pattern                       |
| `@httpLabel`               | `pub id: String`             | `.id(args.id)`                              |
| `WorkspaceMixin`           | (omitted from params)        | `.workspace_id(...).org_id(...)`            |
| `PaginationParams`         | count/page Option<i32>       | conditional set                             |

### Response Macro Generation

For each output shape, emit a `resource_to_json!` macro. Field mapping:

| Smithy Output Type          | JSON Macro Expression                          |
|-----------------------------|------------------------------------------------|
| String field                | `"field": $r.field`                            |
| DateTime field              | `"field": format_datetime(&$r.field)`          |
| HashMap<String, Document>   | `"field": doc_map_to_json(&$r.field)`          |
| Document field              | `"field": doc_to_json(&$r.field)`              |
| Integer/Boolean             | `"field": $r.field`                            |
| Optional<T>                 | `"field": $r.field`  (serde handles None→null) |
| Nested structure            | `"field": nested_to_json!($r.field)`           |

### Operation Classification

| Smithy Trait    | Behavior                                          |
|-----------------|---------------------------------------------------|
| `@readonly`     | GET — no change_reason, no config_tags             |
| `@idempotent`   | PUT — may have @httpPayload wrapping               |
| (default POST)  | POST — standard create/update                      |
| DELETE          | DELETE — minimal params (usually just id)           |

### Special Cases (Handled Deterministically)

1. **`@httpPayload` wrapper**: When input has a `request` field marked
   `@httpPayload`, the params struct flattens the payload fields and the impl
   constructs the wrapper type via its builder.

2. **`ContextIdentifier` union**: The `UpdateOverride` operation uses a union
   type. The codegen can detect `@union` shapes and emit the appropriate
   if/else dispatch.

3. **Enum fields** (e.g., `ExperimentType`, `SortBy`): Parse string → enum
   using `from_str` or match, with validation error on unknown values.

4. **Bulk operations**: Operations with `List<T>` input get iteration in the
   builder call.

## Implementation Steps

### Phase 1: Smithy Plugin Skeleton

1. Create `smithy/mcp-codegen/` as a Gradle/Maven Java project
2. Implement `SmithyIntegration` and `DirectedCodegen` interfaces
3. Register plugin in `smithy-build.json` as `"mcp-rust-codegen"`
4. Walk all operations in the service, group by resource

### Phase 2: Parameter Struct Generator

1. For each operation, resolve the effective input shape (expanding mixins)
2. Classify each member by its traits (@httpQuery, @httpHeader, @httpLabel,
   @httpPayload, @required, etc.)
3. Filter out WorkspaceMixin/OrganisationMixin members (handled implicitly)
4. Emit `#[derive(Debug, Deserialize, JsonSchema)]` struct with doc comments
   from `@documentation`

### Phase 3: Implementation Generator

1. Emit the `impl SuperpositionMcpServer` block per resource
2. For each operation, emit the handler method:
   - Initialize SDK builder: `self.client.{snake_case(operation_name)}()`
   - Always chain `.workspace_id(...)` and `.org_id(...)` (unless org-level op)
   - For `@httpPayload` ops: construct the payload type via its builder first
   - Chain required fields directly
   - Wrap optional fields in `if let Some`
   - Apply type conversions (json_to_doc_map for Document types)
   - `.send().await.map_err(mcp_err)?`
   - Format response using the generated macro

### Phase 4: Tool Registration Generator

1. Emit `#[tool_router] impl` block with all `#[tool(...)]` methods
2. Tool name = `snake_case(resource) + "." + snake_case(verb)`
3. Description = `@documentation` from the Smithy operation

### Phase 5: Response Macro Generator

1. For each output shape, walk its members
2. Emit `macro_rules!` with field-by-field JSON construction
3. Apply type-specific formatting (DateTime, Document, etc.)

### Phase 6: Integration

1. Add to `smithy-build.json` plugins list
2. Wire generated files into `crates/superposition_mcp/src/` via
   `include!()` or as a separate generated module
3. Keep `helpers.rs` (json_to_doc, doc_to_json, mcp_err) as hand-written
   shared utilities
4. Add CI step: `smithy build` then `cargo check` on MCP crate

## Alternative: Standalone Rust Generator (Simpler)

If adding a Smithy Java plugin feels heavyweight, an alternative is a
standalone Rust binary that:

1. Parses the Smithy JSON AST (run `smithy ast` to get JSON)
2. Walks operations and shapes from the JSON
3. Applies the same deterministic rules above
4. Emits `.rs` files via string templates (e.g., `askama` or `tera`)

**Pros**: No Java toolchain, can run as `cargo run -p mcp-codegen`
**Cons**: Must parse Smithy's JSON AST format instead of using the Java API

### Smithy JSON AST Approach

```bash
# Generate JSON AST from Smithy models
cd smithy && smithy ast models/ > ast.json
```

The JSON AST contains all shapes, traits, and relationships needed:

```json
{
  "smithy": "2.0",
  "shapes": {
    "io.superposition#CreateContext": {
      "type": "operation",
      "input": { "target": "io.superposition#CreateContextInput" },
      "output": { "target": "io.superposition#ContextResponse" },
      "traits": {
        "smithy.api#http": { "method": "PUT", "uri": "/context" },
        "smithy.api#documentation": "Creates a new context...",
        "smithy.api#tags": ["Context Management"]
      }
    }
  }
}
```

## File Structure

```
superposition/
├── smithy/
│   ├── models/              # Existing .smithy files (source of truth)
│   ├── smithy-build.json    # Add mcp-codegen plugin
│   └── mcp-codegen/         # Option A: Smithy Java plugin
│       └── src/main/java/
│
├── crates/
│   ├── mcp-codegen/         # Option B: Standalone Rust generator
│   │   ├── Cargo.toml
│   │   ├── src/main.rs      # Reads JSON AST, emits .rs files
│   │   └── templates/       # Tera/Askama templates
│   │
│   └── superposition_mcp/
│       └── src/
│           ├── server.rs         # Hand-written: McpServer struct, config
│           ├── helpers.rs        # Hand-written: json_to_doc, mcp_err, etc.
│           ├── generated/        # AUTO-GENERATED
│           │   ├── mod.rs
│           │   ├── tools/
│           │   │   ├── context.rs
│           │   │   ├── dimension.rs
│           │   │   └── ...
│           │   ├── server_tools.rs  # #[tool_router] impl
│           │   └── response_macros.rs
│           └── tools/            # Optional: hand-written overrides
│               └── mod.rs        # Re-exports generated + any custom tools
```

## Estimated Complexity

- **Smithy Plugin (Java)**: ~500-800 lines of Java/Kotlin
- **Standalone Rust Generator**: ~600-1000 lines of Rust
- **Templates**: ~200 lines of template code
- **Integration/CI**: ~50 lines of build config

The generator would eliminate ~3000+ lines of hand-written boilerplate across
14 tool files and make adding new Smithy operations a zero-code-change process
for the MCP layer.
