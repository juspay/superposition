# Smithy CLI Plugin for Superposition — Implementation Plan

## Goal

Build a **Smithy code-generation plugin** that reads the existing Smithy model files and auto-generates a complete CLI application (Rust, using `clap`) for the Superposition API. Each Smithy operation becomes a CLI subcommand with strongly-typed arguments derived from the operation's input shape.

---

## 1. Architecture Overview

```
smithy/models/*.smithy
        │
        ▼
┌─────────────────────────┐
│  Smithy CLI Codegen     │  ← New Java plugin (SmithyBuildPlugin)
│  Plugin                 │
└────────┬────────────────┘
         │  generates
         ▼
┌─────────────────────────┐
│  crates/superposition_  │  ← Generated Rust crate
│  cli/                   │
│  ├── src/main.rs        │     clap App with subcommands
│  ├── src/commands/      │     one module per resource
│  │   ├── dimension.rs   │
│  │   ├── config.rs      │
│  │   ├── experiment.rs  │
│  │   └── ...            │
│  ├── src/client.rs      │     HTTP client (reuses superposition_sdk)
│  ├── src/output.rs      │     Output formatting (JSON / table / YAML)
│  └── Cargo.toml         │
└─────────────────────────┘
```

The generated CLI delegates HTTP calls to the **already-generated `superposition_sdk`** Rust client, so there's no need to reimplement HTTP/serialization logic.

---

## 2. Smithy Plugin (Java side)

### 2.1 New Maven artifact

Create a new Java module for the plugin:

```
smithy/cli-codegen/
├── pom.xml                          (or build.gradle)
├── src/main/java/io/superposition/cli/codegen/
│   ├── CliCodegenPlugin.java        ← implements SmithyBuildPlugin
│   ├── RustCliGenerator.java        ← main code-generation orchestrator
│   ├── CommandGenerator.java        ← generates per-operation subcommands
│   ├── ArgMapper.java               ← maps Smithy shapes → clap arg types
│   └── TemplateEngine.java          ← simple template rendering
└── src/main/resources/
    └── META-INF/services/
        └── software.amazon.smithy.build.SmithyBuildPlugin
```

### 2.2 Plugin registration

Add to `smithy-build.json`:

```json
"cli-codegen": {
  "service": "io.superposition#Superposition",
  "outputCrate": "superposition_cli",
  "sdkCrate": "superposition_sdk",
  "binaryName": "superposition"
}
```

### 2.3 Shape → Arg mapping strategy

| Smithy Shape       | Clap Arg Type           | CLI Syntax                        |
|---------------------|-------------------------|-----------------------------------|
| `String`            | `String`                | `--name <VALUE>`                  |
| `Integer`           | `i64`                   | `--count 10`                      |
| `Boolean`           | `bool` (flag)           | `--all`                           |
| `Enum`              | clap `ValueEnum`        | `--status created\|concluded`     |
| `List<String>`      | `Vec<String>`           | `--tags foo --tags bar`           |
| `Map<K,V>`          | `Vec<String>` (K=V)     | `--override key=value`            |
| `Document` (JSON)   | `String` (raw JSON)     | `--context '{"os":"android"}'`    |
| `Timestamp`         | `String` (ISO-8601)     | `--from 2025-01-01T00:00:00Z`    |
| `Union`             | clap subcommand or flag | `--type regular` / `--type local-cohort=<id>` |
| `@httpLabel`        | positional arg          | `superposition dimension get <name>` |
| `@httpHeader`       | named flag              | `--workspace <id>` (global)       |
| `@httpPayload`      | `--body <JSON\|@file>`  | Read from file with `@` prefix    |
| `@httpQuery`        | named flag              | `--page 2 --count 20`            |

### 2.4 Global args (from mixins)

The `WorkspaceMixin` and `OrganisationMixin` fields become **global flags** (or env-var defaults):

| Field          | CLI Flag         | Env Var Fallback          |
|----------------|------------------|---------------------------|
| `workspace_id` | `--workspace`    | `SUPERPOSITION_WORKSPACE` |
| `org_id`       | `--org`          | `SUPERPOSITION_ORG`       |
| (auth token)   | `--token`        | `SUPERPOSITION_TOKEN`     |
| (base URL)     | `--base-url`     | `SUPERPOSITION_BASE_URL`  |

### 2.5 Code generation logic

The plugin walks the Smithy model:

1. **For each Resource** → create a clap subcommand group (e.g., `dimension`, `experiment`, `config`)
2. **For each Operation on the resource** → create a sub-subcommand (e.g., `dimension create`, `dimension list`, `dimension get`)
3. **For each field in the operation's input** → generate a clap arg using the mapping table above
4. **For `@required` fields** → mark the clap arg as `.required(true)`
5. **For `@documentation` traits** → use as `.help()` text
6. **For enum fields** → generate `ValueEnum` derive types
7. **For `@httpLabel` fields** → make positional args
8. **For pagination mixin fields** → add `--count`, `--page`, `--all` to list commands

---

## 3. Generated CLI Structure (Rust side)

### 3.1 Command hierarchy

```
superposition [GLOBAL_FLAGS] <RESOURCE> <ACTION> [ARGS]

Examples:
  superposition --workspace dev --org juspay dimension create \
    --name os --position 1 --schema '{"type":"string"}' \
    --description "Operating system"

  superposition dimension list --page 1 --count 20

  superposition experiment create --name "button-color-test" \
    --context '{"os":"android"}' \
    --variants @variants.json

  superposition config get --context '{"os":"android","city":"bangalore"}'

  superposition config fast

  superposition experiment ramp --id <UUID> --traffic-percentage 50

  superposition experiment conclude --id <UUID> --chosen-variant control
```

### 3.2 Resource → Subcommand mapping

| Smithy Resource    | CLI Subcommand     | Actions                                                    |
|--------------------|--------------------|------------------------------------------------------------|
| `Dimension`        | `dimension`        | `create`, `get`, `list`, `update`, `delete`                |
| `DefaultConfig`    | `default-config`   | `create`, `get`, `list`, `update`, `delete`                |
| `Context`          | `context`          | `create`, `get`, `list`, `update-override`, `move`, `delete`, `validate`, `weight-recompute`, `bulk-operation` |
| `Config`           | `config`           | `fast`, `get`, `resolve`, `resolve-with-id`, `toml`, `json`|
| `ConfigVersion`    | `config-version`   | `get`, `list`                                              |
| `Experiments`      | `experiment`       | `create`, `get`, `list`, `update-overrides`, `conclude`, `discard`, `ramp`, `pause`, `resume`, `applicable-variants` |
| `ExperimentGroup`  | `experiment-group` | `create`, `get`, `list`, `update`, `delete`, `add-members`, `remove-members` |
| `Function`         | `function`         | `create`, `get`, `list`, `update`, `delete`, `test`, `publish` |
| `TypeTemplates`    | `type-template`    | `create`, `get`, `list`, `update`, `delete`                |
| `Organisation`     | `organisation`     | `create`, `get`, `list`, `update`                          |
| `Workspace`        | `workspace`        | `create`, `get`, `list`, `update`, `migrate-schema`, `rotate-encryption-key` |
| `Webhook`          | `webhook`          | `create`, `get`, `list`, `update`, `delete`, `get-by-event`|
| `AuditLog`         | `audit`            | `list`                                                     |
| `Variable`         | `variable`         | `create`, `get`, `list`, `update`, `delete`                |
| `Secret`           | `secret`           | `create`, `get`, `list`, `update`, `delete`                |
| `MasterKey`        | `master-key`       | `rotate`                                                   |

### 3.3 Output formatting

Every command supports:
- `--output json` (default) — raw JSON response
- `--output table` — human-friendly tabular output
- `--output yaml` — YAML format

### 3.4 Generated Cargo.toml dependencies

```toml
[dependencies]
superposition_sdk = { path = "../superposition_sdk" }
clap = { version = "4", features = ["derive"] }
tokio = { version = "1", features = ["full"] }
serde = { version = "1", features = ["derive"] }
serde_json = "1"
serde_yaml = "0.9"
comfy-table = "7"       # for table output
```

---

## 4. Implementation Phases

### Phase 1: Plugin scaffold + basic CRUD commands
1. Create the Java codegen plugin module (`smithy/cli-codegen/`)
2. Implement `SmithyBuildPlugin` interface — walk the model, extract resources and operations
3. Implement `ArgMapper` — map Smithy shapes to clap arg types
4. Generate `main.rs` with global args and resource subcommand dispatch
5. Generate command modules for **Dimension** and **DefaultConfig** as initial targets
6. Wire up the generated CLI to call `superposition_sdk` client methods
7. Add to `smithy-build.json`

### Phase 2: Complete operation coverage
8. Generate all remaining resource command modules
9. Handle special cases: `@httpPayload` (JSON body from stdin/file), `Document` types, unions
10. Generate `ValueEnum` types for all Smithy enums
11. Handle pagination args via mixin detection

### Phase 3: UX polish
12. Add output formatting (`--output json|table|yaml`)
13. Add shell completion generation (`superposition completions bash|zsh|fish`)
14. Add `--dry-run` flag to print the HTTP request without sending
15. Add config file support (`~/.superposition/config.toml`) for defaults
16. Error formatting — human-readable error messages with status codes

### Phase 4: Integration & testing
17. Add the generated crate to the Cargo workspace
18. Write integration tests using a mock server
19. Add a CI step to regenerate + build the CLI on Smithy model changes
20. Add a `smithy/patches/cli.patch` for any post-generation fixups
21. Documentation and `--help` text review

---

## 5. Alternative: Template-based approach (no Java plugin)

If building a full Java Smithy plugin feels heavyweight, an alternative is a **Rust build-time code generator** that:

1. Parses the **generated OpenAPI spec** (already produced by the `openapi` plugin) instead of the Smithy model directly
2. Uses a Rust build script (`build.rs`) or a standalone Rust binary to read `openapi.json` and emit `clap` code
3. This avoids writing any Java code and stays within the Rust ecosystem

**Trade-offs:**
| Approach           | Pros                                  | Cons                                  |
|--------------------|---------------------------------------|---------------------------------------|
| Smithy Java Plugin | First-class Smithy integration, access to all traits/metadata, consistent with existing plugins | Requires Java tooling, more complex setup |
| OpenAPI → Rust     | No Java needed, simpler to prototype  | Loses some Smithy-specific metadata (mixins, custom traits), depends on OpenAPI plugin output |
| Handwritten Rust   | Full control, no codegen complexity   | Maintenance burden, drifts from Smithy model |

**Recommendation:** Start with the **Smithy Java Plugin** approach for first-class integration with the existing build pipeline. The project already has 5 codegen plugins configured, so this fits naturally.

---

## 6. Key Design Decisions to Confirm

1. **Language for generated CLI:** Rust (recommended — aligns with the project's primary language and reuses `superposition_sdk`)
2. **Plugin language:** Java (standard for Smithy plugins) vs. OpenAPI-based Rust generator
3. **Auth handling:** Bearer token via `--token` / env var, or also support interactive login?
4. **Body input for complex types:** `--body '{"key":"val"}'` vs. `--body @file.json` vs. individual flags for each field?
5. **Should the CLI be a workspace member crate** or a standalone binary?
6. **Naming:** binary name `superposition` or `sp` (short alias)?
