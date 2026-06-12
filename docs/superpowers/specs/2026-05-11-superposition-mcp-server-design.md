# Superposition MCP Server — Design

**Date:** 2026-05-11
**Status:** Draft, pending review
**Owner:** Natarajan Kannan

## 1. Context

Superposition's smithy models already drive code generation for a Rust SDK and multiple language clients. A new generator — [`juspay/smithy-mcp-generator`](https://github.com/juspay/smithy-mcp-generator) — emits a Rust [MCP](https://modelcontextprotocol.io/) (Model Context Protocol) server from the same smithy IDL, exposing each annotated operation as an MCP tool.

This spec describes how that generated MCP server lands in the superposition repo as a regenerated workspace crate, alongside a hand-written binary that ships as the deployable artifact. The MCP server enables MCP-capable clients (Claude Desktop, Claude Code, mcp-cli, etc.) to call superposition APIs as tools.

## 2. Architecture overview

```
smithy/models/*.smithy             smithy/output/source/mcp-rust/        crates/
   (+@mcpTool on each op)          (raw generator output)                superposition_mcp/         <- generated, regenerable
        │                                  │                                    │  Cargo.toml, src/{lib,types,tools,server}.rs
        │ smithy build (mcp-rust plugin)   │ make smithy-clients (copy)         │
        ▼                                  ▼                             ─────────────────────────────
   ┌──────────────────┐               ┌──────────┐                       superposition_mcp_server/  <- hand-written binary
   │ ToolGenerator    │ ───────────▶  │   JAR    │                              │  Cargo.toml, src/{main,config,auth,dispatch,transport_http}.rs
   │ ServerGenerator  │               │ output   │                              │
   │ StructureGen ... │               └──────────┘                              ▼
   └──────────────────┘                                                  ┌─────────────────────┐
                                                                         │   superposition-mcp │ <- binary
                                                                         │   (stdio | http)    │
                                                                         └─────────────────────┘
                                                                                  │
                                                                                  ▼
                                                                          superposition HTTP API
```

The MCP server runs as a sidecar process. Two transports are supported: stdio (single-tenant, local) and HTTP+SSE (multi-tenant, passthrough-auth). The generated crate is treated as a regenerable artifact, identical in spirit to `crates/superposition_sdk`. Embedding the MCP server in-process inside the main superposition binary is **deferred** (see §12).

## 3. Crate layout

Two new workspace members.

### 3.1 `crates/superposition_mcp` — generated library

Pure generator output. Overwritten on every `make smithy-clients` run.

```
crates/superposition_mcp/
├── Cargo.toml          (generated; patched to inherit version/license/homepage from workspace)
├── README.md           (hand-preserved; `git restore` after smithy-clients)
├── CHANGELOG.md        (hand-preserved)
└── src/
    ├── lib.rs          (re-exports)
    ├── types.rs        (input/output structs)
    ├── tools.rs        (bridge fns: one `handle_<op>(&Client, params) -> Result<Value, McpError>` per @mcpTool op)
    └── server.rs       (McpServer with tool registry)
```

Excluded from `cargo fmt`/`clippy` via the `EXCLUDE_PACKAGES` list in the makefile, matching `superposition_sdk` precedent.

### 3.2 `crates/superposition_mcp_server` — hand-written binary

Normal handwritten code; subject to standard fmt/lint/test.

```
crates/superposition_mcp_server/
├── Cargo.toml
└── src/
    ├── main.rs              (CLI parsing via clap; transport selection; signal handling)
    ├── config.rs            (env-var loading; validation; default workspace/org)
    ├── auth.rs              (AuthValue enum; task-local; static + passthrough ResolveIdentity impls)
    ├── dispatch.rs          (wrapper over superposition_mcp::server that injects default workspace/org)
    └── transport_http.rs    (Axum mount for rmcp HTTP+SSE; Authorization-header middleware)
```

Produces a single binary, `superposition-mcp`.

## 4. Smithy model changes

Three concrete edits.

### 4.1 Trait import

Smithy's `use` statement is file-scoped. Add `use software.amazon.smithy.mcp#mcpTool` to every `smithy/models/*.smithy` file that contains an operation receiving `@mcpTool` (i.e., every operation-bearing file).

### 4.2 Per-operation annotation

**Scope: all operations in the `io.superposition#Superposition` service.** Every operation gets a bare `@mcpTool` annotation. The generator (after PR [juspay/smithy-mcp-generator#5](https://github.com/juspay/smithy-mcp-generator/pull/5)) falls back to each operation's existing `@documentation` trait for the tool description; no description text needs to be restated.

Example diff:

```diff
+@mcpTool
 @documentation("Retrieves the latest config with no processing for high-performance access.")
 @http(method: "GET", uri: "/config/fast")
 @tags(["Configuration Management"])
 operation GetConfigFast {
```

This is mechanical: one new line per operation across approximately 16 files under `smithy/models/`.

### 4.3 No new operations

This work adds zero new operations. Only annotates existing ones.

## 5. Build & CI integration

### 5.1 `smithy/smithy-build.json`

Add an `mcp-rust` plugin block alongside the existing `rust-client-codegen`:

```jsonc
"mcp-rust": {
    "service": "io.superposition#Superposition",
    "package": "superposition_mcp",
    "clientSdk": "smithy-rs",
    "clientCrate": "superposition_sdk",
    "runtimeVersion": "0.1.0"
}
```

Add the codegen Maven dependency to the `maven.dependencies` array:

```
"in.juspay.smithy:smithy-mcp-codegen:0.1.0",
"in.juspay.smithy:smithy-mcp-traits:0.1.0"
```

### 5.2 Local Maven repo for the generator JAR (transitional)

The codegen JAR is sourced from a **bundled local Maven repository** under `smithy/maven-local/`, in standard Maven layout:

```
smithy/maven-local/
└── in/juspay/smithy/
    ├── smithy-mcp-codegen/0.1.0/
    │   ├── smithy-mcp-codegen-0.1.0.jar
    │   └── smithy-mcp-codegen-0.1.0.pom
    └── smithy-mcp-traits/0.1.0/
        ├── smithy-mcp-traits-0.1.0.jar
        └── smithy-mcp-traits-0.1.0.pom
```

The makefile's `SMITHY_MAVEN_REPOS` is extended to include `file://$(CURDIR)/smithy/maven-local`. This is **explicitly transitional** — see §11 for the migration path to a proper Maven repository.

### 5.3 `Cargo.toml` workspace members

Add to `members`:

```toml
"crates/superposition_mcp",
"crates/superposition_mcp_server",
```

Add `superposition_mcp` to `EXCLUDE_PACKAGES` in the makefile (alongside `superposition_sdk`). `superposition_mcp_server` is **not** excluded — it's handwritten and gets normal lint/fmt treatment.

### 5.4 `smithy/patches/mcp-rust.patch`

Following the `superposition_sdk` precedent (`smithy/patches/rust.patch`), a small patch rewrites the generated `crates/superposition_mcp/Cargo.toml` to inherit `version`/`license`/`homepage` from the workspace. Applied automatically by `make smithy-clients`.

### 5.5 Makefile `smithy-clients` extension

```makefile
rm -rf crates/superposition_mcp
mkdir -p crates/superposition_mcp
git restore crates/superposition_mcp/README.md
git restore crates/superposition_mcp/CHANGELOG.md
cp -r $(SMITHY_BUILD_SRC)/mcp-rust/* crates/superposition_mcp
# git apply smithy/patches/*.patch already runs at end of target
```

### 5.6 CI freshness

No new CI work required. The existing `smithy-sdk-generation-check` job at `.github/workflows/ci_check_pr.yaml:391` already:

1. Installs the Smithy CLI.
2. Runs `make smithy-updates` (which calls `smithy-clients`).
3. Fails on any `git diff` after the run.

Once the MCP crate is wired into `smithy-clients`, drift between `smithy/models/*.smithy` and `crates/superposition_mcp/` is automatically caught.

The local Maven repo at `smithy/maven-local/` is checked into the repo (transitionally), so CI doesn't need to fetch the codegen JAR from anywhere external.

## 6. Binary CLI surface

```
superposition-mcp                                    # stdio (default)
superposition-mcp --http 0.0.0.0:8765                # HTTP+SSE on all interfaces
superposition-mcp --http :8765 --allow-static-auth   # HTTP, fall back to env creds when Authorization missing
```

### 6.1 Environment variables

| Variable | Required when | Notes |
|---|---|---|
| `SUPERPOSITION_ENDPOINT` | always | Base URL of the superposition HTTP API. |
| `SUPERPOSITION_BEARER_TOKEN` | stdio mode (if no basic) | Bearer-auth credential. Mutually exclusive with basic. |
| `SUPERPOSITION_BASIC_USER` + `SUPERPOSITION_BASIC_PASS` | stdio mode (if no bearer) | Basic-auth credential. Both must be set together. |
| `SUPERPOSITION_WORKSPACE_ID` | optional | Default `workspace_id` injected into tool calls that omit it. |
| `SUPERPOSITION_ORG_ID` | optional | Default `org_id` injected into tool calls that omit it. |
| `RUST_LOG` | optional | Standard `tracing-subscriber` filter. Default `info`. |

### 6.2 Startup validation

Validation runs before any MCP handshake. Failures exit non-zero with a clear stderr message:

- **stdio mode:** endpoint required; exactly one of bearer or basic required.
- **HTTP mode (default):** endpoint required; credentials must be **absent** (passthrough only). If both `SUPERPOSITION_BEARER_TOKEN` and HTTP mode are set without `--allow-static-auth`, fail with a clear message ("static credentials are ignored in HTTP passthrough mode; pass `--allow-static-auth` to enable fallback").
- **HTTP mode + `--allow-static-auth`:** endpoint required; credentials optional (used only when `Authorization` is absent on a request).

### 6.3 Logging

- `tracing-subscriber` initialised once at startup.
- **Writer hardcoded to `stderr`**, regardless of transport. Stdout is the JSON-RPC channel on stdio and any stray write corrupts the protocol.
- Standard span fields on every tool invocation: `tool_name`, `request_id` (from rmcp), and in HTTP mode `remote_addr`.
- **Never log credential values, even truncated.** Never log full request bodies — they may contain customer configuration payloads.
- One INFO log per tool dispatch: name, duration, outcome (success/error). One WARN log on auth-fallback when `--allow-static-auth` actually fires.

## 7. Auth model

### 7.1 Static mode (stdio, or HTTP with `--allow-static-auth` fallback)

At startup, build a single SDK `Client` with `.bearer_token(...)` or `.basic_auth_login(...)` derived from env. The same `Client` is reused across all tool calls. No per-request work.

### 7.2 Passthrough mode (HTTP default)

The binary defines:

```rust
tokio::task_local! {
    static SUPERPOSITION_AUTH: AuthValue;
}

enum AuthValue {
    Bearer(SecretString),
    Basic { user: String, pass: SecretString },
}
```

A custom smithy-rs `ResolveIdentity` implementation for both `HTTP_BEARER_AUTH_SCHEME_ID` and `HTTP_BASIC_AUTH_SCHEME_ID` reads from this task-local on each SDK call. If the task-local is unset (no `Authorization` header) and `--allow-static-auth` is not enabled, the resolver returns an identity error which propagates as an MCP `AUTH_REQUIRED` error (and HTTP 401 on the transport).

One SDK `Client` is built at startup with both resolvers wired and is `Arc`-shared across the Axum app state. This preserves connection pooling and HTTP/2 multiplexing across all MCP clients.

Axum middleware at the MCP route extracts the inbound `Authorization` header, parses it into an `AuthValue`, and wraps the rmcp request handler:

```rust
SUPERPOSITION_AUTH.scope(auth_value, async move { handler.run().await })
```

No per-request `Client` rebuild.

### 7.3 Mixed mode (`--allow-static-auth`)

- **Authorization header present:** behave as passthrough.
- **Authorization header absent:** task-local stays unset; the resolver falls back to env-loaded identity (captured at startup). Logged at WARN once per process (not per request).
- Intended for local development; help text documents this.

### 7.4 Secrets hygiene

- All credential values held in `secrecy::SecretString` (already a workspace dependency).
- No credential value crosses a log boundary, even truncated.
- Auth-error messages: `"missing or invalid Authorization header"` / `"basic header parse failed"`. **Never** `"token X is invalid"` or any value-bearing message.

## 8. Default workspace/org injection

Many superposition operations take `workspace_id` / `org_id` as input via the `WorkspaceMixin`. The generated MCP tools expose them as required tool parameters — meaning Claude must fill them on every call.

The binary's `dispatch::dispatch_tool(name, params)` wraps the generated `superposition_mcp::server::McpServer::handle_*` functions. Before delegating, it inspects the params JSON:

- If `workspace_id` is absent **and** `SUPERPOSITION_WORKSPACE_ID` is set, inject it.
- Same for `org_id` / `SUPERPOSITION_ORG_ID`.
- If both are absent (no param, no env default), pass through unchanged. The SDK call surfaces superposition's error verbatim; we don't second-guess.

This is **single-tenant convenience only**. In HTTP passthrough mode, env defaults are still allowed (a hosted MCP server can pre-configure a default workspace for its tenants), but the recommended usage is that MCP clients supply per-call.

No generator change required.

## 9. Error mapping

The generator's default mapping already produces `Result<_, McpError>` from each tool bridge. The binary adds two thin layers:

- **Authentication failures from passthrough mode:** MCP error code `-32001` (server-defined: `AUTH_REQUIRED`), message `"missing or invalid Authorization header"`. On HTTP transport, the Axum middleware also surfaces this as a `401`.
- **SDK errors from superposition (4xx/5xx):** propagated verbatim. The smithy-rs `SdkError` carries the upstream status, code, and message; these are forwarded into the MCP error payload without rewording. Claude sees the same error text a CLI user would.
- **Local errors (panic, poisoned state):** rmcp's panic catch converts to a generic MCP error; logged at `error!` to stderr.

## 10. Testing strategy

Three layers, scaled to what each can usefully prove.

### 10.1 Generated-crate compile check

Covered by the existing `smithy-sdk-generation-check` CI job once the MCP crate is wired in. If the generator emits code and the workspace compiles, the generator-produced code is sound. **No new test required.**

### 10.2 Binary unit tests

In `crates/superposition_mcp_server/src/`:

- `config::load_from_env` — table-driven test of valid/invalid env combinations (bearer-only, basic-only, both-set rejected, missing endpoint rejected, HTTP mode with static creds without `--allow-static-auth` rejected).
- `dispatch::inject_defaults` — given a JSON params blob and env defaults, asserts the merged blob (param wins, env fills hole, neither present, malformed JSON).
- `auth::AuthValue::parse_header` — header parsing (Bearer, Basic, malformed, empty).

### 10.3 Integration test

One test in `crates/superposition_mcp_server/tests/`:

- Spin up `wiremock` impersonating superposition; records hits, returns canned JSON.
- Build an `McpServer` against an SDK `Client` pointed at it.
- Invoke `tools/list` and a representative `tools/call` (e.g. `GetConfigFast`) via an in-process rmcp client over an in-memory transport.
- Assertions: `tools/list` returns one entry per `@mcpTool`-annotated operation (count derived dynamically from the generated `superposition_mcp::server::McpServer::tools()` to avoid hardcoding a number); `tools/call` returns expected JSON; wiremock recorded a request bearing the expected `Authorization` header (proving passthrough wiring).

No live-superposition test in this suite. The existing integration suite can add an MCP smoke later if useful.

## 11. Dependency sourcing & prerequisites

External artifacts this work depends on, in publish order:

### 11.1 Generator changes (PR open)

[juspay/smithy-mcp-generator#5](https://github.com/juspay/smithy-mcp-generator/pull/5) — adds the `@documentation` fallback so bare `@mcpTool` annotations work. **Must merge before this work can land.**

### 11.2 `smithy-mcp-codegen` and `smithy-mcp-traits` JARs (bundled, transitional)

Built locally from `juspay/smithy-mcp-generator` at the tag that includes PR #5. Bundled under `smithy/maven-local/` in standard Maven layout. **Both JAR and POM** files for each artifact are required (smithy's Maven resolver walks POMs for transitive deps).

The `smithy/maven-local/` tree is checked into the superposition repo so CI doesn't need to fetch anything external.

**Build recipe** (documented in `smithy/maven-local/README.md`):

```bash
cd /path/to/smithy-mcp-generator
git checkout <tag>
./gradlew publishToMavenLocal
cp -r ~/.m2/repository/in/juspay/smithy/{smithy-mcp-codegen,smithy-mcp-traits} \
      /path/to/superposition/smithy/maven-local/in/juspay/smithy/
```

The implementation may need to add a `maven-publish` Gradle plugin block to `smithy-mcp-generator` if it doesn't already configure `publishToMavenLocal`. This is an upstream prerequisite tracked separately.

### 11.3 `smithy-mcp-runtime` Cargo dependency

Sourced via **git ref** in `crates/superposition_mcp/Cargo.toml`:

```toml
[dependencies.smithy-mcp-runtime]
git = "https://github.com/juspay/smithy-mcp-generator.git"
rev = "<sha>"
features = ["stdio", "http"]
```

The git ref is pinned to the same commit as the codegen JAR's source build, ensuring the runtime's Rust API matches the generated code.

### 11.4 Migration path to a proper Maven repo

The bundled-JAR approach in §5.2 is transitional. A follow-on task — **not blocked on this work** — should migrate to one of:

- **(B) juspay sandbox Maven** (`https://sandbox.assets.juspay.in/smithy/m2`, already in `SMITHY_MAVEN_REPOS`): one `gradle publish` step in the generator repo's release CI, and the bundled JAR + `file://` repo entry are removed.
- **(A) JitPack**: tag the generator repo, add `https://jitpack.io` to `SMITHY_MAVEN_REPOS`, done. Caveat: first-tag latency, third-party availability.

This spec leaves the choice between (A) and (B) to the follow-on. Tracking issue should reference this section.

## 12. Out of scope / deferred

The following are intentionally **not** in this work:

- **In-process embedding** of the MCP server inside the main superposition binary. Reachable later via the generator's `clientSdk: "custom"` `ToolHandler` mode; would expose `/mcp` on the same Actix port. Requires a separate codegen invocation and a meaningful glue layer (one trait impl per operation). Revisit when there's pull from a hosted superposition deployment.
- **MCP-layer authentication** (OAuth 2.1 token swap, etc.). Passthrough is the only auth model in v1. Hosted scenarios that need an identity store for MCP users come later.
- **Tool-level rate limiting in the MCP binary.** Superposition's existing API-layer limits apply.
- **MCP-aware audit log** (distinct from superposition's existing audit). Tool calls appear in the existing audit as normal API calls. An optional `X-Mcp-Client-Id` header injected by the binary is a possible follow-on.
- **Sub-command surface** (`superposition-mcp doctor`, `superposition-mcp tools list --offline`, etc.). Plain `superposition-mcp` is enough for v1.

## 13. Risks & open questions

- **Generator output may not exactly match what `smithy-clients` expects.** The first integration may surface generator-emitted file layouts that the makefile's `cp -r` glob doesn't catch (e.g., hidden files, additional subdirectories). Mitigation: the implementation plan starts by running `make smithy-build` against the model-only changes and inspecting `smithy/output/source/mcp-rust/` before any makefile edits.
- **~85 model edits is enough that a typo will hide.** Mitigation: the implementation plan batches edits per-file under `smithy/models/`, with `smithy build` validation between batches.
- **`smithy-mcp-runtime` git-ref dep means superposition's `Cargo.lock` carries a non-crates.io entry.** PR reviewers will notice. Spec acknowledges this and §11.4 commits to migrating off.
- **Bundled JAR adds binary blobs to the superposition repo.** Smithy build plugin and trait JARs are typically in the low-hundreds-of-KB range, but the exact size will only be known when the JARs are built; the implementation plan re-evaluates if either exceeds ~5MB. Acceptable as a transitional state; tracked for removal in §11.4.
- **`@documentation` text used as `@mcpTool` description.** Some documentation text may be too detailed or HTTP-specific for an MCP tool description (which Claude reads to pick tools). If any operations produce poor tool UX with their documentation-as-description, the fix is to add an explicit `@mcpTool(description: "...")` on a case-by-case basis. The fallback semantics in the generator (PR #5) support both.

## 14. Implementation prerequisites (order)

1. PR [juspay/smithy-mcp-generator#5](https://github.com/juspay/smithy-mcp-generator/pull/5) merges.
2. Generator repo gets a `maven-publish` configuration if absent (small upstream task; can be in the same PR or a follow-on).
3. Generator repo gets a tag at the merged-PR commit; JARs and runtime sources are pinned to that tag.
4. In superposition: model annotations + smithy-build.json plugin block + bundled JAR + makefile wiring + binary crate land together as one PR (after step 3).
