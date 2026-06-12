# Superposition MCP Server Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a Smithy-generated MCP (Model Context Protocol) server to the superposition repo so MCP-capable clients (Claude Desktop, Claude Code, mcp-cli) can call superposition operations as tools. The server ships as a hand-written binary `superposition-mcp` that wraps a generated library crate, with stdio and HTTP+SSE transports.

**Architecture:** Two new workspace crates — `crates/superposition_mcp` (pure smithy-mcp-generator output, regenerable, treated like `crates/superposition_sdk`) and `crates/superposition_mcp_server` (hand-written binary). The binary uses smithy-rs `Client` plus a custom `ResolveIdentity` implementation that reads credentials from a `tokio::task_local!` populated either at startup (stdio, static auth) or per-HTTP-request (passthrough). HTTP transport is implemented directly in the binary against rmcp's `StreamableHttpService` using the runtime's `into_router()` escape hatch, since `smithy-mcp-runtime`'s HTTP support is currently a placeholder.

**Tech Stack:** Smithy IDL + smithy-mcp-generator (Java) for codegen; smithy-rs–generated `superposition_sdk` for HTTP; `smithy-mcp-runtime` (Rust) for the MCP `Router` and stdio transport; `rmcp` 1.2 directly for HTTP+SSE transport; Axum + Tower for HTTP serving; `secrecy` for credential storage; `tokio::task_local!` for per-request auth; `wiremock` for integration tests.

**Reference spec:** `docs/superpowers/specs/2026-05-11-superposition-mcp-server-design.md`. Read it before starting Task 1; it explains the *why* behind each decision below.

**Prerequisite:** [juspay/smithy-mcp-generator#5](https://github.com/juspay/smithy-mcp-generator/pull/5) (`feat: make @mcpTool description optional, fall back to @documentation`) must be merged into `main` of `juspay/smithy-mcp-generator` before Task 1. The PR itself was created during the brainstorming session that produced this plan.

---

## File Structure

| Path | Status | Responsibility |
|---|---|---|
| `smithy/models/*.smithy` (~16 files) | modify | Add `use software.amazon.smithy.mcp#mcpTool` + `@mcpTool` per operation. |
| `smithy/smithy-build.json` | modify | Add `mcp-rust` plugin block + Maven deps. |
| `smithy/maven-local/in/juspay/smithy/smithy-mcp-codegen/0.1.0/{jar,pom}` | create | Bundled codegen artifact (transitional). |
| `smithy/maven-local/in/juspay/smithy/smithy-mcp-traits/0.1.0/{jar,pom}` | create | Bundled trait artifact (transitional). |
| `smithy/maven-local/README.md` | create | Build-from-source recipe and migration note. |
| `smithy/patches/mcp-rust.patch` | create | Rewrites generated `Cargo.toml` to inherit from workspace. |
| `makefile` | modify | Extend `smithy-clients`; add to `EXCLUDE_PACKAGES`; extend `SMITHY_MAVEN_REPOS`. |
| `Cargo.toml` | modify | Add two new workspace members. |
| `crates/superposition_mcp/` | create (regenerated) | Generator output. README + CHANGELOG are hand-preserved. |
| `crates/superposition_mcp_server/Cargo.toml` | create | Binary crate manifest. |
| `crates/superposition_mcp_server/src/main.rs` | create | CLI entry point; transport selection. |
| `crates/superposition_mcp_server/src/config.rs` | create | Env-var loading + validation. |
| `crates/superposition_mcp_server/src/auth.rs` | create | `AuthValue` enum, `parse_header`, task-local, `ResolveIdentity` impls. |
| `crates/superposition_mcp_server/src/dispatch.rs` | create | Default `workspace_id`/`org_id` injection. |
| `crates/superposition_mcp_server/src/transport_http.rs` | create | Axum mount + Tower middleware for HTTP+SSE transport. |
| `crates/superposition_mcp_server/tests/integration.rs` | create | wiremock-backed end-to-end test. |

---

## Task 1: Build & bundle smithy-mcp-codegen JARs into smithy/maven-local

**Files:**
- Create: `smithy/maven-local/in/juspay/smithy/smithy-mcp-codegen/0.1.0/smithy-mcp-codegen-0.1.0.jar`
- Create: `smithy/maven-local/in/juspay/smithy/smithy-mcp-codegen/0.1.0/smithy-mcp-codegen-0.1.0.pom`
- Create: `smithy/maven-local/in/juspay/smithy/smithy-mcp-traits/0.1.0/smithy-mcp-traits-0.1.0.jar`
- Create: `smithy/maven-local/in/juspay/smithy/smithy-mcp-traits/0.1.0/smithy-mcp-traits-0.1.0.pom`
- Create: `smithy/maven-local/README.md`

This task assumes PR #5 has been merged into `juspay/smithy-mcp-generator` `main` and that `juspay/smithy-mcp-generator` is checked out at the merge commit at `../smithy-mcp-generator` relative to the superposition repo.

- [ ] **Step 1: Verify the generator repo's gradle publishing config produces the right Maven coordinates.**

Check the generator's `build.gradle` files for a `maven-publish` plugin block and the `publishing.publications` Maven coordinates. If absent in `smithy-mcp-codegen/build.gradle` or `smithy-mcp-traits/build.gradle`, add it before continuing.

Expected `groupId` / `artifactId` / `version`:

| Artifact | groupId | artifactId | version |
|---|---|---|---|
| codegen | `in.juspay.smithy` | `smithy-mcp-codegen` | `0.1.0` |
| traits | `in.juspay.smithy` | `smithy-mcp-traits` | `0.1.0` |

If the publishing config needs to be added, edit `smithy-mcp-codegen/build.gradle` and `smithy-mcp-traits/build.gradle` to add:

```groovy
plugins {
    id 'java-library'
    id 'maven-publish'
}

group = 'in.juspay.smithy'
version = '0.1.0'

publishing {
    publications {
        maven(MavenPublication) {
            from components.java
        }
    }
}
```

Adjust group/version if the existing config differs.

- [ ] **Step 2: Build and publish to local Maven from the generator repo.**

Run:

```bash
cd ../smithy-mcp-generator
./gradlew :smithy-mcp-traits:publishToMavenLocal :smithy-mcp-codegen:publishToMavenLocal
```

Expected: `BUILD SUCCESSFUL`. Artifacts appear under `~/.m2/repository/in/juspay/smithy/{smithy-mcp-codegen,smithy-mcp-traits}/0.1.0/`.

- [ ] **Step 3: Copy artifacts into the superposition repo.**

```bash
cd /Users/natarajankannan/src/superposition.other
mkdir -p smithy/maven-local/in/juspay/smithy
cp -r ~/.m2/repository/in/juspay/smithy/smithy-mcp-codegen smithy/maven-local/in/juspay/smithy/
cp -r ~/.m2/repository/in/juspay/smithy/smithy-mcp-traits  smithy/maven-local/in/juspay/smithy/
ls smithy/maven-local/in/juspay/smithy/smithy-mcp-codegen/0.1.0/
ls smithy/maven-local/in/juspay/smithy/smithy-mcp-traits/0.1.0/
```

Expected: each directory contains at least `*.jar` and `*.pom` files (additional `.module` and checksum files are fine to copy too — leave them).

- [ ] **Step 4: Write `smithy/maven-local/README.md` documenting the recipe and the migration note.**

```markdown
# smithy/maven-local

Bundled local Maven repository for `smithy-mcp-codegen` and `smithy-mcp-traits`.

## Why this is here

The smithy-mcp-generator does not yet publish its artifacts to a public Maven repository (and GitHub Packages requires authentication for Maven, even for public repos). To unblock superposition's MCP server work without setting up new publishing infrastructure, the generator JARs are bundled here in standard Maven layout. `SMITHY_MAVEN_REPOS` in the makefile points at this directory via a `file://` URL.

## How to refresh

When the generator is updated and a new version needs to be vendored:

\`\`\`bash
cd ../smithy-mcp-generator
git checkout <tag-or-sha>
./gradlew :smithy-mcp-traits:publishToMavenLocal :smithy-mcp-codegen:publishToMavenLocal

cd ../superposition
rm -rf smithy/maven-local/in/juspay/smithy/{smithy-mcp-codegen,smithy-mcp-traits}
cp -r ~/.m2/repository/in/juspay/smithy/smithy-mcp-codegen smithy/maven-local/in/juspay/smithy/
cp -r ~/.m2/repository/in/juspay/smithy/smithy-mcp-traits  smithy/maven-local/in/juspay/smithy/
\`\`\`

Bump `runtimeVersion` in `smithy/smithy-build.json` to match.

## Migration plan

This is transitional. See spec §11.4 — eventual home is either the juspay sandbox Maven repo (already in `SMITHY_MAVEN_REPOS`) or JitPack. When that lands, delete this directory and remove the `file://` entry from `SMITHY_MAVEN_REPOS`.
```

- [ ] **Step 5: Commit.**

```bash
git add smithy/maven-local
git commit -m "build: vendor smithy-mcp-codegen and smithy-mcp-traits JARs

Transitional local Maven repo for the MCP-server smithy build plugin
and trait. See smithy/maven-local/README.md for the build recipe and
migration note tracked in
docs/superpowers/specs/2026-05-11-superposition-mcp-server-design.md
§11.4."
```

---

## Task 2: Wire smithy-build.json + makefile for the mcp-rust plugin

**Files:**
- Modify: `smithy/smithy-build.json`
- Modify: `makefile`

- [ ] **Step 1: Add the mcp-rust plugin block to `smithy/smithy-build.json`.**

Open `smithy/smithy-build.json`. In `maven.dependencies`, append two new entries:

```json
"in.juspay.smithy:smithy-mcp-codegen:0.1.0",
"in.juspay.smithy:smithy-mcp-traits:0.1.0"
```

In the `plugins` object, add a new `"mcp-rust"` entry alongside the existing `"rust-client-codegen"`:

```json
"mcp-rust": {
    "service": "io.superposition#Superposition",
    "package": "superposition_mcp",
    "clientSdk": "smithy-rs",
    "clientCrate": "superposition_sdk",
    "runtimeVersion": "0.1.0"
}
```

- [ ] **Step 2: Extend `SMITHY_MAVEN_REPOS` in the makefile.**

Open `makefile`. Find this line (currently line 52):

```makefile
export SMITHY_MAVEN_REPOS = https://repo1.maven.org/maven2|https://sandbox.assets.juspay.in/smithy/m2
```

Replace it with:

```makefile
export SMITHY_MAVEN_REPOS = https://repo1.maven.org/maven2|https://sandbox.assets.juspay.in/smithy/m2|file://$(CURDIR)/smithy/maven-local
```

- [ ] **Step 3: Run `make smithy-build` to verify the plugin loads and runs without producing the MCP crate yet.**

The smithy models don't have `@mcpTool` annotations yet, so the plugin should run successfully but emit nothing (or produce an empty/skeleton output) — `McpGeneratePlugin.java` returns early when no operations have the trait.

```bash
make smithy-clean-build
ls -la smithy/output/source/
```

Expected: `smithy/output/source/` contains the existing `*-client-codegen` directories. `mcp-rust/` either doesn't exist or is empty. Build succeeds.

If the build fails because the Maven plugin can't be resolved, double-check the `file://` URL has no spaces and that the `pom` files in `smithy/maven-local/` declare the same `groupId:artifactId:version` as the dependency line.

- [ ] **Step 4: Commit.**

```bash
git add smithy/smithy-build.json makefile
git commit -m "build: register smithy-mcp-codegen plugin in smithy-build

Adds the mcp-rust plugin entry and points SMITHY_MAVEN_REPOS at the
bundled local Maven repo. Smithy build is a no-op for MCP output until
@mcpTool annotations are added on operations (next task)."
```

---

## Task 3: Annotate smithy models with @mcpTool

**Files:**
- Modify: each `.smithy` file under `smithy/models/` that contains an operation

**Mechanical operation, applied uniformly:** every operation in the `io.superposition#Superposition` service gets a bare `@mcpTool` annotation. The generator (post PR #5) falls back to `@documentation` for the tool description.

- [ ] **Step 1: Enumerate the smithy model files to edit.**

```bash
ls smithy/models/*.smithy
```

Expected output: 16 files (audit, common, config, context, default-config, dimension, experiment_config, experiment_groups, experiments, functions, main, organisation, secret, type-templates, variable, webhook, workspace — that's actually 17 entries; some don't contain operations).

For each file, check whether it contains an `operation` block:

```bash
grep -l '^operation \|^@http' smithy/models/*.smithy
```

The files printed are the ones to edit.

- [ ] **Step 2: For each operation-bearing file, add the `use` import.**

Open the file. Below the `namespace io.superposition` line, ensure this line is present (add it if absent):

```smithy
use software.amazon.smithy.mcp#mcpTool
```

If the file already has other `use` lines, group with them in alphabetical order.

- [ ] **Step 3: For each operation in the file, add `@mcpTool` directly above the existing `@http` annotation.**

Example diff (config.smithy):

```diff
+@mcpTool
 @documentation("Retrieves the latest config with no processing for high-performance access.")
 @http(method: "GET", uri: "/config/fast")
 @tags(["Configuration Management"])
 operation GetConfigFast {
```

Apply uniformly. There is no scoping decision to make (per spec §4.2: every operation gets the annotation; read-only-only was reconsidered to all-operations during brainstorming).

- [ ] **Step 4: Run `make smithy-build` and verify the MCP crate is generated.**

```bash
make smithy-clean-build
ls -la smithy/output/source/mcp-rust/
```

Expected: `smithy/output/source/mcp-rust/` exists and contains at minimum `Cargo.toml`, `src/lib.rs`, `src/tools.rs`, `src/types.rs`, `src/server.rs`.

- [ ] **Step 5: Sanity-check the tool count against the operation count.**

```bash
grep -c 'pub fn tool_info_' smithy/output/source/mcp-rust/src/tools.rs
grep -c '^operation ' smithy/models/*.smithy | awk -F: '{sum += $2} END {print sum}'
```

Both numbers should match. If they don't, an operation is missing `@mcpTool` — find and fix.

- [ ] **Step 6: Commit.**

```bash
git add smithy/models/
git commit -m "feat(smithy): annotate operations with @mcpTool

Every operation in the io.superposition#Superposition service is
exposed as an MCP tool. Descriptions come from each operation's
existing @documentation trait via the generator's fallback (see
juspay/smithy-mcp-generator#5)."
```

---

## Task 4: Add `mcp-rust.patch` and integrate into `make smithy-clients`

**Files:**
- Create: `smithy/patches/mcp-rust.patch`
- Modify: `makefile`

- [ ] **Step 1: Inspect the generated `Cargo.toml` to learn its exact pre-patch shape.**

```bash
cat smithy/output/source/mcp-rust/Cargo.toml
```

Note the exact `version = "..."` line and `edition = "..."` line — they'll be in the patch.

- [ ] **Step 2: Use the existing `smithy/patches/rust.patch` as a template.**

```bash
cat smithy/patches/rust.patch
```

The patch rewrites the SDK's `Cargo.toml` to inherit `version`/`license`/`homepage` from the workspace and adds a `readme` field. Apply the same transformations to the MCP crate.

- [ ] **Step 3: Create `smithy/patches/mcp-rust.patch`.**

The diff must be against the file at path `crates/superposition_mcp/Cargo.toml` (the destination path after `make smithy-clients` runs). The exact pre-image will be the contents you read in Step 1 — use those line-for-line so the patch applies cleanly.

Template (substitute exact pre-image lines from Step 1):

```diff
diff --git a/crates/superposition_mcp/Cargo.toml b/crates/superposition_mcp/Cargo.toml
--- a/crates/superposition_mcp/Cargo.toml
+++ b/crates/superposition_mcp/Cargo.toml
@@ -1,8 +1,12 @@
 [package]
 name = "superposition_mcp"
-version = "0.1.0"
+version.workspace = true
 edition = "2021"
+license = { workspace = true }
+homepage = { workspace = true }
+repository = "https://github.com/juspay/superposition"
+readme = "README.md"
```

Save the patch.

- [ ] **Step 4: Extend the `smithy-clients` make target to install the MCP crate.**

Open `makefile`. Find the `smithy-clients` target (currently at line 266). After the existing block that handles `crates/superposition_sdk` (around line 296 — look for `rm -rf crates/superposition_sdk`), add the MCP crate's analogue immediately after:

```makefile
	rm -rf crates/superposition_mcp
	mkdir -p crates/superposition_mcp
	cp -r $(SMITHY_BUILD_SRC)/mcp-rust/*\
				crates/superposition_mcp
```

The hand-preserved files (`README.md`, `CHANGELOG.md`) don't exist yet on this first run, so don't add `git restore` lines for them yet — they'll be added in Task 5 once the files are committed.

The trailing `git apply smithy/patches/*.patch` at the end of the target (line 311) will pick up `mcp-rust.patch` automatically.

- [ ] **Step 5: Run `make smithy-clients` and verify the MCP crate appears under `crates/`.**

```bash
make smithy-clients
ls crates/superposition_mcp/
cat crates/superposition_mcp/Cargo.toml
```

Expected: directory exists with `Cargo.toml`, `src/`. The Cargo.toml shows `version.workspace = true`, `license = { workspace = true }`, etc.

If `git apply` fails on `mcp-rust.patch`, the pre-image lines in the patch don't match the generated output — re-do Step 3 using the actual generated text.

- [ ] **Step 6: Commit.**

```bash
git add smithy/patches/mcp-rust.patch makefile
git commit -m "build(smithy): install superposition_mcp via smithy-clients

Patches generated Cargo.toml to inherit workspace fields, mirroring
the superposition_sdk pattern."
```

---

## Task 5: Add `superposition_mcp` to the workspace + hand-preserved README/CHANGELOG

**Files:**
- Modify: `Cargo.toml`
- Modify: `makefile`
- Create: `crates/superposition_mcp/README.md`
- Create: `crates/superposition_mcp/CHANGELOG.md`

- [ ] **Step 1: Add `crates/superposition_mcp` to workspace members.**

Open `Cargo.toml`. Locate the `members = [` list. Add (alphabetically near `superposition_macros`):

```toml
"crates/superposition_mcp",
```

- [ ] **Step 2: Add `superposition_mcp` to `EXCLUDE_PACKAGES` in the makefile.**

Open `makefile`. Line 9 currently reads:

```makefile
EXCLUDE_PACKAGES := experimentation_client_integration_example superposition_sdk
```

Change to:

```makefile
EXCLUDE_PACKAGES := experimentation_client_integration_example superposition_sdk superposition_mcp
```

- [ ] **Step 3: Create the hand-preserved `README.md`.**

```bash
cat > crates/superposition_mcp/README.md <<'EOF'
# superposition_mcp

Generated Rust crate exposing the `io.superposition#Superposition` smithy service as an MCP (Model Context Protocol) server.

**Do not edit files in this crate by hand** — they are regenerated by `make smithy-clients` from `smithy/models/*.smithy`. The exceptions are `README.md` and `CHANGELOG.md`, which are hand-preserved and restored after regeneration.

The deployable binary that wraps this crate lives in `../superposition_mcp_server/`.

See `docs/superpowers/specs/2026-05-11-superposition-mcp-server-design.md` for design context.
EOF
```

- [ ] **Step 4: Create the hand-preserved `CHANGELOG.md`.**

```bash
cat > crates/superposition_mcp/CHANGELOG.md <<'EOF'
# Changelog

This file is hand-maintained alongside the otherwise auto-generated `superposition_mcp` crate.

## Unreleased
EOF
```

- [ ] **Step 5: Update `smithy-clients` target to `git restore` the preserved files.**

Open `makefile`. Find the MCP block added in Task 4, Step 4. Insert `git restore` lines between `mkdir -p` and `cp -r`:

```makefile
	rm -rf crates/superposition_mcp
	mkdir -p crates/superposition_mcp
	git restore crates/superposition_mcp/README.md
	git restore crates/superposition_mcp/CHANGELOG.md
	cp -r $(SMITHY_BUILD_SRC)/mcp-rust/*\
				crates/superposition_mcp
```

Note: `git restore` only works once the files have been committed. They get committed in Step 7 below.

- [ ] **Step 6: Verify `cargo check` passes against the new workspace member.**

```bash
cargo check -p superposition_mcp
```

Expected: compiles. If it complains about a missing `smithy-mcp-runtime` dep, that's a problem — the generated `Cargo.toml` should already declare it. Inspect:

```bash
cat crates/superposition_mcp/Cargo.toml
```

The generated `Cargo.toml` declares `smithy-mcp-runtime = "0.1.0"`, which doesn't resolve from crates.io because the runtime isn't published. Patch the generated Cargo.toml in `smithy/patches/mcp-rust.patch` to use a git ref instead. Add to the patch:

```diff
-smithy-mcp-runtime = "0.1.0"
+smithy-mcp-runtime = { git = "https://github.com/juspay/smithy-mcp-generator.git", rev = "<sha>", features = ["stdio", "http"] }
```

Substitute `<sha>` with the commit SHA from the generator repo that includes PR #5 (look up via `cd ../smithy-mcp-generator && git rev-parse HEAD`).

Re-run:

```bash
make smithy-clients
cargo check -p superposition_mcp
```

Expected: compiles successfully.

- [ ] **Step 7: Commit.**

```bash
git add Cargo.toml makefile crates/superposition_mcp/README.md crates/superposition_mcp/CHANGELOG.md smithy/patches/mcp-rust.patch
git commit -m "feat: add superposition_mcp workspace member

Generated crate from smithy-mcp-generator. Patch redirects
smithy-mcp-runtime to a git ref (it is not yet on crates.io).
README and CHANGELOG are hand-preserved via git-restore in the
smithy-clients make target."
```

- [ ] **Step 8: Also commit the regenerated crate body now that the patch is stable.**

```bash
git add crates/superposition_mcp/
git status
```

Verify no other unrelated files are staged. Then:

```bash
git commit -m "chore(generated): superposition_mcp crate body

Generated by smithy-mcp-generator from smithy/models/*.smithy. Re-run
make smithy-clients to regenerate."
```

---

## Task 6: Scaffold superposition_mcp_server binary crate

**Files:**
- Create: `crates/superposition_mcp_server/Cargo.toml`
- Create: `crates/superposition_mcp_server/src/main.rs` (stub)
- Modify: `Cargo.toml` (workspace members)

- [ ] **Step 1: Create the binary crate manifest.**

```toml
# crates/superposition_mcp_server/Cargo.toml
[package]
name = "superposition_mcp_server"
version.workspace = true
edition.workspace = true
license.workspace = true
homepage.workspace = true
repository = "https://github.com/juspay/superposition"
description = "MCP server binary exposing the superposition API as tools"

[[bin]]
name = "superposition-mcp"
path = "src/main.rs"

[dependencies]
superposition_mcp = { path = "../superposition_mcp" }
superposition_sdk = { path = "../superposition_sdk" }
smithy-mcp-runtime = { git = "https://github.com/juspay/smithy-mcp-generator.git", rev = "<sha>", features = ["stdio", "http"] }

aws-smithy-runtime-api = { workspace = true }
aws-smithy-types = { workspace = true }

anyhow = { workspace = true }
async-trait = "0.1"
clap = { version = "4", features = ["derive", "env"] }
rmcp = { version = "1.2", features = ["server", "transport-streamable-http-server"] }
secrecy = { workspace = true }
serde = { workspace = true }
serde_json = { workspace = true }
tokio = { version = "1", features = ["full"] }
tower = "0.5"
tracing = { workspace = true }
tracing-subscriber = { version = "0.3", features = ["env-filter", "fmt"] }
axum = "0.7"
http = "1"
base64 = { workspace = true }

[dev-dependencies]
wiremock = "0.6"
```

Substitute `<sha>` with the same commit SHA used in `smithy/patches/mcp-rust.patch`. Verify that `aws-smithy-runtime-api`, `aws-smithy-types`, `anyhow`, `secrecy`, `serde`, `serde_json`, `tracing`, `base64` are all in `[workspace.dependencies]` in the root `Cargo.toml` — they are at the time of writing.

If `axum` or `tower` versions need to be added to workspace deps for consistency with other crates, do so. Otherwise pin here is fine since this is a leaf binary.

- [ ] **Step 2: Create a stub `main.rs` so the crate compiles.**

```rust
// crates/superposition_mcp_server/src/main.rs
fn main() {
    println!("superposition-mcp: not yet implemented");
}
```

- [ ] **Step 3: Add to workspace members.**

Open the root `Cargo.toml`. Add to `members`:

```toml
"crates/superposition_mcp_server",
```

- [ ] **Step 4: Verify the workspace compiles.**

```bash
cargo check -p superposition_mcp_server
```

Expected: compiles. If `smithy-mcp-runtime` git ref fails to resolve, double-check the SHA exists on `juspay/smithy-mcp-generator`.

- [ ] **Step 5: Commit.**

```bash
git add Cargo.toml crates/superposition_mcp_server/
git commit -m "feat: scaffold superposition_mcp_server binary crate

Stub main; real wiring lands in subsequent commits."
```

---

## Task 7: `config` module — env-var loading + tests

**Files:**
- Create: `crates/superposition_mcp_server/src/config.rs`
- Modify: `crates/superposition_mcp_server/src/main.rs` (add `mod config;`)

- [ ] **Step 1: Write the failing tests first.**

Create `crates/superposition_mcp_server/src/config.rs`:

```rust
use secrecy::SecretString;

#[derive(Debug, Clone, PartialEq)]
pub enum StaticCreds {
    Bearer(SecretString),
    Basic { user: String, pass: SecretString },
}

impl StaticCreds {
    pub fn is_bearer(&self) -> bool {
        matches!(self, StaticCreds::Bearer(_))
    }
    pub fn is_basic(&self) -> bool {
        matches!(self, StaticCreds::Basic { .. })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Defaults {
    pub workspace_id: Option<String>,
    pub org_id: Option<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Config {
    pub endpoint: String,
    pub creds: Option<StaticCreds>,
    pub defaults: Defaults,
}

#[derive(Debug, thiserror::Error, PartialEq)]
pub enum ConfigError {
    #[error("SUPERPOSITION_ENDPOINT is required")]
    MissingEndpoint,
    #[error("provide either SUPERPOSITION_BEARER_TOKEN or SUPERPOSITION_BASIC_USER+SUPERPOSITION_BASIC_PASS, not both")]
    ConflictingCreds,
    #[error("SUPERPOSITION_BASIC_USER and SUPERPOSITION_BASIC_PASS must be set together")]
    IncompleteBasic,
    #[error("stdio mode requires credentials (bearer or basic)")]
    StdioRequiresCreds,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Mode {
    Stdio,
    HttpPassthrough,
    HttpWithStaticFallback,
}

pub fn load(mode: Mode, env: &dyn EnvLookup) -> Result<Config, ConfigError> {
    let endpoint = env.get("SUPERPOSITION_ENDPOINT").ok_or(ConfigError::MissingEndpoint)?;

    let bearer = env.get("SUPERPOSITION_BEARER_TOKEN");
    let basic_user = env.get("SUPERPOSITION_BASIC_USER");
    let basic_pass = env.get("SUPERPOSITION_BASIC_PASS");

    let creds = match (bearer, basic_user, basic_pass) {
        (Some(_), Some(_), _) | (Some(_), _, Some(_)) => return Err(ConfigError::ConflictingCreds),
        (Some(b), None, None) => Some(StaticCreds::Bearer(SecretString::new(b.into()))),
        (None, Some(u), Some(p)) => Some(StaticCreds::Basic { user: u, pass: SecretString::new(p.into()) }),
        (None, Some(_), None) | (None, None, Some(_)) => return Err(ConfigError::IncompleteBasic),
        (None, None, None) => None,
    };

    match mode {
        Mode::Stdio if creds.is_none() => return Err(ConfigError::StdioRequiresCreds),
        _ => {}
    }

    Ok(Config {
        endpoint,
        creds,
        defaults: Defaults {
            workspace_id: env.get("SUPERPOSITION_WORKSPACE_ID"),
            org_id: env.get("SUPERPOSITION_ORG_ID"),
        },
    })
}

pub trait EnvLookup {
    fn get(&self, key: &str) -> Option<String>;
}

pub struct ProcessEnv;
impl EnvLookup for ProcessEnv {
    fn get(&self, key: &str) -> Option<String> {
        std::env::var(key).ok()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    struct MapEnv(HashMap<String, String>);
    impl EnvLookup for MapEnv {
        fn get(&self, key: &str) -> Option<String> {
            self.0.get(key).cloned()
        }
    }
    fn env(pairs: &[(&str, &str)]) -> MapEnv {
        MapEnv(pairs.iter().map(|(k, v)| (k.to_string(), v.to_string())).collect())
    }

    #[test]
    fn stdio_requires_endpoint() {
        let e = env(&[("SUPERPOSITION_BEARER_TOKEN", "t")]);
        assert_eq!(load(Mode::Stdio, &e), Err(ConfigError::MissingEndpoint));
    }

    #[test]
    fn stdio_requires_creds() {
        let e = env(&[("SUPERPOSITION_ENDPOINT", "https://api.example.com")]);
        assert_eq!(load(Mode::Stdio, &e), Err(ConfigError::StdioRequiresCreds));
    }

    #[test]
    fn stdio_accepts_bearer() {
        let e = env(&[
            ("SUPERPOSITION_ENDPOINT", "https://api.example.com"),
            ("SUPERPOSITION_BEARER_TOKEN", "t"),
        ]);
        let c = load(Mode::Stdio, &e).unwrap();
        assert!(c.creds.unwrap().is_bearer());
    }

    #[test]
    fn stdio_accepts_basic() {
        let e = env(&[
            ("SUPERPOSITION_ENDPOINT", "https://api.example.com"),
            ("SUPERPOSITION_BASIC_USER", "u"),
            ("SUPERPOSITION_BASIC_PASS", "p"),
        ]);
        let c = load(Mode::Stdio, &e).unwrap();
        assert!(c.creds.unwrap().is_basic());
    }

    #[test]
    fn rejects_both_bearer_and_basic() {
        let e = env(&[
            ("SUPERPOSITION_ENDPOINT", "https://api.example.com"),
            ("SUPERPOSITION_BEARER_TOKEN", "t"),
            ("SUPERPOSITION_BASIC_USER", "u"),
            ("SUPERPOSITION_BASIC_PASS", "p"),
        ]);
        assert_eq!(load(Mode::Stdio, &e), Err(ConfigError::ConflictingCreds));
    }

    #[test]
    fn rejects_incomplete_basic() {
        let e = env(&[
            ("SUPERPOSITION_ENDPOINT", "https://api.example.com"),
            ("SUPERPOSITION_BASIC_USER", "u"),
        ]);
        assert_eq!(load(Mode::Stdio, &e), Err(ConfigError::IncompleteBasic));
    }

    #[test]
    fn http_passthrough_accepts_no_creds() {
        let e = env(&[("SUPERPOSITION_ENDPOINT", "https://api.example.com")]);
        let c = load(Mode::HttpPassthrough, &e).unwrap();
        assert!(c.creds.is_none());
    }

    #[test]
    fn defaults_populate_from_env() {
        let e = env(&[
            ("SUPERPOSITION_ENDPOINT", "https://api.example.com"),
            ("SUPERPOSITION_BEARER_TOKEN", "t"),
            ("SUPERPOSITION_WORKSPACE_ID", "w1"),
            ("SUPERPOSITION_ORG_ID", "o1"),
        ]);
        let c = load(Mode::Stdio, &e).unwrap();
        assert_eq!(c.defaults.workspace_id.as_deref(), Some("w1"));
        assert_eq!(c.defaults.org_id.as_deref(), Some("o1"));
    }
}
```

Also add `thiserror` to dependencies in `Cargo.toml` if missing:

```toml
thiserror = { workspace = true }
```

(Confirm `thiserror` is in `[workspace.dependencies]`; if absent, add `thiserror = "1"` to this crate's deps instead.)

- [ ] **Step 2: Register the module in `main.rs`.**

Replace the stub `main.rs` with:

```rust
mod config;

fn main() {
    println!("superposition-mcp: not yet implemented");
}
```

- [ ] **Step 3: Run the tests; verify they pass.**

```bash
cargo test -p superposition_mcp_server config::
```

Expected: 8 tests pass.

- [ ] **Step 4: Commit.**

```bash
git add crates/superposition_mcp_server/
git commit -m "feat(mcp-server): env-var config loading and validation"
```

---

## Task 8: `auth` module — AuthValue, header parsing, task-local, ResolveIdentity

**Files:**
- Create: `crates/superposition_mcp_server/src/auth.rs`
- Modify: `crates/superposition_mcp_server/src/main.rs` (`mod auth;`)

- [ ] **Step 1: Write `auth.rs` with `AuthValue`, header parser, and task-local.**

```rust
// crates/superposition_mcp_server/src/auth.rs
use base64::Engine;
use secrecy::{ExposeSecret, SecretString};

#[derive(Debug, Clone)]
pub enum AuthValue {
    Bearer(SecretString),
    Basic { user: String, pass: SecretString },
}

tokio::task_local! {
    pub static SUPERPOSITION_AUTH: AuthValue;
}

#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum AuthParseError {
    #[error("missing Authorization header")]
    Missing,
    #[error("malformed Authorization header")]
    Malformed,
    #[error("unsupported authentication scheme")]
    UnsupportedScheme,
}

impl AuthValue {
    /// Parse an `Authorization` header value.
    /// Supports `Bearer <token>` and `Basic <base64(user:pass)>`.
    pub fn parse_header(value: Option<&str>) -> Result<AuthValue, AuthParseError> {
        let raw = value.ok_or(AuthParseError::Missing)?.trim();
        let (scheme, rest) = raw.split_once(' ').ok_or(AuthParseError::Malformed)?;
        let scheme = scheme.to_ascii_lowercase();
        let rest = rest.trim();
        match scheme.as_str() {
            "bearer" => {
                if rest.is_empty() { return Err(AuthParseError::Malformed); }
                Ok(AuthValue::Bearer(SecretString::new(rest.to_string().into())))
            }
            "basic" => {
                let decoded = base64::engine::general_purpose::STANDARD
                    .decode(rest)
                    .map_err(|_| AuthParseError::Malformed)?;
                let decoded = String::from_utf8(decoded).map_err(|_| AuthParseError::Malformed)?;
                let (user, pass) = decoded.split_once(':').ok_or(AuthParseError::Malformed)?;
                Ok(AuthValue::Basic {
                    user: user.to_string(),
                    pass: SecretString::new(pass.to_string().into()),
                })
            }
            _ => Err(AuthParseError::UnsupportedScheme),
        }
    }

    /// Bearer-token string (only meaningful for Bearer variant).
    pub fn bearer(&self) -> Option<&str> {
        match self {
            AuthValue::Bearer(t) => Some(t.expose_secret()),
            _ => None,
        }
    }

    /// (user, pass) for Basic variant.
    pub fn basic(&self) -> Option<(&str, &str)> {
        match self {
            AuthValue::Basic { user, pass } => Some((user, pass.expose_secret())),
            _ => None,
        }
    }
}

impl From<crate::config::StaticCreds> for AuthValue {
    fn from(c: crate::config::StaticCreds) -> Self {
        match c {
            crate::config::StaticCreds::Bearer(t) => AuthValue::Bearer(t),
            crate::config::StaticCreds::Basic { user, pass } => AuthValue::Basic { user, pass },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_bearer() {
        let v = AuthValue::parse_header(Some("Bearer abc123")).unwrap();
        assert_eq!(v.bearer(), Some("abc123"));
    }

    #[test]
    fn parses_bearer_case_insensitive_scheme() {
        let v = AuthValue::parse_header(Some("bearer abc")).unwrap();
        assert_eq!(v.bearer(), Some("abc"));
    }

    #[test]
    fn parses_basic() {
        let creds = base64::engine::general_purpose::STANDARD.encode("alice:s3cret");
        let v = AuthValue::parse_header(Some(&format!("Basic {}", creds))).unwrap();
        assert_eq!(v.basic(), Some(("alice", "s3cret")));
    }

    #[test]
    fn rejects_missing() {
        assert_eq!(AuthValue::parse_header(None), Err(AuthParseError::Missing));
    }

    #[test]
    fn rejects_empty_bearer() {
        assert_eq!(AuthValue::parse_header(Some("Bearer ")), Err(AuthParseError::Malformed));
    }

    #[test]
    fn rejects_unknown_scheme() {
        assert_eq!(AuthValue::parse_header(Some("Digest xyz")), Err(AuthParseError::UnsupportedScheme));
    }

    #[test]
    fn rejects_malformed_basic_no_colon() {
        let creds = base64::engine::general_purpose::STANDARD.encode("no-colon-here");
        let v = AuthValue::parse_header(Some(&format!("Basic {}", creds)));
        assert_eq!(v, Err(AuthParseError::Malformed));
    }
}
```

- [ ] **Step 2: Register the module in main.rs.**

```rust
mod auth;
mod config;

fn main() {
    println!("superposition-mcp: not yet implemented");
}
```

- [ ] **Step 3: Run tests.**

```bash
cargo test -p superposition_mcp_server auth::
```

Expected: 7 tests pass.

- [ ] **Step 4: Commit.**

```bash
git add crates/superposition_mcp_server/
git commit -m "feat(mcp-server): AuthValue and header parsing"
```

---

## Task 9: `auth` module continued — smithy-rs `ResolveIdentity` impl

**Files:**
- Modify: `crates/superposition_mcp_server/src/auth.rs`

The SDK `Client` is built once at startup with a custom `ResolveIdentity` for bearer and basic schemes. The resolver reads from the `SUPERPOSITION_AUTH` task-local. If the task-local is unset and a static fallback was configured, the resolver returns that fallback's identity instead.

- [ ] **Step 1: Inspect the smithy-rs identity resolver trait surface in the generated SDK.**

```bash
grep -rn "ResolveIdentity\|IdentityFuture\|http::Token\|http::Login" crates/superposition_sdk/src/config.rs | head -20
```

Note the relevant types: `aws_smithy_runtime_api::client::identity::http::Token` for bearer; `aws_smithy_runtime_api::client::identity::http::Login` for basic; trait `aws_smithy_runtime_api::client::identity::ResolveIdentity`.

- [ ] **Step 2: Add the resolver implementations.**

Append to `auth.rs`:

```rust
use aws_smithy_runtime_api::client::identity::{
    http::{Login, Token},
    Identity, IdentityFuture, ResolveIdentity, SharedIdentityResolver,
};
use aws_smithy_runtime_api::client::runtime_components::RuntimeComponents;
use aws_smithy_runtime_api::client::auth::AuthSchemeEndpointConfig;
use aws_smithy_types::config_bag::ConfigBag;

/// Resolves bearer-token identity from the task-local, falling back to a static value if provided.
#[derive(Debug)]
pub struct BearerResolver {
    pub fallback: Option<SecretString>,
}

impl ResolveIdentity for BearerResolver {
    fn resolve_identity<'a>(
        &'a self,
        _runtime_components: &'a RuntimeComponents,
        _config_bag: &'a ConfigBag,
    ) -> IdentityFuture<'a> {
        IdentityFuture::ready({
            let token = SUPERPOSITION_AUTH
                .try_with(|v| v.bearer().map(|s| s.to_string()))
                .ok()
                .flatten()
                .or_else(|| self.fallback.as_ref().map(|s| s.expose_secret().to_string()));

            match token {
                Some(t) => Ok(Identity::new(Token::new(t, None), None)),
                None => Err("no bearer credential in task-local or fallback".into()),
            }
        })
    }
}

/// Resolves basic-auth identity from the task-local, falling back to a static value if provided.
#[derive(Debug)]
pub struct BasicResolver {
    pub fallback: Option<(String, SecretString)>,
}

impl ResolveIdentity for BasicResolver {
    fn resolve_identity<'a>(
        &'a self,
        _runtime_components: &'a RuntimeComponents,
        _config_bag: &'a ConfigBag,
    ) -> IdentityFuture<'a> {
        IdentityFuture::ready({
            let login = SUPERPOSITION_AUTH
                .try_with(|v| v.basic().map(|(u, p)| (u.to_string(), p.to_string())))
                .ok()
                .flatten()
                .or_else(|| {
                    self.fallback.as_ref().map(|(u, p)| (u.clone(), p.expose_secret().to_string()))
                });

            match login {
                Some((u, p)) => Ok(Identity::new(Login::new(u, p, None), None)),
                None => Err("no basic credential in task-local or fallback".into()),
            }
        })
    }
}

pub fn shared_bearer(fallback: Option<SecretString>) -> SharedIdentityResolver {
    SharedIdentityResolver::new(BearerResolver { fallback })
}

pub fn shared_basic(fallback: Option<(String, SecretString)>) -> SharedIdentityResolver {
    SharedIdentityResolver::new(BasicResolver { fallback })
}
```

- [ ] **Step 3: Verify it compiles.**

```bash
cargo check -p superposition_mcp_server
```

If unresolved imports surface, double-check the exact type paths against what `crates/superposition_sdk/src/config.rs` re-exports — `Token` is `aws_smithy_runtime_api::client::identity::http::Token` and is re-exported as `crate::config::Token` in the SDK, confirmed at `crates/superposition_sdk/src/config.rs:1125`.

- [ ] **Step 4: Commit.**

```bash
git add crates/superposition_mcp_server/src/auth.rs
git commit -m "feat(mcp-server): smithy-rs ResolveIdentity impls for task-local auth"
```

---

## Task 10: `dispatch` module — default workspace/org injection

**Files:**
- Create: `crates/superposition_mcp_server/src/dispatch.rs`
- Modify: `crates/superposition_mcp_server/src/main.rs` (`mod dispatch;`)

- [ ] **Step 1: Write the dispatch wrapper and tests.**

```rust
// crates/superposition_mcp_server/src/dispatch.rs
use serde_json::{Map, Value};

use crate::config::Defaults;

/// If the params is a JSON object and is missing `workspace_id` / `org_id`,
/// inject defaults from the configuration. Other shapes (non-object) are
/// passed through unchanged — the SDK call will surface a typed deserialize
/// error verbatim.
pub fn inject_defaults(params: Value, defaults: &Defaults) -> Value {
    let Value::Object(mut obj) = params else {
        return params;
    };
    if !obj.contains_key("workspace_id") {
        if let Some(w) = defaults.workspace_id.as_ref() {
            obj.insert("workspace_id".to_string(), Value::String(w.clone()));
        }
    }
    if !obj.contains_key("org_id") {
        if let Some(o) = defaults.org_id.as_ref() {
            obj.insert("org_id".to_string(), Value::String(o.clone()));
        }
    }
    Value::Object(obj)
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    fn defaults(w: Option<&str>, o: Option<&str>) -> Defaults {
        Defaults {
            workspace_id: w.map(String::from),
            org_id: o.map(String::from),
        }
    }

    #[test]
    fn injects_both_when_absent() {
        let params = json!({"key": "v"});
        let out = inject_defaults(params, &defaults(Some("w"), Some("o")));
        assert_eq!(out, json!({"key": "v", "workspace_id": "w", "org_id": "o"}));
    }

    #[test]
    fn param_value_wins_over_default() {
        let params = json!({"workspace_id": "explicit"});
        let out = inject_defaults(params, &defaults(Some("default"), None));
        assert_eq!(out, json!({"workspace_id": "explicit"}));
    }

    #[test]
    fn injects_org_only_when_only_org_defaulted() {
        let params = json!({});
        let out = inject_defaults(params, &defaults(None, Some("o")));
        assert_eq!(out, json!({"org_id": "o"}));
    }

    #[test]
    fn no_change_when_no_defaults() {
        let params = json!({"x": 1});
        let out = inject_defaults(params.clone(), &defaults(None, None));
        assert_eq!(out, params);
    }

    #[test]
    fn passes_through_non_object() {
        let params = json!(42);
        let out = inject_defaults(params.clone(), &defaults(Some("w"), Some("o")));
        assert_eq!(out, params);
    }
}
```

- [ ] **Step 2: Register the module.**

```rust
mod auth;
mod config;
mod dispatch;

fn main() {
    println!("superposition-mcp: not yet implemented");
}
```

- [ ] **Step 3: Run tests.**

```bash
cargo test -p superposition_mcp_server dispatch::
```

Expected: 5 tests pass.

- [ ] **Step 4: Commit.**

```bash
git add crates/superposition_mcp_server/
git commit -m "feat(mcp-server): inject workspace/org defaults into tool params"
```

---

## Task 11: Wire the SDK Client + Router with the dispatch wrapper

**Files:**
- Modify: `crates/superposition_mcp_server/src/main.rs`

The generator's `McpServer::new(client)` registers each tool with a closure that calls `tools::handle_<op>(&client, params)`. We need to wrap each registration so the params pass through `dispatch::inject_defaults` first. The cleanest path is to use `McpServer::new(client).into_router()` and *not* use the generator's pre-built router, because we need to intercept params. Instead, we duplicate the per-tool registration with our wrapper inline.

For this task, we accept the duplication and write our own builder. (Followup: spec §11.4 notes that the generator could later grow a parameter-interceptor hook, eliminating this duplication.)

- [ ] **Step 1: Build the customized router with default-injection wrappers.**

Replace `main.rs` with the following, which:

1. Parses CLI args with clap.
2. Loads config from env.
3. Builds the SDK `Client` with appropriate identity resolvers.
4. Constructs a router that, for each `@mcpTool` operation, wraps the generated `handle_<op>` with `inject_defaults`.

Use a build script or generated helper to enumerate tools. **For this plan, we hand-write the registration list.** It can be regenerated mechanically: `grep -o 'pub async fn handle_[a-z_]*' crates/superposition_mcp/src/tools.rs | sort -u` produces the function names; the `tool_info_*` partner has the same suffix.

```rust
// crates/superposition_mcp_server/src/main.rs
mod auth;
mod config;
mod dispatch;

use std::sync::Arc;

use clap::Parser;
use secrecy::ExposeSecret;
use smithy_mcp_runtime::Router;
use superposition_mcp::tools;
use superposition_sdk::Client;
use tracing_subscriber::EnvFilter;

use crate::auth::{shared_basic, shared_bearer, AuthValue, SUPERPOSITION_AUTH};
use crate::config::{Config, Defaults, Mode};

#[derive(Parser, Debug)]
#[command(name = "superposition-mcp", about = "MCP server for the Superposition API")]
struct Cli {
    /// Bind address for HTTP+SSE transport. If unset, stdio is used.
    #[arg(long, value_name = "ADDR")]
    http: Option<String>,

    /// In HTTP mode, fall back to env-var credentials when no Authorization header is present.
    #[arg(long, requires = "http")]
    allow_static_auth: bool,
}

fn init_logging() {
    let filter = EnvFilter::try_from_default_env()
        .unwrap_or_else(|_| EnvFilter::new("info"));
    tracing_subscriber::fmt()
        .with_env_filter(filter)
        .with_writer(std::io::stderr) // STDOUT is the JSON-RPC channel on stdio
        .init();
}

fn build_client(cfg: &Config, mode: Mode) -> Client {
    use superposition_sdk::config::Builder;

    let mut builder = Builder::new().endpoint_url(&cfg.endpoint);

    // Convert static creds (if any) into fallback values for the resolvers.
    let bearer_fallback = match (&cfg.creds, mode) {
        (Some(config::StaticCreds::Bearer(t)), Mode::Stdio) => Some(t.clone()),
        (Some(config::StaticCreds::Bearer(t)), Mode::HttpWithStaticFallback) => Some(t.clone()),
        _ => None,
    };
    let basic_fallback = match (&cfg.creds, mode) {
        (Some(config::StaticCreds::Basic { user, pass }), Mode::Stdio) => {
            Some((user.clone(), pass.clone()))
        }
        (Some(config::StaticCreds::Basic { user, pass }), Mode::HttpWithStaticFallback) => {
            Some((user.clone(), pass.clone()))
        }
        _ => None,
    };

    let bearer = shared_bearer(bearer_fallback);
    let basic = shared_basic(basic_fallback);

    builder = builder
        .bearer_token_resolver(bearer)
        .basic_auth_login_resolver(basic);

    Client::from_conf(builder.build())
}

fn build_router(client: Client, defaults: Defaults) -> Router {
    let client = Arc::new(client);
    let defaults = Arc::new(defaults);
    let mut router = Router::new();

    // Macro to keep the body of each registration small.
    // For every (handle_<op>, tool_info_<op>) pair in superposition_mcp::tools,
    // wrap the call with inject_defaults.
    macro_rules! register {
        ($info_fn:ident, $handle_fn:ident) => {{
            let c = client.clone();
            let d = defaults.clone();
            router.register_tool(tools::$info_fn(), move |params| {
                let c = c.clone();
                let d = d.clone();
                async move {
                    let params = dispatch::inject_defaults(params, &d);
                    tools::$handle_fn(&c, params).await
                }
            });
        }};
    }

    // Generated registration list. To regenerate:
    //   grep -E '^pub async fn handle_' crates/superposition_mcp/src/tools.rs \
    //     | awk '{print $4}' | tr -d '(' \
    //     | sed 's/^handle_\(.*\)$/    register!(tool_info_\1, handle_\1);/'
    //
    // PASTE THE OUTPUT OF THE COMMAND ABOVE BELOW. The placeholder line is a
    // marker for the implementing engineer — replace with the generated list.
    /* BEGIN GENERATED TOOL REGISTRATIONS */
    register!(tool_info_get_config_fast, handle_get_config_fast);
    // ... (~85 more lines)
    /* END GENERATED TOOL REGISTRATIONS */

    router
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    init_logging();
    let cli = Cli::parse();

    let mode = match (&cli.http, cli.allow_static_auth) {
        (None, _) => Mode::Stdio,
        (Some(_), true) => Mode::HttpWithStaticFallback,
        (Some(_), false) => Mode::HttpPassthrough,
    };

    let cfg = config::load(mode, &config::ProcessEnv)?;
    let client = build_client(&cfg, mode);
    let router = build_router(client, cfg.defaults.clone());

    match (mode, cli.http.as_deref()) {
        (Mode::Stdio, _) => stdio_serve(cfg, router).await,
        (Mode::HttpPassthrough, Some(addr)) | (Mode::HttpWithStaticFallback, Some(addr)) => {
            // Implemented in Task 12 (transport_http module).
            unimplemented!("HTTP transport — see Task 12");
        }
        _ => unreachable!(),
    }
}

async fn stdio_serve(cfg: Config, router: Router) -> anyhow::Result<()> {
    // In stdio mode, the static credential is fixed for the entire process.
    // Wrap the entire serve future in SUPERPOSITION_AUTH.scope so the
    // ResolveIdentity implementations can read it.
    let auth_value: AuthValue = cfg
        .creds
        .clone()
        .expect("config::load enforces creds presence for stdio mode")
        .into();

    SUPERPOSITION_AUTH
        .scope(auth_value, async {
            smithy_mcp_runtime::serve_stdio(router)
                .await
                .map_err(|e| anyhow::anyhow!("stdio transport error: {e}"))
        })
        .await
}
```

- [ ] **Step 2: Generate the per-tool registration block.**

```bash
grep -E '^pub async fn handle_' crates/superposition_mcp/src/tools.rs \
  | awk '{print $4}' | tr -d '(' \
  | sed 's/^handle_\(.*\)$/    register!(tool_info_\1, handle_\1);/'
```

Paste the output between the `BEGIN GENERATED TOOL REGISTRATIONS` and `END GENERATED TOOL REGISTRATIONS` markers in `build_router`. Remove the comment instructions above the marker.

- [ ] **Step 3: Verify it compiles.**

```bash
cargo check -p superposition_mcp_server
```

Expected: compiles. Common issues:

- `bearer_token_resolver` / `basic_auth_login_resolver` method names may differ slightly; check the SDK config builder at `crates/superposition_sdk/src/config.rs:208-230`. Adjust accordingly.
- `Client::from_conf` takes a `Config` (not `Builder`); use `builder.build()`.

- [ ] **Step 4: Commit.**

```bash
git add crates/superposition_mcp_server/src/main.rs
git commit -m "feat(mcp-server): stdio transport with default injection

Hand-rolled router builder wraps each generated tool handler with the
workspace/org default injection. The 85-line tool registration block
is regenerated from crates/superposition_mcp/src/tools.rs by the
script in the comment above register!."
```

---

## Task 12: HTTP transport — Axum mount with task-local-scoping middleware

**Files:**
- Create: `crates/superposition_mcp_server/src/transport_http.rs`
- Modify: `crates/superposition_mcp_server/src/main.rs`

- [ ] **Step 1: Write the HTTP transport module.**

```rust
// crates/superposition_mcp_server/src/transport_http.rs
use std::net::SocketAddr;
use std::sync::Arc;

use axum::body::Body;
use axum::extract::Request;
use axum::http::{header, StatusCode};
use axum::middleware::{self, Next};
use axum::response::Response;
use axum::Router as AxumRouter;
use rmcp::transport::streamable_http_server::tower::{StreamableHttpServerConfig, StreamableHttpService};
use rmcp::transport::common::server_side_http::SessionManager;
use rmcp::transport::streamable_http_server::session::local::LocalSessionManager;
use tracing::{error, info, warn};

use crate::auth::{AuthValue, SUPERPOSITION_AUTH};
use crate::config::Mode;

pub async fn serve(
    addr: SocketAddr,
    mode: Mode,
    static_fallback: Option<AuthValue>,
    router: smithy_mcp_runtime::Router,
) -> anyhow::Result<()> {
    let svc = StreamableHttpService::new(
        move || Ok(router.clone()),
        LocalSessionManager::default().into(),
        StreamableHttpServerConfig::default(),
    );

    let allow_static = matches!(mode, Mode::HttpWithStaticFallback);
    let fallback = Arc::new(static_fallback);

    let app = AxumRouter::new()
        .nest_service("/mcp", svc)
        .layer(middleware::from_fn(move |req: Request, next: Next| {
            let fallback = fallback.clone();
            async move { auth_layer(req, next, allow_static, fallback).await }
        }));

    info!(%addr, "MCP server listening (HTTP)");
    let listener = tokio::net::TcpListener::bind(addr).await?;
    axum::serve(listener, app).await?;
    Ok(())
}

async fn auth_layer(
    req: Request,
    next: Next,
    allow_static: bool,
    fallback: Arc<Option<AuthValue>>,
) -> Response {
    let header_value = req
        .headers()
        .get(header::AUTHORIZATION)
        .and_then(|v| v.to_str().ok())
        .map(str::to_owned);

    let auth = match AuthValue::parse_header(header_value.as_deref()) {
        Ok(a) => a,
        Err(crate::auth::AuthParseError::Missing) if allow_static => match fallback.as_ref() {
            Some(a) => {
                warn!("falling back to static credentials (--allow-static-auth)");
                a.clone()
            }
            None => return unauthorized("no Authorization header and no static fallback"),
        },
        Err(_) if allow_static && fallback.is_some() => {
            // A malformed header explicitly fails — don't silently fall back.
            return unauthorized("malformed Authorization header");
        }
        Err(e) => return unauthorized(&format!("{}", e)),
    };

    let fut = next.run(req);
    SUPERPOSITION_AUTH.scope(auth, fut).await
}

fn unauthorized(detail: &str) -> Response {
    error!("auth rejected: {detail}");
    Response::builder()
        .status(StatusCode::UNAUTHORIZED)
        .header(header::WWW_AUTHENTICATE, "Bearer realm=\"superposition-mcp\"")
        .body(Body::from(detail.to_string()))
        .unwrap()
}
```

Some imports above may need adjustment depending on the exact paths rmcp 1.2 exports. If `LocalSessionManager` lives under a slightly different module path, locate it with:

```bash
cargo doc -p rmcp --no-deps --open
# then search for "LocalSessionManager"
```

Or, more cheaply:

```bash
grep -rn "LocalSessionManager\|StreamableHttpService" "$CARGO_HOME"/registry/src/*rmcp-1.2*/ 2>/dev/null | head -10
```

- [ ] **Step 2: Wire the HTTP path in `main.rs`.**

Replace the `unimplemented!` block:

```rust
        (Mode::HttpPassthrough, Some(addr)) | (Mode::HttpWithStaticFallback, Some(addr)) => {
            let socket_addr: std::net::SocketAddr = addr.parse()
                .map_err(|e| anyhow::anyhow!("invalid --http address {addr}: {e}"))?;
            let static_fallback: Option<crate::auth::AuthValue> = cfg.creds.clone().map(Into::into);
            transport_http::serve(socket_addr, mode, static_fallback, router).await
        }
```

Add `mod transport_http;` to the module list at the top of `main.rs`.

- [ ] **Step 3: Verify the binary compiles.**

```bash
cargo check -p superposition_mcp_server
```

- [ ] **Step 4: Commit.**

```bash
git add crates/superposition_mcp_server/
git commit -m "feat(mcp-server): HTTP+SSE transport with Authorization passthrough

Mounts rmcp's StreamableHttpService under /mcp on an Axum router.
A tower middleware extracts the inbound Authorization header, parses
it into AuthValue, and wraps the inner service call in
SUPERPOSITION_AUTH.scope so the smithy-rs ResolveIdentity
implementations can read it per-request. --allow-static-auth enables
fallback to env-var creds when the header is absent."
```

---

## Task 13: Integration test — wiremock-backed end-to-end

**Files:**
- Create: `crates/superposition_mcp_server/src/lib.rs`
- Create: `crates/superposition_mcp_server/src/build.rs`
- Modify: `crates/superposition_mcp_server/Cargo.toml`
- Modify: `crates/superposition_mcp_server/src/main.rs`
- Create: `crates/superposition_mcp_server/tests/integration.rs`

Refactor the crate to expose `build_client` / `build_router` from a library target so integration tests can call them directly.

- [ ] **Step 1: Add `[lib]` and update `[[bin]]` in `Cargo.toml`.**

Add to `crates/superposition_mcp_server/Cargo.toml` (the `[[bin]]` section already exists from Task 6; add the `[lib]` section just above it):

```toml
[lib]
name = "superposition_mcp_server"
path = "src/lib.rs"

[[bin]]
name = "superposition-mcp"
path = "src/main.rs"
```

- [ ] **Step 2: Create `src/lib.rs` re-exporting the modules.**

```rust
// crates/superposition_mcp_server/src/lib.rs
pub mod auth;
pub mod build;
pub mod config;
pub mod dispatch;
```

- [ ] **Step 3: Move `build_client` and `build_router` from `main.rs` to a new `src/build.rs`.**

Cut the `fn build_client(cfg: &Config, mode: Mode) -> Client { ... }` and `fn build_router(client: Client, defaults: Defaults) -> Router { ... }` blocks out of `main.rs` (along with the `register!` macro and the generated registration list inside `build_router`). Paste them into `src/build.rs`. At the top of `build.rs`, change the items from `fn` to `pub fn` and add imports:

```rust
// crates/superposition_mcp_server/src/build.rs
use std::sync::Arc;

use smithy_mcp_runtime::Router;
use superposition_mcp::tools;
use superposition_sdk::{config::Builder, Client};

use crate::auth::{shared_basic, shared_bearer};
use crate::config::{Config, Defaults, Mode, StaticCreds};
use crate::dispatch;

pub fn build_client(cfg: &Config, mode: Mode) -> Client {
    // ... body moved verbatim from main.rs, with `config::StaticCreds`
    // replaced by `StaticCreds` (now imported above) ...
}

pub fn build_router(client: Client, defaults: Defaults) -> Router {
    // ... body moved verbatim from main.rs ...
}
```

In `main.rs`, remove the module declarations (`mod auth; mod config; ...`) — they now live in `lib.rs`. Replace with a `use` of the library:

```rust
use superposition_mcp_server::{auth, build, config, dispatch};

mod transport_http; // binary-only; never imported by tests
```

Update the `main` function so it calls `build::build_client(...)` and `build::build_router(...)` instead of bare names.

- [ ] **Step 4: Verify the crate still compiles.**

```bash
cargo check -p superposition_mcp_server
cargo test -p superposition_mcp_server --lib
```

Expected: the existing config / auth / dispatch unit tests still pass (they're now in the library target).

- [ ] **Step 5: Create the integration test.**

```rust
// crates/superposition_mcp_server/tests/integration.rs
use secrecy::SecretString;
use serde_json::json;
use wiremock::matchers::{header_exists, method, path};
use wiremock::{Mock, MockServer, ResponseTemplate};

use superposition_mcp_server::auth::{AuthValue, SUPERPOSITION_AUTH};
use superposition_mcp_server::build::{build_client, build_router};
use superposition_mcp_server::config::{Config, Defaults, Mode};

async fn router_against(endpoint: &str) -> smithy_mcp_runtime::Router {
    let cfg = Config {
        endpoint: endpoint.to_string(),
        creds: None,
        defaults: Defaults { workspace_id: None, org_id: None },
    };
    let client = build_client(&cfg, Mode::HttpPassthrough);
    build_router(client, cfg.defaults)
}

fn count_operations_in_smithy() -> usize {
    let workspace_root = {
        let mut p = std::env::current_dir().unwrap();
        loop {
            if p.join("smithy/models").exists() && p.join("Cargo.toml").exists() {
                break p;
            }
            p = p.parent().expect("cargo workspace root").to_path_buf();
        }
    };
    let mut count = 0usize;
    for entry in std::fs::read_dir(workspace_root.join("smithy/models")).unwrap() {
        let path = entry.unwrap().path();
        if path.extension().and_then(|s| s.to_str()) == Some("smithy") {
            let body = std::fs::read_to_string(&path).unwrap();
            count += body.matches("@mcpTool").count();
        }
    }
    count
}

#[tokio::test]
async fn tools_list_matches_smithy_annotation_count() {
    let server = MockServer::start().await;
    let router = router_against(&server.uri()).await;
    assert_eq!(
        router.tool_names().len(),
        count_operations_in_smithy(),
        "router tool count must equal @mcpTool annotation count in smithy/models"
    );
}

#[tokio::test]
async fn tool_call_forwards_authorization_header_in_passthrough_mode() {
    let server = MockServer::start().await;

    Mock::given(method("GET"))
        .and(path("/config/fast"))
        .and(header_exists("authorization"))
        .respond_with(
            ResponseTemplate::new(200)
                .insert_header("x-config-version", "1")
                .insert_header("last-modified", "Mon, 11 May 2026 12:00:00 GMT")
                .set_body_json(json!({"hello": "world"})),
        )
        .mount(&server)
        .await;

    let router = router_against(&server.uri()).await;

    let auth = AuthValue::Bearer(SecretString::new("tok-abc".to_string().into()));
    let result = SUPERPOSITION_AUTH
        .scope(auth, async {
            router
                .test_call_tool("GetConfigFast", json!({"workspace_id": "w", "org_id": "o"}))
                .await
        })
        .await;

    let value = result.expect("tool call succeeded");
    assert_eq!(value["hello"], json!("world"));

    let received = server.received_requests().await.expect("received_requests");
    let auth_header = received[0]
        .headers
        .get("authorization")
        .expect("authorization header present")
        .to_str()
        .unwrap();
    assert_eq!(auth_header, "Bearer tok-abc");
}
```

- [ ] **Step 6: Run the integration test.**

```bash
cargo test -p superposition_mcp_server --test integration
```

Expected: both tests pass. If the second test fails because the GetConfigFast operation isn't actually exposed as a tool (rare — would mean the `@mcpTool` annotation was missed on it in Task 3), check `grep '@mcpTool' smithy/models/config.smithy` and fix.

- [ ] **Step 7: Commit.**

```bash
git add crates/superposition_mcp_server/
git commit -m "test(mcp-server): wiremock integration test for tool dispatch and auth passthrough

Refactors build_client / build_router out of main.rs into a lib target
so the integration test can call them directly without duplicating the
~85-tool registration list."
```

---

## Task 14: Local smoke test + final verification

**Files:** none modified

- [ ] **Step 1: Build the binary in release mode.**

```bash
cargo build -p superposition_mcp_server --release
ls -la target/release/superposition-mcp
```

Expected: binary exists. Note its size for the commit message.

- [ ] **Step 2: Run against a local superposition instance via stdio.**

Assumes a local superposition stack is running (`make superposition_dev` or equivalent) and accepts basic auth as `superposition:superposition` (the development default — verify via `keycloak/` config if unsure):

```bash
SUPERPOSITION_ENDPOINT=http://localhost:8080 \
SUPERPOSITION_BASIC_USER=superposition \
SUPERPOSITION_BASIC_PASS=superposition \
SUPERPOSITION_WORKSPACE_ID=test \
SUPERPOSITION_ORG_ID=test \
RUST_LOG=info \
./target/release/superposition-mcp <<'EOF'
{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"smoke","version":"0.0.0"}}}
{"jsonrpc":"2.0","method":"notifications/initialized"}
{"jsonrpc":"2.0","id":2,"method":"tools/list","params":{}}
EOF
```

Expected: stderr shows INFO logs; stdout shows JSON-RPC responses for `initialize` and `tools/list`. The `tools/list` response contains entries.

- [ ] **Step 3: Run the same against HTTP transport.**

```bash
SUPERPOSITION_ENDPOINT=http://localhost:8080 \
RUST_LOG=info \
./target/release/superposition-mcp --http 127.0.0.1:8765 &
HTTP_PID=$!

# In another shell:
curl -s -X POST http://127.0.0.1:8765/mcp \
    -H "Authorization: Basic $(echo -n 'superposition:superposition' | base64)" \
    -H "Accept: application/json, text/event-stream" \
    -H "Content-Type: application/json" \
    -d '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"smoke","version":"0.0.0"}}}'

kill "$HTTP_PID"
```

Expected: HTTP 200 with the initialize response. Without the `Authorization` header, the curl should return HTTP 401.

- [ ] **Step 4: Run the full workspace test suite to catch regressions.**

```bash
make lint
make test
```

Expected: all green. Lint runs against `superposition_mcp_server` (handwritten) but skips `superposition_mcp` (excluded), matching the SDK pattern.

- [ ] **Step 5: Verify CI smithy freshness check passes locally.**

```bash
make smithy-updates
git status
```

Expected: no uncommitted changes after the regeneration. If `crates/superposition_mcp/` shows diff, the patch or the committed crate body is out of sync — re-apply.

- [ ] **Step 6: Final tagging commit.**

If there is anything left to commit (last lint fix, generated-doc tweak), do it now:

```bash
git status
git add <files>
git commit -m "chore(mcp-server): final smoke-test cleanup"
```

Push the branch:

```bash
git push -u origin design/mcp-server
```

Open the PR via `gh pr create` (don't forget to reference the design spec at `docs/superpowers/specs/2026-05-11-superposition-mcp-server-design.md` in the PR body).

---

## Post-implementation follow-ons (not in this plan's scope)

These are tracked separately, **not blockers** for this plan:

1. Publish `smithy-mcp-codegen` to a real Maven repo (juspay sandbox or JitPack); remove `smithy/maven-local/` and `file://` from `SMITHY_MAVEN_REPOS` (spec §11.4).
2. Publish `smithy-mcp-runtime` to crates.io; replace git ref in `superposition_mcp/Cargo.toml` and `superposition_mcp_server/Cargo.toml`.
3. Move HTTP+SSE transport wiring (currently in `transport_http.rs`) upstream into `smithy-mcp-runtime` so the generated `McpServer` exposes a `serve_http(addr)` method analogous to `serve_stdio()`.
4. Add a parameter-interceptor hook to the generator so the per-operation `register!` macro in `main.rs` becomes unnecessary (the generator can take the dispatch wrapper as part of its config).
5. In-process embed of the MCP server inside the main superposition Actix binary (spec §12, option (iii) from brainstorming Q1).
