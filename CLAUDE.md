# Superposition

Feature flagging and experimentation platform by Juspay. Rust workspace with a
Leptos (WASM) frontend and multi-language client SDKs (Java, Python, JS, Haskell, Rust).

## Quick Reference

| Action | Command |
|--------|---------|
| Build frontend + backend | `make build` |
| Run server (dev) | `make run` |
| Run full test suite | `make test CI=1` |
| Formatting + lint check | `make check` |
| Run **all** CI checks locally | `make ci` |

## CI Mirror Targets

These make targets mirror the GitHub Actions jobs in `.github/workflows/ci_check_pr.yaml`
exactly, so local runs match CI behavior.

| CI Job | Local Command | What it does |
|--------|---------------|-------------|
| formatting | `make ci-formatting` | `make check` + `cog verify` on last commit |
| test | `make ci-test` | Full test suite with `CI=1` |
| java-build | `make ci-java-build` | `./gradlew assemble` in `clients/java` |
| provider-tests | `make ci-provider-tests` | All four provider SDKs (kotlin, js, py, rust) |
| binding-generation-check | `make ci-binding-check` | `make uniffi-bindings` + verify no diff |
| smithy-sdk-generation-check | `make ci-smithy-check` | `make smithy-updates` + verify no diff |

Run `make ci` to execute all of the above sequentially.

## Prerequisites

- **Rust 1.90.0** — managed via `rust-toolchain.toml` (includes wasm32-unknown-unknown, rustfmt, clippy)
- **Node.js 18+**, **Bun**, **wasm-pack**
- **JDK 17** (for Java/Kotlin SDK builds)
- **Python 3.12** + **uv** (for Python provider tests)
- **Haskell** GHC 9.6.7, Cabal 3.16 (for Haskell bindings tests)
- **PostgreSQL 15** on port 5432 (password: `docker`, db: `config`)
- **Redis 8** on port 6379
- **Smithy CLI 1.55.0** (for smithy generation checks)
- **cocogitto** (for conventional commit verification)
- **leptosfmt 0.1.33** (for Leptos template formatting)

Start services locally: `docker compose up -d postgres redis`

## Code Style

- Rust formatting: `cargo fmt` + `leptosfmt` (checked via `make check`)
- Clippy lints treated as errors in CI (`-Dwarnings`)
- Conventional commits enforced by cocogitto — use `cog verify` before pushing

## Testing

- **Unit/integration tests**: `cargo test`
- **E2E tests**: `cd tests && bun test:clean` (requires running server)
- **Bindings tests**: `make bindings-test` (Python, JS, Java/Kotlin, Haskell)
- **Provider SDK tests**: `make test-{js,py,kotlin,rust}-provider`
- The `make test` target orchestrates: setup → build → start server → cargo test → e2e → bindings → kill

## Project Layout

```
crates/                  Rust workspace crates
  superposition/         Main server binary (Actix-Web + Leptos SSR)
  frontend/              Leptos WASM frontend
  context_aware_config/  Core context-aware config engine
  experimentation_platform/ Experimentation engine
  superposition_core/    Core library (UniFFI bindings)
  superposition_types/   Shared types + DB schema (Diesel)
  superposition_sdk/     Generated Rust SDK (Smithy)
clients/
  java/                  Java/Kotlin SDK + bindings + provider
  python/                Python SDK + bindings + provider
  javascript/            JS/TS SDK + bindings + provider
  haskell/               Haskell SDK + bindings
smithy/                  Smithy API models → generates all SDKs
tests/                   Bun E2E tests
```

## Environment

Copy `.env.example` to `.env` for local development. Key vars:
- `DATABASE_URL` — Postgres connection string
- `APP_ENV` — set to `TEST` for CI, `DEV` for local
- `AWS_*` — mock credentials for LocalStack
- `REDIS_URL` — Redis connection
