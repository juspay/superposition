# Superposition

<p align="center">
<img src="https://juspay.io/images/superposition/logo.jpg" alt="Superposition Logo" width="400">
</p>

<p align="center">
<a href="https://github.com/juspay/superposition/blob/main/LICENSE"><img src="https://img.shields.io/github/license/juspay/superposition" alt="License" height="22"/></a>
<a href="https://github.com/juspay/superposition/releases"><img src="https://img.shields.io/badge/dynamic/toml?url=https%3A%2F%2Fraw.githubusercontent.com%2Fjuspay%2Fsuperposition%2Frefs%2Fheads%2Fmain%2FCargo.toml&query=workspace.package.version&label=version&color=2ea44f" alt="Version" height="22"/></a>
<a href="https://github.com/juspay/superposition/actions/workflows/release.yaml"><img src="https://img.shields.io/github/actions/workflow/status/juspay/superposition/release.yaml" alt="Release Workflow" height="22"/></a>
<a href="https://context7.com/juspay/superposition"><img src="https://img.shields.io/badge/Context7-LLM%20Docs-7c3aed" alt="Context7" height="22"/></a>
<a href="https://deepwiki.com/juspay/superposition"><img src="https://img.shields.io/badge/DeepWiki-Architecture%20Wiki-0ea5e9" alt="DeepWiki" height="22"/></a>
<a href="https://discord.gg/jNeUJR9Bwr"><img src="https://img.shields.io/discord/1280216553350107258?label=Discord&logo=discord" alt="Discord" height="22"/></a>
</p>

<p align="center">
<a href="https://github.com/juspay/superposition/stargazers"><img src="https://img.shields.io/github/stars/juspay/superposition" alt="GitHub Stars" height="22"/></a>
<a href="https://github.com/juspay/superposition/network/members"><img src="https://img.shields.io/github/forks/juspay/superposition" alt="GitHub Forks" height="22"/></a>
<a href="https://github.com/juspay/superposition/issues"><img src="https://img.shields.io/github/issues/juspay/superposition" alt="GitHub Issues" height="22"/></a>
<a href="https://github.com/juspay/superposition/commits/main"><img src="https://img.shields.io/github/last-commit/juspay/superposition" alt="Last Commit" height="22"/></a>
</p>

<p align="center">
<a href="https://juspay.io/superposition/docs/quick_start"><b>Quick start</b></a>
·
<a href="https://juspay.io/superposition/docs/category/context-aware-config"><b>Concepts</b></a>
·
<a href="https://juspay.io/superposition/docs/setup"><b>Self-hosting & setup</b></a>
·
<a href="https://deepwiki.com/juspay/superposition"><b>Architecture guide</b></a>
</p>

<p align="center"><b>Superposition is an open source configuration management platform that simplifies and enables safe, reliable configuration for applications.</b></p>

Superposition helps teams manage configuration and experimentation in a smarter way. Instead of hardcoding values or juggling between messy config files, it lets you define defaults, set contexts/overrides (Rules) for different dimensions (like per-environment, tenant, region, or device), and safely roll out changes through experiments.

Think of it as a system where your configuration is aware of context: the right settings automatically apply to the right situation, with full type safety and controlled rollouts. Feature flags are just one thing you can do with it, not the main idea.

If you want to see it working first, start with the local demo below. If you want to understand the model before you run it, jump straight to the docs or architecture wiki.

## Start in minutes

The fastest way to try Superposition locally is to run the demo image with a preloaded setup:

```bash
docker run -p 8080:8080 ghcr.io/juspay/superposition-demo:latest
```

Then open [http://localhost:8080](http://localhost:8080/) and explore the admin interface.

If you want to run the repository itself instead of the demo image:

```bash
git clone https://github.com/juspay/superposition.git
cd superposition
# Optional: enter the Nix dev shell if you use Nix
nix develop

# Start local dependencies (Postgres/Redis)
podman compose up -d

# Set up the dev environment and run Superposition
make setup
make run
```

`docker compose up -d` only starts local dependencies from `docker-compose.yaml`; it does not start the Superposition UI/API on `:8080`. Use `make run` to launch the server after setup.

If you are not using Nix, follow the dependency installation steps in the setup guide before running `make setup`.

Need the guided path? Start with the [quick start guide](https://juspay.io/superposition/docs/quick_start) or read the [setup docs](https://juspay.io/superposition/docs/setup).

## Why should you use Superposition

- **Safety**: use typed values, schema checks, validation functions, staggered rollouts, and versioned configs that you can revert to.
- **Reliability**: change configurations through experiments, overrides, and controlled promotion flows instead of shipping every change through application deploys.
- **Easier management**: use cascading configuration to avoid duplicating overrides for every variation across environment, tenant, region, device, or other dimensions.
- **Auditability**: see who changed what, when it changed, why it changed, and the description attached to the change.
- **Integrations**: connect through OpenFeature-compatible providers, SDKs, APIs, webhooks, and client libraries.

Superposition treats configuration like product logic, not scattered environment variables, one-off switch statements, or oversized feature flag trees. Read more about [context-aware configuration](https://juspay.io/superposition/docs/category/context-aware-config) and [experimentation](https://juspay.io/superposition/docs/category/experimentation).

## What you can build with it

Superposition fits anywhere you need configuration to change safely without redeploying application code.

- **Frontend experiences** with dynamic behavior, staged rollouts, and context-aware UI variants.
- **Backend services** with tenant-aware, region-aware, or policy-aware configuration.
- **Payments and fintech decisioning** such as routing, retry ladders, fee logic, or risk thresholds.
- **LLM and AI application configuration** where model, prompt, temperature, or retrieval behavior changes by context.
- **Infrastructure control paths** such as staggered releasers and operational toggles.
- **Storage and platform layers** that need centrally managed dynamic behavior.

Examples in this repository:

- [Dynamic payment fields](https://github.com/juspay/superposition/tree/main/examples/dynamic-payment-fields)
- [K8s staggered releaser](https://juspay.io/superposition/docs/applications/k8s-staggered-releaser)
- [CAC Redis module](https://juspay.io/superposition/docs/applications/cac-redis-module)

Prefer examples over theory? Browse the [examples directory](https://github.com/juspay/superposition/tree/main/examples) and then wire in a client.

## Why this is different from a feature flag service

Most feature flag systems start with booleans and add targeting on top. Superposition starts with context-aware configuration and treats feature flags as one outcome of that model.

That means you can:

- define typed default values,
- resolve them against rich multi-dimensional context,
- override them with predictable specificity,
- and run controlled experiments on top of those values.

If you are comparing tools, that is the lens to use: Superposition is best understood as context-aware configuration first, with feature flags and experimentation built on top.

## Clients and providers

Superposition ships two integration surfaces:

1. **SDK**: use this to interact with the control plane and automate configuration and experimentation workflows.
2. **Provider**: use this OpenFeature-compatible client in applications that need to consume configuration and experiment assignments.

| Language   | SDK | Provider |
|------------|-----|----------|
| Rust       | [![Crates.io Version](https://img.shields.io/crates/v/superposition_sdk?color=green&label=superposition_sdk)](https://crates.io/crates/superposition_sdk) | [![Crates.io Version](https://img.shields.io/crates/v/superposition_provider?color=green&label=superposition_provider)](https://crates.io/crates/superposition_provider) |
| JavaScript | [![NPM Version](https://img.shields.io/npm/v/superposition-sdk?color=green&label=superposition-sdk)](https://www.npmjs.com/package/superposition-sdk) | [![NPM Version](https://img.shields.io/npm/v/superposition-provider?color=green&label=superposition-provider)](https://www.npmjs.com/package/superposition-provider) |
| Python     | [![PyPI - Version](https://img.shields.io/pypi/v/superposition_sdk?color=green&label=superposition_sdk)](https://pypi.org/project/superposition-sdk/) | [![PyPI - Version](https://img.shields.io/pypi/v/superposition_provider?color=green&label=superposition_provider)](https://pypi.org/project/superposition-provider/) |
| Java       | [![Maven Central Version](https://img.shields.io/maven-central/v/io.juspay.superposition/sdk?label=io.juspay.superposition.sdk&color=green)](https://central.sonatype.com/artifact/io.juspay.superposition/sdk) | [![Maven Central Version](https://img.shields.io/maven-central/v/io.juspay.superposition/openfeature-provider?label=io.juspay.superposition.openfeature-provider&color=green)](https://central.sonatype.com/artifact/io.juspay.superposition/openfeature-provider) |
| Haskell    | [![Haskell SDK](https://img.shields.io/badge/source-SuperpositionSDK-green)](https://github.com/juspay/superposition/tree/main/clients/haskell/sdk) | [![Haskell Provider](https://img.shields.io/badge/source-superposition--open--feature--provider-green)](https://github.com/juspay/superposition/tree/main/clients/haskell/open-feature-provider) |
| Go         | TBD | TBD |

Ready to integrate? Use the [quick start guide](https://juspay.io/superposition/docs/quick_start) and choose the SDK or provider that matches your application.

## Key capabilities

- **Admin UI** for managing configurations, contexts, dimensions, experiments, and webhooks.
- **Rich API support** so every major workflow can be automated outside the UI.
- **Safe configuration changes** through validation, auditability, and experiment-driven rollout.
- **Typed values and custom validators** for stronger guarantees at change time.
- **Multi-tenant isolation** so multiple organizations and workspaces can coexist safely.
- **Authentication and authorization controls** to keep operational changes governed.

Want a broader systems view? Open the [DeepWiki architecture guide](https://deepwiki.com/juspay/superposition).

## Learn more

- [Quick start](https://juspay.io/superposition/docs/quick_start)
- [Development setup](https://juspay.io/superposition/docs/setup)
- [Context-aware configuration concepts](https://juspay.io/superposition/docs/category/context-aware-config)
- [Experimentation concepts](https://juspay.io/superposition/docs/category/experimentation)
- [Context7 LLM-friendly docs](https://context7.com/juspay/superposition)
- [DeepWiki repository guide](https://deepwiki.com/juspay/superposition)

## Metrics & observability

The HTTP API exposes Prometheus metrics on `SUPERPOSITION_METRICS_PORT` (default `9091`):

```
curl http://localhost:9091/metrics
```

Health endpoints live on the main port: `GET /healthz`, `/livez`, `/readyz`.

For full details (labels, cardinality, OTLP push), see
[`docs/superpowers/specs/2026-05-10-otel-golden-signals-middleware-design.md`](docs/superpowers/specs/2026-05-10-otel-golden-signals-middleware-design.md).

**Note on `tokio_unstable`.** The workspace's `.cargo/config.toml` enables
`--cfg tokio_unstable` so `tokio-metrics` can collect runtime saturation. This
flag only adds APIs; no behavioural change for existing code. Contributors who
build outside `cargo` (e.g., custom IDE invocations) should pass the same flag,
or accept that the `runtime.tokio.*` metrics will be absent.

## Contributing

We welcome contributions across the platform, clients, docs, and examples.

Start with the [development setup docs](https://juspay.io/superposition/docs/setup). If you'd like help getting started, join the [Discord community](https://discord.gg/jNeUJR9Bwr) or email [superposition@juspay.in](mailto:superposition@juspay.in).

## License

This repository is distributed under the Apache 2.0 license.

See the [LICENSE](https://github.com/juspay/superposition/blob/main/LICENSE) file for the full terms and repository-specific details.
