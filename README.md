# Superposition

<p align="center">
    <img src="https://juspay.io/images/superposition/logo.jpg" alt="Superposition Logo" width="400">
</p>

<p align="center"><b>Superposition is the open-source control plane for context-aware configuration: ship safe config changes and experiment on any value — not just flags — across every dimension of your system.</b></p>

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
    <a href="https://juspay.io/superposition/docs/basic-concepts/context-aware-config"><b>Concepts</b></a>
    ·
    <a href="https://juspay.io/superposition/docs/setup"><b>Self-hosting & setup</b></a>
    ·
    <a href="https://deepwiki.com/juspay/superposition"><b>Architecture guide</b></a>
</p>

Superposition is an open-source platform for context-aware configuration. It helps teams manage typed configuration, resolve values against rich runtime context, and roll out changes safely with experiments layered on top.

Feature flags are one use case, but not the whole model. Superposition treats configuration as the primitive: defaults, dimensions, overrides, and experiments work together so teams can change behavior safely across applications, environments, tenants, regions, devices, and user segments.

If you want to see it working first, start with the local demo below. If you want to understand the model before you run it, jump straight to the docs or architecture wiki.

## Who it is for

Superposition is built primarily for:

- **Platform engineers** who need a safer control plane for multi-tenant, context-heavy configuration.
- **Backend engineers** who own rollout safety, experimentation, and operational decisioning.
- **Product and infrastructure teams** that need more than booleans, but still want a clean OpenFeature-compatible consumption path.

If your team has outgrown static config files, flat feature flags, or brittle rule tables, Superposition is designed for that transition.

## Start in minutes

The fastest way to try Superposition locally is to run the demo image with a preloaded setup:

```sh
docker run -p 8080:8080 ghcr.io/juspay/superposition-demo:latest
```

Then open http://localhost:8080 and explore the admin interface.

If you want to run the repository itself instead of the demo image:

```sh
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

## Why teams use Superposition

- **Treat configuration as a first-class system** instead of attaching JSON payloads to flat flags.
- **Reduce rollout risk** with typed configs, validation hooks, and progressive experimentation.
- **Model real-world context** using dimensions like tenant, geography, device, time, user tier, or domain-specific inputs.
- **Change any value, not just booleans** while keeping application code portable through [OpenFeature](https://openfeature.dev/docs/reference/concepts/provider)-compatible providers.
- **Separate defaults from overrides** so teams can reason about what changes, where it changes, and why.
- **Operate with governance** through multi-tenant isolation, audit-friendly APIs, and approval-aware workflows.

Want the mental model first? Read about [context-aware configuration](https://juspay.io/superposition/docs/basic-concepts/context-aware-config) and [experimentation](https://juspay.io/superposition/docs/basic-concepts/experimentation).

## Our approach

We think configuration should be treated like product logic, not scattered environment variables, one-off switch statements, or oversized flag trees.

Superposition is designed around a few simple ideas:

- **Safety should be built in**: strong typing via [JSON Schema](https://json-schema.org/), custom validation functions, and controlled rollout mechanics help prevent bad changes from spreading.
- **Specificity should be explicit**: configuration can cascade from broad defaults to precise contexts, much like CSS, so teams can understand why a value resolved the way it did.
- **Experimentation should be operational**: experiments are not an afterthought bolted onto flags; they are part of the same workflow used to introduce, validate, and promote changes.
- **Configuration comes before flags**: feature flags are a subset of configuration, not the other way around.
- **Consumption should stay portable**: applications integrate through SDKs and OpenFeature providers instead of coupling tightly to one internal API shape.

If that matches how your team works, the [docs](https://juspay.io/superposition/docs/quick_start) are the best next step.

## What you can build with it

Superposition fits anywhere you need configuration to change safely without redeploying application code.

- **Frontend experiences** with dynamic behavior, staged rollouts, and context-aware UI variants.
- **Backend services** with tenant-aware, region-aware, or policy-aware configuration.
- **Payments and fintech decisioning** such as routing, retry ladders, fee logic, or risk thresholds.
- **LLM and AI application configuration** where model, prompt, temperature, or retrieval behavior changes by context.
- **Infrastructure control paths** such as staggered releasers and operational toggles.
- **Storage and platform layers** that need centrally managed dynamic behavior.

Examples in this repository:

- [Dynamic payment fields](examples/dynamic-payment-fields)
- [K8s staggered releaser](examples/k8s-staggered-releaser)
- [CAC Redis module](examples/cac_redis_module)

Prefer examples over theory? Browse the [examples directory](examples) and then wire in a client.

## Why this is different from a flag service

Most flag systems start with booleans and add targeting on top. Superposition starts with context-aware configuration and treats feature flags as one outcome of that model.

That means you can:

- define typed default values,
- resolve them against rich multi-dimensional context,
- override them with predictable specificity,
- and run controlled experiments on top of those values.

If you are comparing tools, that is the lens to use: Superposition is best understood as context-aware configuration first, with feature flags and experimentation built on top.

## Clients and providers

Superposition ships two integration surfaces:

1. **SDK** — use this to interact with the control plane and automate configuration and experimentation workflows.
2. **Provider** — use this OpenFeature-compatible client in applications that need to consume configuration and experiment assignments.

| Language       | sdk | provider |
|----------------|-----|----------|
| Rust           | [![Crates.io Version](https://img.shields.io/crates/v/superposition_sdk?color=green&label=superposition_sdk)](https://crates.io/crates/superposition_sdk) | [![Crates.io Version](https://img.shields.io/crates/v/superposition_provider?color=green&label=superposition_provider)](https://crates.io/crates/superposition_provider) |
| Javascript     | [![NPM Version](https://img.shields.io/npm/v/superposition-sdk?color=green&label=superposition-sdk)](https://www.npmjs.com/package/superposition-sdk) | [![NPM Version](https://img.shields.io/npm/v/superposition-provider?color=green&label=superposition-provider)](https://www.npmjs.com/package/superposition-provider) |
| Python         | [![PyPI - Version](https://img.shields.io/pypi/v/superposition_sdk?color=green&label=superposition_sdk)](https://pypi.org/project/superposition-sdk/) | [![PyPI - Version](https://img.shields.io/pypi/v/superposition_provider?color=green&label=superposition_provider)](https://pypi.org/project/superposition-provider/) |
| Java           | [![Maven Central Version](https://img.shields.io/maven-central/v/io.juspay.superposition/sdk?label=io.juspay.superposition.sdk&color=green)](https://central.sonatype.com/artifact/io.juspay.superposition/sdk) | [![Maven Central Version](https://img.shields.io/maven-central/v/io.juspay.superposition/openfeature-provider?label=io.juspay.superposition.openfeature-provider&color=green)](https://central.sonatype.com/artifact/io.juspay.superposition/openfeature-provider) |
| Haskell        | WIP | WIP |
| Go             | TBD | TBD |

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
- [Context-aware configuration concepts](https://juspay.io/superposition/docs/basic-concepts/context-aware-config)
- [Experimentation concepts](https://juspay.io/superposition/docs/basic-concepts/experimentation)
- [Context7 LLM-friendly docs](https://context7.com/juspay/superposition)
- [DeepWiki repository guide](https://deepwiki.com/juspay/superposition)

## Contributing

We welcome contributions across the platform, clients, docs, and examples.

Start with the [development setup docs](https://juspay.io/superposition/docs/setup). If you'd like help getting started, join the [Discord community](https://discord.gg/jNeUJR9Bwr) or email [superposition@juspay.in](mailto:superposition@juspay.in).

## License

This repository is distributed under the Apache 2.0 license.

See the [LICENSE](LICENSE) file for the full terms and repository-specific details.
