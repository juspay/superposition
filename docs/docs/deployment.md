---
title: Overview
description: Self-host Superposition with PostgreSQL, optional Redis, and the official container image.
slug: /self-hosting
---

# Self-Hosting Superposition

Superposition runs as a single HTTP service that serves both the API and the
admin UI. It stores global metadata and workspace data in PostgreSQL, and can
use Redis as a shared cache when `REDIS_URL` is configured.

Use `ghcr.io/juspay/superposition:latest` for self-hosted deployments. The
image `ghcr.io/juspay/superposition-demo:latest` is only for local evaluation
with preloaded sample data.

## What You Need

- A PostgreSQL database that Superposition can create schemas and tables in.
- A container runtime or process manager for the app.
- `psql` for bootstrapping the global schema with `superposition.sql`.
- A DNS name and TLS-terminating proxy for public deployments.

## Quick Start

This example uses PostgreSQL for persistence, Redis for caching, and
`superposition.sql` to initialize the global schema automatically on first boot.

```yaml
services:
  postgres:
    image: postgres:16-alpine
    environment:
      POSTGRES_USER: superposition
      POSTGRES_PASSWORD: superposition
      POSTGRES_DB: superposition
    volumes:
      - postgres_data:/var/lib/postgresql/data
      - ./superposition.sql:/docker-entrypoint-initdb.d/001-superposition.sql:ro

  redis:
    image: redis:7
    command: redis-server
    restart: unless-stopped

  superposition:
    image: ghcr.io/juspay/superposition:latest
    depends_on:
      - postgres
      - redis
    ports:
      - "8080:8080"
    restart: unless-stopped
    environment:
      PORT: "8080"
      APP_ENV: DEV
      DB_USER: superposition
      DB_PASSWORD: superposition
      DB_HOST: postgres:5432
      DB_NAME: superposition
      SERVICE_PREFIX: ""
      CAC_HOST: http://localhost:8080
      API_HOSTNAME: http://localhost:8080
      SUPERPOSITION_VERSION: self-hosted
      WORKER_ID: "1"
      RUST_LOG: info,superposition=info,service_utils=info
      MAX_DB_CONNECTION_POOL_SIZE: "10"
      ACTIX_KEEP_ALIVE: "120"
      ALLOW_SAME_KEYS_OVERLAPPING_CTX: "true"
      ALLOW_DIFF_KEYS_OVERLAPPING_CTX: "true"
      ALLOW_SAME_KEYS_NON_OVERLAPPING_CTX: "true"
      TENANT_MIDDLEWARE_EXCLUSION_LIST: "/health,/assets/favicon.ico,/pkg/frontend.js,/pkg,/pkg/frontend_bg.wasm,/pkg/tailwind.css,/pkg/style.css,/assets,/admin,/oidc/login,/admin/organisations,/organisations,/organisations/switch/{organisation_id},/"
      AUTH_PROVIDER: DISABLED
      AUTH_Z_PROVIDER: DISABLED
      REDIS_URL: redis://redis:6379

volumes:
  postgres_data:
```

Start the stack:

```bash
docker compose up -d
curl http://localhost:8080/health
```

Expected response:

```text
Health is good :D
```

## Runtime Architecture

| Component | Required | Notes |
| --- | --- | --- |
| Superposition app | Yes | Runs the Rust API server and bundled admin UI on `PORT`, default `8080`. |
| PostgreSQL | Yes | Stores organisations, workspace metadata, and one schema per workspace. |
| Redis | No | Enables shared caching. If `REDIS_URL` is unset, the app reads from PostgreSQL directly. |
| OIDC provider | Recommended | Required when using `AUTH_PROVIDER=OIDC` or `AUTH_PROVIDER=OIDC_SAAS`. |
| Casbin policy DB | Optional | Required only when `AUTH_Z_PROVIDER=CASBIN`; it can use the same PostgreSQL instance. |
| AWS KMS | Required for `APP_ENV=PROD` | In production mode, secret values such as database passwords are read through KMS. |

The app is mostly stateless. Multiple replicas can run behind a load balancer as
long as they share the same PostgreSQL database and, if enabled, the same Redis
instance.

## First Organisation And Workspace

After the service is healthy, open the admin UI:

```text
http://localhost:8080/admin/organisations
```

For an auth-disabled private deployment, you can also bootstrap through the API:

```bash
curl -X POST http://localhost:8080/superposition/organisations \
  -H 'Content-Type: application/json' \
  -d '{"name":"Acme","admin_email":"admin@example.com"}'
```

Use the returned organisation `id` to create a workspace:

```bash
curl -X POST http://localhost:8080/workspaces \
  -H 'Content-Type: application/json' \
  -H 'x-org-id: <organisation-id>' \
  -d '{"workspace_name":"production","workspace_admin_email":"admin@example.com"}'
```

Workspace APIs require these headers:

- `x-org-id`: organisation ID
- `x-workspace`: workspace name

See [Organisations](./basic-concepts/administration/organisations.md) and
[Workspaces](./basic-concepts/administration/workspaces.md) for the data model.

## Next Steps

- [Environment Variables](./self-hosting/environment-variables.md) for runtime
  configuration, auth, and secret loading.
- [Production](./self-hosting/production.md) for operational guidance, scaling,
  and upgrades.
- [Kubernetes](./self-hosting/kubernetes.md) for Helm-based cluster
  deployments.
