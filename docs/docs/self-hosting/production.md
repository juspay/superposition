---
title: Production
description: Production guidance for self-hosted Superposition deployments.
---

# Production

Use this page as the operational checklist for a production-grade
self-hosted deployment.

## Deployment Model

- Run the official image `ghcr.io/juspay/superposition:latest`.
- Put the app behind HTTPS before exposing it outside a private network.
- Keep PostgreSQL shared across all replicas.
- Use a shared Redis instance if you enable `REDIS_URL` and run multiple replicas.
- Set a unique `WORKER_ID` for each running replica.

The application is mostly stateless, so horizontal scaling is straightforward as
long as replicas share the same PostgreSQL database and Redis cache.

## Security

- Do not expose deployments with `AUTH_PROVIDER=DISABLED` to the public
  internet.
- Prefer OIDC for authentication on public or team-facing environments.
- Use Casbin if you need workspace and organisation-level authorization rules.
- Set `MASTER_ENCRYPTION_KEY` before creating workspaces if you plan to use
  encrypted secrets.
- Store production secret material in AWS KMS for `APP_ENV=PROD`, or in your
  platform secret manager if you are running a plain-env self-hosted mode.

## Database And Networking

- Create an empty PostgreSQL database and initialize the global schema before
  starting the app:

```bash
export DB_USER=superposition
export DB_PASSWORD='<database-password>'
export DB_HOST='postgres.example.com:5432'
export DB_NAME=superposition
export DATABASE_URL="postgres://${DB_USER}:${DB_PASSWORD}@${DB_HOST}/${DB_NAME}"
psql "$DATABASE_URL" -f superposition.sql
```

- Initialize the global schema with `superposition.sql` before starting a new
  deployment.
- Run the latest `superposition.sql` against the target database before an
  upgrade.
- Keep regular PostgreSQL backups and test restore procedures.
- Size `MAX_DB_CONNECTION_POOL_SIZE * replicas` to fit your PostgreSQL capacity.
- Treat Redis as an optimization, not a source of truth.

## Upgrades

Use this sequence for production upgrades:

1. Back up PostgreSQL.
2. Review release notes for migration or environment changes.
3. Run the latest `superposition.sql` against the target database.
4. Roll out the new Superposition image.
5. Check `GET /health`.
6. For existing workspaces, run the workspace schema migration endpoint when the
   release changes `workspace_template.sql`.

Workspace schema migration example:

```bash
curl -X POST "https://superposition.example.com/workspaces/<workspace>/db/migrate" \
  -H "x-org-id: <organisation-id>"
```

Repeat the workspace migration for each workspace that should receive the new
template changes.

## Health And Operations

- Use `GET /health` for load balancer and readiness checks.
- Expect structured JSON logs through `RUST_LOG` and the tracing subscriber.
- If you use multiple replicas, verify each replica gets a unique `WORKER_ID`.
- Monitor PostgreSQL, Redis, and app pool saturation separately.
- Keep the admin UI and API on the same deployment surface unless you are
  intentionally fronting them with separate infrastructure.

For Helm-based cluster rollouts, see [Kubernetes](./kubernetes.md).
