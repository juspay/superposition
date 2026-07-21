---
title: Kronos
description: Choose embedded library mode or a standalone Kronos service for durable webhook delivery.
---

# Kronos

Superposition uses [Kronos](https://github.com/juspay/kronos) to deliver
webhooks durably and retry failed deliveries. You can run it as an embedded
library or as a separate service.

| Mode | Best for | How it runs |
| --- | --- | --- |
| Library (default) | Small and simple deployments | An embedded worker runs inside every Superposition process and stores its tables in the Superposition PostgreSQL database. |
| Service | Independent scaling, isolation, and centralized operations | Superposition calls a separately deployed Kronos API; Kronos runs and stores its jobs independently. |

## Library Mode

Library mode is selected when `KRONOS_URL` is not set. It needs no additional
service or database, so it is the recommended starting point.

```bash
# Do not set KRONOS_URL.
SUPERPOSITION_HOST=https://superposition.example.com
KRONOS_DISPATCH_TOKEN='<random-callback-token>'
KRONOS_ENCRYPTION_KEY='<64-hex-character-key>'
KRONOS_DB_POOL_SIZE=1
KRONOS_TABLE_PREFIX=kronos_
```

Generate the encryption key with `openssl rand -hex 32`. Keep this key stable
 and secret, and use the same value on every Superposition replica. If it is
 unset, Superposition currently defaults to 64 `0` characters, which is not
 suitable for production. The pool size is per replica; its default is `1`.

The embedded worker uses the existing Superposition database connection
settings. Kronos tables are created in each workspace schema. Their prefix
defaults to `kronos_` and can be changed with `KRONOS_TABLE_PREFIX`.

## Service Mode

Use service mode when you want to operate the Kronos API, worker, and database
separately from Superposition.

1. Deploy the Kronos API, worker, and PostgreSQL with `pg_cron` by following the
   [Kronos deployment instructions](https://github.com/juspay/kronos#quickstart).
2. Create the Kronos organisation that Superposition will use.
3. Configure Superposition:

```bash
KRONOS_URL=http://kronos:8080/kronos
KRONOS_API_KEY='<kronos-api-key>'
KRONOS_ORG_ID='<existing-kronos-organisation-id>'
KRONOS_WORKSPACE=superposition

SUPERPOSITION_HOST=https://superposition.example.com
KRONOS_DISPATCH_TOKEN='<random-callback-token>'
```

`KRONOS_URL` is the Kronos API base URL, including its path prefix when one is
configured, but without `/v1`. Setting it switches Superposition to service
mode. The Kronos organisation must already exist; Superposition creates the
shared workspace, callback secret, and dispatcher endpoint if needed. The
workspace value must contain only lowercase letters, digits, and hyphens and
must be no longer than 25 characters.

## Settings Shared By Both Modes

- `SUPERPOSITION_HOST` must be reachable from Kronos and include
  `SERVICE_PREFIX` when one is configured. Kronos posts deliveries back to
  `<SUPERPOSITION_HOST>/dispatch/webhook`. If unset, it falls back to
  `CAC_HOST`.
- `KRONOS_DISPATCH_TOKEN` authenticates only that callback. Use a random value
  that is different from `SUPERPOSITION_TOKEN`.
- In `DEV` and `TEST`, the callback token and service API key are read directly
  from the environment. In `PROD` and `SANDBOX`, provide their AWS KMS
  ciphertexts. `KRONOS_ENCRYPTION_KEY` is always read directly from the
  environment, so inject it with your deployment secret manager.

After startup, check the Superposition logs for either
`Kronos library mode: embedded worker started` or `Kronos service mode` to
confirm the selected mode.
