---
sidebar_position: 9
title: Import & Export
description: Round-trip a whole workspace as a SuperTOML or JSON file
---

# Import & Export

Superposition can serialize an entire workspace's configuration —
default-configs (with their schemas), dimensions and overrides — into a single
SuperTOML or JSON document, and read that same document back in. This makes it
easy to back up a workspace, copy config between workspaces, review changes as a
file diff, or manage configuration as code.

Each format has an export endpoint and an import endpoint:

| Format | Export              | Import                     |
| ------ | ------------------- | -------------------------- |
| TOML   | `GET /config/toml`  | `POST /config/toml/import` |
| JSON   | `GET /config/json`  | `POST /config/json/import` |

All requests carry the usual workspace headers, `x-workspace: <workspace>` and
`x-org-id: <org>` (plus `Authorization`).

## Exporting

```bash
# TOML
curl -X GET https://<host>/config/toml \
  -H "x-workspace: <workspace>" -H "x-org-id: <org>" > config.toml

# JSON
curl -X GET https://<host>/config/json \
  -H "x-workspace: <workspace>" -H "x-org-id: <org>" > config.json
```

The response body is a complete SuperTOML / JSON document — see
[Format Specification](./format-specification) for its shape.

## Importing

POST the file to the matching `/import` endpoint. The body is parsed and **fully
validated** (schemas, dimension positions, cohort relationships, and every
context condition / override) before anything is written. The whole import runs
in a single database transaction.

```bash
curl -X POST https://<host>/config/toml/import \
  -H "x-workspace: <workspace>" -H "x-org-id: <org>" \
  -H "Content-Type: application/toml" \
  --data-binary @config.toml
```

On success you get a JSON summary of what changed:

```json
{
  "mode": "merge",
  "dry_run": false,
  "config_version": "7231...",
  "dimensions":      { "created": 2, "updated": 1, "skipped": 0, "deleted": 0 },
  "default_configs": { "created": 5, "updated": 0, "skipped": 0, "deleted": 0 },
  "contexts":        { "created": 3, "updated": 1, "skipped": 0, "deleted": 0 }
}
```

### Import options

Behaviour is controlled with request headers:

| Header                 | Values                  | Default | Effect                                                                                                   |
| ---------------------- | ----------------------- | ------- | -------------------------------------------------------------------------------------------------------- |
| `x-import-mode`        | `merge` \| `replace`    | `merge` | `merge` upserts what's in the file and leaves everything else alone. `replace` additionally **deletes** any dimension/default-config/context that is _not_ in the file (mirror the file exactly). |
| `x-import-overwrite`   | `true` \| `false`       | `true`  | When `false`, entities that already exist are **skipped** (only new ones are created).                   |
| `x-import-on-error`    | `abort` \| `continue`   | `abort` | `abort` rolls the whole import back on the first error. `continue` applies everything that's valid and returns the per-entity errors in the summary. |
| `x-import-dry-run`     | `true` \| `false`       | `false` | Parse, validate and compute the summary **without writing anything**. Great for previewing an import.    |
| `x-import-value-merge` | `true` \| `false`       | `false` | For object-valued default configs, deep-merge with the existing value instead of replacing it wholesale. |

When `x-import-on-error: continue` is used, failed entities appear under an
`errors` array in the relevant section of the summary:

```json
"default_configs": {
  "created": 4, "updated": 0, "skipped": 0, "deleted": 0,
  "errors": [{ "id": "per_km_rate", "error": "..." }]
}
```

### Tips

- Use `x-import-dry-run: true` first to see exactly what an import would do.
- Use `x-import-mode: replace` to make a workspace _exactly_ match a file
  (e.g. restoring from a backup). Use the default `merge` to layer a file on top
  of existing config without removing anything.
- Use `x-import-overwrite: false` to seed only the keys/dimensions/contexts that
  don't exist yet, without touching anything already configured.
- `x-config-tags` is honoured and recorded against the config version the import
  creates, just like other write endpoints.
- The endpoints are available in the generated SDKs as the `ImportConfigToml`
  and `ImportConfigJson` operations.

:::note
Value-validation and value-compute function bindings are not part of the
exported file, so they are not set by import. Bind functions to dimensions and
default-configs through their dedicated APIs after importing if needed.
:::

:::caution
The imported file is validated as a self-contained config (every context only
references dimensions/keys defined in the same file), and dimension **positions**
are taken verbatim from the file. In `merge` mode this means the file's positions
should be consistent with the dimensions already in the workspace. To avoid
position clashes entirely, export the workspace, edit, and import back — or use
`replace` mode, which makes the workspace match the file exactly.
:::
