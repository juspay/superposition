---
sidebar_position: 2
title: Workspaces
description: Isolated configuration environments within an organisation
---

Workspaces are isolated configuration environments within an [organisation](./organisations.md). Each workspace has its own PostgreSQL schema, providing complete data isolation while sharing the same Superposition deployment.

## Workspace Properties

| Property | Type | Description |
|----------|------|-------------|
| `workspace_name` | String | Unique identifier within the organisation (max 25 chars) |
| `organisation_id` | String | Parent organisation ID |
| `workspace_schema_name` | String | PostgreSQL schema name (`{org_id}_{workspace_name}`) |
| `workspace_status` | Enum | `ENABLED` or `DISABLED` |
| `workspace_admin_email` | String | Email of the workspace administrator |
| `mandatory_dimensions` | String[] | Dimensions required in every context |
| `config_version` | String | Current configuration version (optional) |
| `metrics` | Object | Metrics integration configuration |
| `allow_experiment_self_approval` | Boolean | Allow users to approve their own experiments |
| `auto_populate_control` | Boolean | Auto-populate control variants in experiments |
| `enable_context_validation` | Boolean | Enable context validation functions |
| `enable_change_reason_validation` | Boolean | Enable change reason validation |

## Workspace Status

- **ENABLED** — Workspace is active and accessible
- **DISABLED** — Workspace is disabled; configuration APIs return errors

## Schema Isolation

Each workspace gets its own PostgreSQL schema containing:

```
{org_id}_{workspace_name}/
├── dimensions          — Dimension definitions
├── default_configs     — Default configuration values
├── contexts            — Context rules with conditions
├── overrides           — Context-specific config overrides
├── experiments         — A/B experiment definitions
├── event_log           — Audit trail for all changes
├── functions           — Validation and compute functions
├── type_templates      — Reusable type definitions
├── webhooks            — Webhook configurations
└── secrets             — Encrypted secret values
```

This isolation ensures that changes in one workspace never affect another.

## Creating a Workspace

When a workspace is created:

1. A PostgreSQL schema is created using the workspace template
2. All required tables and triggers are set up
3. The workspace is cached in Redis for fast access
4. Authorization policies are configured for the workspace admin

### Workspace Naming Rules

- Maximum 25 characters
- Only alphanumeric characters, underscores, and hyphens allowed
- Must be unique within the organisation
- Cannot start with a number

## Metrics Integration

Workspaces can integrate with Grafana for experiment metrics:

```json
{
  "metrics": {
    "enabled": true,
    "source": {
      "grafana": {
        "base_url": "https://grafana.example.com",
        "dashboard_uid": "abc123",
        "dashboard_slug": "experiments",
        "variant_id_alias": "variant_id"
      }
    }
  }
}
```

## Workspace Context

API requests to workspace resources require two headers:

- `x-org-id` — The organisation ID
- `x-workspace` — The workspace name

These headers identify which workspace to operate on.

## Encryption

Each workspace has its own encryption key for securing secrets. The key is encrypted using the master encryption key and can be rotated independently. See [Secrets](../safety/secrets.md) for more information.

## Related Concepts

- [Dimensions](../context-aware-config/dimensions.mdx) — Define segmentation criteria
- [Default Configs](../context-aware-config/default-config.mdx) — Base configuration values
- [Contexts](../context-aware-config/overrides.mdx) — Context-specific overrides
- [Experiments](../experimentation/experiments.mdx) — A/B testing within workspaces