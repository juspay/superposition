---
sidebar_position: 1
title: Organisations
description: Top-level entity for multi-tenant configuration management
---

Organisations are the top-level entity in Superposition's hierarchy. They enable multi-tenant configuration management, allowing a single Superposition deployment to serve multiple organisations with complete isolation.

## Organisation Properties

| Property | Type | Description |
|----------|------|-------------|
| `id` | String | Unique identifier (format: `orgid<numeric>`) |
| `name` | String | Display name of the organisation |
| `admin_email` | String | Email of the organisation administrator |
| `status` | Enum | `ACTIVE`, `INACTIVE`, or `PENDING_KYB` |
| `country_code` | String | ISO country code (optional) |
| `contact_email` | String | Contact email address (optional) |
| `contact_phone` | String | Contact phone number (optional) |
| `sector` | String | Business sector (optional) |
| `created_by` | String | User who created the organisation |
| `updated_by` | String | User who last updated the organisation |

## Hierarchy

```
Organisation
└── Workspace(s)
    ├── Dimensions
    ├── Default Configs
    ├── Contexts
    ├── Overrides
    ├── Experiments
    └── Secrets
```

An organisation can contain multiple [workspaces](./workspaces.md), each with its own isolated configuration space.

## Creating an Organisation

When an organisation is created:

1. A unique ID is generated (e.g., `orgid123456789`)
2. The status is set to `PENDING_KYB`
3. Authorization policies are automatically configured for the admin user

## Authorization

Organisation creation and management requires admin privileges. When an organisation is created, the `admin_email` user is automatically granted administrative access to manage the organisation and its workspaces.

## Database Schema

Organisations are stored in the `superposition.organisations` table in PostgreSQL. The table includes indexes on `contact_email`, `admin_email`, `status`, and `created_at` for optimized query performance.