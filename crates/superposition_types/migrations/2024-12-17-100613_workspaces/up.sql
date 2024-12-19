CREATE SCHEMA IF NOT EXISTS superposition;

CREATE TYPE superposition.workspace_status AS ENUM (
    'ENABLED',
    'DISABLED'
);

CREATE TABLE IF NOT EXISTS superposition.workspaces (
  organization_id TEXT NOT NULL,
  organization_name TEXT NOT NULL,
  workspace_name TEXT NOT NULL,
  workspace_schema_name TEXT NOT NULL,
  workspace_status superposition.WORKSPACE_STATUS NOT NULL,
  workspace_admin_email TEXT NOT NULL,
  created_by TEXT NOT NULL,
  last_modified_by TEXT NOT NULL,
  last_modified_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
  mandatory_dimensions TEXT[],
  CONSTRAINT organization_workspace_pkey PRIMARY KEY (organization_id, workspace_name),
  CONSTRAINT unique_workspace_schema_name UNIQUE (workspace_schema_name)
);

CREATE INDEX IF NOT EXISTS idx_workspace_name ON superposition.workspaces (workspace_name);
CREATE INDEX IF NOT EXISTS idx_last_modified_created_by ON superposition.workspaces (last_modified_by, created_by);
