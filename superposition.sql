CREATE SCHEMA IF NOT EXISTS superposition;

CREATE TYPE superposition.org_status AS ENUM ('ACTIVE', 'INACTIVE', 'PENDING_KYB');

CREATE TABLE IF NOT EXISTS superposition.organisations (
    id VARCHAR(30) PRIMARY KEY NOT NULL,
    name TEXT NOT NULL,
    country_code VARCHAR(10),
    contact_email VARCHAR(255),
    contact_phone VARCHAR(15),
    created_by TEXT NOT NULL,
    admin_email TEXT NOT NULL,
    status superposition.org_status NOT NULL DEFAULT 'ACTIVE',
    sector VARCHAR(100),
    created_at TIMESTAMP WITHOUT TIME ZONE DEFAULT NOW () NOT NULL,
    updated_at TIMESTAMP WITHOUT TIME ZONE DEFAULT NOW () NOT NULL,
    updated_by TEXT NOT NULL
);

-- Indexes for optimizing queries
CREATE INDEX IF NOT EXISTS idx_organisation_contact_email ON superposition.organisations (contact_email);

CREATE INDEX IF NOT EXISTS idx_organisation_status ON superposition.organisations (status);

CREATE INDEX IF NOT EXISTS idx_organisation_created_at ON superposition.organisations (created_at);

CREATE INDEX IF NOT EXISTS idx_organisation_admin_email ON superposition.organisations (admin_email);


CREATE TYPE superposition.workspace_status AS ENUM ('ENABLED', 'DISABLED');
CREATE TABLE IF NOT EXISTS superposition.workspaces (
    organisation_id VARCHAR(30) NOT NULL,
    organisation_name TEXT NOT NULL,
    workspace_name VARCHAR(25) NOT NULL,
    workspace_schema_name TEXT NOT NULL,
    workspace_status superposition.WORKSPACE_STATUS NOT NULL,
    workspace_admin_email TEXT NOT NULL,
    created_by TEXT NOT NULL,
    last_modified_by TEXT NOT NULL,
    last_modified_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    mandatory_dimensions TEXT [],
    CONSTRAINT organisation_workspace_pkey PRIMARY KEY (organisation_id, workspace_name),
    CONSTRAINT unique_workspace_schema_name UNIQUE (workspace_schema_name),
    CONSTRAINT fk_organisation FOREIGN KEY (organisation_id) REFERENCES superposition.organisations (id) ON DELETE RESTRICT ON UPDATE CASCADE
);
CREATE INDEX IF NOT EXISTS idx_workspace_name ON superposition.workspaces (workspace_name);
CREATE INDEX IF NOT EXISTS idx_last_modified_created_by ON superposition.workspaces (last_modified_by, created_by);

BEGIN;
-- Setup workspace schema
-- Your SQL goes here
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

DO $$ BEGIN
    CREATE TYPE public.experiment_status_type AS ENUM (
        'CREATED',
        'CONCLUDED',
        'INPROGRESS',
        'DISCARDED'
    );
EXCEPTION
    WHEN duplicate_object THEN null;
END $$;

CREATE TYPE public.function_types AS ENUM (
'VALIDATION',
'AUTOCOMPLETE'
);

CREATE TYPE public.http_method AS ENUM (
    'GET',
    'PUT',
    'POST',
    'DELETE',
    'PATCH',
    'HEAD'
);

ALTER TABLE superposition.workspaces
ADD COLUMN IF NOT EXISTS strict_mode BOOLEAN DEFAULT TRUE;

ALTER TABLE superposition.workspaces ADD COLUMN IF NOT EXISTS metrics JSON DEFAULT '{"enabled": false}'::json NOT NULL;

ALTER TYPE public.experiment_status_type ADD VALUE 'PAUSED';
CREATE TYPE public.experiment_type AS ENUM (
    'DEFAULT',
    'DELETE_OVERRIDES'
);

ALTER TABLE superposition.workspaces add column if not exists config_version bigint;

ALTER TABLE superposition.workspaces ADD COLUMN IF NOT EXISTS allow_experiment_self_approval boolean NOT NULL DEFAULT false;

CREATE TYPE public.GROUP_TYPE AS ENUM (
    'USER_CREATED',
    'SYSTEM_GENERATED'
);

ALTER TABLE superposition.workspaces
ADD COLUMN IF NOT EXISTS auto_populate_control BOOLEAN DEFAULT TRUE;

UPDATE superposition.workspaces SET auto_populate_control = FALSE;

COMMIT;
