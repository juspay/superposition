CREATE SCHEMA IF NOT EXISTS superposition;

DO $$ BEGIN
    CREATE TYPE superposition.org_status AS ENUM ('ACTIVE', 'INACTIVE', 'PENDING_KYB');
EXCEPTION
    WHEN duplicate_object THEN null;
END $$;

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

DO $$ BEGIN
    CREATE TYPE superposition.workspace_status AS ENUM ('ENABLED', 'DISABLED');
EXCEPTION
    WHEN duplicate_object THEN null;
END $$;

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

DO $$ BEGIN
    CREATE TYPE public.function_types AS ENUM (
    'VALIDATION',
    'AUTOCOMPLETE'
    );
EXCEPTION
    WHEN duplicate_object THEN null;
END $$;

DO $$ BEGIN
    CREATE TYPE public.http_method AS ENUM (
        'GET',
        'PUT',
        'POST',
        'DELETE',
        'PATCH',
        'HEAD'
    );
EXCEPTION
    WHEN duplicate_object THEN null;
END $$;

ALTER TABLE superposition.workspaces
ADD COLUMN IF NOT EXISTS strict_mode BOOLEAN DEFAULT TRUE;

ALTER TABLE superposition.workspaces ADD COLUMN IF NOT EXISTS metrics JSON DEFAULT '{"enabled": false}'::json NOT NULL;

ALTER TYPE public.experiment_status_type ADD VALUE IF NOT EXISTS 'PAUSED';

DO $$ BEGIN
    CREATE TYPE public.experiment_type AS ENUM (
        'DEFAULT',
        'DELETE_OVERRIDES'
    );
EXCEPTION
    WHEN duplicate_object THEN null;
END $$;

ALTER TABLE superposition.workspaces add column if not exists config_version bigint;

ALTER TABLE superposition.workspaces ADD COLUMN IF NOT EXISTS allow_experiment_self_approval boolean NOT NULL DEFAULT false;

DO $$ BEGIN
    CREATE TYPE public.GROUP_TYPE AS ENUM (
        'USER_CREATED',
        'SYSTEM_GENERATED'
    );
EXCEPTION
    WHEN duplicate_object THEN null;
END $$;

-- Add column with DEFAULT FALSE (applies to existing rows)
ALTER TABLE superposition.workspaces
ADD COLUMN IF NOT EXISTS auto_populate_control BOOLEAN DEFAULT FALSE;

-- Set the default to TRUE for future inserts
ALTER TABLE superposition.workspaces
ALTER COLUMN auto_populate_control SET DEFAULT TRUE;

DO $$ BEGIN
    CREATE TYPE public.function_types_new AS ENUM (
        'VALUE_VALIDATION',
        'VALUE_COMPUTE',
        'CONTEXT_VALIDATION',
        'CHANGE_REASON_VALIDATION'
    );
EXCEPTION
    WHEN duplicate_object THEN null;
END $$;

ALTER TABLE superposition.workspaces
ADD COLUMN IF NOT EXISTS enable_context_validation BOOLEAN DEFAULT FALSE,
ADD COLUMN IF NOT EXISTS enable_change_reason_validation BOOLEAN DEFAULT FALSE;

ALTER TABLE superposition.workspaces DROP COLUMN IF EXISTS strict_mode;

ALTER TABLE superposition.workspaces
ADD COLUMN IF NOT EXISTS encryption_key TEXT NOT NULL DEFAULT '',
ADD COLUMN IF NOT EXISTS key_rotated_at TIMESTAMPTZ;

ALTER TABLE superposition.workspaces
ADD COLUMN IF NOT EXISTS workspace_lock_id UUID,
ADD COLUMN IF NOT EXISTS workspace_lock_operation TEXT,
ADD COLUMN IF NOT EXISTS workspace_locked_by TEXT,
ADD COLUMN IF NOT EXISTS workspace_lock_acquired_at TIMESTAMPTZ,
ADD COLUMN IF NOT EXISTS workspace_lock_expires_at TIMESTAMPTZ;

DO $$ BEGIN
    CREATE TYPE public.background_job_type AS ENUM (
        'WEBHOOK',
        'PRIORITY_RECOMPUTE',
        'REDUCE'
    );
EXCEPTION
    WHEN duplicate_object THEN null;
END $$;

DO $$ BEGIN
    CREATE TYPE public.background_job_status AS ENUM (
        'CREATED',
        'SCHEDULED',
        'INPROGRESS',
        'FAILED',
        'COMPLETED'
    );
EXCEPTION
    WHEN duplicate_object THEN null;
END $$;

CREATE TABLE IF NOT EXISTS superposition.job_manager (
    id BIGINT PRIMARY KEY,
    kronos_job_id TEXT NOT NULL,
    description TEXT NOT NULL,
    job_type public.background_job_type NOT NULL,
    status public.background_job_status NOT NULL,
    name TEXT NOT NULL,
    progress INT NOT NULL DEFAULT 0 CHECK (progress >= 0 AND progress <= 100),
    workspace_schema TEXT NOT NULL,
    created_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP,
    logs JSONB NOT NULL DEFAULT '{}'::jsonb
);

CREATE INDEX IF NOT EXISTS idx_job_manager_workspace_schema
    ON superposition.job_manager (workspace_schema);

CREATE INDEX IF NOT EXISTS idx_job_manager_kronos_job_id
    ON superposition.job_manager (kronos_job_id);

CREATE INDEX IF NOT EXISTS idx_job_manager_type
    ON superposition.job_manager (job_type);

CREATE INDEX IF NOT EXISTS idx_job_manager_status
    ON superposition.job_manager (status);

CREATE INDEX IF NOT EXISTS idx_job_manager_status_job_type
    ON superposition.job_manager (status, job_type);

CREATE INDEX IF NOT EXISTS idx_job_manager_created_at
    ON superposition.job_manager (created_at DESC);

