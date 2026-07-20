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