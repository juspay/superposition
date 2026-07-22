DROP TABLE IF EXISTS superposition.job_manager;

DROP INDEX IF EXISTS superposition.idx_job_manager_workspace_schema;
DROP INDEX IF EXISTS superposition.idx_job_manager_status;
DROP INDEX IF EXISTS superposition.idx_job_manager_type;
DROP INDEX IF EXISTS superposition.idx_job_manager_kronos_job_id;
DROP INDEX IF EXISTS superposition.idx_job_manager_status_job_type;
DROP INDEX IF EXISTS superposition.idx_job_manager_created_at;

DO $$ BEGIN
    DROP TYPE public.background_job_type;
EXCEPTION
    WHEN undefined_object THEN null;
END $$;

DO $$ BEGIN
    DROP TYPE public.background_job_status;
EXCEPTION
    WHEN undefined_object THEN null;
END $$;
