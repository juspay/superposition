DROP TABLE IF EXISTS superposition.job_manager;

DROP INDEX IF EXISTS superposition.idx_job_manager_workspace_schema;
DROP INDEX IF EXISTS superposition.idx_job_manager_status;
DROP INDEX IF EXISTS superposition.idx_job_manager_type;
DROP INDEX IF EXISTS superposition.idx_job_manager_kronos_job_id;
DROP INDEX IF EXISTS superposition.idx_job_manager_status_job_type;
DROP INDEX IF EXISTS superposition.idx_job_manager_created_at;

DROP TYPE public.background_job_type;

DROP TYPE public.background_job_status;
