-- Rollback for 2026-01-18-125824-0000_response_templates

-- Drop trigger first to avoid dependency issues
DROP TRIGGER IF EXISTS response_templates_audit ON public.response_templates;

-- Drop indexes (optional if you drop the table, but kept explicit for clarity)
DROP INDEX IF EXISTS public.idx_response_templates_last_modified_at;
DROP INDEX IF EXISTS public.idx_response_templates_created_at;
DROP INDEX IF EXISTS public.idx_response_templates_name;

-- Drop table
DROP TABLE IF EXISTS public.response_templates;
