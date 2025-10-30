DROP TRIGGER IF EXISTS variables_audit ON public.variables;
DROP INDEX IF EXISTS idx_variables_last_modified_at;
DROP INDEX IF EXISTS idx_variables_created_at;
DROP TABLE IF EXISTS public.variables;