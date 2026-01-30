DROP TRIGGER IF EXISTS secrets_audit ON public.secrets;

DROP INDEX IF EXISTS public.idx_secrets_last_modified_at;
DROP INDEX IF EXISTS public.idx_secrets_created_at;

DROP TABLE IF EXISTS public.secrets;

ALTER TABLE superposition.workspaces
DROP COLUMN IF EXISTS key_rotated_at,
DROP COLUMN IF EXISTS encryption_key;