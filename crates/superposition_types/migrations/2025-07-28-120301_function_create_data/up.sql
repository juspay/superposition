-- Your SQL goes here
ALTER TABLE public.functions
ADD COLUMN IF NOT EXISTS created_at TIMESTAMP,
ADD COLUMN IF NOT EXISTS created_by TEXT NOT NULL DEFAULT('NOT AVAILABLE');

ALTER TABLE public.functions ALTER COLUMN created_by DROP DEFAULT;

UPDATE public.functions SET created_at = last_modified_at WHERE created_at IS NULL;

ALTER TABLE public.functions
ALTER COLUMN created_at SET DEFAULT CURRENT_TIMESTAMP,
ALTER COLUMN created_at SET NOT NULL;
