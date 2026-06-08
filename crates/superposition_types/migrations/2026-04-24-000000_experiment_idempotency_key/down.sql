-- This file should undo anything in `up.sql`
DROP INDEX IF EXISTS public.experiments_idempotency_key_idx;
ALTER TABLE public.experiments DROP COLUMN IF EXISTS idempotency_key;
