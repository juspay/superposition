-- This file should undo anything in `up.sql`
ALTER TABLE public.functions
DROP COLUMN IF EXISTS created_at,
DROP COLUMN IF EXISTS created_by;