-- This file should undo anything in `up.sql`
ALTER TABLE public.dimensions
ALTER COLUMN priority DROP DEFAULT;