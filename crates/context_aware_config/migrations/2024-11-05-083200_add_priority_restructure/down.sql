-- This file should undo anything in `up.sql`
ALTER TABLE public.dimensions
drop column position;

ALTER TABLE public.contexts
drop column weightage;

DROP INDEX IF EXISTS idx_contexts_weightage;