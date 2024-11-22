-- This file should undo anything in `up.sql`
ALTER TABLE public.dimensions
drop column position;

ALTER TABLE public.contexts
drop column weight;

DROP INDEX IF EXISTS idx_contexts_weight;