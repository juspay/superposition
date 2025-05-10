-- This file should undo anything in `up.sql`
ALTER TABLE superposition.workspaces DROP COLUMN metrics;

ALTER TABLE public.experiments
DROP COLUMN metrics,
DROP COLUMN started_at,
DROP COLUMN started_by;
