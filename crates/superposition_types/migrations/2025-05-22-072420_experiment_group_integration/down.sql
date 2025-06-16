-- This file should undo anything in `up.sql`
ALTER TABLE public.experiments DROP COLUMN IF EXISTS experiment_group_id;