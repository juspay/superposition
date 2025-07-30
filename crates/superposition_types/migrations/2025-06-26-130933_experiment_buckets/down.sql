-- This file should undo anything in `up.sql`
ALTER TABLE public.experiment_groups DROP COLUMN IF EXISTS group_type;

DROP TYPE public.group_type;

ALTER TABLE public.experiment_groups DROP COLUMN IF EXISTS buckets;