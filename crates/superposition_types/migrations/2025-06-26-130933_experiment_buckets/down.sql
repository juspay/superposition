-- This file should undo anything in `up.sql`
ALTER TABLE public.experiment_groups DROP COLUMN group_type;

DROP TYPE public.GROUP_TYPE;

ALTER TABLE public.experiment_groups DROP COLUMN buckets;