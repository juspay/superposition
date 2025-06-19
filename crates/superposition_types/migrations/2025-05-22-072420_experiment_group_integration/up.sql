-- Your SQL goes here
ALTER TABLE public.experiments ADD COLUMN IF NOT EXISTS experiment_group_id bigint;