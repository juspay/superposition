-- This file should undo anything in `up.sql`
ALTER TABLE public.dimensions
ALTER COLUMN priority DROP DEFAULT;

ALTER table public.dimensions drop constraint if exists dimension_unique_position;