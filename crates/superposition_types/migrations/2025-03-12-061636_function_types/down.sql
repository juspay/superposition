-- This file should undo anything in `up.sql`
ALTER TABLE public.functions DROP COLUMN function_type;

DROP TYPE IF EXISTS public.function_types CASCADE;

