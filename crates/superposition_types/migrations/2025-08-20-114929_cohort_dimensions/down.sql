-- This file should undo anything in `up.sql`
ALTER TABLE public.dimensions DROP COLUMN IF EXISTS dimension_type;

DROP TYPE public.dimension_type;
