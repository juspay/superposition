-- This file should undo anything in `up.sql`
DELETE FROM public.config_versions;
DROP TABLE IF EXISTS public.config_versions;