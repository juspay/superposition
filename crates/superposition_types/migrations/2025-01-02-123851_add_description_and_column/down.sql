-- This file should undo anything in `up.sql`
ALTER TABLE public.config_versions DROP COLUMN IF EXISTS description;
ALTER TABLE public.config_versions DROP COLUMN IF EXISTS change_reason;

ALTER TABLE public.functions ALTER COLUMN description DROP NOT NULL;
ALTER TABLE public.functions ALTER COLUMN description DROP DEFAULT;
ALTER TABLE public.functions DROP COLUMN IF EXISTS change_reason;

ALTER TABLE public.functions RENAME COLUMN description TO function_description;

ALTER TABLE public.type_templates DROP COLUMN IF EXISTS description;
ALTER TABLE public.type_templates DROP COLUMN IF EXISTS change_reason;

ALTER TABLE public.default_configs DROP COLUMN IF EXISTS description;
ALTER TABLE public.default_configs DROP COLUMN IF EXISTS change_reason;

ALTER TABLE public.dimensions DROP COLUMN IF EXISTS description;
ALTER TABLE public.dimensions DROP COLUMN IF EXISTS change_reason;

ALTER TABLE public.contexts DROP COLUMN IF EXISTS description;
ALTER TABLE public.contexts DROP COLUMN IF EXISTS change_reason;

ALTER TABLE public.experiments DROP COLUMN IF EXISTS description;
ALTER TABLE public.experiments DROP COLUMN IF EXISTS change_reason;