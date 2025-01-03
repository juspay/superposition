-- Your SQL goes here
ALTER TABLE public.contexts ADD COLUMN IF NOT EXISTS description TEXT DEFAULT '' NOT NULL;
ALTER TABLE public.contexts ADD COLUMN IF NOT EXISTS change_reason TEXT DEFAULT '' NOT NULL;

ALTER TABLE public.dimensions ADD COLUMN IF NOT EXISTS description TEXT DEFAULT '' NOT NULL;
ALTER TABLE public.dimensions ADD COLUMN IF NOT EXISTS change_reason TEXT DEFAULT '' NOT NULL;

ALTER TABLE public.default_configs ADD COLUMN IF NOT EXISTS description TEXT DEFAULT '' NOT NULL;
ALTER TABLE public.default_configs ADD COLUMN IF NOT EXISTS change_reason TEXT DEFAULT '' NOT NULL;

ALTER TABLE public.type_templates ADD COLUMN IF NOT EXISTS description TEXT DEFAULT '' NOT NULL;
ALTER TABLE public.type_templates ADD COLUMN IF NOT EXISTS change_reason TEXT DEFAULT '' NOT NULL;

ALTER TABLE public.functions RENAME COLUMN function_description TO description;
ALTER TABLE public.functions ADD COLUMN IF NOT EXISTS change_reason TEXT DEFAULT '' NOT NULL;

ALTER TABLE public.functions ALTER COLUMN description SET DEFAULT '';
ALTER TABLE public.functions ALTER COLUMN description SET NOT NULL; 

ALTER TABLE public.config_versions ADD COLUMN IF NOT EXISTS description TEXT DEFAULT '' NOT NULL;
ALTER TABLE public.config_versions ADD COLUMN IF NOT EXISTS change_reason TEXT DEFAULT '' NOT NULL;

ALTER TABLE public.experiments ADD COLUMN IF NOT EXISTS description TEXT DEFAULT '' NOT NULL;
ALTER TABLE public.experiments ADD COLUMN IF NOT EXISTS change_reason TEXT DEFAULT '' NOT NULL;