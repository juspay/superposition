-- This file should undo anything in `up.sql`

ALTER TABLE public.functions
drop column last_modified_at,
drop column last_modified_by;

ALTER TABLE public.dimensions
drop column last_modified_at,
drop column last_modified_by;

ALTER TABLE public.contexts
drop column last_modified_at,
drop column last_modified_by;

ALTER TABLE public.default_configs
drop column last_modified_at,
drop column last_modified_by;

ALTER TABLE public.type_templates
rename column last_modified_at to last_modified;

ALTER TABLE public.type_templates
drop column last_modified_by;