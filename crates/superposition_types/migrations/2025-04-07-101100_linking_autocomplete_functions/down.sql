ALTER TABLE public.default_configs DROP CONSTRAINT IF EXISTS default_configs_autocomplete_function_name_fkey;

ALTER TABLE public.default_configs DROP COLUMN autocomplete_function_name;

ALTER TABLE public.dimensions DROP CONSTRAINT IF EXISTS dimensions_autocomplete_function_name_fkey;

ALTER TABLE public.dimensions DROP COLUMN autocomplete_function_name;