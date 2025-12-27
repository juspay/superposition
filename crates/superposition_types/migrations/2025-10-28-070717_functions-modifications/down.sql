-- This file should undo anything in `up.sql`
ALTER TABLE public.functions
  ALTER COLUMN function_type DROP DEFAULT,
  ALTER COLUMN function_type TYPE text;

DROP TYPE public.function_types_new;

ALTER TABLE public.functions
  ALTER COLUMN function_type TYPE public.function_types
  USING CASE function_type
          WHEN 'VALUE_VALIDATION' THEN 'VALIDATION'::public.function_types
          WHEN 'VALUE_COMPUTE' THEN 'AUTOCOMPLETE'::public.function_types
          WHEN 'CONTEXT_VALIDATION' THEN 'VALIDATION'::public.function_types
        END,
  ALTER COLUMN function_type SET DEFAULT 'VALIDATION'::public.function_types;

DELETE FROM public.functions 
WHERE function_name = 'change_reason_validation' 
AND function_type = 'CHANGE_REASON_VALIDATION';

DELETE FROM public.functions 
WHERE function_name = 'context_validation' 
AND function_type = 'CONTEXT_VALIDATION';

ALTER TABLE superposition.workspaces
DROP COLUMN IF EXISTS enable_change_reason_validation,
DROP COLUMN IF EXISTS enable_context_validation;

ALTER TABLE public.default_configs
DROP CONSTRAINT IF EXISTS default_configs_value_compute_function_name_fkey;

ALTER TABLE public.default_configs
RENAME COLUMN value_compute_function_name TO autocomplete_function_name;

ALTER TABLE public.default_configs
ADD FOREIGN KEY (autocomplete_function_name)
REFERENCES public.functions(function_name);

ALTER TABLE public.dimensions
DROP CONSTRAINT IF EXISTS dimensions_value_compute_function_name_fkey;

ALTER TABLE public.dimensions
RENAME COLUMN value_compute_function_name TO autocomplete_function_name;

ALTER TABLE public.dimensions
ADD FOREIGN KEY (autocomplete_function_name)
REFERENCES public.functions(function_name);

ALTER TABLE public.default_configs
DROP CONSTRAINT IF EXISTS default_configs_value_validation_function_name_fkey;

ALTER TABLE public.default_configs
RENAME COLUMN value_validation_function_name TO function_name;

ALTER TABLE public.default_configs
ADD CONSTRAINT default_configs_function_name_fkey
FOREIGN KEY (function_name)
REFERENCES public.functions(function_name);

ALTER TABLE public.dimensions
DROP CONSTRAINT IF EXISTS dimensions_value_validation_function_name_fkey;

ALTER TABLE public.dimensions
RENAME COLUMN value_validation_function_name TO function_name;

ALTER TABLE public.dimensions
ADD CONSTRAINT dimensions_function_name_fkey
FOREIGN KEY (function_name)
REFERENCES public.functions(function_name);

UPDATE {replaceme}.functions
SET draft_runtime_version = '1.0.0'
WHERE draft_runtime_version = '1.0';

UPDATE {replaceme}.functions
SET published_runtime_version = '1.0.0'
WHERE published_runtime_version = '1.0';