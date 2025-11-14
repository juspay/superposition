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
WHERE function_name = 'change_reason_validation_function' 
AND function_type = 'CHANGE_REASON_VALIDATION';

DELETE FROM public.functions 
WHERE function_name = 'context_validation_function' 
AND function_type = 'CONTEXT_VALIDATION';

ALTER TABLE superposition.workspaces
DROP COLUMN IF EXISTS enable_change_reason_validation;

ALTER TABLE superposition.workspaces
DROP COLUMN IF EXISTS enable_context_validation;
