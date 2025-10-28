-- Your SQL goes here
CREATE TYPE public.function_types_new AS ENUM (
  'VALUE-VALIDATION',
  'VALUE-COMPUTE',
  'CONTEXT-VALIDATION'
);

ALTER TABLE localorg_dev.functions
  ALTER COLUMN function_type DROP DEFAULT,
  ALTER COLUMN function_type TYPE public.function_types_new
  USING CASE function_type
          WHEN 'VALIDATION' THEN 'VALUE-VALIDATION'::public.function_types_new
          WHEN 'AUTOCOMPLETE' THEN 'VALUE-COMPUTE'::public.function_types_new
        END,
  ALTER COLUMN function_type SET DEFAULT 'VALUE-VALIDATION';

ALTER TABLE localorg_test.functions
  ALTER COLUMN function_type DROP DEFAULT,
  ALTER COLUMN function_type TYPE public.function_types_new
  USING CASE function_type
          WHEN 'VALIDATION' THEN 'VALUE-VALIDATION'::public.function_types_new
          WHEN 'AUTOCOMPLETE' THEN 'VALUE-COMPUTE'::public.function_types_new
        END,
  ALTER COLUMN function_type SET DEFAULT 'VALUE-VALIDATION';

DROP TYPE public.function_types;

ALTER TYPE public.function_types_new RENAME TO function_types;
