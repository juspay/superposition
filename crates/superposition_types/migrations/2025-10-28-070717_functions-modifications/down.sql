-- This file should undo anything in `up.sql`
CREATE TYPE public.function_types_old AS ENUM (
  'VALIDATION',
  'AUTOCOMPLETE'
);

ALTER TABLE localorg_dev.functions
  ALTER COLUMN function_type DROP DEFAULT,
  ALTER COLUMN function_type TYPE public.function_types_old
  USING CASE function_type
          WHEN 'VALUE-VALIDATION' THEN 'VALIDATION'::public.function_types_old
          WHEN 'VALUE-COMPUTE' THEN 'AUTOCOMPLETE'::public.function_types_old
          WHEN 'CONTEXT-VALIDATION' THEN 'VALIDATION'::public.function_types_old
        END,
  ALTER COLUMN function_type SET DEFAULT 'VALIDATION';

ALTER TABLE localorg_test.functions
  ALTER COLUMN function_type DROP DEFAULT,
  ALTER COLUMN function_type TYPE public.function_types_old
  USING CASE function_type
          WHEN 'VALUE-VALIDATION' THEN 'VALIDATION'::public.function_types_old
          WHEN 'VALUE-COMPUTE' THEN 'AUTOCOMPLETE'::public.function_types_old
          WHEN 'CONTEXT-VALIDATION' THEN 'VALIDATION'::public.function_types_old
        END,
  ALTER COLUMN function_type SET DEFAULT 'VALIDATION';

DROP TYPE public.function_types;

ALTER TYPE public.function_types_old RENAME TO function_types;
