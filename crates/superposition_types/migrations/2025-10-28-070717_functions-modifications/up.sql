-- Your SQL goes here
CREATE TYPE public.function_types_new AS ENUM (
  'VALUE_VALIDATION',
  'VALUE_COMPUTE',
  'CONTEXT_VALIDATION',
  'CHANGE_REASON_VALIDATION'
);

ALTER TABLE public.functions
  ALTER COLUMN function_type DROP DEFAULT,
  ALTER COLUMN function_type TYPE public.function_types_new
  USING CASE function_type::text
          WHEN 'VALIDATION' THEN 'VALUE_VALIDATION'::public.function_types_new
          WHEN 'AUTOCOMPLETE' THEN 'VALUE_COMPUTE'::public.function_types_new
        END,
  ALTER COLUMN function_type SET DEFAULT 'VALUE_VALIDATION'::public.function_types_new;

INSERT INTO public.functions (
    function_name,
    published_code,
    draft_code,
    description,
    published_runtime_version,
    draft_runtime_version,
    published_at,
    draft_edited_at,
    published_by,
    draft_edited_by,
    last_modified_at,
    last_modified_by,
    change_reason,
    function_type,
    created_by,
    created_at
) VALUES (
    'context_validation_function',
    'Ly8gZW52aXJvbm1lbnQ6IG9iamVjdCB7IGNvbnRleHQ6IFtPYmplY3RdLCBvdmVycmlkZXM6IFtPYmplY3RdIH0gLSBjYXB0dXJlcyBvdXQgZWxlbWVudHMgaW4gdGhlIGZvcm0gbGlrZSBjb250ZXh0LCBvdmVycmlkZXMgZXRjLgovLyByZXR1cm5zOiBib29sZWFuCmFzeW5jIGZ1bmN0aW9uIHZhbGlkYXRlX2NvbnRleHQoZW52aXJvbm1lbnQpIHsKICAgIHJldHVybiB0cnVlOwp9',
    'Ly8gZW52aXJvbm1lbnQ6IG9iamVjdCB7IGNvbnRleHQ6IFtPYmplY3RdLCBvdmVycmlkZXM6IFtPYmplY3RdIH0gLSBjYXB0dXJlcyBvdXQgZWxlbWVudHMgaW4gdGhlIGZvcm0gbGlrZSBjb250ZXh0LCBvdmVycmlkZXMgZXRjLgovLyByZXR1cm5zOiBib29sZWFuCmFzeW5jIGZ1bmN0aW9uIHZhbGlkYXRlX2NvbnRleHQoZW52aXJvbm1lbnQpIHsKICAgIHJldHVybiB0cnVlOwp9',
    'Validates the entire context object for the whole workspace before processing. Returns true if context is valid, false otherwise.',
    '1.0.0',
    '1.0.0',
    NOW(),
    NOW(),
    'user@superposition.io',
    'user@superposition.io',
    NOW(),
    'user@superposition.io',
    'Initial creation of context validation function',
    'CONTEXT_VALIDATION',
    'user@superposition.io',
    NOW()
);

INSERT INTO public.functions (
    function_name,
    published_code,
    draft_code,
    description,
    published_runtime_version,
    draft_runtime_version,
    published_at,
    draft_edited_at,
    published_by,
    draft_edited_by,
    last_modified_at,
    last_modified_by,
    change_reason,
    function_type,
    created_by,
    created_at
) VALUES (
    'change_reason_validation_function',
    'Ly8gY2hhbmdlX3JlYXNvbjogc3RyaW5nIC0gcmVhc29uIGZvciBjaGFuZ2UgcHJvdmlkZWQgYnkgdXNlcgovLyByZXR1cm5zOiBib29sZWFuCmFzeW5jIGZ1bmN0aW9uIHZhbGlkYXRlX2NoYW5nZV9yZWFzb24oY2hhbmdlX3JlYXNvbikgewogICAgcmV0dXJuIHRydWU7Cn0=',
    'Ly8gY2hhbmdlX3JlYXNvbjogc3RyaW5nIC0gcmVhc29uIGZvciBjaGFuZ2UgcHJvdmlkZWQgYnkgdXNlcgovLyByZXR1cm5zOiBib29sZWFuCmFzeW5jIGZ1bmN0aW9uIHZhbGlkYXRlX2NoYW5nZV9yZWFzb24oY2hhbmdlX3JlYXNvbikgewogICAgcmV0dXJuIHRydWU7Cn0=',
    'Validates the change reason for any creation/updation. Returns true if context is valid, false otherwise.',
    '1.0.0',
    '1.0.0',
    NOW(),
    NOW(),
    'user@superposition.io',
    'user@superposition.io',
    NOW(),
    'user@superposition.io',
    'Initial creation of change reason validation function',
    'CHANGE_REASON_VALIDATION',
    'user@superposition.io',
    NOW()
);

ALTER TABLE superposition.workspaces
ADD COLUMN IF NOT EXISTS enable_context_validation BOOLEAN DEFAULT FALSE,
ADD COLUMN IF NOT EXISTS enable_change_reason_validation BOOLEAN DEFAULT FALSE;

ALTER TABLE public.dimensions
DROP CONSTRAINT IF EXISTS dimensions_autocomplete_function_name_fkey;

ALTER TABLE public.dimensions
RENAME COLUMN autocomplete_function_name TO value_compute_function_name;

ALTER TABLE public.dimensions
ADD FOREIGN KEY (value_compute_function_name)
REFERENCES public.functions(function_name);

ALTER TABLE public.default_configs
DROP CONSTRAINT IF EXISTS default_configs_autocomplete_function_name_fkey;

ALTER TABLE public.default_configs
RENAME COLUMN autocomplete_function_name TO value_compute_function_name;

ALTER TABLE public.default_configs
ADD FOREIGN KEY (value_compute_function_name)
REFERENCES public.functions(function_name);

ALTER TABLE public.dimensions
rename column function_name to value_validation_function_name;

ALTER TABLE public.default_configs
rename column function_name to value_validation_function_name;