-- Your SQL goes here
CREATE TYPE public.function_types AS ENUM (
'VALIDATION',
'AUTOCOMPLETE'
);

ALTER TABLE public.functions ADD COLUMN function_type public.FUNCTION_TYPES NOT NULL DEFAULT 'VALIDATION';
