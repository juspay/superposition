-- Your SQL goes here
ALTER TABLE public.experiment_groups ADD COLUMN IF NOT EXISTS buckets JSON[] 
DEFAULT array_fill(NULL::JSON, ARRAY[100]) NOT NULL
CHECK (cardinality(buckets) = 100);

CREATE TYPE public.group_type AS ENUM (
    'USER_CREATED',
    'SYSTEM_GENERATED'
);

ALTER TABLE public.experiment_groups 
ADD COLUMN IF NOT EXISTS group_type public.group_type DEFAULT 'USER_CREATED';