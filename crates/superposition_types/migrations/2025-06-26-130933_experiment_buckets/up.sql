-- Your SQL goes here
ALTER TABLE public.experiment_groups ADD COLUMN buckets JSON[] 
DEFAULT array_fill(NULL::JSON, ARRAY[100]) NOT NULL
CHECK (cardinality(buckets) = 100);

CREATE TYPE public.GROUP_TYPE AS ENUM (
    'USER_CREATED',
    'SYSTEM_GENERATED'
);

ALTER TABLE public.experiment_groups 
ADD COLUMN group_type public.GROUP_TYPE DEFAULT 'USER_CREATED';