-- Your SQL goes here
ALTER TABLE public.experiment_groups ADD COLUMN buckets TEXT[] DEFAULT array_fill(NULL::TEXT, ARRAY[100]) NOT NULL;

CREATE TYPE public.GROUP_TYPE AS ENUM (
    'USER_CREATED',
    'SYSTEM_GENERATED'
);

ALTER TABLE public.experiment_groups 
ADD COLUMN group_type public.GROUP_TYPE DEFAULT 'USER_CREATED';