CREATE TYPE public.dimension_type AS ENUM (
    'REGULAR',
    'LOCAL_COHORT',
    'REMOTE_COHORT',
    
);

ALTER TABLE public.dimensions
ADD COLUMN dimension_type DIMENSION_TYPE NOT NULL DEFAULT 'REGULAR';
