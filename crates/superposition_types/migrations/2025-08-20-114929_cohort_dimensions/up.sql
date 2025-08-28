CREATE TYPE public.dimension_type AS ENUM (
    'REGULAR',
    'COHORT'
);

ALTER TABLE public.dimensions
ADD COLUMN dimension_type DIMENSION_TYPE NOT NULL DEFAULT 'REGULAR',
ADD COLUMN cohort_based_on TEXT;
