CREATE TYPE public.experiment_type AS ENUM (
'DEFAULT',
'DELETE_OVERRIDES'
);

ALTER TABLE public.experiments ADD COLUMN experiment_type public.experiment_type NOT NULL DEFAULT 'DEFAULT';