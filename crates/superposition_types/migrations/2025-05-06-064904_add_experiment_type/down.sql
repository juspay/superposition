ALTER TABLE public.experiments DROP COLUMN experiment_type;

DROP TYPE IF EXISTS public.experiment_type CASCADE;