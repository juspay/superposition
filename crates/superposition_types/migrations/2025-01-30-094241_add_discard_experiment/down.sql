-- This file should undo anything in `up.sql`
CREATE TYPE public.experiment_status_type_new as enum ('CREATED', 'CONCLUDED', 'INPROGRESS');
ALTER TABLE public.experiments ALTER COLUMN status TYPE public.experiment_status_type_new USING experiment_status_type::text::public.experiment_status_type_new;
DROP TYPE public.experiment_status_type;
ALTER TYPE public.experiment_status_type_new RENAME TO experiment_status_type;
