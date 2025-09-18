ALTER TABLE public.dimensions ADD COLUMN IF NOT EXISTS dimension_type TEXT NOT NULL DEFAULT 'REGULAR';

ALTER TABLE public.dimensions DROP COLUMN IF EXISTS dependencies;

ALTER TABLE public.dimensions DROP COLUMN IF EXISTS dependents;