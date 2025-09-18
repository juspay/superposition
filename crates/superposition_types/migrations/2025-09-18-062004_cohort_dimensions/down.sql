ALTER TABLE public.dimensions DROP COLUMN IF EXISTS dimension_type;

ALTER TABLE public.dimensions ADD COLUMN IF NOT EXISTS dependencies TEXT[] default '{}' NOT NULL;

ALTER TABLE public.dimensions ADD COLUMN IF NOT EXISTS dependents TEXT[] default '{}' NOT NULL;