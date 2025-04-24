-- Your SQL goes here
ALTER TABLE public.dimensions
ALTER COLUMN priority SET DEFAULT 1;

ALTER TABLE public.dimensions
ADD CONSTRAINT dimension_unique_position UNIQUE (position) DEFERRABLE INITIALLY DEFERRED;