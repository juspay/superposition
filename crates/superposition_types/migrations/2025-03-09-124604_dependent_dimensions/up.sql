-- Your SQL goes here
ALTER TABLE public.dimensions 
ADD COLUMN dependency_graph JSON default '{}'::json NOT NULL,
ADD COLUMN dependents TEXT[] default '{}' NOT NULL,
ADD COLUMN dependencies TEXT[] default '{}' NOT NULL;