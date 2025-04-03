-- Your SQL goes here
ALTER TABLE public.dimensions 
ADD COLUMN dependency_graph JSON default '{}'::json NOT NULL,
ADD COLUMN immediate_parents TEXT[] default '{}' NOT NULL,
ADD COLUMN immediate_childrens TEXT[] default '{}' NOT NULL;