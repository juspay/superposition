-- Your SQL goes here
ALTER TABLE public.dimensions 
ADD COLUMN dependency_graph JSON,
ADD COLUMN immediate_parents TEXT[],
ADD COLUMN immediate_childrens TEXT[];
