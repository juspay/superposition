-- This file should undo anything in `up.sql`
ALTER TABLE public.dimensions 
DROP COLUMN dependency_graph,
DROP COLUMN immediate_parents,
DROP COLUMN immediate_childrens;