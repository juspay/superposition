-- This file should undo anything in `up.sql`
ALTER TABLE public.dimensions 
DROP COLUMN dependency_graph,
DROP COLUMN dependents,
DROP COLUMN immediate_childrens;