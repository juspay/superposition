-- Your SQL goes here
ALTER TABLE public.dimensions
add column position integer DEFAULT 1 NOT NULL;

ALTER TABLE public.contexts
add column weight numeric(1000,0) DEFAULT 1 NOT NULL;

CREATE INDEX IF NOT EXISTS idx_contexts_weight ON public.contexts(weight);