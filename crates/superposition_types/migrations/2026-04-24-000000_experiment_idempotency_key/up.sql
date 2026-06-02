-- Your SQL goes here
ALTER TABLE public.experiments ADD COLUMN IF NOT EXISTS idempotency_key TEXT;
CREATE UNIQUE INDEX IF NOT EXISTS experiments_idempotency_key_idx ON public.experiments(idempotency_key) WHERE idempotency_key IS NOT NULL;
