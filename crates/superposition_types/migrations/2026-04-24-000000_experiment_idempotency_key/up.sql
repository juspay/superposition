-- Your SQL goes here
ALTER TABLE experiments ADD COLUMN IF NOT EXISTS idempotency_key TEXT;
CREATE UNIQUE INDEX IF NOT EXISTS experiments_idempotency_key_idx ON experiments(idempotency_key);
