-- This file should undo anything in `up.sql`
DROP INDEX IF EXISTS experiments_idempotency_key_idx;
ALTER TABLE experiments DROP COLUMN IF EXISTS idempotency_key;
