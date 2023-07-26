-- This file should undo anything in `up.sql`
ALTER TABLE cac_v1.experiments
ALTER COLUMN last_modified DROP NOT NULL,
ALTER COLUMN last_modified DROP DEFAULT;
DROP INDEX experiment_status_index;
