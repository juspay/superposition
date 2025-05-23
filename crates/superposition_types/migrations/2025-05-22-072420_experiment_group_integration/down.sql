-- This file should undo anything in `up.sql`
ALTER TABLE localorg_test.experiments DROP COLUMN IF EXISTS experiment_group_id;