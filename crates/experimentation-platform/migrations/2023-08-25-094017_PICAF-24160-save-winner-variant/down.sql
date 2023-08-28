-- This file should undo anything in `up.sql`
ALTER TABLE cac_v1.experiments
DROP COLUMN chosen_variant;
