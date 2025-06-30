-- This file should undo anything in `up.sql`
ALTER TABLE superposition.workspaces
DROP COLUMN IF EXISTS auto_populate_control;