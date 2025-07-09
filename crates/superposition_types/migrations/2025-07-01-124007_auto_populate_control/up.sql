-- Your SQL goes here
ALTER TABLE superposition.workspaces
ADD COLUMN IF NOT EXISTS auto_populate_control BOOLEAN DEFAULT FALSE;