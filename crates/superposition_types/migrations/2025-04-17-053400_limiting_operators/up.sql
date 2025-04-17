-- Your SQL goes here
ALTER TABLE superposition.workspaces
ADD COLUMN IF NOT EXISTS strict_mode BOOLEAN DEFAULT FALSE;
