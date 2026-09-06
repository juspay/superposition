ALTER TABLE superposition.workspaces
ADD COLUMN IF NOT EXISTS enable_auto_reduce BOOLEAN NOT NULL DEFAULT FALSE;
