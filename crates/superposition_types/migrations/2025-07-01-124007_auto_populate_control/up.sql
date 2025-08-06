ALTER TABLE superposition.workspaces
ADD COLUMN IF NOT EXISTS auto_populate_control BOOLEAN DEFAULT TRUE;

UPDATE superposition.workspaces SET auto_populate_control = FALSE;