ALTER TABLE superposition.workspaces ADD COLUMN IF NOT EXISTS change_reason TEXT DEFAULT '' NOT NULL;
