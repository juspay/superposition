ALTER TABLE superposition.workspaces
ADD COLUMN IF NOT EXISTS workspace_lock_id UUID,
ADD COLUMN IF NOT EXISTS workspace_lock_operation TEXT,
ADD COLUMN IF NOT EXISTS workspace_locked_by TEXT,
ADD COLUMN IF NOT EXISTS workspace_lock_acquired_at TIMESTAMPTZ,
ADD COLUMN IF NOT EXISTS workspace_lock_expires_at TIMESTAMPTZ;
