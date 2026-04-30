ALTER TABLE superposition.workspaces
DROP COLUMN IF EXISTS workspace_lock_expires_at,
DROP COLUMN IF EXISTS workspace_lock_acquired_at,
DROP COLUMN IF EXISTS workspace_locked_by,
DROP COLUMN IF EXISTS workspace_lock_operation,
DROP COLUMN IF EXISTS workspace_lock_id;
