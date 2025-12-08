ALTER TABLE superposition.workspaces
DROP COLUMN IF EXISTS key_rotation_at,
DROP COLUMN IF EXISTS previous_encryption_key,
DROP COLUMN IF EXISTS encryption_key;
