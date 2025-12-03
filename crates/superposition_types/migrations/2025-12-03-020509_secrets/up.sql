ALTER TABLE public.workspaces 
ADD COLUMN IF NOT EXISTS encryption_key TEXT,
ADD COLUMN IF NOT EXISTS previous_encryption_key TEXT,
ADD COLUMN IF NOT EXISTS key_rotation_at TIMESTAMPTZ;
