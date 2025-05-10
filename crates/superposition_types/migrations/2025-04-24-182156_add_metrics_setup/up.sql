-- Your SQL goes here
ALTER TABLE superposition.workspaces ADD COLUMN IF NOT EXISTS metrics JSON DEFAULT '{"enabled": false}'::json NOT NULL;

ALTER TABLE public.experiments
ADD COLUMN IF NOT EXISTS metrics JSON DEFAULT '{"enabled": false}'::json NOT NULL,
ADD COLUMN IF NOT EXISTS started_at TIMESTAMP,
ADD COLUMN IF NOT EXISTS started_by TEXT;