-- Your SQL goes here
ALTER TABLE superposition.workspaces ADD COLUMN IF NOT EXISTS allow_experiment_self_approval boolean NOT NULL DEFAULT false;