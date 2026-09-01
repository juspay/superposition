ALTER TABLE superposition.workspaces
DROP COLUMN IF EXISTS merge_strategy;

DROP TYPE IF EXISTS superposition.merge_strategy;
