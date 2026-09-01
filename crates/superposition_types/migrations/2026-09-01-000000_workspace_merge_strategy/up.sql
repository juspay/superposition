DO $$ BEGIN
    CREATE TYPE superposition.merge_strategy AS ENUM ('MERGE', 'REPLACE');
EXCEPTION
    WHEN duplicate_object THEN null;
END $$;

ALTER TABLE superposition.workspaces
ADD COLUMN IF NOT EXISTS merge_strategy superposition.MERGE_STRATEGY NOT NULL DEFAULT 'MERGE';
