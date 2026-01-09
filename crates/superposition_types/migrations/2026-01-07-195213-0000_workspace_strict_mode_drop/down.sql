-- This file should undo anything in `up.sql`
ALTER TABLE superposition.workspaces ADD COLUMN strict_mode BOOLEAN DEFAULT TRUE NOT NULL;
