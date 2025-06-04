-- This file should undo anything in `up.sql`
ALTER TABLE superposition.workspaces drop column if exists config_version;