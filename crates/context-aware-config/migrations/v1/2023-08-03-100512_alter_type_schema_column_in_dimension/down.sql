-- This file should undo anything in `up.sql`
ALTER TABLE cac_v1.dimensions DROP COLUMN schema;
ALTER TABLE cac_v1.dimensions ADD COLUMN type dimension_type NOT NULL DEFAULT 'STRING';

UPDATE cac_v1.dimensions SET type = 'NUMBER' WHERE dimension = 'toss';
UPDATE cac_v1.dimensions SET type = 'NUMBER' WHERE dimension = 'tier';
UPDATE cac_v1.dimensions SET type = 'BOOL' WHERE dimension = 'internalUser';
