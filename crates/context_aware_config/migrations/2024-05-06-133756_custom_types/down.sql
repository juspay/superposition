-- This file should undo anything in `up.sql`
DELETE FROM TABLE jsonschema_types;

DROP TABLE IF EXISTS jsonschema_types;

DROP INDEX IF EXISTS json_schema_types_index;