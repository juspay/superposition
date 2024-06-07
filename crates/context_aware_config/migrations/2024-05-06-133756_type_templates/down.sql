-- This file should undo anything in `up.sql`
DELETE FROM TABLE type_templates;

DROP TABLE IF EXISTS type_templates;

DROP INDEX IF EXISTS type_templates_index;
DROP INDEX IF EXISTS type_templates_created_at_index;
DROP INDEX IF EXISTS type_templates_last_modifed_index;
