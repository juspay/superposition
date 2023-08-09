-- This file should undo anything in `up.sql`
DROP TRIGGER contexts_audit ON cac_v1.contexts;
DROP TRIGGER dimensions_audit ON cac_v1.dimensions;
DROP TRIGGER default_configs_audit ON cac_v1.default_configs;

DROP TABLE cac_v1.event_log;
