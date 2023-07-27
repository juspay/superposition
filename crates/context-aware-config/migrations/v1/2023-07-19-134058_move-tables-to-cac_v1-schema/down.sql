-- This file should undo anything in `up.sql`
ALTER TABLE cac_v1.contexts
    SET SCHEMA public;

ALTER TABLE cac_v1.default_configs
    SET SCHEMA public;

ALTER TABLE cac_v1.dimensions
    SET SCHEMA public;
