CREATE SCHEMA IF NOT EXISTS cac_v1;

ALTER TABLE contexts
    SET SCHEMA cac_v1;

ALTER TABLE default_configs
    SET SCHEMA cac_v1;

ALTER TABLE dimensions
    SET SCHEMA cac_v1;
