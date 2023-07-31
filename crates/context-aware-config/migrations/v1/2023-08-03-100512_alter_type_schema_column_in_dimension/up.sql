-- Your SQL goes here
ALTER TABLE cac_v1.dimensions ADD COLUMN schema JSON NOT NULL DEFAULT '{}';
ALTER TABLE cac_v1.dimensions DROP COLUMN type;

UPDATE cac_v1.dimensions SET schema = '{"enum":["android","ios","web"],"type":"string"}' WHERE dimension = 'os';
UPDATE cac_v1.dimensions SET schema = '{"type":"number"}' WHERE dimension = 'toss';
UPDATE cac_v1.dimensions SET schema = '{"pattern":"^[a-z0-9].*$","type":"string"}' WHERE dimension = 'clientId';
UPDATE cac_v1.dimensions SET schema = '{"enum":["beta","release","cug"],"type":"string"}' WHERE dimension = 'scope';
UPDATE cac_v1.dimensions SET schema = '{"type":"boolean"}' WHERE dimension = 'internalUser';
UPDATE cac_v1.dimensions SET schema = '{"type":"number"}' WHERE dimension = 'tier';
