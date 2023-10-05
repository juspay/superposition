UPDATE cac_v1.dimensions SET priority=1 where dimension='os';
UPDATE cac_v1.dimensions SET priority=32 where dimension='internalUser';
UPDATE cac_v1.dimensions SET priority=64 where dimension='variantIds';
INSERT INTO cac_v1.dimensions (dimension, priority, created_by, schema)
    VALUES ('toss', '2', 'migration_down@juspay.in', '{"type":"number"}');
INSERT INTO cac_v1.dimensions (dimension, priority, created_by, schema)
    VALUES ('scope', '16', 'migration_down@juspay.in', '{"enum":["beta","release","cug"],"type":"string"}');