DELETE FROM cac_v1.dimensions where dimension='toss';
DELETE FROM cac_v1.dimensions where dimension='scope';
UPDATE cac_v1.dimensions SET priority=1 where dimension='variantIds';
UPDATE cac_v1.dimensions SET priority=2 where dimension='os';
UPDATE cac_v1.dimensions SET priority=16 where dimension='internalUser';