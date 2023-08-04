-- Your SQL goes here

-- Your SQL goes here
ALTER TABLE cac_v1.experiments
ADD COLUMN last_modified_by TEXT NOT NULL DEFAULT 'Null';
