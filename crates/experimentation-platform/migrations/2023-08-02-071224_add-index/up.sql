-- Your SQL goes here
ALTER TABLE cac_v1.experiments
ALTER COLUMN last_modified SET NOT NULL,
ALTER COLUMN last_modified SET DEFAULT NOW();
CREATE INDEX experiment_status_index ON cac_v1.experiments (status) INCLUDE (created_at, last_modified);
