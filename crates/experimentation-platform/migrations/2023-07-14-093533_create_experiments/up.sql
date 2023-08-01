-- Your SQL goes here
CREATE TYPE cac_v1.experiment_status_type as enum ('CREATED', 'CONCLUDED', 'INPROGRESS');
CREATE DOMAIN cac_v1.not_null_text as TEXT NOT NULL;

CREATE TABLE cac_v1.experiments (
  id BIGINT PRIMARY KEY,
  created_at timestamp WITH time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
  created_by TEXT NOT NULL,
  last_modified timestamp WITH time zone,

  name TEXT NOT NULL,
  override_keys cac_v1.not_null_text [] NOT NULL,
  status cac_v1.experiment_status_type NOT NULL,
  traffic_percentage INTEGER NOT NULL CHECK (traffic_percentage >= 0),

  context JSON NOT NULL,
  variants JSON NOT NULL
);

CREATE INDEX experiment_created_date_index ON cac_v1.experiments (created_at) INCLUDE (id);
CREATE INDEX experiment_last_modified_index ON cac_v1.experiments (last_modified) INCLUDE (id, created_at);
