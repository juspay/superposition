-- Your SQL goes here
CREATE TYPE experiment_status_type as enum ('CREATED', 'CONCLUDED', 'INPROGRESS');

CREATE TABLE experiments (
  id BIGINT PRIMARY KEY,
  created_at timestamp WITH time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
  created_by TEXT NOT NULL,
  last_modified timestamp WITH time zone,

  name TEXT NOT NULL,
  override_keys TEXT[] NOT NULL,
  status experiment_status_type NOT NULL,
  traffic_percentage INTEGER NOT NULL CHECK (traffic_percentage >= 0),

  context JSON NOT NULL,
  variants JSON NOT NULL,

  constraint check_override_keys_not_null
    check ( array_position(override_keys, null) is null and override_keys <> '{}' )
);

CREATE INDEX experiment_created_date_index ON experiments (created_at) INCLUDE (id);
CREATE INDEX experiment_last_modified_index ON experiments (last_modified) INCLUDE (id, created_at);
