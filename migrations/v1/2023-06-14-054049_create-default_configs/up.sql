CREATE TABLE IF NOT EXISTS default_configs (
  key varchar PRIMARY KEY,
  value JSON NOT NULL,
  created_at timestamp WITH time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
  created_by varchar NOT NULL
);
