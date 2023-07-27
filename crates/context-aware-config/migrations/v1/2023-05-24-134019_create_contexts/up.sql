CREATE TABLE contexts (
  id VARCHAR PRIMARY KEY,
  value JSON NOT NULL,
  override_id VARCHAR NOT NULL,
  created_at timestamp WITH time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
  created_by VARCHAR NOT NULL
);
