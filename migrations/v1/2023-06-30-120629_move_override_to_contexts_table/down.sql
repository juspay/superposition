ALTER TABLE contexts DROP COLUMN override;
CREATE TABLE overrides (
  id VARCHAR PRIMARY KEY,
  value JSON NOT NULL,
  created_at timestamp WITH time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
  created_by VARCHAR NOT NULL
);
