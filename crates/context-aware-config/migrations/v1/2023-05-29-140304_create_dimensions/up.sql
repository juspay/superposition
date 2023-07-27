CREATE TYPE dimension_type as enum ('NULL', 'BOOL', 'NUMBER', 'STRING', 'ARRAY', 'OBJECT');

CREATE TABLE dimensions (
  dimension VARCHAR PRIMARY KEY,
  priority integer NOT NULL,
  type dimension_type NOT NULL,
  created_at timestamp WITH time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
  created_by VARCHAR NOT NULL
);
