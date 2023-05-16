
CREATE table dimensions (
  dimension VARCHAR NOT NULL,
  priority integer NOT NULL,
  last_modified timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
  created_on timestamp with time zone default CURRENT_TIMESTAMP NOT NULL,
  PRIMARY KEY(dimension)
);

CREATE TABLE global_config (
  key VARCHAR NOT NULL,
  value json NOT NULL,
  last_modified timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
  created_on timestamp with time zone default CURRENT_TIMESTAMP NOT NULL,
  PRIMARY KEY(key)
);

CREATE TABLE overrides (
  key VARCHAR NOT NULL,
  value json NOT NULL,
  last_modified timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
  created_on timestamp with time zone default CURRENT_TIMESTAMP NOT NULL,
  PRIMARY KEY(key)
);

CREATE TABLE contexts (
  key VARCHAR NOT NULL,
  value VARCHAR NOT NULL,
  last_modified timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
  created_on timestamp with time zone default CURRENT_TIMESTAMP NOT NULL,
  PRIMARY KEY(key)
);

CREATE TABLE ctxoverrides (
  context_id VARCHAR NOT NULL,
  override_id VARCHAR NOT NULL,
  last_modified timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
  created_on timestamp with time zone default CURRENT_TIMESTAMP NOT NULL,
  PRIMARY KEY(context_id)
);

CREATE TABLE newcontexts (
  key VARCHAR NOT NULL,
  value JSON NOT NULL,
  column1 VARCHAR,
  column2 VARCHAR,
  column3 VARCHAR,
  column4 VARCHAR,
  last_modified timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
  created_on timestamp with time zone default CURRENT_TIMESTAMP NOT NULL,
  PRIMARY KEY(key)
);
