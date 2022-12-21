CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE table dimensions ( 
  uuid uuid DEFAULT uuid_generate_v4() NOT NULL UNIQUE, 
  dimension VARCHAR NOT NULL, 
  priority integer NOT NULL, 
  last_modified timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP, 
  created_on timestamp with time zone default CURRENT_TIMESTAMP NOT NULL,
  PRIMARY KEY(dimension) 
);

CREATE TABLE global_config (
  uuid uuid DEFAULT uuid_generate_v4() NOT NULL UNIQUE,
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