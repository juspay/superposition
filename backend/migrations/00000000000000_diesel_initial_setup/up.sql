-- Setting up DB


CREATE EXTENSION IF NOT EXISTS "uuid-ossp";


CREATE OR REPLACE FUNCTION diesel_manage_updated_at(_tbl regclass) RETURNS VOID AS $$
BEGIN
    EXECUTE format('CREATE TRIGGER set_updated_at BEFORE UPDATE ON %s
                    FOR EACH ROW EXECUTE PROCEDURE diesel_set_updated_at()', _tbl);
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION diesel_set_updated_at() RETURNS trigger AS $$
BEGIN
    IF (
        NEW IS DISTINCT FROM OLD AND
        NEW.last_modified IS NOT DISTINCT FROM OLD.last_modified
    ) THEN
        NEW.last_modified := current_timestamp;
    END IF;
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;



CREATE TABLE IF NOT EXISTS dimensions (
  uuid uuid DEFAULT uuid_generate_v4() NOT NULL UNIQUE,
  dimension VARCHAR NOT NULL,
  priority integer NOT NULL,
  last_modified timestamp WITH time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
  created_on timestamp WITH time zone default CURRENT_TIMESTAMP NOT NULL,
  PRIMARY KEY(dimension)
);

CREATE TABLE IF NOT EXISTS global_config (
  uuid uuid DEFAULT uuid_generate_v4() NOT NULL UNIQUE,
  key VARCHAR NOT NULL,
  value json NOT NULL,
  last_modified timestamp WITH time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
  created_on timestamp WITH time zone default CURRENT_TIMESTAMP NOT NULL,
  PRIMARY KEY(key)
);

CREATE TABLE IF NOT EXISTS overrides (
  key VARCHAR NOT NULL,
  value json NOT NULL,
  last_modified timestamp WITH time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
  created_on timestamp WITH time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY(key)
);

CREATE TABLE IF NOT EXISTS contexts (
  key VARCHAR NOT NULL,
  value json NOT NULL,
  last_modified timestamp WITH time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
  created_on timestamp WITH time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY(key)
);

SELECT diesel_manage_updated_at('dimensions');
SELECT diesel_manage_updated_at('global_config');
SELECT diesel_manage_updated_at('overrides');
SELECT diesel_manage_updated_at('contexts');
