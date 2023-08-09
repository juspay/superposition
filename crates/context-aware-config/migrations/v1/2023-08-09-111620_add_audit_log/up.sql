-- Your SQL goes here
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
CREATE TABLE cac_v1.event_log (
    id UUID DEFAULT uuid_generate_v4(),
    table_name TEXT NOT NULL,
    user_name TEXT NOT NULL,
    timestamp TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    action TEXT NOT NULL,
    original_data json,
    new_data json,
    query TEXT NOT NULL,
    PRIMARY KEY(id, timestamp)
) PARTITION BY RANGE (timestamp);

CREATE INDEX event_log_table_name_index ON cac_v1.event_log (table_name) INCLUDE (action, timestamp);
CREATE INDEX event_log_timestamp_index ON cac_v1.event_log (timestamp) INCLUDE (action, table_name);
CREATE INDEX event_log_action_index on cac_v1.event_log (action) INCLUDE (timestamp, table_name);


CREATE OR REPLACE FUNCTION cac_v1.event_logger() RETURNS TRIGGER AS $$
DECLARE
    old_data json;
    new_data json;
BEGIN
    IF (TG_OP = 'UPDATE') THEN
        old_data := row_to_json(OLD);
        new_data := row_to_json(NEW);

        INSERT INTO cac_v1.event_log
            (table_name, user_name, action, original_data, new_data, query)
            VALUES (
                TG_TABLE_NAME::TEXT,
                session_user::TEXT,
                TG_OP,
                old_data,
                new_data,
                current_query()
            );

    ELSIF (TG_OP = 'DELETE') THEN
        old_data := row_to_json(OLD);

        INSERT INTO cac_v1.event_log
            (table_name, user_name, action, original_data, query)
            VALUES (
                TG_TABLE_NAME::TEXT,
                session_user::TEXT,
                TG_OP,
                old_data,
                current_query()
            );

    ELSIF (TG_OP = 'INSERT') THEN
        new_data = row_to_json(NEW);

        INSERT INTO cac_v1.event_log
            (table_name, user_name, action, new_data, query)
            VALUES (
                TG_TABLE_NAME::TEXT,
                session_user::TEXT,
                TG_OP,
                new_data,
                current_query()
            );
    END IF;

    RETURN NULL;
END;
$$ LANGUAGE 'plpgsql';

CREATE TRIGGER contexts_audit
AFTER INSERT OR UPDATE OR DELETE ON cac_v1.contexts
FOR EACH ROW EXECUTE PROCEDURE cac_v1.event_logger();

CREATE TRIGGER dimensions_audit
AFTER INSERT OR UPDATE OR DELETE ON cac_v1.dimensions
FOR EACH ROW EXECUTE PROCEDURE cac_v1.event_logger();

CREATE TRIGGER default_configs_audit
AFTER INSERT OR UPDATE OR DELETE ON cac_v1.default_configs
FOR EACH ROW EXECUTE PROCEDURE cac_v1.event_logger();

-- Partitions for event_log table
CREATE TABLE cac_v1.event_log_y2023m08 PARTITION OF cac_v1.event_log FOR
VALUES
FROM ('2023-08-01') TO ('2023-09-01');

CREATE TABLE cac_v1.event_log_y2023m09 PARTITION OF cac_v1.event_log FOR
VALUES
FROM ('2023-09-01') TO ('2023-10-01');

CREATE TABLE cac_v1.event_log_y2023m10 PARTITION OF cac_v1.event_log FOR
VALUES
FROM ('2023-10-01') TO ('2023-11-01');

CREATE TABLE cac_v1.event_log_y2023m11 PARTITION OF cac_v1.event_log FOR
VALUES
FROM ('2023-11-01') TO ('2023-12-01');

CREATE TABLE cac_v1.event_log_y2023m12 PARTITION OF cac_v1.event_log FOR
VALUES
FROM ('2023-12-01') TO ('2024-01-01');

CREATE TABLE cac_v1.event_log_y2024m01 PARTITION OF cac_v1.event_log FOR
VALUES
FROM ('2024-01-01') TO ('2024-02-01');

CREATE TABLE cac_v1.event_log_y2024m02 PARTITION OF cac_v1.event_log FOR
VALUES
FROM ('2024-02-01') TO ('2024-03-01');

CREATE TABLE cac_v1.event_log_y2024m03 PARTITION OF cac_v1.event_log FOR
VALUES
FROM ('2024-03-01') TO ('2024-04-01');

CREATE TABLE cac_v1.event_log_y2024m04 PARTITION OF cac_v1.event_log FOR
VALUES
FROM ('2024-04-01') TO ('2024-05-01');

CREATE TABLE cac_v1.event_log_y2024m05 PARTITION OF cac_v1.event_log FOR
VALUES
FROM ('2024-05-01') TO ('2024-06-01');

CREATE TABLE cac_v1.event_log_y2024m06 PARTITION OF cac_v1.event_log FOR
VALUES
FROM ('2024-06-01') TO ('2024-07-01');

CREATE TABLE cac_v1.event_log_y2024m07 PARTITION OF cac_v1.event_log FOR
VALUES
FROM ('2024-07-01') TO ('2024-08-01');
