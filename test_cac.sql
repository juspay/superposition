-- Your SQL goes here
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
--
-- Name: test_cac; Type: SCHEMA; Schema: -; Owner: -
--
CREATE SCHEMA IF NOT EXISTS test_cac;
--
-- Name: dimension_type; Type: TYPE; Schema: test_cac; Owner: -
--
CREATE TYPE test_cac.dimension_type AS ENUM (
    'NULL',
    'BOOL',
    'NUMBER',
    'STRING',
    'ARRAY',
    'OBJECT'
);
--
-- Name: event_logger(); Type: FUNCTION; Schema: test_cac; Owner: -
--
CREATE OR REPLACE FUNCTION test_cac.event_logger() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
    old_data json;
    new_data json;
BEGIN
    IF (TG_OP = 'UPDATE') THEN
        old_data := row_to_json(OLD);
        new_data := row_to_json(NEW);
        INSERT INTO test_cac.event_log
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
        INSERT INTO test_cac.event_log
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
        INSERT INTO test_cac.event_log
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
$$;
SET default_tablespace = '';
SET default_table_access_method = heap;
--
-- Name: contexts; Type: TABLE; Schema: test_cac; Owner: -
--
CREATE TABLE test_cac.contexts (
    id character varying PRIMARY KEY,
    value json NOT NULL,
    override_id character varying NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_by character varying NOT NULL,
    priority integer DEFAULT 1 NOT NULL,
    override json DEFAULT '{}'::json NOT NULL
);
--
-- Name: default_configs; Type: TABLE; Schema: test_cac; Owner: -
--
CREATE TABLE test_cac.default_configs (
    key character varying PRIMARY KEY,
    value json NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_by character varying NOT NULL,
    schema json DEFAULT '{}'::json NOT NULL
);
--
-- Name: dimensions; Type: TABLE; Schema: test_cac; Owner: -
--
CREATE TABLE test_cac.dimensions (
    dimension character varying PRIMARY KEY,
    priority integer NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_by character varying NOT NULL,
    schema json DEFAULT '{}'::json NOT NULL
);
--
-- Name: event_log; Type: TABLE; Schema: test_cac; Owner: -
--
CREATE TABLE IF NOT EXISTS test_cac.event_log (
    id uuid DEFAULT uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL,
    PRIMARY KEY(id, timestamp)
) PARTITION BY RANGE ("timestamp");

--
-- Name: event_log_action_index; Type: INDEX; Schema: test_cac; Owner: -
--
CREATE INDEX IF NOT EXISTS event_log_action_index ON ONLY test_cac.event_log USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_table_name_index; Type: INDEX; Schema: test_cac; Owner: -
--
CREATE INDEX IF NOT EXISTS event_log_table_name_index ON ONLY test_cac.event_log USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_timestamp_index; Type: INDEX; Schema: test_cac; Owner: -
--
CREATE INDEX IF NOT EXISTS event_log_timestamp_index ON ONLY test_cac.event_log USING btree ("timestamp") INCLUDE (action, table_name);

--
-- Event log parititons
--
CREATE TABLE IF NOT EXISTS test_cac.event_log_y2023m08 PARTITION OF test_cac.event_log FOR
VALUES
FROM ('2023-08-01') TO ('2023-09-01');

CREATE TABLE IF NOT EXISTS test_cac.event_log_y2023m09 PARTITION OF test_cac.event_log FOR
VALUES
FROM ('2023-09-01') TO ('2023-10-01');

CREATE TABLE IF NOT EXISTS test_cac.event_log_y2023m10 PARTITION OF test_cac.event_log FOR
VALUES
FROM ('2023-10-01') TO ('2023-11-01');

CREATE TABLE IF NOT EXISTS test_cac.event_log_y2023m11 PARTITION OF test_cac.event_log FOR
VALUES
FROM ('2023-11-01') TO ('2023-12-01');

CREATE TABLE IF NOT EXISTS test_cac.event_log_y2023m12 PARTITION OF test_cac.event_log FOR
VALUES
FROM ('2023-12-01') TO ('2024-01-01');

CREATE TABLE IF NOT EXISTS test_cac.event_log_y2024m01 PARTITION OF test_cac.event_log FOR
VALUES
FROM ('2024-01-01') TO ('2024-02-01');

CREATE TABLE IF NOT EXISTS test_cac.event_log_y2024m02 PARTITION OF test_cac.event_log FOR
VALUES
FROM ('2024-02-01') TO ('2024-03-01');

CREATE TABLE IF NOT EXISTS test_cac.event_log_y2024m03 PARTITION OF test_cac.event_log FOR
VALUES
FROM ('2024-03-01') TO ('2024-04-01');

CREATE TABLE IF NOT EXISTS test_cac.event_log_y2024m04 PARTITION OF test_cac.event_log FOR
VALUES
FROM ('2024-04-01') TO ('2024-05-01');

CREATE TABLE IF NOT EXISTS test_cac.event_log_y2024m05 PARTITION OF test_cac.event_log FOR
VALUES
FROM ('2024-05-01') TO ('2024-06-01');

CREATE TABLE IF NOT EXISTS test_cac.event_log_y2024m06 PARTITION OF test_cac.event_log FOR
VALUES
FROM ('2024-06-01') TO ('2024-07-01');

CREATE TABLE IF NOT EXISTS test_cac.event_log_y2024m07 PARTITION OF test_cac.event_log FOR
VALUES
FROM ('2024-07-01') TO ('2024-08-01');

--
-- Name: contexts contexts_audit; Type: TRIGGER; Schema: test_cac; Owner: -
--
CREATE TRIGGER contexts_audit AFTER INSERT OR DELETE OR UPDATE ON test_cac.contexts FOR EACH ROW EXECUTE FUNCTION test_cac.event_logger();
--
-- Name: default_configs default_configs_audit; Type: TRIGGER; Schema: test_cac; Owner: -
--
CREATE TRIGGER default_configs_audit AFTER INSERT OR DELETE OR UPDATE ON test_cac.default_configs FOR EACH ROW EXECUTE FUNCTION test_cac.event_logger();
--
-- Name: dimensions dimensions_audit; Type: TRIGGER; Schema: test_cac; Owner: -
--
CREATE TRIGGER dimensions_audit AFTER INSERT OR DELETE OR UPDATE ON test_cac.dimensions FOR EACH ROW EXECUTE FUNCTION test_cac.event_logger();
-- Your SQL goes here
CREATE TABLE IF NOT EXISTS test_cac.event_log_y2024m08 PARTITION OF test_cac.event_log FOR
VALUES
FROM ('2024-08-01') TO ('2024-09-01');

CREATE TABLE IF NOT EXISTS test_cac.event_log_y2024m09 PARTITION OF test_cac.event_log FOR
VALUES
FROM ('2024-09-01') TO ('2024-10-01');

CREATE TABLE IF NOT EXISTS test_cac.event_log_y2024m10 PARTITION OF test_cac.event_log FOR
VALUES
FROM ('2024-10-01') TO ('2024-11-01');

CREATE TABLE IF NOT EXISTS test_cac.event_log_y2024m11 PARTITION OF test_cac.event_log FOR
VALUES
FROM ('2024-11-01') TO ('2024-12-01');

CREATE TABLE IF NOT EXISTS test_cac.event_log_y2024m12 PARTITION OF test_cac.event_log FOR
VALUES
FROM ('2024-12-01') TO ('2025-01-01');

------------ Parititions for 2025 -----------
CREATE TABLE IF NOT EXISTS test_cac.event_log_y2025m01 PARTITION OF test_cac.event_log FOR
VALUES
FROM ('2025-01-01') TO ('2025-02-01');

CREATE TABLE IF NOT EXISTS test_cac.event_log_y2025m02 PARTITION OF test_cac.event_log FOR
VALUES
FROM ('2025-02-01') TO ('2025-03-01');

CREATE TABLE IF NOT EXISTS test_cac.event_log_y2025m03 PARTITION OF test_cac.event_log FOR
VALUES
FROM ('2025-03-01') TO ('2025-04-01');

CREATE TABLE IF NOT EXISTS test_cac.event_log_y2025m04 PARTITION OF test_cac.event_log FOR
VALUES
FROM ('2025-04-01') TO ('2025-05-01');

CREATE TABLE IF NOT EXISTS test_cac.event_log_y2025m05 PARTITION OF test_cac.event_log FOR
VALUES
FROM ('2025-05-01') TO ('2025-06-01');

CREATE TABLE IF NOT EXISTS test_cac.event_log_y2025m06 PARTITION OF test_cac.event_log FOR
VALUES
FROM ('2025-06-01') TO ('2025-07-01');

CREATE TABLE IF NOT EXISTS test_cac.event_log_y2025m07 PARTITION OF test_cac.event_log FOR
VALUES
FROM ('2025-07-01') TO ('2025-08-01');

CREATE TABLE IF NOT EXISTS test_cac.event_log_y2025m08 PARTITION OF test_cac.event_log FOR
VALUES
FROM ('2025-08-01') TO ('2025-09-01');

CREATE TABLE IF NOT EXISTS test_cac.event_log_y2025m09 PARTITION OF test_cac.event_log FOR
VALUES
FROM ('2025-09-01') TO ('2025-10-01');

CREATE TABLE IF NOT EXISTS test_cac.event_log_y2025m10 PARTITION OF test_cac.event_log FOR
VALUES
FROM ('2025-10-01') TO ('2025-11-01');

CREATE TABLE IF NOT EXISTS test_cac.event_log_y2025m11 PARTITION OF test_cac.event_log FOR
VALUES
FROM ('2025-11-01') TO ('2025-12-01');

CREATE TABLE IF NOT EXISTS test_cac.event_log_y2025m12 PARTITION OF test_cac.event_log FOR
VALUES
FROM ('2025-12-01') TO ('2026-01-01');

------------ Parititions for 2026 -----------
CREATE TABLE IF NOT EXISTS test_cac.event_log_y2026m01 PARTITION OF test_cac.event_log FOR
VALUES
FROM ('2026-01-01') TO ('2026-02-01');

CREATE TABLE IF NOT EXISTS test_cac.event_log_y2026m02 PARTITION OF test_cac.event_log FOR
VALUES
FROM ('2026-02-01') TO ('2026-03-01');

CREATE TABLE IF NOT EXISTS test_cac.event_log_y2026m03 PARTITION OF test_cac.event_log FOR
VALUES
FROM ('2026-03-01') TO ('2026-04-01');

CREATE TABLE IF NOT EXISTS test_cac.event_log_y2026m04 PARTITION OF test_cac.event_log FOR
VALUES
FROM ('2026-04-01') TO ('2026-05-01');

CREATE TABLE IF NOT EXISTS test_cac.event_log_y2026m05 PARTITION OF test_cac.event_log FOR
VALUES
FROM ('2026-05-01') TO ('2026-06-01');

CREATE TABLE IF NOT EXISTS test_cac.event_log_y2026m06 PARTITION OF test_cac.event_log FOR
VALUES
FROM ('2026-06-01') TO ('2026-07-01');

CREATE TABLE IF NOT EXISTS test_cac.event_log_y2026m07 PARTITION OF test_cac.event_log FOR
VALUES
FROM ('2026-07-01') TO ('2026-08-01');

CREATE TABLE IF NOT EXISTS test_cac.event_log_y2026m08 PARTITION OF test_cac.event_log FOR
VALUES
FROM ('2026-08-01') TO ('2026-09-01');

CREATE TABLE IF NOT EXISTS test_cac.event_log_y2026m09 PARTITION OF test_cac.event_log FOR
VALUES
FROM ('2026-09-01') TO ('2026-10-01');

CREATE TABLE IF NOT EXISTS test_cac.event_log_y2026m10 PARTITION OF test_cac.event_log FOR
VALUES
FROM ('2026-10-01') TO ('2026-11-01');

CREATE TABLE IF NOT EXISTS test_cac.event_log_y2026m11 PARTITION OF test_cac.event_log FOR
VALUES
FROM ('2026-11-01') TO ('2026-12-01');

CREATE TABLE IF NOT EXISTS test_cac.event_log_y2026m12 PARTITION OF test_cac.event_log FOR
VALUES
FROM ('2026-12-01') TO ('2027-01-01');
-- Your SQL goes here
-- Name: functions; Type: TABLE; Schema: test_cac; Owner: -
--
CREATE TABLE test_cac.functions (
    function_name text PRIMARY KEY,
    published_code text,
    draft_code text NOT NULL,
    function_description text NOT NULL,
    published_runtime_version VARCHAR(16),
    draft_runtime_version VARCHAR(16) NOT NULL,
    published_at timestamp without time zone,
    draft_edited_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    published_by text,
    draft_edited_by text NOT NULL
);
--
-- Name: functions functions_audit; Type: TRIGGER; Schema: test_cac; Owner: -
--
CREATE TRIGGER functions_audit AFTER INSERT OR DELETE OR UPDATE ON test_cac.functions FOR EACH ROW EXECUTE FUNCTION test_cac.event_logger();
-- Your SQL goes here
ALTER TABLE test_cac.dimensions ADD COLUMN function_name text NULL;

ALTER TABLE test_cac.dimensions ADD FOREIGN KEY(function_name) REFERENCES test_cac.functions(function_name);

ALTER TABLE test_cac.default_configs ADD COLUMN function_name text NULL;

ALTER TABLE test_cac.default_configs ADD FOREIGN KEY(function_name) REFERENCES test_cac.functions(function_name);
