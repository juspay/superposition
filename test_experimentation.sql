-- Your SQL goes here
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
--
-- Name: test_experimentation; Type: SCHEMA; Schema: -; Owner: -
--
CREATE SCHEMA IF NOT EXISTS test_experimentation;
--
-- Name: experiment_status_type; Type: TYPE; Schema: test_experimentation; Owner: -
--
CREATE TYPE test_experimentation.experiment_status_type AS ENUM (
    'CREATED',
    'CONCLUDED',
    'INPROGRESS'
);
--
-- Name: not_null_text; Type: DOMAIN; Schema: test_experimentation; Owner: -
--
CREATE DOMAIN test_experimentation.not_null_text AS text NOT NULL;
--
-- Name: event_logger(); Type: FUNCTION; Schema: test_experimentation; Owner: -
--
CREATE OR REPLACE FUNCTION test_experimentation.event_logger() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
    old_data json;
    new_data json;
BEGIN
    IF (TG_OP = 'UPDATE') THEN
        old_data := row_to_json(OLD);
        new_data := row_to_json(NEW);
        INSERT INTO test_experimentation.event_log
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
        INSERT INTO test_experimentation.event_log
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
        INSERT INTO test_experimentation.event_log
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
-- Name: experiments; Type: TABLE; Schema: test_experimentation; Owner: -
--
CREATE TABLE test_experimentation.experiments (
    id bigint PRIMARY KEY,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_by text NOT NULL,
    last_modified timestamp with time zone DEFAULT now() NOT NULL,
    name text NOT NULL,
    override_keys test_experimentation.not_null_text[] NOT NULL,
    status test_experimentation.experiment_status_type NOT NULL,
    traffic_percentage integer NOT NULL,
    context json NOT NULL,
    variants json NOT NULL,
    last_modified_by text DEFAULT 'Null'::text NOT NULL,
    chosen_variant text,
    CONSTRAINT experiments_traffic_percentage_check CHECK ((traffic_percentage >= 0))
);
--
-- Name: experiment_created_date_index; Type: INDEX; Schema: test_experimentation; Owner: -
--
CREATE INDEX experiment_created_date_index ON test_experimentation.experiments USING btree (created_at) INCLUDE (id);
--
-- Name: experiment_last_modified_index; Type: INDEX; Schema: test_experimentation; Owner: -
--
CREATE INDEX experiment_last_modified_index ON test_experimentation.experiments USING btree (last_modified) INCLUDE (id, created_at);
--
-- Name: experiment_status_index; Type: INDEX; Schema: test_experimentation; Owner: -
--
CREATE INDEX experiment_status_index ON test_experimentation.experiments USING btree (status) INCLUDE (created_at, last_modified);

--
-- Name: event_log; Type: TABLE; Schema: test_experimentation; Owner: -
--
CREATE TABLE IF NOT EXISTS test_experimentation.event_log (
    id uuid DEFAULT uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL,
    PRIMARY KEY(id, timestamp)
)
PARTITION BY RANGE ("timestamp");

--
-- Name: event_log_action_index; Type: INDEX; Schema: test_experimentation; Owner: -
--
CREATE INDEX IF NOT EXISTS event_log_action_index ON ONLY test_experimentation.event_log USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_table_name_index; Type: INDEX; Schema: test_experimentation; Owner: -
--
CREATE INDEX IF NOT EXISTS event_log_table_name_index ON ONLY test_experimentation.event_log USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_timestamp_index; Type: INDEX; Schema: test_experimentation; Owner: -
--
CREATE INDEX IF NOT EXISTS event_log_timestamp_index ON ONLY test_experimentation.event_log USING btree ("timestamp") INCLUDE (action, table_name);

--
-- event_log table partitions
--
CREATE TABLE IF NOT EXISTS test_experimentation.event_log_y2023m08 PARTITION OF test_experimentation.event_log FOR
VALUES
FROM ('2023-08-01') TO ('2023-09-01');

CREATE TABLE IF NOT EXISTS test_experimentation.event_log_y2023m09 PARTITION OF test_experimentation.event_log FOR
VALUES
FROM ('2023-09-01') TO ('2023-10-01');

CREATE TABLE IF NOT EXISTS test_experimentation.event_log_y2023m10 PARTITION OF test_experimentation.event_log FOR
VALUES
FROM ('2023-10-01') TO ('2023-11-01');

CREATE TABLE IF NOT EXISTS test_experimentation.event_log_y2023m11 PARTITION OF test_experimentation.event_log FOR
VALUES
FROM ('2023-11-01') TO ('2023-12-01');

CREATE TABLE IF NOT EXISTS test_experimentation.event_log_y2023m12 PARTITION OF test_experimentation.event_log FOR
VALUES
FROM ('2023-12-01') TO ('2024-01-01');

CREATE TABLE IF NOT EXISTS test_experimentation.event_log_y2024m01 PARTITION OF test_experimentation.event_log FOR
VALUES
FROM ('2024-01-01') TO ('2024-02-01');

CREATE TABLE IF NOT EXISTS test_experimentation.event_log_y2024m02 PARTITION OF test_experimentation.event_log FOR
VALUES
FROM ('2024-02-01') TO ('2024-03-01');

CREATE TABLE IF NOT EXISTS test_experimentation.event_log_y2024m03 PARTITION OF test_experimentation.event_log FOR
VALUES
FROM ('2024-03-01') TO ('2024-04-01');

CREATE TABLE IF NOT EXISTS test_experimentation.event_log_y2024m04 PARTITION OF test_experimentation.event_log FOR
VALUES
FROM ('2024-04-01') TO ('2024-05-01');

CREATE TABLE IF NOT EXISTS test_experimentation.event_log_y2024m05 PARTITION OF test_experimentation.event_log FOR
VALUES
FROM ('2024-05-01') TO ('2024-06-01');

CREATE TABLE IF NOT EXISTS test_experimentation.event_log_y2024m06 PARTITION OF test_experimentation.event_log FOR
VALUES
FROM ('2024-06-01') TO ('2024-07-01');

CREATE TABLE IF NOT EXISTS test_experimentation.event_log_y2024m07 PARTITION OF test_experimentation.event_log FOR
VALUES
FROM ('2024-07-01') TO ('2024-08-01');

--
-- Name: experiments experiments_audit; Type: TRIGGER; Schema: test_experimentation; Owner: -
--
CREATE TRIGGER experiments_audit AFTER INSERT OR DELETE OR UPDATE ON test_experimentation.experiments FOR EACH ROW EXECUTE FUNCTION test_experimentation.event_logger();
-- Your SQL goes here
CREATE TABLE IF NOT EXISTS test_experimentation.event_log_y2024m08 PARTITION OF test_experimentation.event_log FOR
VALUES
FROM ('2024-08-01') TO ('2024-09-01');

CREATE TABLE IF NOT EXISTS test_experimentation.event_log_y2024m09 PARTITION OF test_experimentation.event_log FOR
VALUES
FROM ('2024-09-01') TO ('2024-10-01');

CREATE TABLE IF NOT EXISTS test_experimentation.event_log_y2024m10 PARTITION OF test_experimentation.event_log FOR
VALUES
FROM ('2024-10-01') TO ('2024-11-01');

CREATE TABLE IF NOT EXISTS test_experimentation.event_log_y2024m11 PARTITION OF test_experimentation.event_log FOR
VALUES
FROM ('2024-11-01') TO ('2024-12-01');

CREATE TABLE IF NOT EXISTS test_experimentation.event_log_y2024m12 PARTITION OF test_experimentation.event_log FOR
VALUES
FROM ('2024-12-01') TO ('2025-01-01');

------------ Parititions for 2025 -----------
CREATE TABLE IF NOT EXISTS test_experimentation.event_log_y2025m01 PARTITION OF test_experimentation.event_log FOR
VALUES
FROM ('2025-01-01') TO ('2025-02-01');

CREATE TABLE IF NOT EXISTS test_experimentation.event_log_y2025m02 PARTITION OF test_experimentation.event_log FOR
VALUES
FROM ('2025-02-01') TO ('2025-03-01');

CREATE TABLE IF NOT EXISTS test_experimentation.event_log_y2025m03 PARTITION OF test_experimentation.event_log FOR
VALUES
FROM ('2025-03-01') TO ('2025-04-01');

CREATE TABLE IF NOT EXISTS test_experimentation.event_log_y2025m04 PARTITION OF test_experimentation.event_log FOR
VALUES
FROM ('2025-04-01') TO ('2025-05-01');

CREATE TABLE IF NOT EXISTS test_experimentation.event_log_y2025m05 PARTITION OF test_experimentation.event_log FOR
VALUES
FROM ('2025-05-01') TO ('2025-06-01');

CREATE TABLE IF NOT EXISTS test_experimentation.event_log_y2025m06 PARTITION OF test_experimentation.event_log FOR
VALUES
FROM ('2025-06-01') TO ('2025-07-01');

CREATE TABLE IF NOT EXISTS test_experimentation.event_log_y2025m07 PARTITION OF test_experimentation.event_log FOR
VALUES
FROM ('2025-07-01') TO ('2025-08-01');

CREATE TABLE IF NOT EXISTS test_experimentation.event_log_y2025m08 PARTITION OF test_experimentation.event_log FOR
VALUES
FROM ('2025-08-01') TO ('2025-09-01');

CREATE TABLE IF NOT EXISTS test_experimentation.event_log_y2025m09 PARTITION OF test_experimentation.event_log FOR
VALUES
FROM ('2025-09-01') TO ('2025-10-01');

CREATE TABLE IF NOT EXISTS test_experimentation.event_log_y2025m10 PARTITION OF test_experimentation.event_log FOR
VALUES
FROM ('2025-10-01') TO ('2025-11-01');

CREATE TABLE IF NOT EXISTS test_experimentation.event_log_y2025m11 PARTITION OF test_experimentation.event_log FOR
VALUES
FROM ('2025-11-01') TO ('2025-12-01');

CREATE TABLE IF NOT EXISTS test_experimentation.event_log_y2025m12 PARTITION OF test_experimentation.event_log FOR
VALUES
FROM ('2025-12-01') TO ('2026-01-01');

------------ Parititions for 2026 -----------
CREATE TABLE IF NOT EXISTS test_experimentation.event_log_y2026m01 PARTITION OF test_experimentation.event_log FOR
VALUES
FROM ('2026-01-01') TO ('2026-02-01');

CREATE TABLE IF NOT EXISTS test_experimentation.event_log_y2026m02 PARTITION OF test_experimentation.event_log FOR
VALUES
FROM ('2026-02-01') TO ('2026-03-01');

CREATE TABLE IF NOT EXISTS test_experimentation.event_log_y2026m03 PARTITION OF test_experimentation.event_log FOR
VALUES
FROM ('2026-03-01') TO ('2026-04-01');

CREATE TABLE IF NOT EXISTS test_experimentation.event_log_y2026m04 PARTITION OF test_experimentation.event_log FOR
VALUES
FROM ('2026-04-01') TO ('2026-05-01');

CREATE TABLE IF NOT EXISTS test_experimentation.event_log_y2026m05 PARTITION OF test_experimentation.event_log FOR
VALUES
FROM ('2026-05-01') TO ('2026-06-01');

CREATE TABLE IF NOT EXISTS test_experimentation.event_log_y2026m06 PARTITION OF test_experimentation.event_log FOR
VALUES
FROM ('2026-06-01') TO ('2026-07-01');

CREATE TABLE IF NOT EXISTS test_experimentation.event_log_y2026m07 PARTITION OF test_experimentation.event_log FOR
VALUES
FROM ('2026-07-01') TO ('2026-08-01');

CREATE TABLE IF NOT EXISTS test_experimentation.event_log_y2026m08 PARTITION OF test_experimentation.event_log FOR
VALUES
FROM ('2026-08-01') TO ('2026-09-01');

CREATE TABLE IF NOT EXISTS test_experimentation.event_log_y2026m09 PARTITION OF test_experimentation.event_log FOR
VALUES
FROM ('2026-09-01') TO ('2026-10-01');

CREATE TABLE IF NOT EXISTS test_experimentation.event_log_y2026m10 PARTITION OF test_experimentation.event_log FOR
VALUES
FROM ('2026-10-01') TO ('2026-11-01');

CREATE TABLE IF NOT EXISTS test_experimentation.event_log_y2026m11 PARTITION OF test_experimentation.event_log FOR
VALUES
FROM ('2026-11-01') TO ('2026-12-01');

CREATE TABLE IF NOT EXISTS test_experimentation.event_log_y2026m12 PARTITION OF test_experimentation.event_log FOR
VALUES
FROM ('2026-12-01') TO ('2027-01-01');
