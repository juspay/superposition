-- Your SQL goes here
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
--
-- Name: public; Type: SCHEMA; Schema: -; Owner: -
--
CREATE SCHEMA IF NOT EXISTS public;
--
-- Name: dimension_type; Type: TYPE; Schema: public; Owner: -
--
CREATE TYPE public.dimension_type AS ENUM (
    'NULL',
    'BOOL',
    'NUMBER',
    'STRING',
    'ARRAY',
    'OBJECT'
);
--
-- Name: event_logger(); Type: FUNCTION; Schema: public; Owner: -
--
CREATE OR REPLACE FUNCTION public.event_logger() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
    old_data json;
    new_data json;
BEGIN
    IF (TG_OP = 'UPDATE') THEN
        old_data := row_to_json(OLD);
        new_data := row_to_json(NEW);
        INSERT INTO public.event_log
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
        INSERT INTO public.event_log
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
        INSERT INTO public.event_log
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
-- Name: contexts; Type: TABLE; Schema: public; Owner: -
--
CREATE TABLE public.contexts (
    id character varying PRIMARY KEY,
    value json NOT NULL,
    override_id character varying NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_by character varying NOT NULL,
    priority integer DEFAULT 1 NOT NULL,
    override json DEFAULT '{}'::json NOT NULL
);
--
-- Name: default_configs; Type: TABLE; Schema: public; Owner: -
--
CREATE TABLE public.default_configs (
    key character varying PRIMARY KEY,
    value json NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_by character varying NOT NULL,
    schema json DEFAULT '{}'::json NOT NULL
);
--
-- Name: dimensions; Type: TABLE; Schema: public; Owner: -
--
CREATE TABLE public.dimensions (
    dimension character varying PRIMARY KEY,
    priority integer NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_by character varying NOT NULL,
    schema json DEFAULT '{}'::json NOT NULL
);
--
-- Name: event_log; Type: TABLE; Schema: public; Owner: -
--
CREATE TABLE IF NOT EXISTS public.event_log (
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
-- Name: event_log_action_index; Type: INDEX; Schema: public; Owner: -
--
CREATE INDEX IF NOT EXISTS event_log_action_index ON ONLY public.event_log USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_table_name_index; Type: INDEX; Schema: public; Owner: -
--
CREATE INDEX IF NOT EXISTS event_log_table_name_index ON ONLY public.event_log USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_timestamp_index; Type: INDEX; Schema: public; Owner: -
--
CREATE INDEX IF NOT EXISTS event_log_timestamp_index ON ONLY public.event_log USING btree ("timestamp") INCLUDE (action, table_name);

--
-- Event log parititons
--
CREATE TABLE IF NOT EXISTS public.event_log_y2023m08 PARTITION OF public.event_log FOR
VALUES
FROM ('2023-08-01') TO ('2023-09-01');

CREATE TABLE IF NOT EXISTS public.event_log_y2023m09 PARTITION OF public.event_log FOR
VALUES
FROM ('2023-09-01') TO ('2023-10-01');

CREATE TABLE IF NOT EXISTS public.event_log_y2023m10 PARTITION OF public.event_log FOR
VALUES
FROM ('2023-10-01') TO ('2023-11-01');

CREATE TABLE IF NOT EXISTS public.event_log_y2023m11 PARTITION OF public.event_log FOR
VALUES
FROM ('2023-11-01') TO ('2023-12-01');

CREATE TABLE IF NOT EXISTS public.event_log_y2023m12 PARTITION OF public.event_log FOR
VALUES
FROM ('2023-12-01') TO ('2024-01-01');

CREATE TABLE IF NOT EXISTS public.event_log_y2024m01 PARTITION OF public.event_log FOR
VALUES
FROM ('2024-01-01') TO ('2024-02-01');

CREATE TABLE IF NOT EXISTS public.event_log_y2024m02 PARTITION OF public.event_log FOR
VALUES
FROM ('2024-02-01') TO ('2024-03-01');

CREATE TABLE IF NOT EXISTS public.event_log_y2024m03 PARTITION OF public.event_log FOR
VALUES
FROM ('2024-03-01') TO ('2024-04-01');

CREATE TABLE IF NOT EXISTS public.event_log_y2024m04 PARTITION OF public.event_log FOR
VALUES
FROM ('2024-04-01') TO ('2024-05-01');

CREATE TABLE IF NOT EXISTS public.event_log_y2024m05 PARTITION OF public.event_log FOR
VALUES
FROM ('2024-05-01') TO ('2024-06-01');

CREATE TABLE IF NOT EXISTS public.event_log_y2024m06 PARTITION OF public.event_log FOR
VALUES
FROM ('2024-06-01') TO ('2024-07-01');

CREATE TABLE IF NOT EXISTS public.event_log_y2024m07 PARTITION OF public.event_log FOR
VALUES
FROM ('2024-07-01') TO ('2024-08-01');

--
-- Name: contexts contexts_audit; Type: TRIGGER; Schema: public; Owner: -
--
CREATE TRIGGER contexts_audit AFTER INSERT OR DELETE OR UPDATE ON public.contexts FOR EACH ROW EXECUTE FUNCTION public.event_logger();
--
-- Name: default_configs default_configs_audit; Type: TRIGGER; Schema: public; Owner: -
--
CREATE TRIGGER default_configs_audit AFTER INSERT OR DELETE OR UPDATE ON public.default_configs FOR EACH ROW EXECUTE FUNCTION public.event_logger();
--
-- Name: dimensions dimensions_audit; Type: TRIGGER; Schema: public; Owner: -
--
CREATE TRIGGER dimensions_audit AFTER INSERT OR DELETE OR UPDATE ON public.dimensions FOR EACH ROW EXECUTE FUNCTION public.event_logger();