--
-- PostgreSQL database dump
--
-- Dumped from database version 12.22
-- Dumped by pg_dump version 12.22
SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;
--
-- Name: dev; Type: SCHEMA; Schema: -; Owner: postgres
--
CREATE SCHEMA dev;
ALTER SCHEMA dev OWNER TO postgres;
--
-- Name: test; Type: SCHEMA; Schema: -; Owner: postgres
--
CREATE SCHEMA test;
ALTER SCHEMA test OWNER TO postgres;
--
-- Name: uuid-ossp; Type: EXTENSION; Schema: -; Owner: -
--
CREATE EXTENSION IF NOT EXISTS "uuid-ossp" WITH SCHEMA public;
--
-- Name: EXTENSION "uuid-ossp"; Type: COMMENT; Schema: -; Owner:
--
COMMENT ON EXTENSION "uuid-ossp" IS 'generate universally unique identifiers (UUIDs)';
--
-- Name: dimension_type; Type: TYPE; Schema: dev; Owner: postgres
--
CREATE TYPE dev.dimension_type AS ENUM (
    'NULL',
    'BOOL',
    'NUMBER',
    'STRING',
    'ARRAY',
    'OBJECT'
);
ALTER TYPE dev.dimension_type OWNER TO postgres;
--
-- Name: experiment_status_type; Type: TYPE; Schema: dev; Owner: postgres
--
CREATE TYPE dev.experiment_status_type AS ENUM (
    'CREATED',
    'CONCLUDED',
    'INPROGRESS'
);
ALTER TYPE dev.experiment_status_type OWNER TO postgres;
--
-- Name: not_null_text; Type: DOMAIN; Schema: dev; Owner: postgres
--
CREATE DOMAIN dev.not_null_text AS text NOT NULL;
ALTER DOMAIN dev.not_null_text OWNER TO postgres;
--
-- Name: workspace_status; Type: TYPE; Schema: dev; Owner: postgres
--
CREATE TYPE dev.workspace_status AS ENUM ('ENABLED', 'DISABLED');
ALTER TYPE dev.workspace_status OWNER TO postgres;
--
-- Name: dimension_type; Type: TYPE; Schema: test; Owner: postgres
--
CREATE TYPE test.dimension_type AS ENUM (
    'NULL',
    'BOOL',
    'NUMBER',
    'STRING',
    'ARRAY',
    'OBJECT'
);
ALTER TYPE test.dimension_type OWNER TO postgres;
--
-- Name: experiment_status_type; Type: TYPE; Schema: test; Owner: postgres
--
CREATE TYPE test.experiment_status_type AS ENUM (
    'CREATED',
    'CONCLUDED',
    'INPROGRESS'
);
ALTER TYPE test.experiment_status_type OWNER TO postgres;
--
-- Name: not_null_text; Type: DOMAIN; Schema: test; Owner: postgres
--
CREATE DOMAIN test.not_null_text AS text NOT NULL;
ALTER DOMAIN test.not_null_text OWNER TO postgres;
--
-- Name: workspace_status; Type: TYPE; Schema: test; Owner: postgres
--
CREATE TYPE test.workspace_status AS ENUM ('ENABLED', 'DISABLED');
ALTER TYPE test.workspace_status OWNER TO postgres;
--
-- Name: event_logger(); Type: FUNCTION; Schema: dev; Owner: postgres
--
CREATE FUNCTION dev.event_logger() RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE old_data json;
new_data json;
BEGIN IF (TG_OP = 'UPDATE') THEN old_data := row_to_json(OLD);
new_data := row_to_json(NEW);
INSERT INTO dev.event_log (
        table_name,
        user_name,
        action,
        original_data,
        new_data,
        query
    )
VALUES (
        TG_TABLE_NAME::TEXT,
        session_user::TEXT,
        TG_OP,
        old_data,
        new_data,
        current_query()
    );
ELSIF (TG_OP = 'DELETE') THEN old_data := row_to_json(OLD);
INSERT INTO dev.event_log (
        table_name,
        user_name,
        action,
        original_data,
        query
    )
VALUES (
        TG_TABLE_NAME::TEXT,
        session_user::TEXT,
        TG_OP,
        old_data,
        current_query()
    );
ELSIF (TG_OP = 'INSERT') THEN new_data = row_to_json(NEW);
INSERT INTO dev.event_log (table_name, user_name, action, new_data, query)
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
ALTER FUNCTION dev.event_logger() OWNER TO postgres;
--
-- Name: event_logger(); Type: FUNCTION; Schema: test; Owner: postgres
--
CREATE FUNCTION test.event_logger() RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE old_data json;
new_data json;
BEGIN IF (TG_OP = 'UPDATE') THEN old_data := row_to_json(OLD);
new_data := row_to_json(NEW);
INSERT INTO test.event_log (
        table_name,
        user_name,
        action,
        original_data,
        new_data,
        query
    )
VALUES (
        TG_TABLE_NAME::TEXT,
        session_user::TEXT,
        TG_OP,
        old_data,
        new_data,
        current_query()
    );
ELSIF (TG_OP = 'DELETE') THEN old_data := row_to_json(OLD);
INSERT INTO test.event_log (
        table_name,
        user_name,
        action,
        original_data,
        query
    )
VALUES (
        TG_TABLE_NAME::TEXT,
        session_user::TEXT,
        TG_OP,
        old_data,
        current_query()
    );
ELSIF (TG_OP = 'INSERT') THEN new_data = row_to_json(NEW);
INSERT INTO test.event_log (table_name, user_name, action, new_data, query)
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
ALTER FUNCTION test.event_logger() OWNER TO postgres;
SET default_tablespace = '';
SET default_table_access_method = heap;
--
-- Name: config_versions; Type: TABLE; Schema: dev; Owner: postgres
--
CREATE TABLE dev.config_versions (
    id bigint NOT NULL,
    config json NOT NULL,
    config_hash text NOT NULL,
    tags character varying(100) [],
    created_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT config_versions_tags_check CHECK (
        (
            array_position(tags, NULL::character varying) IS NULL
        )
    )
);
ALTER TABLE dev.config_versions OWNER TO postgres;
--
-- Name: contexts; Type: TABLE; Schema: dev; Owner: postgres
--
CREATE TABLE dev.contexts (
    id character varying NOT NULL,
    value json NOT NULL,
    override_id character varying NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_by character varying NOT NULL,
    priority integer DEFAULT 1 NOT NULL,
    override json DEFAULT '{}'::json NOT NULL,
    last_modified_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    last_modified_by character varying(200) DEFAULT 'null'::character varying NOT NULL,
    weight numeric(1000, 0) DEFAULT 1 NOT NULL
);
ALTER TABLE dev.contexts OWNER TO postgres;
--
-- Name: default_configs; Type: TABLE; Schema: dev; Owner: postgres
--
CREATE TABLE dev.default_configs (
    key character varying NOT NULL,
    value json NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_by character varying NOT NULL,
    schema json DEFAULT '{}'::json NOT NULL,
    function_name text,
    last_modified_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    last_modified_by character varying(200) DEFAULT 'null'::character varying NOT NULL
);
ALTER TABLE dev.default_configs OWNER TO postgres;
--
-- Name: dimensions; Type: TABLE; Schema: dev; Owner: postgres
--
CREATE TABLE dev.dimensions (
    dimension character varying NOT NULL,
    priority integer DEFAULT 1 NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_by character varying NOT NULL,
    schema json DEFAULT '{}'::json NOT NULL,
    function_name text,
    last_modified_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    last_modified_by character varying(200) DEFAULT 'null'::character varying NOT NULL,
    "position" integer DEFAULT 0 NOT NULL
);
ALTER TABLE dev.dimensions OWNER TO postgres;
--
-- Name: event_log; Type: TABLE; Schema: dev; Owner: postgres
--
CREATE TABLE dev.event_log (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
) PARTITION BY RANGE ("timestamp");
ALTER TABLE dev.event_log OWNER TO postgres;
--
-- Name: event_log_y2023m08; Type: TABLE; Schema: dev; Owner: postgres
--
CREATE TABLE dev.event_log_y2023m08 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev.event_log ATTACH PARTITION dev.event_log_y2023m08 FOR
VALUES
FROM ('2023-08-01 00:00:00') TO ('2023-09-01 00:00:00');
ALTER TABLE dev.event_log_y2023m08 OWNER TO postgres;
--
-- Name: event_log_y2023m09; Type: TABLE; Schema: dev; Owner: postgres
--
CREATE TABLE dev.event_log_y2023m09 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev.event_log ATTACH PARTITION dev.event_log_y2023m09 FOR
VALUES
FROM ('2023-09-01 00:00:00') TO ('2023-10-01 00:00:00');
ALTER TABLE dev.event_log_y2023m09 OWNER TO postgres;
--
-- Name: event_log_y2023m10; Type: TABLE; Schema: dev; Owner: postgres
--
CREATE TABLE dev.event_log_y2023m10 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev.event_log ATTACH PARTITION dev.event_log_y2023m10 FOR
VALUES
FROM ('2023-10-01 00:00:00') TO ('2023-11-01 00:00:00');
ALTER TABLE dev.event_log_y2023m10 OWNER TO postgres;
--
-- Name: event_log_y2023m11; Type: TABLE; Schema: dev; Owner: postgres
--
CREATE TABLE dev.event_log_y2023m11 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev.event_log ATTACH PARTITION dev.event_log_y2023m11 FOR
VALUES
FROM ('2023-11-01 00:00:00') TO ('2023-12-01 00:00:00');
ALTER TABLE dev.event_log_y2023m11 OWNER TO postgres;
--
-- Name: event_log_y2023m12; Type: TABLE; Schema: dev; Owner: postgres
--
CREATE TABLE dev.event_log_y2023m12 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev.event_log ATTACH PARTITION dev.event_log_y2023m12 FOR
VALUES
FROM ('2023-12-01 00:00:00') TO ('2024-01-01 00:00:00');
ALTER TABLE dev.event_log_y2023m12 OWNER TO postgres;
--
-- Name: event_log_y2024m01; Type: TABLE; Schema: dev; Owner: postgres
--
CREATE TABLE dev.event_log_y2024m01 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev.event_log ATTACH PARTITION dev.event_log_y2024m01 FOR
VALUES
FROM ('2024-01-01 00:00:00') TO ('2024-02-01 00:00:00');
ALTER TABLE dev.event_log_y2024m01 OWNER TO postgres;
--
-- Name: event_log_y2024m02; Type: TABLE; Schema: dev; Owner: postgres
--
CREATE TABLE dev.event_log_y2024m02 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev.event_log ATTACH PARTITION dev.event_log_y2024m02 FOR
VALUES
FROM ('2024-02-01 00:00:00') TO ('2024-03-01 00:00:00');
ALTER TABLE dev.event_log_y2024m02 OWNER TO postgres;
--
-- Name: event_log_y2024m03; Type: TABLE; Schema: dev; Owner: postgres
--
CREATE TABLE dev.event_log_y2024m03 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev.event_log ATTACH PARTITION dev.event_log_y2024m03 FOR
VALUES
FROM ('2024-03-01 00:00:00') TO ('2024-04-01 00:00:00');
ALTER TABLE dev.event_log_y2024m03 OWNER TO postgres;
--
-- Name: event_log_y2024m04; Type: TABLE; Schema: dev; Owner: postgres
--
CREATE TABLE dev.event_log_y2024m04 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev.event_log ATTACH PARTITION dev.event_log_y2024m04 FOR
VALUES
FROM ('2024-04-01 00:00:00') TO ('2024-05-01 00:00:00');
ALTER TABLE dev.event_log_y2024m04 OWNER TO postgres;
--
-- Name: event_log_y2024m05; Type: TABLE; Schema: dev; Owner: postgres
--
CREATE TABLE dev.event_log_y2024m05 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev.event_log ATTACH PARTITION dev.event_log_y2024m05 FOR
VALUES
FROM ('2024-05-01 00:00:00') TO ('2024-06-01 00:00:00');
ALTER TABLE dev.event_log_y2024m05 OWNER TO postgres;
--
-- Name: event_log_y2024m06; Type: TABLE; Schema: dev; Owner: postgres
--
CREATE TABLE dev.event_log_y2024m06 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev.event_log ATTACH PARTITION dev.event_log_y2024m06 FOR
VALUES
FROM ('2024-06-01 00:00:00') TO ('2024-07-01 00:00:00');
ALTER TABLE dev.event_log_y2024m06 OWNER TO postgres;
--
-- Name: event_log_y2024m07; Type: TABLE; Schema: dev; Owner: postgres
--
CREATE TABLE dev.event_log_y2024m07 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev.event_log ATTACH PARTITION dev.event_log_y2024m07 FOR
VALUES
FROM ('2024-07-01 00:00:00') TO ('2024-08-01 00:00:00');
ALTER TABLE dev.event_log_y2024m07 OWNER TO postgres;
--
-- Name: event_log_y2024m08; Type: TABLE; Schema: dev; Owner: postgres
--
CREATE TABLE dev.event_log_y2024m08 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev.event_log ATTACH PARTITION dev.event_log_y2024m08 FOR
VALUES
FROM ('2024-08-01 00:00:00') TO ('2024-09-01 00:00:00');
ALTER TABLE dev.event_log_y2024m08 OWNER TO postgres;
--
-- Name: event_log_y2024m09; Type: TABLE; Schema: dev; Owner: postgres
--
CREATE TABLE dev.event_log_y2024m09 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev.event_log ATTACH PARTITION dev.event_log_y2024m09 FOR
VALUES
FROM ('2024-09-01 00:00:00') TO ('2024-10-01 00:00:00');
ALTER TABLE dev.event_log_y2024m09 OWNER TO postgres;
--
-- Name: event_log_y2024m10; Type: TABLE; Schema: dev; Owner: postgres
--
CREATE TABLE dev.event_log_y2024m10 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev.event_log ATTACH PARTITION dev.event_log_y2024m10 FOR
VALUES
FROM ('2024-10-01 00:00:00') TO ('2024-11-01 00:00:00');
ALTER TABLE dev.event_log_y2024m10 OWNER TO postgres;
--
-- Name: event_log_y2024m11; Type: TABLE; Schema: dev; Owner: postgres
--
CREATE TABLE dev.event_log_y2024m11 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev.event_log ATTACH PARTITION dev.event_log_y2024m11 FOR
VALUES
FROM ('2024-11-01 00:00:00') TO ('2024-12-01 00:00:00');
ALTER TABLE dev.event_log_y2024m11 OWNER TO postgres;
--
-- Name: event_log_y2024m12; Type: TABLE; Schema: dev; Owner: postgres
--
CREATE TABLE dev.event_log_y2024m12 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev.event_log ATTACH PARTITION dev.event_log_y2024m12 FOR
VALUES
FROM ('2024-12-01 00:00:00') TO ('2025-01-01 00:00:00');
ALTER TABLE dev.event_log_y2024m12 OWNER TO postgres;
--
-- Name: event_log_y2025m01; Type: TABLE; Schema: dev; Owner: postgres
--
CREATE TABLE dev.event_log_y2025m01 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev.event_log ATTACH PARTITION dev.event_log_y2025m01 FOR
VALUES
FROM ('2025-01-01 00:00:00') TO ('2025-02-01 00:00:00');
ALTER TABLE dev.event_log_y2025m01 OWNER TO postgres;
--
-- Name: event_log_y2025m02; Type: TABLE; Schema: dev; Owner: postgres
--
CREATE TABLE dev.event_log_y2025m02 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev.event_log ATTACH PARTITION dev.event_log_y2025m02 FOR
VALUES
FROM ('2025-02-01 00:00:00') TO ('2025-03-01 00:00:00');
ALTER TABLE dev.event_log_y2025m02 OWNER TO postgres;
--
-- Name: event_log_y2025m03; Type: TABLE; Schema: dev; Owner: postgres
--
CREATE TABLE dev.event_log_y2025m03 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev.event_log ATTACH PARTITION dev.event_log_y2025m03 FOR
VALUES
FROM ('2025-03-01 00:00:00') TO ('2025-04-01 00:00:00');
ALTER TABLE dev.event_log_y2025m03 OWNER TO postgres;
--
-- Name: event_log_y2025m04; Type: TABLE; Schema: dev; Owner: postgres
--
CREATE TABLE dev.event_log_y2025m04 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev.event_log ATTACH PARTITION dev.event_log_y2025m04 FOR
VALUES
FROM ('2025-04-01 00:00:00') TO ('2025-05-01 00:00:00');
ALTER TABLE dev.event_log_y2025m04 OWNER TO postgres;
--
-- Name: event_log_y2025m05; Type: TABLE; Schema: dev; Owner: postgres
--
CREATE TABLE dev.event_log_y2025m05 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev.event_log ATTACH PARTITION dev.event_log_y2025m05 FOR
VALUES
FROM ('2025-05-01 00:00:00') TO ('2025-06-01 00:00:00');
ALTER TABLE dev.event_log_y2025m05 OWNER TO postgres;
--
-- Name: event_log_y2025m06; Type: TABLE; Schema: dev; Owner: postgres
--
CREATE TABLE dev.event_log_y2025m06 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev.event_log ATTACH PARTITION dev.event_log_y2025m06 FOR
VALUES
FROM ('2025-06-01 00:00:00') TO ('2025-07-01 00:00:00');
ALTER TABLE dev.event_log_y2025m06 OWNER TO postgres;
--
-- Name: event_log_y2025m07; Type: TABLE; Schema: dev; Owner: postgres
--
CREATE TABLE dev.event_log_y2025m07 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev.event_log ATTACH PARTITION dev.event_log_y2025m07 FOR
VALUES
FROM ('2025-07-01 00:00:00') TO ('2025-08-01 00:00:00');
ALTER TABLE dev.event_log_y2025m07 OWNER TO postgres;
--
-- Name: event_log_y2025m08; Type: TABLE; Schema: dev; Owner: postgres
--
CREATE TABLE dev.event_log_y2025m08 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev.event_log ATTACH PARTITION dev.event_log_y2025m08 FOR
VALUES
FROM ('2025-08-01 00:00:00') TO ('2025-09-01 00:00:00');
ALTER TABLE dev.event_log_y2025m08 OWNER TO postgres;
--
-- Name: event_log_y2025m09; Type: TABLE; Schema: dev; Owner: postgres
--
CREATE TABLE dev.event_log_y2025m09 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev.event_log ATTACH PARTITION dev.event_log_y2025m09 FOR
VALUES
FROM ('2025-09-01 00:00:00') TO ('2025-10-01 00:00:00');
ALTER TABLE dev.event_log_y2025m09 OWNER TO postgres;
--
-- Name: event_log_y2025m10; Type: TABLE; Schema: dev; Owner: postgres
--
CREATE TABLE dev.event_log_y2025m10 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev.event_log ATTACH PARTITION dev.event_log_y2025m10 FOR
VALUES
FROM ('2025-10-01 00:00:00') TO ('2025-11-01 00:00:00');
ALTER TABLE dev.event_log_y2025m10 OWNER TO postgres;
--
-- Name: event_log_y2025m11; Type: TABLE; Schema: dev; Owner: postgres
--
CREATE TABLE dev.event_log_y2025m11 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev.event_log ATTACH PARTITION dev.event_log_y2025m11 FOR
VALUES
FROM ('2025-11-01 00:00:00') TO ('2025-12-01 00:00:00');
ALTER TABLE dev.event_log_y2025m11 OWNER TO postgres;
--
-- Name: event_log_y2025m12; Type: TABLE; Schema: dev; Owner: postgres
--
CREATE TABLE dev.event_log_y2025m12 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev.event_log ATTACH PARTITION dev.event_log_y2025m12 FOR
VALUES
FROM ('2025-12-01 00:00:00') TO ('2026-01-01 00:00:00');
ALTER TABLE dev.event_log_y2025m12 OWNER TO postgres;
--
-- Name: event_log_y2026m01; Type: TABLE; Schema: dev; Owner: postgres
--
CREATE TABLE dev.event_log_y2026m01 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev.event_log ATTACH PARTITION dev.event_log_y2026m01 FOR
VALUES
FROM ('2026-01-01 00:00:00') TO ('2026-02-01 00:00:00');
ALTER TABLE dev.event_log_y2026m01 OWNER TO postgres;
--
-- Name: event_log_y2026m02; Type: TABLE; Schema: dev; Owner: postgres
--
CREATE TABLE dev.event_log_y2026m02 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev.event_log ATTACH PARTITION dev.event_log_y2026m02 FOR
VALUES
FROM ('2026-02-01 00:00:00') TO ('2026-03-01 00:00:00');
ALTER TABLE dev.event_log_y2026m02 OWNER TO postgres;
--
-- Name: event_log_y2026m03; Type: TABLE; Schema: dev; Owner: postgres
--
CREATE TABLE dev.event_log_y2026m03 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev.event_log ATTACH PARTITION dev.event_log_y2026m03 FOR
VALUES
FROM ('2026-03-01 00:00:00') TO ('2026-04-01 00:00:00');
ALTER TABLE dev.event_log_y2026m03 OWNER TO postgres;
--
-- Name: event_log_y2026m04; Type: TABLE; Schema: dev; Owner: postgres
--
CREATE TABLE dev.event_log_y2026m04 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev.event_log ATTACH PARTITION dev.event_log_y2026m04 FOR
VALUES
FROM ('2026-04-01 00:00:00') TO ('2026-05-01 00:00:00');
ALTER TABLE dev.event_log_y2026m04 OWNER TO postgres;
--
-- Name: event_log_y2026m05; Type: TABLE; Schema: dev; Owner: postgres
--
CREATE TABLE dev.event_log_y2026m05 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev.event_log ATTACH PARTITION dev.event_log_y2026m05 FOR
VALUES
FROM ('2026-05-01 00:00:00') TO ('2026-06-01 00:00:00');
ALTER TABLE dev.event_log_y2026m05 OWNER TO postgres;
--
-- Name: event_log_y2026m06; Type: TABLE; Schema: dev; Owner: postgres
--
CREATE TABLE dev.event_log_y2026m06 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev.event_log ATTACH PARTITION dev.event_log_y2026m06 FOR
VALUES
FROM ('2026-06-01 00:00:00') TO ('2026-07-01 00:00:00');
ALTER TABLE dev.event_log_y2026m06 OWNER TO postgres;
--
-- Name: event_log_y2026m07; Type: TABLE; Schema: dev; Owner: postgres
--
CREATE TABLE dev.event_log_y2026m07 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev.event_log ATTACH PARTITION dev.event_log_y2026m07 FOR
VALUES
FROM ('2026-07-01 00:00:00') TO ('2026-08-01 00:00:00');
ALTER TABLE dev.event_log_y2026m07 OWNER TO postgres;
--
-- Name: event_log_y2026m08; Type: TABLE; Schema: dev; Owner: postgres
--
CREATE TABLE dev.event_log_y2026m08 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev.event_log ATTACH PARTITION dev.event_log_y2026m08 FOR
VALUES
FROM ('2026-08-01 00:00:00') TO ('2026-09-01 00:00:00');
ALTER TABLE dev.event_log_y2026m08 OWNER TO postgres;
--
-- Name: event_log_y2026m09; Type: TABLE; Schema: dev; Owner: postgres
--
CREATE TABLE dev.event_log_y2026m09 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev.event_log ATTACH PARTITION dev.event_log_y2026m09 FOR
VALUES
FROM ('2026-09-01 00:00:00') TO ('2026-10-01 00:00:00');
ALTER TABLE dev.event_log_y2026m09 OWNER TO postgres;
--
-- Name: event_log_y2026m10; Type: TABLE; Schema: dev; Owner: postgres
--
CREATE TABLE dev.event_log_y2026m10 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev.event_log ATTACH PARTITION dev.event_log_y2026m10 FOR
VALUES
FROM ('2026-10-01 00:00:00') TO ('2026-11-01 00:00:00');
ALTER TABLE dev.event_log_y2026m10 OWNER TO postgres;
--
-- Name: event_log_y2026m11; Type: TABLE; Schema: dev; Owner: postgres
--
CREATE TABLE dev.event_log_y2026m11 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev.event_log ATTACH PARTITION dev.event_log_y2026m11 FOR
VALUES
FROM ('2026-11-01 00:00:00') TO ('2026-12-01 00:00:00');
ALTER TABLE dev.event_log_y2026m11 OWNER TO postgres;
--
-- Name: event_log_y2026m12; Type: TABLE; Schema: dev; Owner: postgres
--
CREATE TABLE dev.event_log_y2026m12 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev.event_log ATTACH PARTITION dev.event_log_y2026m12 FOR
VALUES
FROM ('2026-12-01 00:00:00') TO ('2027-01-01 00:00:00');
ALTER TABLE dev.event_log_y2026m12 OWNER TO postgres;
--
-- Name: experiments; Type: TABLE; Schema: dev; Owner: postgres
--
CREATE TABLE dev.experiments (
    id bigint NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_by text NOT NULL,
    last_modified timestamp with time zone DEFAULT now() NOT NULL,
    name text NOT NULL,
    override_keys dev.not_null_text [] NOT NULL,
    status dev.experiment_status_type NOT NULL,
    traffic_percentage integer NOT NULL,
    context json NOT NULL,
    variants json NOT NULL,
    last_modified_by text DEFAULT 'Null'::text NOT NULL,
    chosen_variant text,
    CONSTRAINT experiments_traffic_percentage_check CHECK ((traffic_percentage >= 0))
);
ALTER TABLE dev.experiments OWNER TO postgres;
--
-- Name: functions; Type: TABLE; Schema: dev; Owner: postgres
--
CREATE TABLE dev.functions (
    function_name text NOT NULL,
    published_code text,
    draft_code text NOT NULL,
    function_description text NOT NULL,
    published_runtime_version character varying(16),
    draft_runtime_version character varying(16) NOT NULL,
    published_at timestamp without time zone,
    draft_edited_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    published_by text,
    draft_edited_by text NOT NULL,
    last_modified_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    last_modified_by character varying(200) DEFAULT 'null'::character varying NOT NULL
);
ALTER TABLE dev.functions OWNER TO postgres;
--
-- Name: type_templates; Type: TABLE; Schema: dev; Owner: postgres
--
CREATE TABLE dev.type_templates (
    type_name text NOT NULL,
    type_schema json NOT NULL,
    created_by text NOT NULL,
    created_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    last_modified_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    last_modified_by character varying(200) DEFAULT 'null'::character varying NOT NULL
);
ALTER TABLE dev.type_templates OWNER TO postgres;
--
-- Name: workspaces; Type: TABLE; Schema: dev; Owner: postgres
--
CREATE TABLE dev.workspaces (
    organisation_id text NOT NULL,
    organisation_name text NOT NULL,
    workspace_name text NOT NULL,
    workspace_schema_name text NOT NULL,
    workspace_status dev.workspace_status NOT NULL,
    workspace_admin_email text NOT NULL,
    created_by text NOT NULL,
    last_modified_by text NOT NULL,
    last_modified_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
ALTER TABLE dev.workspaces OWNER TO postgres;
--
-- Name: __diesel_schema_migrations; Type: TABLE; Schema: public; Owner: postgres
--
CREATE TABLE public.__diesel_schema_migrations (
    version character varying(50) NOT NULL,
    run_on timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
ALTER TABLE public.__diesel_schema_migrations OWNER TO postgres;
--
-- Name: config_versions; Type: TABLE; Schema: test; Owner: postgres
--
CREATE TABLE test.config_versions (
    id bigint NOT NULL,
    config json NOT NULL,
    config_hash text NOT NULL,
    tags character varying(100) [],
    created_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT config_versions_tags_check CHECK (
        (
            array_position(tags, NULL::character varying) IS NULL
        )
    )
);
ALTER TABLE test.config_versions OWNER TO postgres;
--
-- Name: contexts; Type: TABLE; Schema: test; Owner: postgres
--
CREATE TABLE test.contexts (
    id character varying NOT NULL,
    value json NOT NULL,
    override_id character varying NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_by character varying NOT NULL,
    priority integer DEFAULT 1 NOT NULL,
    override json DEFAULT '{}'::json NOT NULL,
    last_modified_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    last_modified_by character varying(200) DEFAULT 'null'::character varying NOT NULL,
    weight numeric(1000, 0) DEFAULT 1 NOT NULL
);
ALTER TABLE test.contexts OWNER TO postgres;
--
-- Name: default_configs; Type: TABLE; Schema: test; Owner: postgres
--
CREATE TABLE test.default_configs (
    key character varying NOT NULL,
    value json NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_by character varying NOT NULL,
    schema json DEFAULT '{}'::json NOT NULL,
    function_name text,
    last_modified_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    last_modified_by character varying(200) DEFAULT 'null'::character varying NOT NULL
);
ALTER TABLE test.default_configs OWNER TO postgres;
--
-- Name: dimensions; Type: TABLE; Schema: test; Owner: postgres
--
CREATE TABLE test.dimensions (
    dimension character varying NOT NULL,
    priority integer DEFAULT 1 NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_by character varying NOT NULL,
    schema json DEFAULT '{}'::json NOT NULL,
    function_name text,
    last_modified_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    last_modified_by character varying(200) DEFAULT 'null'::character varying NOT NULL,
    "position" integer DEFAULT 0 NOT NULL
);
ALTER TABLE test.dimensions OWNER TO postgres;
--
-- Name: event_log; Type: TABLE; Schema: test; Owner: postgres
--
CREATE TABLE test.event_log (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
) PARTITION BY RANGE ("timestamp");
ALTER TABLE test.event_log OWNER TO postgres;
--
-- Name: event_log_y2023m08; Type: TABLE; Schema: test; Owner: postgres
--
CREATE TABLE test.event_log_y2023m08 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test.event_log ATTACH PARTITION test.event_log_y2023m08 FOR
VALUES
FROM ('2023-08-01 00:00:00') TO ('2023-09-01 00:00:00');
ALTER TABLE test.event_log_y2023m08 OWNER TO postgres;
--
-- Name: event_log_y2023m09; Type: TABLE; Schema: test; Owner: postgres
--
CREATE TABLE test.event_log_y2023m09 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test.event_log ATTACH PARTITION test.event_log_y2023m09 FOR
VALUES
FROM ('2023-09-01 00:00:00') TO ('2023-10-01 00:00:00');
ALTER TABLE test.event_log_y2023m09 OWNER TO postgres;
--
-- Name: event_log_y2023m10; Type: TABLE; Schema: test; Owner: postgres
--
CREATE TABLE test.event_log_y2023m10 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test.event_log ATTACH PARTITION test.event_log_y2023m10 FOR
VALUES
FROM ('2023-10-01 00:00:00') TO ('2023-11-01 00:00:00');
ALTER TABLE test.event_log_y2023m10 OWNER TO postgres;
--
-- Name: event_log_y2023m11; Type: TABLE; Schema: test; Owner: postgres
--
CREATE TABLE test.event_log_y2023m11 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test.event_log ATTACH PARTITION test.event_log_y2023m11 FOR
VALUES
FROM ('2023-11-01 00:00:00') TO ('2023-12-01 00:00:00');
ALTER TABLE test.event_log_y2023m11 OWNER TO postgres;
--
-- Name: event_log_y2023m12; Type: TABLE; Schema: test; Owner: postgres
--
CREATE TABLE test.event_log_y2023m12 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test.event_log ATTACH PARTITION test.event_log_y2023m12 FOR
VALUES
FROM ('2023-12-01 00:00:00') TO ('2024-01-01 00:00:00');
ALTER TABLE test.event_log_y2023m12 OWNER TO postgres;
--
-- Name: event_log_y2024m01; Type: TABLE; Schema: test; Owner: postgres
--
CREATE TABLE test.event_log_y2024m01 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test.event_log ATTACH PARTITION test.event_log_y2024m01 FOR
VALUES
FROM ('2024-01-01 00:00:00') TO ('2024-02-01 00:00:00');
ALTER TABLE test.event_log_y2024m01 OWNER TO postgres;
--
-- Name: event_log_y2024m02; Type: TABLE; Schema: test; Owner: postgres
--
CREATE TABLE test.event_log_y2024m02 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test.event_log ATTACH PARTITION test.event_log_y2024m02 FOR
VALUES
FROM ('2024-02-01 00:00:00') TO ('2024-03-01 00:00:00');
ALTER TABLE test.event_log_y2024m02 OWNER TO postgres;
--
-- Name: event_log_y2024m03; Type: TABLE; Schema: test; Owner: postgres
--
CREATE TABLE test.event_log_y2024m03 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test.event_log ATTACH PARTITION test.event_log_y2024m03 FOR
VALUES
FROM ('2024-03-01 00:00:00') TO ('2024-04-01 00:00:00');
ALTER TABLE test.event_log_y2024m03 OWNER TO postgres;
--
-- Name: event_log_y2024m04; Type: TABLE; Schema: test; Owner: postgres
--
CREATE TABLE test.event_log_y2024m04 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test.event_log ATTACH PARTITION test.event_log_y2024m04 FOR
VALUES
FROM ('2024-04-01 00:00:00') TO ('2024-05-01 00:00:00');
ALTER TABLE test.event_log_y2024m04 OWNER TO postgres;
--
-- Name: event_log_y2024m05; Type: TABLE; Schema: test; Owner: postgres
--
CREATE TABLE test.event_log_y2024m05 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test.event_log ATTACH PARTITION test.event_log_y2024m05 FOR
VALUES
FROM ('2024-05-01 00:00:00') TO ('2024-06-01 00:00:00');
ALTER TABLE test.event_log_y2024m05 OWNER TO postgres;
--
-- Name: event_log_y2024m06; Type: TABLE; Schema: test; Owner: postgres
--
CREATE TABLE test.event_log_y2024m06 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test.event_log ATTACH PARTITION test.event_log_y2024m06 FOR
VALUES
FROM ('2024-06-01 00:00:00') TO ('2024-07-01 00:00:00');
ALTER TABLE test.event_log_y2024m06 OWNER TO postgres;
--
-- Name: event_log_y2024m07; Type: TABLE; Schema: test; Owner: postgres
--
CREATE TABLE test.event_log_y2024m07 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test.event_log ATTACH PARTITION test.event_log_y2024m07 FOR
VALUES
FROM ('2024-07-01 00:00:00') TO ('2024-08-01 00:00:00');
ALTER TABLE test.event_log_y2024m07 OWNER TO postgres;
--
-- Name: event_log_y2024m08; Type: TABLE; Schema: test; Owner: postgres
--
CREATE TABLE test.event_log_y2024m08 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test.event_log ATTACH PARTITION test.event_log_y2024m08 FOR
VALUES
FROM ('2024-08-01 00:00:00') TO ('2024-09-01 00:00:00');
ALTER TABLE test.event_log_y2024m08 OWNER TO postgres;
--
-- Name: event_log_y2024m09; Type: TABLE; Schema: test; Owner: postgres
--
CREATE TABLE test.event_log_y2024m09 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test.event_log ATTACH PARTITION test.event_log_y2024m09 FOR
VALUES
FROM ('2024-09-01 00:00:00') TO ('2024-10-01 00:00:00');
ALTER TABLE test.event_log_y2024m09 OWNER TO postgres;
--
-- Name: event_log_y2024m10; Type: TABLE; Schema: test; Owner: postgres
--
CREATE TABLE test.event_log_y2024m10 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test.event_log ATTACH PARTITION test.event_log_y2024m10 FOR
VALUES
FROM ('2024-10-01 00:00:00') TO ('2024-11-01 00:00:00');
ALTER TABLE test.event_log_y2024m10 OWNER TO postgres;
--
-- Name: event_log_y2024m11; Type: TABLE; Schema: test; Owner: postgres
--
CREATE TABLE test.event_log_y2024m11 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test.event_log ATTACH PARTITION test.event_log_y2024m11 FOR
VALUES
FROM ('2024-11-01 00:00:00') TO ('2024-12-01 00:00:00');
ALTER TABLE test.event_log_y2024m11 OWNER TO postgres;
--
-- Name: event_log_y2024m12; Type: TABLE; Schema: test; Owner: postgres
--
CREATE TABLE test.event_log_y2024m12 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test.event_log ATTACH PARTITION test.event_log_y2024m12 FOR
VALUES
FROM ('2024-12-01 00:00:00') TO ('2025-01-01 00:00:00');
ALTER TABLE test.event_log_y2024m12 OWNER TO postgres;
--
-- Name: event_log_y2025m01; Type: TABLE; Schema: test; Owner: postgres
--
CREATE TABLE test.event_log_y2025m01 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test.event_log ATTACH PARTITION test.event_log_y2025m01 FOR
VALUES
FROM ('2025-01-01 00:00:00') TO ('2025-02-01 00:00:00');
ALTER TABLE test.event_log_y2025m01 OWNER TO postgres;
--
-- Name: event_log_y2025m02; Type: TABLE; Schema: test; Owner: postgres
--
CREATE TABLE test.event_log_y2025m02 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test.event_log ATTACH PARTITION test.event_log_y2025m02 FOR
VALUES
FROM ('2025-02-01 00:00:00') TO ('2025-03-01 00:00:00');
ALTER TABLE test.event_log_y2025m02 OWNER TO postgres;
--
-- Name: event_log_y2025m03; Type: TABLE; Schema: test; Owner: postgres
--
CREATE TABLE test.event_log_y2025m03 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test.event_log ATTACH PARTITION test.event_log_y2025m03 FOR
VALUES
FROM ('2025-03-01 00:00:00') TO ('2025-04-01 00:00:00');
ALTER TABLE test.event_log_y2025m03 OWNER TO postgres;
--
-- Name: event_log_y2025m04; Type: TABLE; Schema: test; Owner: postgres
--
CREATE TABLE test.event_log_y2025m04 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test.event_log ATTACH PARTITION test.event_log_y2025m04 FOR
VALUES
FROM ('2025-04-01 00:00:00') TO ('2025-05-01 00:00:00');
ALTER TABLE test.event_log_y2025m04 OWNER TO postgres;
--
-- Name: event_log_y2025m05; Type: TABLE; Schema: test; Owner: postgres
--
CREATE TABLE test.event_log_y2025m05 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test.event_log ATTACH PARTITION test.event_log_y2025m05 FOR
VALUES
FROM ('2025-05-01 00:00:00') TO ('2025-06-01 00:00:00');
ALTER TABLE test.event_log_y2025m05 OWNER TO postgres;
--
-- Name: event_log_y2025m06; Type: TABLE; Schema: test; Owner: postgres
--
CREATE TABLE test.event_log_y2025m06 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test.event_log ATTACH PARTITION test.event_log_y2025m06 FOR
VALUES
FROM ('2025-06-01 00:00:00') TO ('2025-07-01 00:00:00');
ALTER TABLE test.event_log_y2025m06 OWNER TO postgres;
--
-- Name: event_log_y2025m07; Type: TABLE; Schema: test; Owner: postgres
--
CREATE TABLE test.event_log_y2025m07 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test.event_log ATTACH PARTITION test.event_log_y2025m07 FOR
VALUES
FROM ('2025-07-01 00:00:00') TO ('2025-08-01 00:00:00');
ALTER TABLE test.event_log_y2025m07 OWNER TO postgres;
--
-- Name: event_log_y2025m08; Type: TABLE; Schema: test; Owner: postgres
--
CREATE TABLE test.event_log_y2025m08 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test.event_log ATTACH PARTITION test.event_log_y2025m08 FOR
VALUES
FROM ('2025-08-01 00:00:00') TO ('2025-09-01 00:00:00');
ALTER TABLE test.event_log_y2025m08 OWNER TO postgres;
--
-- Name: event_log_y2025m09; Type: TABLE; Schema: test; Owner: postgres
--
CREATE TABLE test.event_log_y2025m09 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test.event_log ATTACH PARTITION test.event_log_y2025m09 FOR
VALUES
FROM ('2025-09-01 00:00:00') TO ('2025-10-01 00:00:00');
ALTER TABLE test.event_log_y2025m09 OWNER TO postgres;
--
-- Name: event_log_y2025m10; Type: TABLE; Schema: test; Owner: postgres
--
CREATE TABLE test.event_log_y2025m10 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test.event_log ATTACH PARTITION test.event_log_y2025m10 FOR
VALUES
FROM ('2025-10-01 00:00:00') TO ('2025-11-01 00:00:00');
ALTER TABLE test.event_log_y2025m10 OWNER TO postgres;
--
-- Name: event_log_y2025m11; Type: TABLE; Schema: test; Owner: postgres
--
CREATE TABLE test.event_log_y2025m11 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test.event_log ATTACH PARTITION test.event_log_y2025m11 FOR
VALUES
FROM ('2025-11-01 00:00:00') TO ('2025-12-01 00:00:00');
ALTER TABLE test.event_log_y2025m11 OWNER TO postgres;
--
-- Name: event_log_y2025m12; Type: TABLE; Schema: test; Owner: postgres
--
CREATE TABLE test.event_log_y2025m12 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test.event_log ATTACH PARTITION test.event_log_y2025m12 FOR
VALUES
FROM ('2025-12-01 00:00:00') TO ('2026-01-01 00:00:00');
ALTER TABLE test.event_log_y2025m12 OWNER TO postgres;
--
-- Name: event_log_y2026m01; Type: TABLE; Schema: test; Owner: postgres
--
CREATE TABLE test.event_log_y2026m01 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test.event_log ATTACH PARTITION test.event_log_y2026m01 FOR
VALUES
FROM ('2026-01-01 00:00:00') TO ('2026-02-01 00:00:00');
ALTER TABLE test.event_log_y2026m01 OWNER TO postgres;
--
-- Name: event_log_y2026m02; Type: TABLE; Schema: test; Owner: postgres
--
CREATE TABLE test.event_log_y2026m02 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test.event_log ATTACH PARTITION test.event_log_y2026m02 FOR
VALUES
FROM ('2026-02-01 00:00:00') TO ('2026-03-01 00:00:00');
ALTER TABLE test.event_log_y2026m02 OWNER TO postgres;
--
-- Name: event_log_y2026m03; Type: TABLE; Schema: test; Owner: postgres
--
CREATE TABLE test.event_log_y2026m03 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test.event_log ATTACH PARTITION test.event_log_y2026m03 FOR
VALUES
FROM ('2026-03-01 00:00:00') TO ('2026-04-01 00:00:00');
ALTER TABLE test.event_log_y2026m03 OWNER TO postgres;
--
-- Name: event_log_y2026m04; Type: TABLE; Schema: test; Owner: postgres
--
CREATE TABLE test.event_log_y2026m04 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test.event_log ATTACH PARTITION test.event_log_y2026m04 FOR
VALUES
FROM ('2026-04-01 00:00:00') TO ('2026-05-01 00:00:00');
ALTER TABLE test.event_log_y2026m04 OWNER TO postgres;
--
-- Name: event_log_y2026m05; Type: TABLE; Schema: test; Owner: postgres
--
CREATE TABLE test.event_log_y2026m05 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test.event_log ATTACH PARTITION test.event_log_y2026m05 FOR
VALUES
FROM ('2026-05-01 00:00:00') TO ('2026-06-01 00:00:00');
ALTER TABLE test.event_log_y2026m05 OWNER TO postgres;
--
-- Name: event_log_y2026m06; Type: TABLE; Schema: test; Owner: postgres
--
CREATE TABLE test.event_log_y2026m06 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test.event_log ATTACH PARTITION test.event_log_y2026m06 FOR
VALUES
FROM ('2026-06-01 00:00:00') TO ('2026-07-01 00:00:00');
ALTER TABLE test.event_log_y2026m06 OWNER TO postgres;
--
-- Name: event_log_y2026m07; Type: TABLE; Schema: test; Owner: postgres
--
CREATE TABLE test.event_log_y2026m07 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test.event_log ATTACH PARTITION test.event_log_y2026m07 FOR
VALUES
FROM ('2026-07-01 00:00:00') TO ('2026-08-01 00:00:00');
ALTER TABLE test.event_log_y2026m07 OWNER TO postgres;
--
-- Name: event_log_y2026m08; Type: TABLE; Schema: test; Owner: postgres
--
CREATE TABLE test.event_log_y2026m08 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test.event_log ATTACH PARTITION test.event_log_y2026m08 FOR
VALUES
FROM ('2026-08-01 00:00:00') TO ('2026-09-01 00:00:00');
ALTER TABLE test.event_log_y2026m08 OWNER TO postgres;
--
-- Name: event_log_y2026m09; Type: TABLE; Schema: test; Owner: postgres
--
CREATE TABLE test.event_log_y2026m09 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test.event_log ATTACH PARTITION test.event_log_y2026m09 FOR
VALUES
FROM ('2026-09-01 00:00:00') TO ('2026-10-01 00:00:00');
ALTER TABLE test.event_log_y2026m09 OWNER TO postgres;
--
-- Name: event_log_y2026m10; Type: TABLE; Schema: test; Owner: postgres
--
CREATE TABLE test.event_log_y2026m10 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test.event_log ATTACH PARTITION test.event_log_y2026m10 FOR
VALUES
FROM ('2026-10-01 00:00:00') TO ('2026-11-01 00:00:00');
ALTER TABLE test.event_log_y2026m10 OWNER TO postgres;
--
-- Name: event_log_y2026m11; Type: TABLE; Schema: test; Owner: postgres
--
CREATE TABLE test.event_log_y2026m11 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test.event_log ATTACH PARTITION test.event_log_y2026m11 FOR
VALUES
FROM ('2026-11-01 00:00:00') TO ('2026-12-01 00:00:00');
ALTER TABLE test.event_log_y2026m11 OWNER TO postgres;
--
-- Name: event_log_y2026m12; Type: TABLE; Schema: test; Owner: postgres
--
CREATE TABLE test.event_log_y2026m12 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test.event_log ATTACH PARTITION test.event_log_y2026m12 FOR
VALUES
FROM ('2026-12-01 00:00:00') TO ('2027-01-01 00:00:00');
ALTER TABLE test.event_log_y2026m12 OWNER TO postgres;
--
-- Name: experiments; Type: TABLE; Schema: test; Owner: postgres
--
CREATE TABLE test.experiments (
    id bigint NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_by text NOT NULL,
    last_modified timestamp with time zone DEFAULT now() NOT NULL,
    name text NOT NULL,
    override_keys test.not_null_text [] NOT NULL,
    status test.experiment_status_type NOT NULL,
    traffic_percentage integer NOT NULL,
    context json NOT NULL,
    variants json NOT NULL,
    last_modified_by text DEFAULT 'Null'::text NOT NULL,
    chosen_variant text,
    CONSTRAINT experiments_traffic_percentage_check CHECK ((traffic_percentage >= 0))
);
ALTER TABLE test.experiments OWNER TO postgres;
--
-- Name: functions; Type: TABLE; Schema: test; Owner: postgres
--
CREATE TABLE test.functions (
    function_name text NOT NULL,
    published_code text,
    draft_code text NOT NULL,
    function_description text NOT NULL,
    published_runtime_version character varying(16),
    draft_runtime_version character varying(16) NOT NULL,
    published_at timestamp without time zone,
    draft_edited_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    published_by text,
    draft_edited_by text NOT NULL,
    last_modified_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    last_modified_by character varying(200) DEFAULT 'null'::character varying NOT NULL
);
ALTER TABLE test.functions OWNER TO postgres;
--
-- Name: type_templates; Type: TABLE; Schema: test; Owner: postgres
--
CREATE TABLE test.type_templates (
    type_name text NOT NULL,
    type_schema json NOT NULL,
    created_by text NOT NULL,
    created_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    last_modified_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    last_modified_by character varying(200) DEFAULT 'null'::character varying NOT NULL
);
ALTER TABLE test.type_templates OWNER TO postgres;
--
-- Name: workspaces; Type: TABLE; Schema: test; Owner: postgres
--
CREATE TABLE test.workspaces (
    organisation_id text NOT NULL,
    organisation_name text NOT NULL,
    workspace_name text NOT NULL,
    workspace_schema_name text NOT NULL,
    workspace_status test.workspace_status NOT NULL,
    workspace_admin_email text NOT NULL,
    created_by text NOT NULL,
    last_modified_by text NOT NULL,
    last_modified_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
ALTER TABLE test.workspaces OWNER TO postgres;
--
-- Data for Name: config_versions; Type: TABLE DATA; Schema: dev; Owner: postgres
--
COPY dev.config_versions (id, config, config_hash, tags, created_at)
FROM stdin;
7275106583758114816 { "contexts" :[{"condition":{"==":[{"var":"city"},"Bangalore"] },
"id" :"11c6bd932b8888a9e50be8f822b76116fbaa20fd58b059f98b9e7179543bb9a4",
"override_with_keys" :["2a2eb08ced59872063128fc531b17a04e0e3be31d565b57273508008b207636a"],
"priority" :0,
"weight" :0 },
{ "condition": { "==" :[{"var":"city"},"Chennai"] },
"id" :"e47edbe36bf3333f42ca2ded0a4604997adc4f6e0bf6176079272597c01353d8",
"override_with_keys" :["645b12a924823be87eb423791f3c42393ab19103afcd8ce923c4a1112408b32b"],
"priority" :1,
"weight" :1 },
{ "condition": { "==" :[{"var":"city"},"Seattle"] },
"id" :"3f0e5fe95e2e758151d4cef712185767a137c926a9e7156b6face3a98b167d35",
"override_with_keys" :["f73412831793a34df7df95a79bc65754b1eebf31534e4ccfa9c79681fcf8ac69"],
"priority" :2,
"weight" :2 },
{ "condition": { "and" :[{"==":[{"var":"city"},"Bangalore"] },
{ "==" :[{"var":"vehicle_type"},"cab"] } ] },
"id" :"9fb61ecfb662467e9036ec79bfd8c02ac244c1836f113242aec5fe6d9265d8dc",
"override_with_keys" :["e2411e811b927824f910f9edd75b984a3ad226c46a1216ffa735b7bda60c1ede"],
"priority" :3,
"weight" :3 } ],
"default_configs": { "base_rate" :10.0,
"currency" :"INR",
"distance_unit" :"Km",
"foo.bar" :2,
"foo.foo" :1,
"hello_message" :"Hello World !!!",
"hello_message_color" :"black",
"logo" :"https://cdn1.vectorstock.com/i/1000x1000/03/20/big-city-logo-vector-20480320.jpg",
"per_distance_unit_rate" :15.0 },
"overrides": { "2a2eb08ced59872063128fc531b17a04e0e3be31d565b57273508008b207636a": { "base_rate" :20,
"distance_unit" :"Km",
"hello_message" :"ನಮಸ್ಕಾರ ಬೆಂಗಳೂರು",
"hello_message_color" :"red",
"logo" :"https://www.shutterstock.com/image-vector/bangalore-logo-vidhana-soudha-600nw-1506258893.jpg",
"per_distance_unit_rate" :15 },
"645b12a924823be87eb423791f3c42393ab19103afcd8ce923c4a1112408b32b": { "hello_message" :"வணக்கம் சென்னை",
"hello_message_color" :"green",
"logo" :"https://www.shutterstock.com/image-vector/chennai-skyline-color-landmarks-blue-260nw-515862346.jpg" },
"e2411e811b927824f910f9edd75b984a3ad226c46a1216ffa735b7bda60c1ede": { "base_rate" :12 },
"f73412831793a34df7df95a79bc65754b1eebf31534e4ccfa9c79681fcf8ac69": { "base_rate" :5,
"currency" :"USD",
"distance_unit" :"Miles",
"hello_message" :"Hello Seattle",
"hello_message_color" :"blue",
"logo" :"https://t4.ftcdn.net/jpg/04/24/15/07/360_F_424150716_kDZIgUONDIKpIhHqsxlDcVIiglyjIOQs.jpg",
"per_distance_unit_rate" :2.5 } } } baf44a6fd791ff3f0eae438e6925dae48d16f8ba45591d7bdb36a519bef8db39 \ N 2024 -12 -18 11 :16 :14.514624 \.--
-- Data for Name: contexts; Type: TABLE DATA; Schema: dev; Owner: postgres
--
COPY dev.contexts (
    id,
    value,
    override_id,
    created_at,
    created_by,
    priority,
    override,
    last_modified_at,
    last_modified_by,
    weight
)
FROM stdin;
11c6bd932b8888a9e50be8f822b76116fbaa20fd58b059f98b9e7179543bb9a4 { "==" :[{"var":"city"},"Bangalore"] } 2a2eb08ced59872063128fc531b17a04e0e3be31d565b57273508008b207636a 2024 -08 -22 12 :16 :35.579131 + 00 user @superposition.io 4 { "base_rate" :20,
"distance_unit" :"Km",
"hello_message" :"ನಮಸ್ಕಾರ ಬೆಂಗಳೂರು",
"hello_message_color" :"red",
"logo" :"https://www.shutterstock.com/image-vector/bangalore-logo-vidhana-soudha-600nw-1506258893.jpg",
"per_distance_unit_rate" :15 } 2024 -12 -18 11 :16 :14.500284 user @superposition.io 2 e47edbe36bf3333f42ca2ded0a4604997adc4f6e0bf6176079272597c01353d8 { "==" :[{"var":"city"},"Chennai"] } 645b12a924823be87eb423791f3c42393ab19103afcd8ce923c4a1112408b32b 2024 -08 -22 12 :36 :23.489214 + 00 user @superposition.io 4 { "hello_message" :"வணக்கம் சென்னை",
"hello_message_color" :"green",
"logo" :"https://www.shutterstock.com/image-vector/chennai-skyline-color-landmarks-blue-260nw-515862346.jpg" } 2024 -12 -18 11 :16 :14.500284 user @superposition.io 2 3f0e5fe95e2e758151d4cef712185767a137c926a9e7156b6face3a98b167d35 { "==" :[{"var":"city"},"Seattle"] } f73412831793a34df7df95a79bc65754b1eebf31534e4ccfa9c79681fcf8ac69 2024 -08 -22 12 :37 :32.306949 + 00 user @superposition.io 4 { "base_rate" :5,
"currency" :"USD",
"distance_unit" :"Miles",
"hello_message" :"Hello Seattle",
"hello_message_color" :"blue",
"logo" :"https://t4.ftcdn.net/jpg/04/24/15/07/360_F_424150716_kDZIgUONDIKpIhHqsxlDcVIiglyjIOQs.jpg",
"per_distance_unit_rate" :2.5 } 2024 -12 -18 11 :16 :14.500284 user @superposition.io 2 9fb61ecfb662467e9036ec79bfd8c02ac244c1836f113242aec5fe6d9265d8dc { "and" :[{"==":[{"var":"city"},"Bangalore"] },
{ "==" :[{"var":"vehicle_type"},"cab"] } ] } e2411e811b927824f910f9edd75b984a3ad226c46a1216ffa735b7bda60c1ede 2024 -08 -22 12 :37 :53.206065 + 00 user @superposition.io 6 { "base_rate" :12 } 2024 -12 -18 11 :16 :14.500284 user @superposition.io 6 \.--
-- Data for Name: default_configs; Type: TABLE DATA; Schema: dev; Owner: postgres
--
COPY dev.default_configs (
    key,
    value,
    created_at,
    created_by,
    schema,
    function_name,
    last_modified_at,
    last_modified_by
)
FROM stdin;
base_rate 10.0 2024 -08 -22 12 :11 :28.122453 + 00 user @superposition.io { "type" :"number" } \ N 2024 -08 -22 12 :11 :28.122802 user @superposition.io currency "INR" 2024 -08 -22 12 :12 :19.765579 + 00 user @superposition.io { "enum" :["INR","USD","EUR"],
"type" :"string" } \ N 2024 -08 -22 12 :12 :19.765591 user @superposition.io per_distance_unit_rate 15.0 2024 -08 -22 12 :12 :37.370436 + 00 user @superposition.io { "type" :"number" } \ N 2024 -08 -22 12 :12 :37.370446 user @superposition.io distance_unit "Km" 2024 -08 -22 12 :12 :53.814292 + 00 user @superposition.io { "enum" :["Km","Miles"],
"type" :"string" } \ N 2024 -08 -22 12 :12 :53.814303 user @superposition.io foo.foo 1 2024 -08 -22 12 :13 :24.43122 + 00 user @superposition.io { "type" :"integer" } \ N 2024 -08 -22 12 :13 :24.431231 user @superposition.io foo.bar 2 2024 -08 -22 12 :13 :37.788173 + 00 user @superposition.io { "type" :"number" } \ N 2024 -08 -22 12 :13 :37.788184 user @superposition.io hello_message "Hello World !!!" 2024 -08 -22 12 :14 :02.505322 + 00 user @superposition.io { "pattern" :".*",
"type" :"string" } \ N 2024 -08 -22 12 :14 :02.505334 user @superposition.io hello_message_color "black" 2024 -08 -22 12 :14 :27.222544 + 00 user @superposition.io { "enum" :["black","red","blue","green","pink"],
"type" :"string" } \ N 2024 -08 -22 12 :14 :27.222551 user @superposition.io logo "https://cdn1.vectorstock.com/i/1000x1000/03/20/big-city-logo-vector-20480320.jpg" 2024 -08 -22 12 :15 :08.701643 + 00 user @superposition.io { "pattern" :"https://.*",
"type" :"string" } axios_validator_example 2024 -08 -22 12 :15 :08.701648 user @superposition.io \.--
-- Data for Name: dimensions; Type: TABLE DATA; Schema: dev; Owner: postgres
--
COPY dev.dimensions (
    dimension,
    priority,
    created_at,
    created_by,
    schema,
    function_name,
    last_modified_at,
    last_modified_by,
    "position"
)
FROM stdin;
variantIds 1 2024 -12 -18 11 :04 :37.258074 + 00 user @example.com { "type": "string",
"pattern": ".*" } \ N 2024 -12 -18 11 :04 :37.258074 null 0 city 1 2024 -12 -18 11 :05 :14.166214 + 00 user @superposition.io { "pattern" :".*",
"type" :"string" } \ N 2024 -12 -18 11 :05 :14.166262 user @superposition.io 1 vehicle_type 1 2024 -12 -18 11 :05 :34.056043 + 00 user @superposition.io { "enum" :["auto","cab"],
"type" :"string" } \ N 2024 -12 -18 11 :05 :34.056048 user @superposition.io 2 hour_of_day 1 2024 -12 -18 11 :05 :44.634258 + 00 user @superposition.io { "type" :"integer" } \ N 2024 -12 -18 11 :05 :44.634265 user @superposition.io 3 \.--
-- Data for Name: event_log_y2023m08; Type: TABLE DATA; Schema: dev; Owner: postgres
--
COPY dev.event_log_y2023m08 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2023m09; Type: TABLE DATA; Schema: dev; Owner: postgres
--
COPY dev.event_log_y2023m09 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2023m10; Type: TABLE DATA; Schema: dev; Owner: postgres
--
COPY dev.event_log_y2023m10 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2023m11; Type: TABLE DATA; Schema: dev; Owner: postgres
--
COPY dev.event_log_y2023m11 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2023m12; Type: TABLE DATA; Schema: dev; Owner: postgres
--
COPY dev.event_log_y2023m12 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2024m01; Type: TABLE DATA; Schema: dev; Owner: postgres
--
COPY dev.event_log_y2024m01 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2024m02; Type: TABLE DATA; Schema: dev; Owner: postgres
--
COPY dev.event_log_y2024m02 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2024m03; Type: TABLE DATA; Schema: dev; Owner: postgres
--
COPY dev.event_log_y2024m03 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2024m04; Type: TABLE DATA; Schema: dev; Owner: postgres
--
COPY dev.event_log_y2024m04 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2024m05; Type: TABLE DATA; Schema: dev; Owner: postgres
--
COPY dev.event_log_y2024m05 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2024m06; Type: TABLE DATA; Schema: dev; Owner: postgres
--
COPY dev.event_log_y2024m06 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2024m07; Type: TABLE DATA; Schema: dev; Owner: postgres
--
COPY dev.event_log_y2024m07 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2024m08; Type: TABLE DATA; Schema: dev; Owner: postgres
--
COPY dev.event_log_y2024m08 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2024m09; Type: TABLE DATA; Schema: dev; Owner: postgres
--
COPY dev.event_log_y2024m09 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2024m10; Type: TABLE DATA; Schema: dev; Owner: postgres
--
COPY dev.event_log_y2024m10 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2024m11; Type: TABLE DATA; Schema: dev; Owner: postgres
--
COPY dev.event_log_y2024m11 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2024m12; Type: TABLE DATA; Schema: dev; Owner: postgres
--
COPY dev.event_log_y2024m12 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
1ac0835a-2ccc-4c66-a968-e5688b1ea0eb dimensions postgres 2024 -12 -18 11 :04 :37.258074
INSERT \ N { "dimension" :"variantIds",
    "priority" :1,
    "created_at" :"2024-12-18T11:04:37.258074+00:00",
    "created_by" :"user@example.com",
    "schema": { "type": "string",
    "pattern": ".*" },
    "function_name" :null,
    "last_modified_at" :"2024-12-18T11:04:37.258074",
    "last_modified_by" :"null",
    "position" :0 }
INSERT INTO dev.dimensions (
        dimension,
        priority,
        created_at,
        created_by,
        schema,
        function_name
    )
VALUES (
        'variantIds',
        1,
        CURRENT_TIMESTAMP,
        'user@example.com',
        '{"type": "string","pattern": ".*"}'::json,
        null
    );
ab801663-848f-436f-a766-9f12f8686854 dimensions postgres 2024 -12 -18 11 :05 :14.166899
INSERT \ N { "dimension" :"city",
    "priority" :1,
    "created_at" :"2024-12-18T11:05:14.166214+00:00",
    "created_by" :"user@superposition.io",
    "schema": { "pattern" :".*",
    "type" :"string" },
    "function_name" :null,
    "last_modified_at" :"2024-12-18T11:05:14.166262",
    "last_modified_by" :"user@superposition.io",
    "position" :1 }
INSERT INTO "dimensions" (
        "dimension",
        "created_at",
        "created_by",
        "schema",
        "function_name",
        "last_modified_at",
        "last_modified_by",
        "position"
    )
VALUES ($1, $2, $3, $4, DEFAULT, $5, $6, $7)
RETURNING "dimensions"."dimension",
    "dimensions"."created_at",
    "dimensions"."created_by",
    "dimensions"."schema",
    "dimensions"."function_name",
    "dimensions"."last_modified_at",
    "dimensions"."last_modified_by",
    "dimensions"."position" f8ea8bba-1b45-4e1d-a472-914c8ee62b3c dimensions postgres 2024 -12 -18 11 :05 :34.056642
INSERT \ N { "dimension" :"vehicle_type",
    "priority" :1,
    "created_at" :"2024-12-18T11:05:34.056043+00:00",
    "created_by" :"user@superposition.io",
    "schema": { "enum" :["auto","cab"],
    "type" :"string" },
    "function_name" :null,
    "last_modified_at" :"2024-12-18T11:05:34.056048",
    "last_modified_by" :"user@superposition.io",
    "position" :2 }
INSERT INTO "dimensions" (
        "dimension",
        "created_at",
        "created_by",
        "schema",
        "function_name",
        "last_modified_at",
        "last_modified_by",
        "position"
    )
VALUES ($1, $2, $3, $4, DEFAULT, $5, $6, $7)
RETURNING "dimensions"."dimension",
    "dimensions"."created_at",
    "dimensions"."created_by",
    "dimensions"."schema",
    "dimensions"."function_name",
    "dimensions"."last_modified_at",
    "dimensions"."last_modified_by",
    "dimensions"."position" de9b97e0-e4e2-40a4-ac99-3c96a1f175c8 dimensions postgres 2024 -12 -18 11 :05 :44.634924
INSERT \ N { "dimension" :"hour_of_day",
    "priority" :1,
    "created_at" :"2024-12-18T11:05:44.634258+00:00",
    "created_by" :"user@superposition.io",
    "schema": { "type" :"integer" },
    "function_name" :null,
    "last_modified_at" :"2024-12-18T11:05:44.634265",
    "last_modified_by" :"user@superposition.io",
    "position" :3 }
INSERT INTO "dimensions" (
        "dimension",
        "created_at",
        "created_by",
        "schema",
        "function_name",
        "last_modified_at",
        "last_modified_by",
        "position"
    )
VALUES ($1, $2, $3, $4, DEFAULT, $5, $6, $7)
RETURNING "dimensions"."dimension",
    "dimensions"."created_at",
    "dimensions"."created_by",
    "dimensions"."schema",
    "dimensions"."function_name",
    "dimensions"."last_modified_at",
    "dimensions"."last_modified_by",
    "dimensions"."position" cfd002bb-192a-4c6c-bcc8-b56f1c29d461 contexts postgres 2024 -12 -18 11 :13 :38.535445
INSERT \ N { "id" :"3f0e5fe95e2e758151d4cef712185767a137c926a9e7156b6face3a98b167d35",
    "value": { "==" :[{"var":"city"},"Seattle"] },
    "override_id" :"f73412831793a34df7df95a79bc65754b1eebf31534e4ccfa9c79681fcf8ac69",
    "created_at" :"2024-08-22T12:37:32.306949+00:00",
    "created_by" :"user@superposition.io",
    "priority" :4,
    "override": { "base_rate" :5,
    "currency" :"USD",
    "distance_unit" :"Miles",
    "hello_message" :"Hello Seattle",
    "hello_message_color" :"blue",
    "logo" :"https://t4.ftcdn.net/jpg/04/24/15/07/360_F_424150716_kDZIgUONDIKpIhHqsxlDcVIiglyjIOQs.jpg",
    "per_distance_unit_rate" :2.5 },
    "last_modified_at" :"2024-08-22T12:37:32.306953",
    "last_modified_by" :"user@superposition.io",
    "weight" :1 } COPY dev.contexts (
        id,
        value,
        override_id,
        created_at,
        created_by,
        priority,
        override,
        last_modified_at,
        last_modified_by
    )
FROM stdin;
16140d62-b6b6-43bf-b00b-7bf3a687228d functions postgres 2024 -12 -18 11 :10 :40.326712
INSERT \ N { "function_name" :"axios_validator_example",
    "published_code" :"YXN5bmMgZnVuY3Rpb24gdmFsaWRhdGUoa2V5LCB2YWx1ZSkgewogICAgcmV0dXJuIGF4aW9zLmdldCh2YWx1ZSkudGhlbihyZXNwb25zZSA9PiByZXNwb25zZS5zdGF0dXMgPT0gJzIwMCcpCiAgICAuY2F0Y2goZXJyID0+IHsKICAgICAgICBjb25zb2xlLmxvZyhlcnIpOwogICAgICAgIHJldHVybiBmYWxzZTsKICAgIH0pOwp9Cg==",
    "draft_code" :"YXN5bmMgZnVuY3Rpb24gdmFsaWRhdGUoa2V5LCB2YWx1ZSkgewogICAgcmV0dXJuIGF4aW9zLmdldCh2YWx1ZSkudGhlbihyZXNwb25zZSA9PiByZXNwb25zZS5zdGF0dXMgPT0gJzIwMCcpCiAgICAuY2F0Y2goZXJyID0+IHsKICAgICAgICBjb25zb2xlLmxvZyhlcnIpOwogICAgICAgIHJldHVybiBmYWxzZTsKICAgIH0pOwp9Cg==",
    "function_description" :"An example function that shows off validator functions",
    "published_runtime_version" :"1.0.0",
    "draft_runtime_version" :"1.0.0",
    "published_at" :"2024-08-22T12:10:48.787527",
    "draft_edited_at" :"2024-08-22T12:10:44.416185",
    "published_by" :"user@superposition.io",
    "draft_edited_by" :"user@superposition.io",
    "last_modified_at" :"2024-08-22T12:10:44.416187",
    "last_modified_by" :"user@superposition.io" } COPY dev.functions (
        function_name,
        published_code,
        draft_code,
        function_description,
        published_runtime_version,
        draft_runtime_version,
        published_at,
        draft_edited_at,
        published_by,
        draft_edited_by,
        last_modified_at,
        last_modified_by
    )
FROM stdin;
fbf21085-b896-491d-a164-9f07b49b3fbf default_configs postgres 2024 -12 -18 11 :10 :56.255505
INSERT \ N { "key" :"base_rate",
    "value" :10.0,
    "created_at" :"2024-08-22T12:11:28.122453+00:00",
    "created_by" :"user@superposition.io",
    "schema": { "type" :"number" },
    "function_name" :null,
    "last_modified_at" :"2024-08-22T12:11:28.122802",
    "last_modified_by" :"user@superposition.io" } COPY dev.default_configs (
        key,
        value,
        created_at,
        created_by,
        schema,
        function_name,
        last_modified_at,
        last_modified_by
    )
FROM stdin;
c36d9ed9-6522-4bfd-9f4f-38b965e24494 default_configs postgres 2024 -12 -18 11 :10 :56.255505
INSERT \ N { "key" :"currency",
    "value" :"INR",
    "created_at" :"2024-08-22T12:12:19.765579+00:00",
    "created_by" :"user@superposition.io",
    "schema": { "enum" :["INR","USD","EUR"],
    "type" :"string" },
    "function_name" :null,
    "last_modified_at" :"2024-08-22T12:12:19.765591",
    "last_modified_by" :"user@superposition.io" } COPY dev.default_configs (
        key,
        value,
        created_at,
        created_by,
        schema,
        function_name,
        last_modified_at,
        last_modified_by
    )
FROM stdin;
98bd5824-7a6b-45c7-a850-98a1a73177a8 default_configs postgres 2024 -12 -18 11 :10 :56.255505
INSERT \ N { "key" :"per_distance_unit_rate",
    "value" :15.0,
    "created_at" :"2024-08-22T12:12:37.370436+00:00",
    "created_by" :"user@superposition.io",
    "schema": { "type" :"number" },
    "function_name" :null,
    "last_modified_at" :"2024-08-22T12:12:37.370446",
    "last_modified_by" :"user@superposition.io" } COPY dev.default_configs (
        key,
        value,
        created_at,
        created_by,
        schema,
        function_name,
        last_modified_at,
        last_modified_by
    )
FROM stdin;
bf1e9c65-aa19-4169-aa89-43a9496a69e1 default_configs postgres 2024 -12 -18 11 :10 :56.255505
INSERT \ N { "key" :"distance_unit",
    "value" :"Km",
    "created_at" :"2024-08-22T12:12:53.814292+00:00",
    "created_by" :"user@superposition.io",
    "schema": { "enum" :["Km","Miles"],
    "type" :"string" },
    "function_name" :null,
    "last_modified_at" :"2024-08-22T12:12:53.814303",
    "last_modified_by" :"user@superposition.io" } COPY dev.default_configs (
        key,
        value,
        created_at,
        created_by,
        schema,
        function_name,
        last_modified_at,
        last_modified_by
    )
FROM stdin;
c3247769-5428-4c74-b0c3-1219e5721b6e default_configs postgres 2024 -12 -18 11 :10 :56.255505
INSERT \ N { "key" :"foo.foo",
    "value" :1,
    "created_at" :"2024-08-22T12:13:24.43122+00:00",
    "created_by" :"user@superposition.io",
    "schema": { "type" :"integer" },
    "function_name" :null,
    "last_modified_at" :"2024-08-22T12:13:24.431231",
    "last_modified_by" :"user@superposition.io" } COPY dev.default_configs (
        key,
        value,
        created_at,
        created_by,
        schema,
        function_name,
        last_modified_at,
        last_modified_by
    )
FROM stdin;
734f2163-6186-405b-afd5-533d93f4444d default_configs postgres 2024 -12 -18 11 :10 :56.255505
INSERT \ N { "key" :"foo.bar",
    "value" :2,
    "created_at" :"2024-08-22T12:13:37.788173+00:00",
    "created_by" :"user@superposition.io",
    "schema": { "type" :"number" },
    "function_name" :null,
    "last_modified_at" :"2024-08-22T12:13:37.788184",
    "last_modified_by" :"user@superposition.io" } COPY dev.default_configs (
        key,
        value,
        created_at,
        created_by,
        schema,
        function_name,
        last_modified_at,
        last_modified_by
    )
FROM stdin;
bbc862ad-611d-4068-860c-51575392e869 default_configs postgres 2024 -12 -18 11 :10 :56.255505
INSERT \ N { "key" :"hello_message",
    "value" :"Hello World !!!",
    "created_at" :"2024-08-22T12:14:02.505322+00:00",
    "created_by" :"user@superposition.io",
    "schema": { "pattern" :".*",
    "type" :"string" },
    "function_name" :null,
    "last_modified_at" :"2024-08-22T12:14:02.505334",
    "last_modified_by" :"user@superposition.io" } COPY dev.default_configs (
        key,
        value,
        created_at,
        created_by,
        schema,
        function_name,
        last_modified_at,
        last_modified_by
    )
FROM stdin;
d5804037-0a25-4bdd-b0b3-ccdfe6f7d0f4 default_configs postgres 2024 -12 -18 11 :10 :56.255505
INSERT \ N { "key" :"hello_message_color",
    "value" :"black",
    "created_at" :"2024-08-22T12:14:27.222544+00:00",
    "created_by" :"user@superposition.io",
    "schema": { "enum" :["black","red","blue","green","pink"],
    "type" :"string" },
    "function_name" :null,
    "last_modified_at" :"2024-08-22T12:14:27.222551",
    "last_modified_by" :"user@superposition.io" } COPY dev.default_configs (
        key,
        value,
        created_at,
        created_by,
        schema,
        function_name,
        last_modified_at,
        last_modified_by
    )
FROM stdin;
c76172fd-6501-442d-8f32-0fce2803e582 default_configs postgres 2024 -12 -18 11 :10 :56.255505
INSERT \ N { "key" :"logo",
    "value" :"https://cdn1.vectorstock.com/i/1000x1000/03/20/big-city-logo-vector-20480320.jpg",
    "created_at" :"2024-08-22T12:15:08.701643+00:00",
    "created_by" :"user@superposition.io",
    "schema": { "pattern" :"https://.*",
    "type" :"string" },
    "function_name" :"axios_validator_example",
    "last_modified_at" :"2024-08-22T12:15:08.701648",
    "last_modified_by" :"user@superposition.io" } COPY dev.default_configs (
        key,
        value,
        created_at,
        created_by,
        schema,
        function_name,
        last_modified_at,
        last_modified_by
    )
FROM stdin;
b9168329-65bf-4ea8-b18c-81a89e1631c6 contexts postgres 2024 -12 -18 11 :13 :38.535445
INSERT \ N { "id" :"11c6bd932b8888a9e50be8f822b76116fbaa20fd58b059f98b9e7179543bb9a4",
    "value": { "==" :[{"var":"city"},"Bangalore"] },
    "override_id" :"2a2eb08ced59872063128fc531b17a04e0e3be31d565b57273508008b207636a",
    "created_at" :"2024-08-22T12:16:35.579131+00:00",
    "created_by" :"user@superposition.io",
    "priority" :4,
    "override": { "base_rate" :20,
    "distance_unit" :"Km",
    "hello_message" :"ನಮಸ್ಕಾರ ಬೆಂಗಳೂರು",
    "hello_message_color" :"red",
    "logo" :"https://www.shutterstock.com/image-vector/bangalore-logo-vidhana-soudha-600nw-1506258893.jpg",
    "per_distance_unit_rate" :15 },
    "last_modified_at" :"2024-08-22T12:16:43.897928",
    "last_modified_by" :"user@superposition.io",
    "weight" :1 } COPY dev.contexts (
        id,
        value,
        override_id,
        created_at,
        created_by,
        priority,
        override,
        last_modified_at,
        last_modified_by
    )
FROM stdin;
36124c72-664e-41f6-bef4-5aa0c9495c7c contexts postgres 2024 -12 -18 11 :13 :38.535445
INSERT \ N { "id" :"e47edbe36bf3333f42ca2ded0a4604997adc4f6e0bf6176079272597c01353d8",
    "value": { "==" :[{"var":"city"},"Chennai"] },
    "override_id" :"645b12a924823be87eb423791f3c42393ab19103afcd8ce923c4a1112408b32b",
    "created_at" :"2024-08-22T12:36:23.489214+00:00",
    "created_by" :"user@superposition.io",
    "priority" :4,
    "override": { "hello_message" :"வணக்கம் சென்னை",
    "hello_message_color" :"green",
    "logo" :"https://www.shutterstock.com/image-vector/chennai-skyline-color-landmarks-blue-260nw-515862346.jpg" },
    "last_modified_at" :"2024-08-22T12:36:23.489216",
    "last_modified_by" :"user@superposition.io",
    "weight" :1 } COPY dev.contexts (
        id,
        value,
        override_id,
        created_at,
        created_by,
        priority,
        override,
        last_modified_at,
        last_modified_by
    )
FROM stdin;
36666a01-6ea8-460f-89e0-36093b718685 contexts postgres 2024 -12 -18 11 :13 :38.535445
INSERT \ N { "id" :"9fb61ecfb662467e9036ec79bfd8c02ac244c1836f113242aec5fe6d9265d8dc",
    "value": { "and" :[{"==":[{"var":"city"},"Bangalore"] },
    { "==" :[{"var":"vehicle_type"},"cab"] } ] },
    "override_id" :"e2411e811b927824f910f9edd75b984a3ad226c46a1216ffa735b7bda60c1ede",
    "created_at" :"2024-08-22T12:37:53.206065+00:00",
    "created_by" :"user@superposition.io",
    "priority" :6,
    "override": { "base_rate" :12 },
    "last_modified_at" :"2024-08-22T12:37:53.206068",
    "last_modified_by" :"user@superposition.io",
    "weight" :1 } COPY dev.contexts (
        id,
        value,
        override_id,
        created_at,
        created_by,
        priority,
        override,
        last_modified_at,
        last_modified_by
    )
FROM stdin;
fdd3842d-13b5-432c-a562-8d57458f08d3 contexts postgres 2024 -12 -18 11 :16 :14.500765
UPDATE { "id" :"11c6bd932b8888a9e50be8f822b76116fbaa20fd58b059f98b9e7179543bb9a4",
    "value": { "==" :[{"var":"city"},"Bangalore"] },
    "override_id" :"2a2eb08ced59872063128fc531b17a04e0e3be31d565b57273508008b207636a",
    "created_at" :"2024-08-22T12:16:35.579131+00:00",
    "created_by" :"user@superposition.io",
    "priority" :4,
    "override": { "base_rate" :20,
    "distance_unit" :"Km",
    "hello_message" :"ನಮಸ್ಕಾರ ಬೆಂಗಳೂರು",
    "hello_message_color" :"red",
    "logo" :"https://www.shutterstock.com/image-vector/bangalore-logo-vidhana-soudha-600nw-1506258893.jpg",
    "per_distance_unit_rate" :15 },
    "last_modified_at" :"2024-08-22T12:16:43.897928",
    "last_modified_by" :"user@superposition.io",
    "weight" :1 } { "id" :"11c6bd932b8888a9e50be8f822b76116fbaa20fd58b059f98b9e7179543bb9a4",
    "value": { "==" :[{"var":"city"},"Bangalore"] },
    "override_id" :"2a2eb08ced59872063128fc531b17a04e0e3be31d565b57273508008b207636a",
    "created_at" :"2024-08-22T12:16:35.579131+00:00",
    "created_by" :"user@superposition.io",
    "priority" :4,
    "override": { "base_rate" :20,
    "distance_unit" :"Km",
    "hello_message" :"ನಮಸ್ಕಾರ ಬೆಂಗಳೂರು",
    "hello_message_color" :"red",
    "logo" :"https://www.shutterstock.com/image-vector/bangalore-logo-vidhana-soudha-600nw-1506258893.jpg",
    "per_distance_unit_rate" :15 },
    "last_modified_at" :"2024-12-18T11:16:14.500284",
    "last_modified_by" :"user@superposition.io",
    "weight" :2 }
UPDATE "contexts"
SET "weight" = $1,
    "last_modified_at" = $2,
    "last_modified_by" = $3
WHERE ("contexts"."id" = $4) f756587b-ca13-4a98-acf9-a89a28d89b1e contexts postgres 2024 -12 -18 11 :16 :14.500765
UPDATE { "id" :"e47edbe36bf3333f42ca2ded0a4604997adc4f6e0bf6176079272597c01353d8",
    "value": { "==" :[{"var":"city"},"Chennai"] },
    "override_id" :"645b12a924823be87eb423791f3c42393ab19103afcd8ce923c4a1112408b32b",
    "created_at" :"2024-08-22T12:36:23.489214+00:00",
    "created_by" :"user@superposition.io",
    "priority" :4,
    "override": { "hello_message" :"வணக்கம் சென்னை",
    "hello_message_color" :"green",
    "logo" :"https://www.shutterstock.com/image-vector/chennai-skyline-color-landmarks-blue-260nw-515862346.jpg" },
    "last_modified_at" :"2024-08-22T12:36:23.489216",
    "last_modified_by" :"user@superposition.io",
    "weight" :1 } { "id" :"e47edbe36bf3333f42ca2ded0a4604997adc4f6e0bf6176079272597c01353d8",
    "value": { "==" :[{"var":"city"},"Chennai"] },
    "override_id" :"645b12a924823be87eb423791f3c42393ab19103afcd8ce923c4a1112408b32b",
    "created_at" :"2024-08-22T12:36:23.489214+00:00",
    "created_by" :"user@superposition.io",
    "priority" :4,
    "override": { "hello_message" :"வணக்கம் சென்னை",
    "hello_message_color" :"green",
    "logo" :"https://www.shutterstock.com/image-vector/chennai-skyline-color-landmarks-blue-260nw-515862346.jpg" },
    "last_modified_at" :"2024-12-18T11:16:14.500284",
    "last_modified_by" :"user@superposition.io",
    "weight" :2 }
UPDATE "contexts"
SET "weight" = $1,
    "last_modified_at" = $2,
    "last_modified_by" = $3
WHERE ("contexts"."id" = $4) 4b708912-d76c-4b5d-9d6c-5d9df93bfaf8 contexts postgres 2024 -12 -18 11 :16 :14.500765
UPDATE { "id" :"3f0e5fe95e2e758151d4cef712185767a137c926a9e7156b6face3a98b167d35",
    "value": { "==" :[{"var":"city"},"Seattle"] },
    "override_id" :"f73412831793a34df7df95a79bc65754b1eebf31534e4ccfa9c79681fcf8ac69",
    "created_at" :"2024-08-22T12:37:32.306949+00:00",
    "created_by" :"user@superposition.io",
    "priority" :4,
    "override": { "base_rate" :5,
    "currency" :"USD",
    "distance_unit" :"Miles",
    "hello_message" :"Hello Seattle",
    "hello_message_color" :"blue",
    "logo" :"https://t4.ftcdn.net/jpg/04/24/15/07/360_F_424150716_kDZIgUONDIKpIhHqsxlDcVIiglyjIOQs.jpg",
    "per_distance_unit_rate" :2.5 },
    "last_modified_at" :"2024-08-22T12:37:32.306953",
    "last_modified_by" :"user@superposition.io",
    "weight" :1 } { "id" :"3f0e5fe95e2e758151d4cef712185767a137c926a9e7156b6face3a98b167d35",
    "value": { "==" :[{"var":"city"},"Seattle"] },
    "override_id" :"f73412831793a34df7df95a79bc65754b1eebf31534e4ccfa9c79681fcf8ac69",
    "created_at" :"2024-08-22T12:37:32.306949+00:00",
    "created_by" :"user@superposition.io",
    "priority" :4,
    "override": { "base_rate" :5,
    "currency" :"USD",
    "distance_unit" :"Miles",
    "hello_message" :"Hello Seattle",
    "hello_message_color" :"blue",
    "logo" :"https://t4.ftcdn.net/jpg/04/24/15/07/360_F_424150716_kDZIgUONDIKpIhHqsxlDcVIiglyjIOQs.jpg",
    "per_distance_unit_rate" :2.5 },
    "last_modified_at" :"2024-12-18T11:16:14.500284",
    "last_modified_by" :"user@superposition.io",
    "weight" :2 }
UPDATE "contexts"
SET "weight" = $1,
    "last_modified_at" = $2,
    "last_modified_by" = $3
WHERE ("contexts"."id" = $4) 4433ca4b-983d-4fe3-b1d4-7e6f9a2919de contexts postgres 2024 -12 -18 11 :16 :14.500765
UPDATE { "id" :"9fb61ecfb662467e9036ec79bfd8c02ac244c1836f113242aec5fe6d9265d8dc",
    "value": { "and" :[{"==":[{"var":"city"},"Bangalore"] },
    { "==" :[{"var":"vehicle_type"},"cab"] } ] },
    "override_id" :"e2411e811b927824f910f9edd75b984a3ad226c46a1216ffa735b7bda60c1ede",
    "created_at" :"2024-08-22T12:37:53.206065+00:00",
    "created_by" :"user@superposition.io",
    "priority" :6,
    "override": { "base_rate" :12 },
    "last_modified_at" :"2024-08-22T12:37:53.206068",
    "last_modified_by" :"user@superposition.io",
    "weight" :1 } { "id" :"9fb61ecfb662467e9036ec79bfd8c02ac244c1836f113242aec5fe6d9265d8dc",
    "value": { "and" :[{"==":[{"var":"city"},"Bangalore"] },
    { "==" :[{"var":"vehicle_type"},"cab"] } ] },
    "override_id" :"e2411e811b927824f910f9edd75b984a3ad226c46a1216ffa735b7bda60c1ede",
    "created_at" :"2024-08-22T12:37:53.206065+00:00",
    "created_by" :"user@superposition.io",
    "priority" :6,
    "override": { "base_rate" :12 },
    "last_modified_at" :"2024-12-18T11:16:14.500284",
    "last_modified_by" :"user@superposition.io",
    "weight" :6 }
UPDATE "contexts"
SET "weight" = $1,
    "last_modified_at" = $2,
    "last_modified_by" = $3
WHERE ("contexts"."id" = $4) \.--
    -- Data for Name: event_log_y2025m01; Type: TABLE DATA; Schema: dev; Owner: postgres
    --
    COPY dev.event_log_y2025m01 (
        id,
        table_name,
        user_name,
        "timestamp",
        action,
        original_data,
        new_data,
        query
    )
FROM stdin;
\.--
-- Data for Name: event_log_y2025m02; Type: TABLE DATA; Schema: dev; Owner: postgres
--
COPY dev.event_log_y2025m02 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2025m03; Type: TABLE DATA; Schema: dev; Owner: postgres
--
COPY dev.event_log_y2025m03 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2025m04; Type: TABLE DATA; Schema: dev; Owner: postgres
--
COPY dev.event_log_y2025m04 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2025m05; Type: TABLE DATA; Schema: dev; Owner: postgres
--
COPY dev.event_log_y2025m05 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2025m06; Type: TABLE DATA; Schema: dev; Owner: postgres
--
COPY dev.event_log_y2025m06 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2025m07; Type: TABLE DATA; Schema: dev; Owner: postgres
--
COPY dev.event_log_y2025m07 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2025m08; Type: TABLE DATA; Schema: dev; Owner: postgres
--
COPY dev.event_log_y2025m08 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2025m09; Type: TABLE DATA; Schema: dev; Owner: postgres
--
COPY dev.event_log_y2025m09 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2025m10; Type: TABLE DATA; Schema: dev; Owner: postgres
--
COPY dev.event_log_y2025m10 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2025m11; Type: TABLE DATA; Schema: dev; Owner: postgres
--
COPY dev.event_log_y2025m11 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2025m12; Type: TABLE DATA; Schema: dev; Owner: postgres
--
COPY dev.event_log_y2025m12 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2026m01; Type: TABLE DATA; Schema: dev; Owner: postgres
--
COPY dev.event_log_y2026m01 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2026m02; Type: TABLE DATA; Schema: dev; Owner: postgres
--
COPY dev.event_log_y2026m02 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2026m03; Type: TABLE DATA; Schema: dev; Owner: postgres
--
COPY dev.event_log_y2026m03 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2026m04; Type: TABLE DATA; Schema: dev; Owner: postgres
--
COPY dev.event_log_y2026m04 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2026m05; Type: TABLE DATA; Schema: dev; Owner: postgres
--
COPY dev.event_log_y2026m05 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2026m06; Type: TABLE DATA; Schema: dev; Owner: postgres
--
COPY dev.event_log_y2026m06 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2026m07; Type: TABLE DATA; Schema: dev; Owner: postgres
--
COPY dev.event_log_y2026m07 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2026m08; Type: TABLE DATA; Schema: dev; Owner: postgres
--
COPY dev.event_log_y2026m08 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2026m09; Type: TABLE DATA; Schema: dev; Owner: postgres
--
COPY dev.event_log_y2026m09 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2026m10; Type: TABLE DATA; Schema: dev; Owner: postgres
--
COPY dev.event_log_y2026m10 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2026m11; Type: TABLE DATA; Schema: dev; Owner: postgres
--
COPY dev.event_log_y2026m11 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2026m12; Type: TABLE DATA; Schema: dev; Owner: postgres
--
COPY dev.event_log_y2026m12 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: experiments; Type: TABLE DATA; Schema: dev; Owner: postgres
--
COPY dev.experiments (
    id,
    created_at,
    created_by,
    last_modified,
    name,
    override_keys,
    status,
    traffic_percentage,
    context,
    variants,
    last_modified_by,
    chosen_variant
)
FROM stdin;
\.--
-- Data for Name: functions; Type: TABLE DATA; Schema: dev; Owner: postgres
--
COPY dev.functions (
    function_name,
    published_code,
    draft_code,
    function_description,
    published_runtime_version,
    draft_runtime_version,
    published_at,
    draft_edited_at,
    published_by,
    draft_edited_by,
    last_modified_at,
    last_modified_by
)
FROM stdin;
axios_validator_example YXN5bmMgZnVuY3Rpb24gdmFsaWRhdGUoa2V5LCB2YWx1ZSkgewogICAgcmV0dXJuIGF4aW9zLmdldCh2YWx1ZSkudGhlbihyZXNwb25zZSA9PiByZXNwb25zZS5zdGF0dXMgPT0gJzIwMCcpCiAgICAuY2F0Y2goZXJyID0 + IHsKICAgICAgICBjb25zb2xlLmxvZyhlcnIpOwogICAgICAgIHJldHVybiBmYWxzZTsKICAgIH0pOwp9Cg == YXN5bmMgZnVuY3Rpb24gdmFsaWRhdGUoa2V5LCB2YWx1ZSkgewogICAgcmV0dXJuIGF4aW9zLmdldCh2YWx1ZSkudGhlbihyZXNwb25zZSA9PiByZXNwb25zZS5zdGF0dXMgPT0gJzIwMCcpCiAgICAuY2F0Y2goZXJyID0 + IHsKICAgICAgICBjb25zb2xlLmxvZyhlcnIpOwogICAgICAgIHJldHVybiBmYWxzZTsKICAgIH0pOwp9Cg == An example function that shows off validator functions 1.0.0 1.0.0 2024 -08 -22 12 :10 :48.787527 2024 -08 -22 12 :10 :44.416185 user @superposition.io user @superposition.io 2024 -08 -22 12 :10 :44.416187 user @superposition.io \.--
-- Data for Name: type_templates; Type: TABLE DATA; Schema: dev; Owner: postgres
--
COPY dev.type_templates (
    type_name,
    type_schema,
    created_by,
    created_at,
    last_modified_at,
    last_modified_by
)
FROM stdin;
Number { "type": "integer" } user @superposition.io 2024 -10 -18 10 :34 :00.376562 2024 -10 -18 10 :34 :00.376562 null Decimal { "type": "number" } user @superposition.io 2024 -10 -18 10 :35 :00.376562 2024 -10 -18 10 :35 :00.376562 null Boolean { "type": "boolean" } user @superposition.io 2024 -10 -18 10 :36 :00.376562 2024 -10 -18 10 :36 :00.376562 null Enum { "type": "string",
"enum": ["android", "ios"] } user @superposition.io 2024 -10 -18 10 :37 :00.376562 2024 -10 -18 10 :37 :00.376562 null Pattern { "type": "string",
"pattern": ".*" } user @superposition.io 2024 -10 -18 10 :38 :00.376562 2024 -10 -18 10 :38 :00.376562 null \.--
-- Data for Name: workspaces; Type: TABLE DATA; Schema: dev; Owner: postgres
--
COPY dev.workspaces (
    organisation_id,
    organisation_name,
    workspace_name,
    workspace_schema_name,
    workspace_status,
    workspace_admin_email,
    created_by,
    last_modified_by,
    last_modified_at,
    created_at
)
FROM stdin;
\.--
-- Data for Name: __diesel_schema_migrations; Type: TABLE DATA; Schema: public; Owner: postgres
--
COPY public.__diesel_schema_migrations (version, run_on)
FROM stdin;
\.--
-- Data for Name: config_versions; Type: TABLE DATA; Schema: test; Owner: postgres
--
COPY test.config_versions (id, config, config_hash, tags, created_at)
FROM stdin;
\.--
-- Data for Name: contexts; Type: TABLE DATA; Schema: test; Owner: postgres
--
COPY test.contexts (
    id,
    value,
    override_id,
    created_at,
    created_by,
    priority,
    override,
    last_modified_at,
    last_modified_by,
    weight
)
FROM stdin;
\.--
-- Data for Name: default_configs; Type: TABLE DATA; Schema: test; Owner: postgres
--
COPY test.default_configs (
    key,
    value,
    created_at,
    created_by,
    schema,
    function_name,
    last_modified_at,
    last_modified_by
)
FROM stdin;
\.--
-- Data for Name: dimensions; Type: TABLE DATA; Schema: test; Owner: postgres
--
COPY test.dimensions (
    dimension,
    priority,
    created_at,
    created_by,
    schema,
    function_name,
    last_modified_at,
    last_modified_by,
    "position"
)
FROM stdin;
variantIds 1 2024 -12 -18 11 :04 :43.661677 + 00 user @example.com { "type": "string",
"pattern": ".*" } \ N 2024 -12 -18 11 :04 :43.661677 null 0 \.--
-- Data for Name: event_log_y2023m08; Type: TABLE DATA; Schema: test; Owner: postgres
--
COPY test.event_log_y2023m08 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2023m09; Type: TABLE DATA; Schema: test; Owner: postgres
--
COPY test.event_log_y2023m09 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2023m10; Type: TABLE DATA; Schema: test; Owner: postgres
--
COPY test.event_log_y2023m10 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2023m11; Type: TABLE DATA; Schema: test; Owner: postgres
--
COPY test.event_log_y2023m11 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2023m12; Type: TABLE DATA; Schema: test; Owner: postgres
--
COPY test.event_log_y2023m12 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2024m01; Type: TABLE DATA; Schema: test; Owner: postgres
--
COPY test.event_log_y2024m01 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2024m02; Type: TABLE DATA; Schema: test; Owner: postgres
--
COPY test.event_log_y2024m02 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2024m03; Type: TABLE DATA; Schema: test; Owner: postgres
--
COPY test.event_log_y2024m03 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2024m04; Type: TABLE DATA; Schema: test; Owner: postgres
--
COPY test.event_log_y2024m04 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2024m05; Type: TABLE DATA; Schema: test; Owner: postgres
--
COPY test.event_log_y2024m05 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2024m06; Type: TABLE DATA; Schema: test; Owner: postgres
--
COPY test.event_log_y2024m06 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2024m07; Type: TABLE DATA; Schema: test; Owner: postgres
--
COPY test.event_log_y2024m07 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2024m08; Type: TABLE DATA; Schema: test; Owner: postgres
--
COPY test.event_log_y2024m08 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2024m09; Type: TABLE DATA; Schema: test; Owner: postgres
--
COPY test.event_log_y2024m09 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2024m10; Type: TABLE DATA; Schema: test; Owner: postgres
--
COPY test.event_log_y2024m10 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2024m11; Type: TABLE DATA; Schema: test; Owner: postgres
--
COPY test.event_log_y2024m11 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2024m12; Type: TABLE DATA; Schema: test; Owner: postgres
--
COPY test.event_log_y2024m12 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
d56f8e2d-a323-4385-9d2a-730b41e62234 dimensions postgres 2024 -12 -18 11 :04 :43.661677
INSERT \ N { "dimension" :"variantIds",
    "priority" :1,
    "created_at" :"2024-12-18T11:04:43.661677+00:00",
    "created_by" :"user@example.com",
    "schema": { "type": "string",
    "pattern": ".*" },
    "function_name" :null,
    "last_modified_at" :"2024-12-18T11:04:43.661677",
    "last_modified_by" :"null",
    "position" :0 }
INSERT INTO test.dimensions (
        dimension,
        priority,
        created_at,
        created_by,
        schema,
        function_name
    )
VALUES (
        'variantIds',
        1,
        CURRENT_TIMESTAMP,
        'user@example.com',
        '{"type": "string","pattern": ".*"}'::json,
        null
    );
\.--
-- Data for Name: event_log_y2025m01; Type: TABLE DATA; Schema: test; Owner: postgres
--
COPY test.event_log_y2025m01 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2025m02; Type: TABLE DATA; Schema: test; Owner: postgres
--
COPY test.event_log_y2025m02 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2025m03; Type: TABLE DATA; Schema: test; Owner: postgres
--
COPY test.event_log_y2025m03 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2025m04; Type: TABLE DATA; Schema: test; Owner: postgres
--
COPY test.event_log_y2025m04 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2025m05; Type: TABLE DATA; Schema: test; Owner: postgres
--
COPY test.event_log_y2025m05 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2025m06; Type: TABLE DATA; Schema: test; Owner: postgres
--
COPY test.event_log_y2025m06 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2025m07; Type: TABLE DATA; Schema: test; Owner: postgres
--
COPY test.event_log_y2025m07 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2025m08; Type: TABLE DATA; Schema: test; Owner: postgres
--
COPY test.event_log_y2025m08 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2025m09; Type: TABLE DATA; Schema: test; Owner: postgres
--
COPY test.event_log_y2025m09 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2025m10; Type: TABLE DATA; Schema: test; Owner: postgres
--
COPY test.event_log_y2025m10 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2025m11; Type: TABLE DATA; Schema: test; Owner: postgres
--
COPY test.event_log_y2025m11 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2025m12; Type: TABLE DATA; Schema: test; Owner: postgres
--
COPY test.event_log_y2025m12 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2026m01; Type: TABLE DATA; Schema: test; Owner: postgres
--
COPY test.event_log_y2026m01 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2026m02; Type: TABLE DATA; Schema: test; Owner: postgres
--
COPY test.event_log_y2026m02 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2026m03; Type: TABLE DATA; Schema: test; Owner: postgres
--
COPY test.event_log_y2026m03 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2026m04; Type: TABLE DATA; Schema: test; Owner: postgres
--
COPY test.event_log_y2026m04 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2026m05; Type: TABLE DATA; Schema: test; Owner: postgres
--
COPY test.event_log_y2026m05 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2026m06; Type: TABLE DATA; Schema: test; Owner: postgres
--
COPY test.event_log_y2026m06 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2026m07; Type: TABLE DATA; Schema: test; Owner: postgres
--
COPY test.event_log_y2026m07 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2026m08; Type: TABLE DATA; Schema: test; Owner: postgres
--
COPY test.event_log_y2026m08 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2026m09; Type: TABLE DATA; Schema: test; Owner: postgres
--
COPY test.event_log_y2026m09 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2026m10; Type: TABLE DATA; Schema: test; Owner: postgres
--
COPY test.event_log_y2026m10 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2026m11; Type: TABLE DATA; Schema: test; Owner: postgres
--
COPY test.event_log_y2026m11 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: event_log_y2026m12; Type: TABLE DATA; Schema: test; Owner: postgres
--
COPY test.event_log_y2026m12 (
    id,
    table_name,
    user_name,
    "timestamp",
    action,
    original_data,
    new_data,
    query
)
FROM stdin;
\.--
-- Data for Name: experiments; Type: TABLE DATA; Schema: test; Owner: postgres
--
COPY test.experiments (
    id,
    created_at,
    created_by,
    last_modified,
    name,
    override_keys,
    status,
    traffic_percentage,
    context,
    variants,
    last_modified_by,
    chosen_variant
)
FROM stdin;
\.--
-- Data for Name: functions; Type: TABLE DATA; Schema: test; Owner: postgres
--
COPY test.functions (
    function_name,
    published_code,
    draft_code,
    function_description,
    published_runtime_version,
    draft_runtime_version,
    published_at,
    draft_edited_at,
    published_by,
    draft_edited_by,
    last_modified_at,
    last_modified_by
)
FROM stdin;
\.--
-- Data for Name: type_templates; Type: TABLE DATA; Schema: test; Owner: postgres
--
COPY test.type_templates (
    type_name,
    type_schema,
    created_by,
    created_at,
    last_modified_at,
    last_modified_by
)
FROM stdin;
Number { "type": "integer" } user @superposition.io 2024 -10 -18 10 :34 :00.376562 2024 -10 -18 10 :34 :00.376562 null Decimal { "type": "number" } user @superposition.io 2024 -10 -18 10 :35 :00.376562 2024 -10 -18 10 :35 :00.376562 null Boolean { "type": "boolean" } user @superposition.io 2024 -10 -18 10 :36 :00.376562 2024 -10 -18 10 :36 :00.376562 null Enum { "type": "string",
"enum": ["android", "ios"] } user @superposition.io 2024 -10 -18 10 :37 :00.376562 2024 -10 -18 10 :37 :00.376562 null Pattern { "type": "string",
"pattern": ".*" } user @superposition.io 2024 -10 -18 10 :38 :00.376562 2024 -10 -18 10 :38 :00.376562 null \.--
-- Data for Name: workspaces; Type: TABLE DATA; Schema: test; Owner: postgres
--
COPY test.workspaces (
    organisation_id,
    organisation_name,
    workspace_name,
    workspace_schema_name,
    workspace_status,
    workspace_admin_email,
    created_by,
    last_modified_by,
    last_modified_at,
    created_at
)
FROM stdin;
\.--
-- Name: config_versions config_versions_pkey; Type: CONSTRAINT; Schema: dev; Owner: postgres
--
ALTER TABLE ONLY dev.config_versions
ADD CONSTRAINT config_versions_pkey PRIMARY KEY (id);
--
-- Name: contexts contexts_pkey; Type: CONSTRAINT; Schema: dev; Owner: postgres
--
ALTER TABLE ONLY dev.contexts
ADD CONSTRAINT contexts_pkey PRIMARY KEY (id);
--
-- Name: default_configs default_configs_pkey; Type: CONSTRAINT; Schema: dev; Owner: postgres
--
ALTER TABLE ONLY dev.default_configs
ADD CONSTRAINT default_configs_pkey PRIMARY KEY (key);
--
-- Name: dimensions dimension_unique_position; Type: CONSTRAINT; Schema: dev; Owner: postgres
--
ALTER TABLE ONLY dev.dimensions
ADD CONSTRAINT dimension_unique_position UNIQUE ("position");
--
-- Name: dimensions dimensions_pkey; Type: CONSTRAINT; Schema: dev; Owner: postgres
--
ALTER TABLE ONLY dev.dimensions
ADD CONSTRAINT dimensions_pkey PRIMARY KEY (dimension);
--
-- Name: event_log event_log_pkey; Type: CONSTRAINT; Schema: dev; Owner: postgres
--
ALTER TABLE ONLY dev.event_log
ADD CONSTRAINT event_log_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2023m08 event_log_y2023m08_pkey; Type: CONSTRAINT; Schema: dev; Owner: postgres
--
ALTER TABLE ONLY dev.event_log_y2023m08
ADD CONSTRAINT event_log_y2023m08_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2023m09 event_log_y2023m09_pkey; Type: CONSTRAINT; Schema: dev; Owner: postgres
--
ALTER TABLE ONLY dev.event_log_y2023m09
ADD CONSTRAINT event_log_y2023m09_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2023m10 event_log_y2023m10_pkey; Type: CONSTRAINT; Schema: dev; Owner: postgres
--
ALTER TABLE ONLY dev.event_log_y2023m10
ADD CONSTRAINT event_log_y2023m10_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2023m11 event_log_y2023m11_pkey; Type: CONSTRAINT; Schema: dev; Owner: postgres
--
ALTER TABLE ONLY dev.event_log_y2023m11
ADD CONSTRAINT event_log_y2023m11_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2023m12 event_log_y2023m12_pkey; Type: CONSTRAINT; Schema: dev; Owner: postgres
--
ALTER TABLE ONLY dev.event_log_y2023m12
ADD CONSTRAINT event_log_y2023m12_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2024m01 event_log_y2024m01_pkey; Type: CONSTRAINT; Schema: dev; Owner: postgres
--
ALTER TABLE ONLY dev.event_log_y2024m01
ADD CONSTRAINT event_log_y2024m01_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2024m02 event_log_y2024m02_pkey; Type: CONSTRAINT; Schema: dev; Owner: postgres
--
ALTER TABLE ONLY dev.event_log_y2024m02
ADD CONSTRAINT event_log_y2024m02_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2024m03 event_log_y2024m03_pkey; Type: CONSTRAINT; Schema: dev; Owner: postgres
--
ALTER TABLE ONLY dev.event_log_y2024m03
ADD CONSTRAINT event_log_y2024m03_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2024m04 event_log_y2024m04_pkey; Type: CONSTRAINT; Schema: dev; Owner: postgres
--
ALTER TABLE ONLY dev.event_log_y2024m04
ADD CONSTRAINT event_log_y2024m04_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2024m05 event_log_y2024m05_pkey; Type: CONSTRAINT; Schema: dev; Owner: postgres
--
ALTER TABLE ONLY dev.event_log_y2024m05
ADD CONSTRAINT event_log_y2024m05_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2024m06 event_log_y2024m06_pkey; Type: CONSTRAINT; Schema: dev; Owner: postgres
--
ALTER TABLE ONLY dev.event_log_y2024m06
ADD CONSTRAINT event_log_y2024m06_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2024m07 event_log_y2024m07_pkey; Type: CONSTRAINT; Schema: dev; Owner: postgres
--
ALTER TABLE ONLY dev.event_log_y2024m07
ADD CONSTRAINT event_log_y2024m07_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2024m08 event_log_y2024m08_pkey; Type: CONSTRAINT; Schema: dev; Owner: postgres
--
ALTER TABLE ONLY dev.event_log_y2024m08
ADD CONSTRAINT event_log_y2024m08_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2024m09 event_log_y2024m09_pkey; Type: CONSTRAINT; Schema: dev; Owner: postgres
--
ALTER TABLE ONLY dev.event_log_y2024m09
ADD CONSTRAINT event_log_y2024m09_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2024m10 event_log_y2024m10_pkey; Type: CONSTRAINT; Schema: dev; Owner: postgres
--
ALTER TABLE ONLY dev.event_log_y2024m10
ADD CONSTRAINT event_log_y2024m10_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2024m11 event_log_y2024m11_pkey; Type: CONSTRAINT; Schema: dev; Owner: postgres
--
ALTER TABLE ONLY dev.event_log_y2024m11
ADD CONSTRAINT event_log_y2024m11_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2024m12 event_log_y2024m12_pkey; Type: CONSTRAINT; Schema: dev; Owner: postgres
--
ALTER TABLE ONLY dev.event_log_y2024m12
ADD CONSTRAINT event_log_y2024m12_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2025m01 event_log_y2025m01_pkey; Type: CONSTRAINT; Schema: dev; Owner: postgres
--
ALTER TABLE ONLY dev.event_log_y2025m01
ADD CONSTRAINT event_log_y2025m01_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2025m02 event_log_y2025m02_pkey; Type: CONSTRAINT; Schema: dev; Owner: postgres
--
ALTER TABLE ONLY dev.event_log_y2025m02
ADD CONSTRAINT event_log_y2025m02_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2025m03 event_log_y2025m03_pkey; Type: CONSTRAINT; Schema: dev; Owner: postgres
--
ALTER TABLE ONLY dev.event_log_y2025m03
ADD CONSTRAINT event_log_y2025m03_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2025m04 event_log_y2025m04_pkey; Type: CONSTRAINT; Schema: dev; Owner: postgres
--
ALTER TABLE ONLY dev.event_log_y2025m04
ADD CONSTRAINT event_log_y2025m04_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2025m05 event_log_y2025m05_pkey; Type: CONSTRAINT; Schema: dev; Owner: postgres
--
ALTER TABLE ONLY dev.event_log_y2025m05
ADD CONSTRAINT event_log_y2025m05_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2025m06 event_log_y2025m06_pkey; Type: CONSTRAINT; Schema: dev; Owner: postgres
--
ALTER TABLE ONLY dev.event_log_y2025m06
ADD CONSTRAINT event_log_y2025m06_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2025m07 event_log_y2025m07_pkey; Type: CONSTRAINT; Schema: dev; Owner: postgres
--
ALTER TABLE ONLY dev.event_log_y2025m07
ADD CONSTRAINT event_log_y2025m07_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2025m08 event_log_y2025m08_pkey; Type: CONSTRAINT; Schema: dev; Owner: postgres
--
ALTER TABLE ONLY dev.event_log_y2025m08
ADD CONSTRAINT event_log_y2025m08_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2025m09 event_log_y2025m09_pkey; Type: CONSTRAINT; Schema: dev; Owner: postgres
--
ALTER TABLE ONLY dev.event_log_y2025m09
ADD CONSTRAINT event_log_y2025m09_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2025m10 event_log_y2025m10_pkey; Type: CONSTRAINT; Schema: dev; Owner: postgres
--
ALTER TABLE ONLY dev.event_log_y2025m10
ADD CONSTRAINT event_log_y2025m10_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2025m11 event_log_y2025m11_pkey; Type: CONSTRAINT; Schema: dev; Owner: postgres
--
ALTER TABLE ONLY dev.event_log_y2025m11
ADD CONSTRAINT event_log_y2025m11_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2025m12 event_log_y2025m12_pkey; Type: CONSTRAINT; Schema: dev; Owner: postgres
--
ALTER TABLE ONLY dev.event_log_y2025m12
ADD CONSTRAINT event_log_y2025m12_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2026m01 event_log_y2026m01_pkey; Type: CONSTRAINT; Schema: dev; Owner: postgres
--
ALTER TABLE ONLY dev.event_log_y2026m01
ADD CONSTRAINT event_log_y2026m01_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2026m02 event_log_y2026m02_pkey; Type: CONSTRAINT; Schema: dev; Owner: postgres
--
ALTER TABLE ONLY dev.event_log_y2026m02
ADD CONSTRAINT event_log_y2026m02_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2026m03 event_log_y2026m03_pkey; Type: CONSTRAINT; Schema: dev; Owner: postgres
--
ALTER TABLE ONLY dev.event_log_y2026m03
ADD CONSTRAINT event_log_y2026m03_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2026m04 event_log_y2026m04_pkey; Type: CONSTRAINT; Schema: dev; Owner: postgres
--
ALTER TABLE ONLY dev.event_log_y2026m04
ADD CONSTRAINT event_log_y2026m04_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2026m05 event_log_y2026m05_pkey; Type: CONSTRAINT; Schema: dev; Owner: postgres
--
ALTER TABLE ONLY dev.event_log_y2026m05
ADD CONSTRAINT event_log_y2026m05_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2026m06 event_log_y2026m06_pkey; Type: CONSTRAINT; Schema: dev; Owner: postgres
--
ALTER TABLE ONLY dev.event_log_y2026m06
ADD CONSTRAINT event_log_y2026m06_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2026m07 event_log_y2026m07_pkey; Type: CONSTRAINT; Schema: dev; Owner: postgres
--
ALTER TABLE ONLY dev.event_log_y2026m07
ADD CONSTRAINT event_log_y2026m07_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2026m08 event_log_y2026m08_pkey; Type: CONSTRAINT; Schema: dev; Owner: postgres
--
ALTER TABLE ONLY dev.event_log_y2026m08
ADD CONSTRAINT event_log_y2026m08_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2026m09 event_log_y2026m09_pkey; Type: CONSTRAINT; Schema: dev; Owner: postgres
--
ALTER TABLE ONLY dev.event_log_y2026m09
ADD CONSTRAINT event_log_y2026m09_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2026m10 event_log_y2026m10_pkey; Type: CONSTRAINT; Schema: dev; Owner: postgres
--
ALTER TABLE ONLY dev.event_log_y2026m10
ADD CONSTRAINT event_log_y2026m10_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2026m11 event_log_y2026m11_pkey; Type: CONSTRAINT; Schema: dev; Owner: postgres
--
ALTER TABLE ONLY dev.event_log_y2026m11
ADD CONSTRAINT event_log_y2026m11_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2026m12 event_log_y2026m12_pkey; Type: CONSTRAINT; Schema: dev; Owner: postgres
--
ALTER TABLE ONLY dev.event_log_y2026m12
ADD CONSTRAINT event_log_y2026m12_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: experiments experiments_pkey; Type: CONSTRAINT; Schema: dev; Owner: postgres
--
ALTER TABLE ONLY dev.experiments
ADD CONSTRAINT experiments_pkey PRIMARY KEY (id);
--
-- Name: functions functions_pkey; Type: CONSTRAINT; Schema: dev; Owner: postgres
--
ALTER TABLE ONLY dev.functions
ADD CONSTRAINT functions_pkey PRIMARY KEY (function_name);
--
-- Name: workspaces organisation_workspace_pkey; Type: CONSTRAINT; Schema: dev; Owner: postgres
--
ALTER TABLE ONLY dev.workspaces
ADD CONSTRAINT organisation_workspace_pkey PRIMARY KEY (organisation_id, workspace_name);
--
-- Name: type_templates type_templates_pkey; Type: CONSTRAINT; Schema: dev; Owner: postgres
--
ALTER TABLE ONLY dev.type_templates
ADD CONSTRAINT type_templates_pkey PRIMARY KEY (type_name);
--
-- Name: workspaces unique_workspace_schema_name; Type: CONSTRAINT; Schema: dev; Owner: postgres
--
ALTER TABLE ONLY dev.workspaces
ADD CONSTRAINT unique_workspace_schema_name UNIQUE (workspace_schema_name);
--
-- Name: config_versions config_versions_pkey; Type: CONSTRAINT; Schema: test; Owner: postgres
--
ALTER TABLE ONLY test.config_versions
ADD CONSTRAINT config_versions_pkey PRIMARY KEY (id);
--
-- Name: contexts contexts_pkey; Type: CONSTRAINT; Schema: test; Owner: postgres
--
ALTER TABLE ONLY test.contexts
ADD CONSTRAINT contexts_pkey PRIMARY KEY (id);
--
-- Name: default_configs default_configs_pkey; Type: CONSTRAINT; Schema: test; Owner: postgres
--
ALTER TABLE ONLY test.default_configs
ADD CONSTRAINT default_configs_pkey PRIMARY KEY (key);
--
-- Name: dimensions dimension_unique_position; Type: CONSTRAINT; Schema: test; Owner: postgres
--
ALTER TABLE ONLY test.dimensions
ADD CONSTRAINT dimension_unique_position UNIQUE ("position");
--
-- Name: dimensions dimensions_pkey; Type: CONSTRAINT; Schema: test; Owner: postgres
--
ALTER TABLE ONLY test.dimensions
ADD CONSTRAINT dimensions_pkey PRIMARY KEY (dimension);
--
-- Name: event_log event_log_pkey; Type: CONSTRAINT; Schema: test; Owner: postgres
--
ALTER TABLE ONLY test.event_log
ADD CONSTRAINT event_log_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2023m08 event_log_y2023m08_pkey; Type: CONSTRAINT; Schema: test; Owner: postgres
--
ALTER TABLE ONLY test.event_log_y2023m08
ADD CONSTRAINT event_log_y2023m08_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2023m09 event_log_y2023m09_pkey; Type: CONSTRAINT; Schema: test; Owner: postgres
--
ALTER TABLE ONLY test.event_log_y2023m09
ADD CONSTRAINT event_log_y2023m09_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2023m10 event_log_y2023m10_pkey; Type: CONSTRAINT; Schema: test; Owner: postgres
--
ALTER TABLE ONLY test.event_log_y2023m10
ADD CONSTRAINT event_log_y2023m10_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2023m11 event_log_y2023m11_pkey; Type: CONSTRAINT; Schema: test; Owner: postgres
--
ALTER TABLE ONLY test.event_log_y2023m11
ADD CONSTRAINT event_log_y2023m11_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2023m12 event_log_y2023m12_pkey; Type: CONSTRAINT; Schema: test; Owner: postgres
--
ALTER TABLE ONLY test.event_log_y2023m12
ADD CONSTRAINT event_log_y2023m12_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2024m01 event_log_y2024m01_pkey; Type: CONSTRAINT; Schema: test; Owner: postgres
--
ALTER TABLE ONLY test.event_log_y2024m01
ADD CONSTRAINT event_log_y2024m01_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2024m02 event_log_y2024m02_pkey; Type: CONSTRAINT; Schema: test; Owner: postgres
--
ALTER TABLE ONLY test.event_log_y2024m02
ADD CONSTRAINT event_log_y2024m02_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2024m03 event_log_y2024m03_pkey; Type: CONSTRAINT; Schema: test; Owner: postgres
--
ALTER TABLE ONLY test.event_log_y2024m03
ADD CONSTRAINT event_log_y2024m03_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2024m04 event_log_y2024m04_pkey; Type: CONSTRAINT; Schema: test; Owner: postgres
--
ALTER TABLE ONLY test.event_log_y2024m04
ADD CONSTRAINT event_log_y2024m04_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2024m05 event_log_y2024m05_pkey; Type: CONSTRAINT; Schema: test; Owner: postgres
--
ALTER TABLE ONLY test.event_log_y2024m05
ADD CONSTRAINT event_log_y2024m05_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2024m06 event_log_y2024m06_pkey; Type: CONSTRAINT; Schema: test; Owner: postgres
--
ALTER TABLE ONLY test.event_log_y2024m06
ADD CONSTRAINT event_log_y2024m06_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2024m07 event_log_y2024m07_pkey; Type: CONSTRAINT; Schema: test; Owner: postgres
--
ALTER TABLE ONLY test.event_log_y2024m07
ADD CONSTRAINT event_log_y2024m07_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2024m08 event_log_y2024m08_pkey; Type: CONSTRAINT; Schema: test; Owner: postgres
--
ALTER TABLE ONLY test.event_log_y2024m08
ADD CONSTRAINT event_log_y2024m08_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2024m09 event_log_y2024m09_pkey; Type: CONSTRAINT; Schema: test; Owner: postgres
--
ALTER TABLE ONLY test.event_log_y2024m09
ADD CONSTRAINT event_log_y2024m09_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2024m10 event_log_y2024m10_pkey; Type: CONSTRAINT; Schema: test; Owner: postgres
--
ALTER TABLE ONLY test.event_log_y2024m10
ADD CONSTRAINT event_log_y2024m10_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2024m11 event_log_y2024m11_pkey; Type: CONSTRAINT; Schema: test; Owner: postgres
--
ALTER TABLE ONLY test.event_log_y2024m11
ADD CONSTRAINT event_log_y2024m11_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2024m12 event_log_y2024m12_pkey; Type: CONSTRAINT; Schema: test; Owner: postgres
--
ALTER TABLE ONLY test.event_log_y2024m12
ADD CONSTRAINT event_log_y2024m12_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2025m01 event_log_y2025m01_pkey; Type: CONSTRAINT; Schema: test; Owner: postgres
--
ALTER TABLE ONLY test.event_log_y2025m01
ADD CONSTRAINT event_log_y2025m01_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2025m02 event_log_y2025m02_pkey; Type: CONSTRAINT; Schema: test; Owner: postgres
--
ALTER TABLE ONLY test.event_log_y2025m02
ADD CONSTRAINT event_log_y2025m02_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2025m03 event_log_y2025m03_pkey; Type: CONSTRAINT; Schema: test; Owner: postgres
--
ALTER TABLE ONLY test.event_log_y2025m03
ADD CONSTRAINT event_log_y2025m03_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2025m04 event_log_y2025m04_pkey; Type: CONSTRAINT; Schema: test; Owner: postgres
--
ALTER TABLE ONLY test.event_log_y2025m04
ADD CONSTRAINT event_log_y2025m04_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2025m05 event_log_y2025m05_pkey; Type: CONSTRAINT; Schema: test; Owner: postgres
--
ALTER TABLE ONLY test.event_log_y2025m05
ADD CONSTRAINT event_log_y2025m05_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2025m06 event_log_y2025m06_pkey; Type: CONSTRAINT; Schema: test; Owner: postgres
--
ALTER TABLE ONLY test.event_log_y2025m06
ADD CONSTRAINT event_log_y2025m06_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2025m07 event_log_y2025m07_pkey; Type: CONSTRAINT; Schema: test; Owner: postgres
--
ALTER TABLE ONLY test.event_log_y2025m07
ADD CONSTRAINT event_log_y2025m07_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2025m08 event_log_y2025m08_pkey; Type: CONSTRAINT; Schema: test; Owner: postgres
--
ALTER TABLE ONLY test.event_log_y2025m08
ADD CONSTRAINT event_log_y2025m08_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2025m09 event_log_y2025m09_pkey; Type: CONSTRAINT; Schema: test; Owner: postgres
--
ALTER TABLE ONLY test.event_log_y2025m09
ADD CONSTRAINT event_log_y2025m09_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2025m10 event_log_y2025m10_pkey; Type: CONSTRAINT; Schema: test; Owner: postgres
--
ALTER TABLE ONLY test.event_log_y2025m10
ADD CONSTRAINT event_log_y2025m10_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2025m11 event_log_y2025m11_pkey; Type: CONSTRAINT; Schema: test; Owner: postgres
--
ALTER TABLE ONLY test.event_log_y2025m11
ADD CONSTRAINT event_log_y2025m11_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2025m12 event_log_y2025m12_pkey; Type: CONSTRAINT; Schema: test; Owner: postgres
--
ALTER TABLE ONLY test.event_log_y2025m12
ADD CONSTRAINT event_log_y2025m12_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2026m01 event_log_y2026m01_pkey; Type: CONSTRAINT; Schema: test; Owner: postgres
--
ALTER TABLE ONLY test.event_log_y2026m01
ADD CONSTRAINT event_log_y2026m01_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2026m02 event_log_y2026m02_pkey; Type: CONSTRAINT; Schema: test; Owner: postgres
--
ALTER TABLE ONLY test.event_log_y2026m02
ADD CONSTRAINT event_log_y2026m02_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2026m03 event_log_y2026m03_pkey; Type: CONSTRAINT; Schema: test; Owner: postgres
--
ALTER TABLE ONLY test.event_log_y2026m03
ADD CONSTRAINT event_log_y2026m03_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2026m04 event_log_y2026m04_pkey; Type: CONSTRAINT; Schema: test; Owner: postgres
--
ALTER TABLE ONLY test.event_log_y2026m04
ADD CONSTRAINT event_log_y2026m04_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2026m05 event_log_y2026m05_pkey; Type: CONSTRAINT; Schema: test; Owner: postgres
--
ALTER TABLE ONLY test.event_log_y2026m05
ADD CONSTRAINT event_log_y2026m05_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2026m06 event_log_y2026m06_pkey; Type: CONSTRAINT; Schema: test; Owner: postgres
--
ALTER TABLE ONLY test.event_log_y2026m06
ADD CONSTRAINT event_log_y2026m06_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2026m07 event_log_y2026m07_pkey; Type: CONSTRAINT; Schema: test; Owner: postgres
--
ALTER TABLE ONLY test.event_log_y2026m07
ADD CONSTRAINT event_log_y2026m07_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2026m08 event_log_y2026m08_pkey; Type: CONSTRAINT; Schema: test; Owner: postgres
--
ALTER TABLE ONLY test.event_log_y2026m08
ADD CONSTRAINT event_log_y2026m08_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2026m09 event_log_y2026m09_pkey; Type: CONSTRAINT; Schema: test; Owner: postgres
--
ALTER TABLE ONLY test.event_log_y2026m09
ADD CONSTRAINT event_log_y2026m09_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2026m10 event_log_y2026m10_pkey; Type: CONSTRAINT; Schema: test; Owner: postgres
--
ALTER TABLE ONLY test.event_log_y2026m10
ADD CONSTRAINT event_log_y2026m10_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2026m11 event_log_y2026m11_pkey; Type: CONSTRAINT; Schema: test; Owner: postgres
--
ALTER TABLE ONLY test.event_log_y2026m11
ADD CONSTRAINT event_log_y2026m11_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: event_log_y2026m12 event_log_y2026m12_pkey; Type: CONSTRAINT; Schema: test; Owner: postgres
--
ALTER TABLE ONLY test.event_log_y2026m12
ADD CONSTRAINT event_log_y2026m12_pkey PRIMARY KEY (id, "timestamp");
--
-- Name: experiments experiments_pkey; Type: CONSTRAINT; Schema: test; Owner: postgres
--
ALTER TABLE ONLY test.experiments
ADD CONSTRAINT experiments_pkey PRIMARY KEY (id);
--
-- Name: functions functions_pkey; Type: CONSTRAINT; Schema: test; Owner: postgres
--
ALTER TABLE ONLY test.functions
ADD CONSTRAINT functions_pkey PRIMARY KEY (function_name);
--
-- Name: workspaces organisation_workspace_pkey; Type: CONSTRAINT; Schema: test; Owner: postgres
--
ALTER TABLE ONLY test.workspaces
ADD CONSTRAINT organisation_workspace_pkey PRIMARY KEY (organisation_id, workspace_name);
--
-- Name: type_templates type_templates_pkey; Type: CONSTRAINT; Schema: test; Owner: postgres
--
ALTER TABLE ONLY test.type_templates
ADD CONSTRAINT type_templates_pkey PRIMARY KEY (type_name);
--
-- Name: workspaces unique_workspace_schema_name; Type: CONSTRAINT; Schema: test; Owner: postgres
--
ALTER TABLE ONLY test.workspaces
ADD CONSTRAINT unique_workspace_schema_name UNIQUE (workspace_schema_name);
--
-- Name: config_verions_tags_index; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX config_verions_tags_index ON dev.config_versions USING gin (tags);
--
-- Name: config_versions_id_index; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX config_versions_id_index ON dev.config_versions USING btree (id);
--
-- Name: event_log_action_index; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_action_index ON ONLY dev.event_log USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_table_name_index; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_table_name_index ON ONLY dev.event_log USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_timestamp_index; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_timestamp_index ON ONLY dev.event_log USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2023m08_action_timestamp_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2023m08_action_timestamp_table_name_idx ON dev.event_log_y2023m08 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2023m08_table_name_action_timestamp_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2023m08_table_name_action_timestamp_idx ON dev.event_log_y2023m08 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2023m08_timestamp_action_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2023m08_timestamp_action_table_name_idx ON dev.event_log_y2023m08 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2023m09_action_timestamp_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2023m09_action_timestamp_table_name_idx ON dev.event_log_y2023m09 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2023m09_table_name_action_timestamp_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2023m09_table_name_action_timestamp_idx ON dev.event_log_y2023m09 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2023m09_timestamp_action_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2023m09_timestamp_action_table_name_idx ON dev.event_log_y2023m09 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2023m10_action_timestamp_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2023m10_action_timestamp_table_name_idx ON dev.event_log_y2023m10 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2023m10_table_name_action_timestamp_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2023m10_table_name_action_timestamp_idx ON dev.event_log_y2023m10 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2023m10_timestamp_action_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2023m10_timestamp_action_table_name_idx ON dev.event_log_y2023m10 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2023m11_action_timestamp_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2023m11_action_timestamp_table_name_idx ON dev.event_log_y2023m11 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2023m11_table_name_action_timestamp_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2023m11_table_name_action_timestamp_idx ON dev.event_log_y2023m11 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2023m11_timestamp_action_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2023m11_timestamp_action_table_name_idx ON dev.event_log_y2023m11 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2023m12_action_timestamp_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2023m12_action_timestamp_table_name_idx ON dev.event_log_y2023m12 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2023m12_table_name_action_timestamp_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2023m12_table_name_action_timestamp_idx ON dev.event_log_y2023m12 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2023m12_timestamp_action_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2023m12_timestamp_action_table_name_idx ON dev.event_log_y2023m12 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2024m01_action_timestamp_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2024m01_action_timestamp_table_name_idx ON dev.event_log_y2024m01 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2024m01_table_name_action_timestamp_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2024m01_table_name_action_timestamp_idx ON dev.event_log_y2024m01 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2024m01_timestamp_action_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2024m01_timestamp_action_table_name_idx ON dev.event_log_y2024m01 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2024m02_action_timestamp_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2024m02_action_timestamp_table_name_idx ON dev.event_log_y2024m02 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2024m02_table_name_action_timestamp_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2024m02_table_name_action_timestamp_idx ON dev.event_log_y2024m02 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2024m02_timestamp_action_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2024m02_timestamp_action_table_name_idx ON dev.event_log_y2024m02 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2024m03_action_timestamp_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2024m03_action_timestamp_table_name_idx ON dev.event_log_y2024m03 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2024m03_table_name_action_timestamp_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2024m03_table_name_action_timestamp_idx ON dev.event_log_y2024m03 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2024m03_timestamp_action_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2024m03_timestamp_action_table_name_idx ON dev.event_log_y2024m03 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2024m04_action_timestamp_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2024m04_action_timestamp_table_name_idx ON dev.event_log_y2024m04 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2024m04_table_name_action_timestamp_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2024m04_table_name_action_timestamp_idx ON dev.event_log_y2024m04 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2024m04_timestamp_action_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2024m04_timestamp_action_table_name_idx ON dev.event_log_y2024m04 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2024m05_action_timestamp_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2024m05_action_timestamp_table_name_idx ON dev.event_log_y2024m05 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2024m05_table_name_action_timestamp_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2024m05_table_name_action_timestamp_idx ON dev.event_log_y2024m05 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2024m05_timestamp_action_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2024m05_timestamp_action_table_name_idx ON dev.event_log_y2024m05 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2024m06_action_timestamp_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2024m06_action_timestamp_table_name_idx ON dev.event_log_y2024m06 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2024m06_table_name_action_timestamp_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2024m06_table_name_action_timestamp_idx ON dev.event_log_y2024m06 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2024m06_timestamp_action_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2024m06_timestamp_action_table_name_idx ON dev.event_log_y2024m06 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2024m07_action_timestamp_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2024m07_action_timestamp_table_name_idx ON dev.event_log_y2024m07 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2024m07_table_name_action_timestamp_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2024m07_table_name_action_timestamp_idx ON dev.event_log_y2024m07 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2024m07_timestamp_action_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2024m07_timestamp_action_table_name_idx ON dev.event_log_y2024m07 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2024m08_action_timestamp_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2024m08_action_timestamp_table_name_idx ON dev.event_log_y2024m08 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2024m08_table_name_action_timestamp_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2024m08_table_name_action_timestamp_idx ON dev.event_log_y2024m08 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2024m08_timestamp_action_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2024m08_timestamp_action_table_name_idx ON dev.event_log_y2024m08 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2024m09_action_timestamp_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2024m09_action_timestamp_table_name_idx ON dev.event_log_y2024m09 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2024m09_table_name_action_timestamp_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2024m09_table_name_action_timestamp_idx ON dev.event_log_y2024m09 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2024m09_timestamp_action_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2024m09_timestamp_action_table_name_idx ON dev.event_log_y2024m09 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2024m10_action_timestamp_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2024m10_action_timestamp_table_name_idx ON dev.event_log_y2024m10 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2024m10_table_name_action_timestamp_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2024m10_table_name_action_timestamp_idx ON dev.event_log_y2024m10 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2024m10_timestamp_action_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2024m10_timestamp_action_table_name_idx ON dev.event_log_y2024m10 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2024m11_action_timestamp_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2024m11_action_timestamp_table_name_idx ON dev.event_log_y2024m11 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2024m11_table_name_action_timestamp_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2024m11_table_name_action_timestamp_idx ON dev.event_log_y2024m11 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2024m11_timestamp_action_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2024m11_timestamp_action_table_name_idx ON dev.event_log_y2024m11 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2024m12_action_timestamp_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2024m12_action_timestamp_table_name_idx ON dev.event_log_y2024m12 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2024m12_table_name_action_timestamp_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2024m12_table_name_action_timestamp_idx ON dev.event_log_y2024m12 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2024m12_timestamp_action_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2024m12_timestamp_action_table_name_idx ON dev.event_log_y2024m12 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2025m01_action_timestamp_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2025m01_action_timestamp_table_name_idx ON dev.event_log_y2025m01 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2025m01_table_name_action_timestamp_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2025m01_table_name_action_timestamp_idx ON dev.event_log_y2025m01 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2025m01_timestamp_action_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2025m01_timestamp_action_table_name_idx ON dev.event_log_y2025m01 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2025m02_action_timestamp_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2025m02_action_timestamp_table_name_idx ON dev.event_log_y2025m02 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2025m02_table_name_action_timestamp_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2025m02_table_name_action_timestamp_idx ON dev.event_log_y2025m02 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2025m02_timestamp_action_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2025m02_timestamp_action_table_name_idx ON dev.event_log_y2025m02 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2025m03_action_timestamp_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2025m03_action_timestamp_table_name_idx ON dev.event_log_y2025m03 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2025m03_table_name_action_timestamp_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2025m03_table_name_action_timestamp_idx ON dev.event_log_y2025m03 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2025m03_timestamp_action_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2025m03_timestamp_action_table_name_idx ON dev.event_log_y2025m03 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2025m04_action_timestamp_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2025m04_action_timestamp_table_name_idx ON dev.event_log_y2025m04 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2025m04_table_name_action_timestamp_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2025m04_table_name_action_timestamp_idx ON dev.event_log_y2025m04 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2025m04_timestamp_action_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2025m04_timestamp_action_table_name_idx ON dev.event_log_y2025m04 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2025m05_action_timestamp_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2025m05_action_timestamp_table_name_idx ON dev.event_log_y2025m05 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2025m05_table_name_action_timestamp_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2025m05_table_name_action_timestamp_idx ON dev.event_log_y2025m05 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2025m05_timestamp_action_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2025m05_timestamp_action_table_name_idx ON dev.event_log_y2025m05 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2025m06_action_timestamp_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2025m06_action_timestamp_table_name_idx ON dev.event_log_y2025m06 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2025m06_table_name_action_timestamp_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2025m06_table_name_action_timestamp_idx ON dev.event_log_y2025m06 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2025m06_timestamp_action_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2025m06_timestamp_action_table_name_idx ON dev.event_log_y2025m06 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2025m07_action_timestamp_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2025m07_action_timestamp_table_name_idx ON dev.event_log_y2025m07 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2025m07_table_name_action_timestamp_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2025m07_table_name_action_timestamp_idx ON dev.event_log_y2025m07 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2025m07_timestamp_action_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2025m07_timestamp_action_table_name_idx ON dev.event_log_y2025m07 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2025m08_action_timestamp_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2025m08_action_timestamp_table_name_idx ON dev.event_log_y2025m08 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2025m08_table_name_action_timestamp_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2025m08_table_name_action_timestamp_idx ON dev.event_log_y2025m08 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2025m08_timestamp_action_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2025m08_timestamp_action_table_name_idx ON dev.event_log_y2025m08 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2025m09_action_timestamp_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2025m09_action_timestamp_table_name_idx ON dev.event_log_y2025m09 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2025m09_table_name_action_timestamp_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2025m09_table_name_action_timestamp_idx ON dev.event_log_y2025m09 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2025m09_timestamp_action_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2025m09_timestamp_action_table_name_idx ON dev.event_log_y2025m09 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2025m10_action_timestamp_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2025m10_action_timestamp_table_name_idx ON dev.event_log_y2025m10 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2025m10_table_name_action_timestamp_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2025m10_table_name_action_timestamp_idx ON dev.event_log_y2025m10 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2025m10_timestamp_action_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2025m10_timestamp_action_table_name_idx ON dev.event_log_y2025m10 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2025m11_action_timestamp_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2025m11_action_timestamp_table_name_idx ON dev.event_log_y2025m11 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2025m11_table_name_action_timestamp_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2025m11_table_name_action_timestamp_idx ON dev.event_log_y2025m11 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2025m11_timestamp_action_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2025m11_timestamp_action_table_name_idx ON dev.event_log_y2025m11 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2025m12_action_timestamp_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2025m12_action_timestamp_table_name_idx ON dev.event_log_y2025m12 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2025m12_table_name_action_timestamp_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2025m12_table_name_action_timestamp_idx ON dev.event_log_y2025m12 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2025m12_timestamp_action_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2025m12_timestamp_action_table_name_idx ON dev.event_log_y2025m12 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2026m01_action_timestamp_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2026m01_action_timestamp_table_name_idx ON dev.event_log_y2026m01 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2026m01_table_name_action_timestamp_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2026m01_table_name_action_timestamp_idx ON dev.event_log_y2026m01 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2026m01_timestamp_action_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2026m01_timestamp_action_table_name_idx ON dev.event_log_y2026m01 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2026m02_action_timestamp_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2026m02_action_timestamp_table_name_idx ON dev.event_log_y2026m02 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2026m02_table_name_action_timestamp_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2026m02_table_name_action_timestamp_idx ON dev.event_log_y2026m02 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2026m02_timestamp_action_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2026m02_timestamp_action_table_name_idx ON dev.event_log_y2026m02 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2026m03_action_timestamp_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2026m03_action_timestamp_table_name_idx ON dev.event_log_y2026m03 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2026m03_table_name_action_timestamp_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2026m03_table_name_action_timestamp_idx ON dev.event_log_y2026m03 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2026m03_timestamp_action_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2026m03_timestamp_action_table_name_idx ON dev.event_log_y2026m03 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2026m04_action_timestamp_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2026m04_action_timestamp_table_name_idx ON dev.event_log_y2026m04 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2026m04_table_name_action_timestamp_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2026m04_table_name_action_timestamp_idx ON dev.event_log_y2026m04 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2026m04_timestamp_action_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2026m04_timestamp_action_table_name_idx ON dev.event_log_y2026m04 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2026m05_action_timestamp_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2026m05_action_timestamp_table_name_idx ON dev.event_log_y2026m05 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2026m05_table_name_action_timestamp_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2026m05_table_name_action_timestamp_idx ON dev.event_log_y2026m05 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2026m05_timestamp_action_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2026m05_timestamp_action_table_name_idx ON dev.event_log_y2026m05 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2026m06_action_timestamp_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2026m06_action_timestamp_table_name_idx ON dev.event_log_y2026m06 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2026m06_table_name_action_timestamp_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2026m06_table_name_action_timestamp_idx ON dev.event_log_y2026m06 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2026m06_timestamp_action_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2026m06_timestamp_action_table_name_idx ON dev.event_log_y2026m06 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2026m07_action_timestamp_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2026m07_action_timestamp_table_name_idx ON dev.event_log_y2026m07 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2026m07_table_name_action_timestamp_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2026m07_table_name_action_timestamp_idx ON dev.event_log_y2026m07 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2026m07_timestamp_action_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2026m07_timestamp_action_table_name_idx ON dev.event_log_y2026m07 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2026m08_action_timestamp_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2026m08_action_timestamp_table_name_idx ON dev.event_log_y2026m08 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2026m08_table_name_action_timestamp_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2026m08_table_name_action_timestamp_idx ON dev.event_log_y2026m08 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2026m08_timestamp_action_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2026m08_timestamp_action_table_name_idx ON dev.event_log_y2026m08 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2026m09_action_timestamp_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2026m09_action_timestamp_table_name_idx ON dev.event_log_y2026m09 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2026m09_table_name_action_timestamp_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2026m09_table_name_action_timestamp_idx ON dev.event_log_y2026m09 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2026m09_timestamp_action_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2026m09_timestamp_action_table_name_idx ON dev.event_log_y2026m09 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2026m10_action_timestamp_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2026m10_action_timestamp_table_name_idx ON dev.event_log_y2026m10 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2026m10_table_name_action_timestamp_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2026m10_table_name_action_timestamp_idx ON dev.event_log_y2026m10 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2026m10_timestamp_action_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2026m10_timestamp_action_table_name_idx ON dev.event_log_y2026m10 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2026m11_action_timestamp_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2026m11_action_timestamp_table_name_idx ON dev.event_log_y2026m11 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2026m11_table_name_action_timestamp_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2026m11_table_name_action_timestamp_idx ON dev.event_log_y2026m11 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2026m11_timestamp_action_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2026m11_timestamp_action_table_name_idx ON dev.event_log_y2026m11 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2026m12_action_timestamp_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2026m12_action_timestamp_table_name_idx ON dev.event_log_y2026m12 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2026m12_table_name_action_timestamp_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2026m12_table_name_action_timestamp_idx ON dev.event_log_y2026m12 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2026m12_timestamp_action_table_name_idx; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX event_log_y2026m12_timestamp_action_table_name_idx ON dev.event_log_y2026m12 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: experiment_created_date_index; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX experiment_created_date_index ON dev.experiments USING btree (created_at) INCLUDE (id);
--
-- Name: experiment_last_modified_index; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX experiment_last_modified_index ON dev.experiments USING btree (last_modified) INCLUDE (id, created_at);
--
-- Name: experiment_status_index; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX experiment_status_index ON dev.experiments USING btree (status) INCLUDE (created_at, last_modified);
--
-- Name: idx_contexts_weight; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX idx_contexts_weight ON dev.contexts USING btree (weight);
--
-- Name: idx_last_modified_created_by; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX idx_last_modified_created_by ON dev.workspaces USING btree (last_modified_by, created_by);
--
-- Name: idx_workspace_name; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX idx_workspace_name ON dev.workspaces USING btree (workspace_name);
--
-- Name: type_templates_created_at_index; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX type_templates_created_at_index ON dev.type_templates USING btree (created_at);
--
-- Name: type_templates_index; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX type_templates_index ON dev.type_templates USING btree (type_name);
--
-- Name: type_templates_last_modifed_index; Type: INDEX; Schema: dev; Owner: postgres
--
CREATE INDEX type_templates_last_modifed_index ON dev.type_templates USING btree (last_modified_at);
--
-- Name: config_verions_tags_index; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX config_verions_tags_index ON test.config_versions USING gin (tags);
--
-- Name: config_versions_id_index; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX config_versions_id_index ON test.config_versions USING btree (id);
--
-- Name: event_log_action_index; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_action_index ON ONLY test.event_log USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_table_name_index; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_table_name_index ON ONLY test.event_log USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_timestamp_index; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_timestamp_index ON ONLY test.event_log USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2023m08_action_timestamp_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2023m08_action_timestamp_table_name_idx ON test.event_log_y2023m08 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2023m08_table_name_action_timestamp_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2023m08_table_name_action_timestamp_idx ON test.event_log_y2023m08 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2023m08_timestamp_action_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2023m08_timestamp_action_table_name_idx ON test.event_log_y2023m08 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2023m09_action_timestamp_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2023m09_action_timestamp_table_name_idx ON test.event_log_y2023m09 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2023m09_table_name_action_timestamp_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2023m09_table_name_action_timestamp_idx ON test.event_log_y2023m09 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2023m09_timestamp_action_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2023m09_timestamp_action_table_name_idx ON test.event_log_y2023m09 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2023m10_action_timestamp_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2023m10_action_timestamp_table_name_idx ON test.event_log_y2023m10 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2023m10_table_name_action_timestamp_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2023m10_table_name_action_timestamp_idx ON test.event_log_y2023m10 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2023m10_timestamp_action_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2023m10_timestamp_action_table_name_idx ON test.event_log_y2023m10 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2023m11_action_timestamp_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2023m11_action_timestamp_table_name_idx ON test.event_log_y2023m11 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2023m11_table_name_action_timestamp_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2023m11_table_name_action_timestamp_idx ON test.event_log_y2023m11 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2023m11_timestamp_action_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2023m11_timestamp_action_table_name_idx ON test.event_log_y2023m11 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2023m12_action_timestamp_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2023m12_action_timestamp_table_name_idx ON test.event_log_y2023m12 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2023m12_table_name_action_timestamp_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2023m12_table_name_action_timestamp_idx ON test.event_log_y2023m12 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2023m12_timestamp_action_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2023m12_timestamp_action_table_name_idx ON test.event_log_y2023m12 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2024m01_action_timestamp_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2024m01_action_timestamp_table_name_idx ON test.event_log_y2024m01 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2024m01_table_name_action_timestamp_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2024m01_table_name_action_timestamp_idx ON test.event_log_y2024m01 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2024m01_timestamp_action_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2024m01_timestamp_action_table_name_idx ON test.event_log_y2024m01 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2024m02_action_timestamp_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2024m02_action_timestamp_table_name_idx ON test.event_log_y2024m02 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2024m02_table_name_action_timestamp_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2024m02_table_name_action_timestamp_idx ON test.event_log_y2024m02 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2024m02_timestamp_action_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2024m02_timestamp_action_table_name_idx ON test.event_log_y2024m02 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2024m03_action_timestamp_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2024m03_action_timestamp_table_name_idx ON test.event_log_y2024m03 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2024m03_table_name_action_timestamp_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2024m03_table_name_action_timestamp_idx ON test.event_log_y2024m03 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2024m03_timestamp_action_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2024m03_timestamp_action_table_name_idx ON test.event_log_y2024m03 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2024m04_action_timestamp_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2024m04_action_timestamp_table_name_idx ON test.event_log_y2024m04 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2024m04_table_name_action_timestamp_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2024m04_table_name_action_timestamp_idx ON test.event_log_y2024m04 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2024m04_timestamp_action_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2024m04_timestamp_action_table_name_idx ON test.event_log_y2024m04 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2024m05_action_timestamp_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2024m05_action_timestamp_table_name_idx ON test.event_log_y2024m05 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2024m05_table_name_action_timestamp_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2024m05_table_name_action_timestamp_idx ON test.event_log_y2024m05 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2024m05_timestamp_action_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2024m05_timestamp_action_table_name_idx ON test.event_log_y2024m05 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2024m06_action_timestamp_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2024m06_action_timestamp_table_name_idx ON test.event_log_y2024m06 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2024m06_table_name_action_timestamp_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2024m06_table_name_action_timestamp_idx ON test.event_log_y2024m06 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2024m06_timestamp_action_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2024m06_timestamp_action_table_name_idx ON test.event_log_y2024m06 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2024m07_action_timestamp_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2024m07_action_timestamp_table_name_idx ON test.event_log_y2024m07 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2024m07_table_name_action_timestamp_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2024m07_table_name_action_timestamp_idx ON test.event_log_y2024m07 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2024m07_timestamp_action_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2024m07_timestamp_action_table_name_idx ON test.event_log_y2024m07 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2024m08_action_timestamp_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2024m08_action_timestamp_table_name_idx ON test.event_log_y2024m08 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2024m08_table_name_action_timestamp_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2024m08_table_name_action_timestamp_idx ON test.event_log_y2024m08 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2024m08_timestamp_action_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2024m08_timestamp_action_table_name_idx ON test.event_log_y2024m08 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2024m09_action_timestamp_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2024m09_action_timestamp_table_name_idx ON test.event_log_y2024m09 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2024m09_table_name_action_timestamp_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2024m09_table_name_action_timestamp_idx ON test.event_log_y2024m09 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2024m09_timestamp_action_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2024m09_timestamp_action_table_name_idx ON test.event_log_y2024m09 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2024m10_action_timestamp_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2024m10_action_timestamp_table_name_idx ON test.event_log_y2024m10 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2024m10_table_name_action_timestamp_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2024m10_table_name_action_timestamp_idx ON test.event_log_y2024m10 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2024m10_timestamp_action_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2024m10_timestamp_action_table_name_idx ON test.event_log_y2024m10 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2024m11_action_timestamp_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2024m11_action_timestamp_table_name_idx ON test.event_log_y2024m11 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2024m11_table_name_action_timestamp_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2024m11_table_name_action_timestamp_idx ON test.event_log_y2024m11 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2024m11_timestamp_action_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2024m11_timestamp_action_table_name_idx ON test.event_log_y2024m11 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2024m12_action_timestamp_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2024m12_action_timestamp_table_name_idx ON test.event_log_y2024m12 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2024m12_table_name_action_timestamp_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2024m12_table_name_action_timestamp_idx ON test.event_log_y2024m12 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2024m12_timestamp_action_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2024m12_timestamp_action_table_name_idx ON test.event_log_y2024m12 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2025m01_action_timestamp_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2025m01_action_timestamp_table_name_idx ON test.event_log_y2025m01 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2025m01_table_name_action_timestamp_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2025m01_table_name_action_timestamp_idx ON test.event_log_y2025m01 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2025m01_timestamp_action_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2025m01_timestamp_action_table_name_idx ON test.event_log_y2025m01 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2025m02_action_timestamp_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2025m02_action_timestamp_table_name_idx ON test.event_log_y2025m02 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2025m02_table_name_action_timestamp_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2025m02_table_name_action_timestamp_idx ON test.event_log_y2025m02 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2025m02_timestamp_action_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2025m02_timestamp_action_table_name_idx ON test.event_log_y2025m02 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2025m03_action_timestamp_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2025m03_action_timestamp_table_name_idx ON test.event_log_y2025m03 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2025m03_table_name_action_timestamp_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2025m03_table_name_action_timestamp_idx ON test.event_log_y2025m03 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2025m03_timestamp_action_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2025m03_timestamp_action_table_name_idx ON test.event_log_y2025m03 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2025m04_action_timestamp_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2025m04_action_timestamp_table_name_idx ON test.event_log_y2025m04 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2025m04_table_name_action_timestamp_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2025m04_table_name_action_timestamp_idx ON test.event_log_y2025m04 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2025m04_timestamp_action_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2025m04_timestamp_action_table_name_idx ON test.event_log_y2025m04 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2025m05_action_timestamp_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2025m05_action_timestamp_table_name_idx ON test.event_log_y2025m05 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2025m05_table_name_action_timestamp_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2025m05_table_name_action_timestamp_idx ON test.event_log_y2025m05 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2025m05_timestamp_action_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2025m05_timestamp_action_table_name_idx ON test.event_log_y2025m05 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2025m06_action_timestamp_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2025m06_action_timestamp_table_name_idx ON test.event_log_y2025m06 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2025m06_table_name_action_timestamp_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2025m06_table_name_action_timestamp_idx ON test.event_log_y2025m06 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2025m06_timestamp_action_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2025m06_timestamp_action_table_name_idx ON test.event_log_y2025m06 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2025m07_action_timestamp_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2025m07_action_timestamp_table_name_idx ON test.event_log_y2025m07 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2025m07_table_name_action_timestamp_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2025m07_table_name_action_timestamp_idx ON test.event_log_y2025m07 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2025m07_timestamp_action_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2025m07_timestamp_action_table_name_idx ON test.event_log_y2025m07 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2025m08_action_timestamp_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2025m08_action_timestamp_table_name_idx ON test.event_log_y2025m08 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2025m08_table_name_action_timestamp_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2025m08_table_name_action_timestamp_idx ON test.event_log_y2025m08 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2025m08_timestamp_action_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2025m08_timestamp_action_table_name_idx ON test.event_log_y2025m08 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2025m09_action_timestamp_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2025m09_action_timestamp_table_name_idx ON test.event_log_y2025m09 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2025m09_table_name_action_timestamp_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2025m09_table_name_action_timestamp_idx ON test.event_log_y2025m09 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2025m09_timestamp_action_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2025m09_timestamp_action_table_name_idx ON test.event_log_y2025m09 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2025m10_action_timestamp_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2025m10_action_timestamp_table_name_idx ON test.event_log_y2025m10 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2025m10_table_name_action_timestamp_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2025m10_table_name_action_timestamp_idx ON test.event_log_y2025m10 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2025m10_timestamp_action_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2025m10_timestamp_action_table_name_idx ON test.event_log_y2025m10 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2025m11_action_timestamp_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2025m11_action_timestamp_table_name_idx ON test.event_log_y2025m11 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2025m11_table_name_action_timestamp_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2025m11_table_name_action_timestamp_idx ON test.event_log_y2025m11 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2025m11_timestamp_action_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2025m11_timestamp_action_table_name_idx ON test.event_log_y2025m11 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2025m12_action_timestamp_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2025m12_action_timestamp_table_name_idx ON test.event_log_y2025m12 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2025m12_table_name_action_timestamp_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2025m12_table_name_action_timestamp_idx ON test.event_log_y2025m12 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2025m12_timestamp_action_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2025m12_timestamp_action_table_name_idx ON test.event_log_y2025m12 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2026m01_action_timestamp_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2026m01_action_timestamp_table_name_idx ON test.event_log_y2026m01 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2026m01_table_name_action_timestamp_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2026m01_table_name_action_timestamp_idx ON test.event_log_y2026m01 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2026m01_timestamp_action_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2026m01_timestamp_action_table_name_idx ON test.event_log_y2026m01 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2026m02_action_timestamp_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2026m02_action_timestamp_table_name_idx ON test.event_log_y2026m02 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2026m02_table_name_action_timestamp_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2026m02_table_name_action_timestamp_idx ON test.event_log_y2026m02 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2026m02_timestamp_action_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2026m02_timestamp_action_table_name_idx ON test.event_log_y2026m02 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2026m03_action_timestamp_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2026m03_action_timestamp_table_name_idx ON test.event_log_y2026m03 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2026m03_table_name_action_timestamp_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2026m03_table_name_action_timestamp_idx ON test.event_log_y2026m03 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2026m03_timestamp_action_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2026m03_timestamp_action_table_name_idx ON test.event_log_y2026m03 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2026m04_action_timestamp_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2026m04_action_timestamp_table_name_idx ON test.event_log_y2026m04 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2026m04_table_name_action_timestamp_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2026m04_table_name_action_timestamp_idx ON test.event_log_y2026m04 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2026m04_timestamp_action_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2026m04_timestamp_action_table_name_idx ON test.event_log_y2026m04 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2026m05_action_timestamp_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2026m05_action_timestamp_table_name_idx ON test.event_log_y2026m05 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2026m05_table_name_action_timestamp_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2026m05_table_name_action_timestamp_idx ON test.event_log_y2026m05 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2026m05_timestamp_action_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2026m05_timestamp_action_table_name_idx ON test.event_log_y2026m05 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2026m06_action_timestamp_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2026m06_action_timestamp_table_name_idx ON test.event_log_y2026m06 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2026m06_table_name_action_timestamp_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2026m06_table_name_action_timestamp_idx ON test.event_log_y2026m06 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2026m06_timestamp_action_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2026m06_timestamp_action_table_name_idx ON test.event_log_y2026m06 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2026m07_action_timestamp_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2026m07_action_timestamp_table_name_idx ON test.event_log_y2026m07 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2026m07_table_name_action_timestamp_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2026m07_table_name_action_timestamp_idx ON test.event_log_y2026m07 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2026m07_timestamp_action_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2026m07_timestamp_action_table_name_idx ON test.event_log_y2026m07 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2026m08_action_timestamp_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2026m08_action_timestamp_table_name_idx ON test.event_log_y2026m08 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2026m08_table_name_action_timestamp_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2026m08_table_name_action_timestamp_idx ON test.event_log_y2026m08 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2026m08_timestamp_action_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2026m08_timestamp_action_table_name_idx ON test.event_log_y2026m08 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2026m09_action_timestamp_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2026m09_action_timestamp_table_name_idx ON test.event_log_y2026m09 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2026m09_table_name_action_timestamp_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2026m09_table_name_action_timestamp_idx ON test.event_log_y2026m09 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2026m09_timestamp_action_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2026m09_timestamp_action_table_name_idx ON test.event_log_y2026m09 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2026m10_action_timestamp_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2026m10_action_timestamp_table_name_idx ON test.event_log_y2026m10 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2026m10_table_name_action_timestamp_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2026m10_table_name_action_timestamp_idx ON test.event_log_y2026m10 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2026m10_timestamp_action_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2026m10_timestamp_action_table_name_idx ON test.event_log_y2026m10 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2026m11_action_timestamp_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2026m11_action_timestamp_table_name_idx ON test.event_log_y2026m11 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2026m11_table_name_action_timestamp_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2026m11_table_name_action_timestamp_idx ON test.event_log_y2026m11 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2026m11_timestamp_action_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2026m11_timestamp_action_table_name_idx ON test.event_log_y2026m11 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: event_log_y2026m12_action_timestamp_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2026m12_action_timestamp_table_name_idx ON test.event_log_y2026m12 USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_y2026m12_table_name_action_timestamp_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2026m12_table_name_action_timestamp_idx ON test.event_log_y2026m12 USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_y2026m12_timestamp_action_table_name_idx; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX event_log_y2026m12_timestamp_action_table_name_idx ON test.event_log_y2026m12 USING btree ("timestamp") INCLUDE (action, table_name);
--
-- Name: experiment_created_date_index; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX experiment_created_date_index ON test.experiments USING btree (created_at) INCLUDE (id);
--
-- Name: experiment_last_modified_index; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX experiment_last_modified_index ON test.experiments USING btree (last_modified) INCLUDE (id, created_at);
--
-- Name: experiment_status_index; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX experiment_status_index ON test.experiments USING btree (status) INCLUDE (created_at, last_modified);
--
-- Name: idx_contexts_weight; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX idx_contexts_weight ON test.contexts USING btree (weight);
--
-- Name: idx_last_modified_created_by; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX idx_last_modified_created_by ON test.workspaces USING btree (last_modified_by, created_by);
--
-- Name: idx_workspace_name; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX idx_workspace_name ON test.workspaces USING btree (workspace_name);
--
-- Name: type_templates_created_at_index; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX type_templates_created_at_index ON test.type_templates USING btree (created_at);
--
-- Name: type_templates_index; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX type_templates_index ON test.type_templates USING btree (type_name);
--
-- Name: type_templates_last_modifed_index; Type: INDEX; Schema: test; Owner: postgres
--
CREATE INDEX type_templates_last_modifed_index ON test.type_templates USING btree (last_modified_at);
--
-- Name: event_log_y2023m08_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_action_index ATTACH PARTITION dev.event_log_y2023m08_action_timestamp_table_name_idx;
--
-- Name: event_log_y2023m08_pkey; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_pkey ATTACH PARTITION dev.event_log_y2023m08_pkey;
--
-- Name: event_log_y2023m08_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_table_name_index ATTACH PARTITION dev.event_log_y2023m08_table_name_action_timestamp_idx;
--
-- Name: event_log_y2023m08_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_timestamp_index ATTACH PARTITION dev.event_log_y2023m08_timestamp_action_table_name_idx;
--
-- Name: event_log_y2023m09_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_action_index ATTACH PARTITION dev.event_log_y2023m09_action_timestamp_table_name_idx;
--
-- Name: event_log_y2023m09_pkey; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_pkey ATTACH PARTITION dev.event_log_y2023m09_pkey;
--
-- Name: event_log_y2023m09_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_table_name_index ATTACH PARTITION dev.event_log_y2023m09_table_name_action_timestamp_idx;
--
-- Name: event_log_y2023m09_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_timestamp_index ATTACH PARTITION dev.event_log_y2023m09_timestamp_action_table_name_idx;
--
-- Name: event_log_y2023m10_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_action_index ATTACH PARTITION dev.event_log_y2023m10_action_timestamp_table_name_idx;
--
-- Name: event_log_y2023m10_pkey; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_pkey ATTACH PARTITION dev.event_log_y2023m10_pkey;
--
-- Name: event_log_y2023m10_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_table_name_index ATTACH PARTITION dev.event_log_y2023m10_table_name_action_timestamp_idx;
--
-- Name: event_log_y2023m10_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_timestamp_index ATTACH PARTITION dev.event_log_y2023m10_timestamp_action_table_name_idx;
--
-- Name: event_log_y2023m11_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_action_index ATTACH PARTITION dev.event_log_y2023m11_action_timestamp_table_name_idx;
--
-- Name: event_log_y2023m11_pkey; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_pkey ATTACH PARTITION dev.event_log_y2023m11_pkey;
--
-- Name: event_log_y2023m11_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_table_name_index ATTACH PARTITION dev.event_log_y2023m11_table_name_action_timestamp_idx;
--
-- Name: event_log_y2023m11_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_timestamp_index ATTACH PARTITION dev.event_log_y2023m11_timestamp_action_table_name_idx;
--
-- Name: event_log_y2023m12_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_action_index ATTACH PARTITION dev.event_log_y2023m12_action_timestamp_table_name_idx;
--
-- Name: event_log_y2023m12_pkey; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_pkey ATTACH PARTITION dev.event_log_y2023m12_pkey;
--
-- Name: event_log_y2023m12_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_table_name_index ATTACH PARTITION dev.event_log_y2023m12_table_name_action_timestamp_idx;
--
-- Name: event_log_y2023m12_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_timestamp_index ATTACH PARTITION dev.event_log_y2023m12_timestamp_action_table_name_idx;
--
-- Name: event_log_y2024m01_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_action_index ATTACH PARTITION dev.event_log_y2024m01_action_timestamp_table_name_idx;
--
-- Name: event_log_y2024m01_pkey; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_pkey ATTACH PARTITION dev.event_log_y2024m01_pkey;
--
-- Name: event_log_y2024m01_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_table_name_index ATTACH PARTITION dev.event_log_y2024m01_table_name_action_timestamp_idx;
--
-- Name: event_log_y2024m01_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_timestamp_index ATTACH PARTITION dev.event_log_y2024m01_timestamp_action_table_name_idx;
--
-- Name: event_log_y2024m02_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_action_index ATTACH PARTITION dev.event_log_y2024m02_action_timestamp_table_name_idx;
--
-- Name: event_log_y2024m02_pkey; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_pkey ATTACH PARTITION dev.event_log_y2024m02_pkey;
--
-- Name: event_log_y2024m02_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_table_name_index ATTACH PARTITION dev.event_log_y2024m02_table_name_action_timestamp_idx;
--
-- Name: event_log_y2024m02_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_timestamp_index ATTACH PARTITION dev.event_log_y2024m02_timestamp_action_table_name_idx;
--
-- Name: event_log_y2024m03_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_action_index ATTACH PARTITION dev.event_log_y2024m03_action_timestamp_table_name_idx;
--
-- Name: event_log_y2024m03_pkey; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_pkey ATTACH PARTITION dev.event_log_y2024m03_pkey;
--
-- Name: event_log_y2024m03_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_table_name_index ATTACH PARTITION dev.event_log_y2024m03_table_name_action_timestamp_idx;
--
-- Name: event_log_y2024m03_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_timestamp_index ATTACH PARTITION dev.event_log_y2024m03_timestamp_action_table_name_idx;
--
-- Name: event_log_y2024m04_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_action_index ATTACH PARTITION dev.event_log_y2024m04_action_timestamp_table_name_idx;
--
-- Name: event_log_y2024m04_pkey; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_pkey ATTACH PARTITION dev.event_log_y2024m04_pkey;
--
-- Name: event_log_y2024m04_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_table_name_index ATTACH PARTITION dev.event_log_y2024m04_table_name_action_timestamp_idx;
--
-- Name: event_log_y2024m04_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_timestamp_index ATTACH PARTITION dev.event_log_y2024m04_timestamp_action_table_name_idx;
--
-- Name: event_log_y2024m05_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_action_index ATTACH PARTITION dev.event_log_y2024m05_action_timestamp_table_name_idx;
--
-- Name: event_log_y2024m05_pkey; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_pkey ATTACH PARTITION dev.event_log_y2024m05_pkey;
--
-- Name: event_log_y2024m05_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_table_name_index ATTACH PARTITION dev.event_log_y2024m05_table_name_action_timestamp_idx;
--
-- Name: event_log_y2024m05_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_timestamp_index ATTACH PARTITION dev.event_log_y2024m05_timestamp_action_table_name_idx;
--
-- Name: event_log_y2024m06_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_action_index ATTACH PARTITION dev.event_log_y2024m06_action_timestamp_table_name_idx;
--
-- Name: event_log_y2024m06_pkey; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_pkey ATTACH PARTITION dev.event_log_y2024m06_pkey;
--
-- Name: event_log_y2024m06_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_table_name_index ATTACH PARTITION dev.event_log_y2024m06_table_name_action_timestamp_idx;
--
-- Name: event_log_y2024m06_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_timestamp_index ATTACH PARTITION dev.event_log_y2024m06_timestamp_action_table_name_idx;
--
-- Name: event_log_y2024m07_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_action_index ATTACH PARTITION dev.event_log_y2024m07_action_timestamp_table_name_idx;
--
-- Name: event_log_y2024m07_pkey; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_pkey ATTACH PARTITION dev.event_log_y2024m07_pkey;
--
-- Name: event_log_y2024m07_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_table_name_index ATTACH PARTITION dev.event_log_y2024m07_table_name_action_timestamp_idx;
--
-- Name: event_log_y2024m07_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_timestamp_index ATTACH PARTITION dev.event_log_y2024m07_timestamp_action_table_name_idx;
--
-- Name: event_log_y2024m08_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_action_index ATTACH PARTITION dev.event_log_y2024m08_action_timestamp_table_name_idx;
--
-- Name: event_log_y2024m08_pkey; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_pkey ATTACH PARTITION dev.event_log_y2024m08_pkey;
--
-- Name: event_log_y2024m08_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_table_name_index ATTACH PARTITION dev.event_log_y2024m08_table_name_action_timestamp_idx;
--
-- Name: event_log_y2024m08_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_timestamp_index ATTACH PARTITION dev.event_log_y2024m08_timestamp_action_table_name_idx;
--
-- Name: event_log_y2024m09_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_action_index ATTACH PARTITION dev.event_log_y2024m09_action_timestamp_table_name_idx;
--
-- Name: event_log_y2024m09_pkey; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_pkey ATTACH PARTITION dev.event_log_y2024m09_pkey;
--
-- Name: event_log_y2024m09_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_table_name_index ATTACH PARTITION dev.event_log_y2024m09_table_name_action_timestamp_idx;
--
-- Name: event_log_y2024m09_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_timestamp_index ATTACH PARTITION dev.event_log_y2024m09_timestamp_action_table_name_idx;
--
-- Name: event_log_y2024m10_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_action_index ATTACH PARTITION dev.event_log_y2024m10_action_timestamp_table_name_idx;
--
-- Name: event_log_y2024m10_pkey; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_pkey ATTACH PARTITION dev.event_log_y2024m10_pkey;
--
-- Name: event_log_y2024m10_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_table_name_index ATTACH PARTITION dev.event_log_y2024m10_table_name_action_timestamp_idx;
--
-- Name: event_log_y2024m10_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_timestamp_index ATTACH PARTITION dev.event_log_y2024m10_timestamp_action_table_name_idx;
--
-- Name: event_log_y2024m11_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_action_index ATTACH PARTITION dev.event_log_y2024m11_action_timestamp_table_name_idx;
--
-- Name: event_log_y2024m11_pkey; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_pkey ATTACH PARTITION dev.event_log_y2024m11_pkey;
--
-- Name: event_log_y2024m11_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_table_name_index ATTACH PARTITION dev.event_log_y2024m11_table_name_action_timestamp_idx;
--
-- Name: event_log_y2024m11_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_timestamp_index ATTACH PARTITION dev.event_log_y2024m11_timestamp_action_table_name_idx;
--
-- Name: event_log_y2024m12_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_action_index ATTACH PARTITION dev.event_log_y2024m12_action_timestamp_table_name_idx;
--
-- Name: event_log_y2024m12_pkey; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_pkey ATTACH PARTITION dev.event_log_y2024m12_pkey;
--
-- Name: event_log_y2024m12_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_table_name_index ATTACH PARTITION dev.event_log_y2024m12_table_name_action_timestamp_idx;
--
-- Name: event_log_y2024m12_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_timestamp_index ATTACH PARTITION dev.event_log_y2024m12_timestamp_action_table_name_idx;
--
-- Name: event_log_y2025m01_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_action_index ATTACH PARTITION dev.event_log_y2025m01_action_timestamp_table_name_idx;
--
-- Name: event_log_y2025m01_pkey; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_pkey ATTACH PARTITION dev.event_log_y2025m01_pkey;
--
-- Name: event_log_y2025m01_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_table_name_index ATTACH PARTITION dev.event_log_y2025m01_table_name_action_timestamp_idx;
--
-- Name: event_log_y2025m01_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_timestamp_index ATTACH PARTITION dev.event_log_y2025m01_timestamp_action_table_name_idx;
--
-- Name: event_log_y2025m02_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_action_index ATTACH PARTITION dev.event_log_y2025m02_action_timestamp_table_name_idx;
--
-- Name: event_log_y2025m02_pkey; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_pkey ATTACH PARTITION dev.event_log_y2025m02_pkey;
--
-- Name: event_log_y2025m02_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_table_name_index ATTACH PARTITION dev.event_log_y2025m02_table_name_action_timestamp_idx;
--
-- Name: event_log_y2025m02_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_timestamp_index ATTACH PARTITION dev.event_log_y2025m02_timestamp_action_table_name_idx;
--
-- Name: event_log_y2025m03_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_action_index ATTACH PARTITION dev.event_log_y2025m03_action_timestamp_table_name_idx;
--
-- Name: event_log_y2025m03_pkey; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_pkey ATTACH PARTITION dev.event_log_y2025m03_pkey;
--
-- Name: event_log_y2025m03_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_table_name_index ATTACH PARTITION dev.event_log_y2025m03_table_name_action_timestamp_idx;
--
-- Name: event_log_y2025m03_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_timestamp_index ATTACH PARTITION dev.event_log_y2025m03_timestamp_action_table_name_idx;
--
-- Name: event_log_y2025m04_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_action_index ATTACH PARTITION dev.event_log_y2025m04_action_timestamp_table_name_idx;
--
-- Name: event_log_y2025m04_pkey; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_pkey ATTACH PARTITION dev.event_log_y2025m04_pkey;
--
-- Name: event_log_y2025m04_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_table_name_index ATTACH PARTITION dev.event_log_y2025m04_table_name_action_timestamp_idx;
--
-- Name: event_log_y2025m04_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_timestamp_index ATTACH PARTITION dev.event_log_y2025m04_timestamp_action_table_name_idx;
--
-- Name: event_log_y2025m05_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_action_index ATTACH PARTITION dev.event_log_y2025m05_action_timestamp_table_name_idx;
--
-- Name: event_log_y2025m05_pkey; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_pkey ATTACH PARTITION dev.event_log_y2025m05_pkey;
--
-- Name: event_log_y2025m05_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_table_name_index ATTACH PARTITION dev.event_log_y2025m05_table_name_action_timestamp_idx;
--
-- Name: event_log_y2025m05_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_timestamp_index ATTACH PARTITION dev.event_log_y2025m05_timestamp_action_table_name_idx;
--
-- Name: event_log_y2025m06_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_action_index ATTACH PARTITION dev.event_log_y2025m06_action_timestamp_table_name_idx;
--
-- Name: event_log_y2025m06_pkey; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_pkey ATTACH PARTITION dev.event_log_y2025m06_pkey;
--
-- Name: event_log_y2025m06_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_table_name_index ATTACH PARTITION dev.event_log_y2025m06_table_name_action_timestamp_idx;
--
-- Name: event_log_y2025m06_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_timestamp_index ATTACH PARTITION dev.event_log_y2025m06_timestamp_action_table_name_idx;
--
-- Name: event_log_y2025m07_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_action_index ATTACH PARTITION dev.event_log_y2025m07_action_timestamp_table_name_idx;
--
-- Name: event_log_y2025m07_pkey; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_pkey ATTACH PARTITION dev.event_log_y2025m07_pkey;
--
-- Name: event_log_y2025m07_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_table_name_index ATTACH PARTITION dev.event_log_y2025m07_table_name_action_timestamp_idx;
--
-- Name: event_log_y2025m07_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_timestamp_index ATTACH PARTITION dev.event_log_y2025m07_timestamp_action_table_name_idx;
--
-- Name: event_log_y2025m08_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_action_index ATTACH PARTITION dev.event_log_y2025m08_action_timestamp_table_name_idx;
--
-- Name: event_log_y2025m08_pkey; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_pkey ATTACH PARTITION dev.event_log_y2025m08_pkey;
--
-- Name: event_log_y2025m08_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_table_name_index ATTACH PARTITION dev.event_log_y2025m08_table_name_action_timestamp_idx;
--
-- Name: event_log_y2025m08_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_timestamp_index ATTACH PARTITION dev.event_log_y2025m08_timestamp_action_table_name_idx;
--
-- Name: event_log_y2025m09_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_action_index ATTACH PARTITION dev.event_log_y2025m09_action_timestamp_table_name_idx;
--
-- Name: event_log_y2025m09_pkey; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_pkey ATTACH PARTITION dev.event_log_y2025m09_pkey;
--
-- Name: event_log_y2025m09_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_table_name_index ATTACH PARTITION dev.event_log_y2025m09_table_name_action_timestamp_idx;
--
-- Name: event_log_y2025m09_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_timestamp_index ATTACH PARTITION dev.event_log_y2025m09_timestamp_action_table_name_idx;
--
-- Name: event_log_y2025m10_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_action_index ATTACH PARTITION dev.event_log_y2025m10_action_timestamp_table_name_idx;
--
-- Name: event_log_y2025m10_pkey; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_pkey ATTACH PARTITION dev.event_log_y2025m10_pkey;
--
-- Name: event_log_y2025m10_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_table_name_index ATTACH PARTITION dev.event_log_y2025m10_table_name_action_timestamp_idx;
--
-- Name: event_log_y2025m10_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_timestamp_index ATTACH PARTITION dev.event_log_y2025m10_timestamp_action_table_name_idx;
--
-- Name: event_log_y2025m11_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_action_index ATTACH PARTITION dev.event_log_y2025m11_action_timestamp_table_name_idx;
--
-- Name: event_log_y2025m11_pkey; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_pkey ATTACH PARTITION dev.event_log_y2025m11_pkey;
--
-- Name: event_log_y2025m11_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_table_name_index ATTACH PARTITION dev.event_log_y2025m11_table_name_action_timestamp_idx;
--
-- Name: event_log_y2025m11_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_timestamp_index ATTACH PARTITION dev.event_log_y2025m11_timestamp_action_table_name_idx;
--
-- Name: event_log_y2025m12_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_action_index ATTACH PARTITION dev.event_log_y2025m12_action_timestamp_table_name_idx;
--
-- Name: event_log_y2025m12_pkey; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_pkey ATTACH PARTITION dev.event_log_y2025m12_pkey;
--
-- Name: event_log_y2025m12_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_table_name_index ATTACH PARTITION dev.event_log_y2025m12_table_name_action_timestamp_idx;
--
-- Name: event_log_y2025m12_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_timestamp_index ATTACH PARTITION dev.event_log_y2025m12_timestamp_action_table_name_idx;
--
-- Name: event_log_y2026m01_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_action_index ATTACH PARTITION dev.event_log_y2026m01_action_timestamp_table_name_idx;
--
-- Name: event_log_y2026m01_pkey; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_pkey ATTACH PARTITION dev.event_log_y2026m01_pkey;
--
-- Name: event_log_y2026m01_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_table_name_index ATTACH PARTITION dev.event_log_y2026m01_table_name_action_timestamp_idx;
--
-- Name: event_log_y2026m01_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_timestamp_index ATTACH PARTITION dev.event_log_y2026m01_timestamp_action_table_name_idx;
--
-- Name: event_log_y2026m02_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_action_index ATTACH PARTITION dev.event_log_y2026m02_action_timestamp_table_name_idx;
--
-- Name: event_log_y2026m02_pkey; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_pkey ATTACH PARTITION dev.event_log_y2026m02_pkey;
--
-- Name: event_log_y2026m02_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_table_name_index ATTACH PARTITION dev.event_log_y2026m02_table_name_action_timestamp_idx;
--
-- Name: event_log_y2026m02_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_timestamp_index ATTACH PARTITION dev.event_log_y2026m02_timestamp_action_table_name_idx;
--
-- Name: event_log_y2026m03_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_action_index ATTACH PARTITION dev.event_log_y2026m03_action_timestamp_table_name_idx;
--
-- Name: event_log_y2026m03_pkey; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_pkey ATTACH PARTITION dev.event_log_y2026m03_pkey;
--
-- Name: event_log_y2026m03_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_table_name_index ATTACH PARTITION dev.event_log_y2026m03_table_name_action_timestamp_idx;
--
-- Name: event_log_y2026m03_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_timestamp_index ATTACH PARTITION dev.event_log_y2026m03_timestamp_action_table_name_idx;
--
-- Name: event_log_y2026m04_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_action_index ATTACH PARTITION dev.event_log_y2026m04_action_timestamp_table_name_idx;
--
-- Name: event_log_y2026m04_pkey; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_pkey ATTACH PARTITION dev.event_log_y2026m04_pkey;
--
-- Name: event_log_y2026m04_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_table_name_index ATTACH PARTITION dev.event_log_y2026m04_table_name_action_timestamp_idx;
--
-- Name: event_log_y2026m04_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_timestamp_index ATTACH PARTITION dev.event_log_y2026m04_timestamp_action_table_name_idx;
--
-- Name: event_log_y2026m05_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_action_index ATTACH PARTITION dev.event_log_y2026m05_action_timestamp_table_name_idx;
--
-- Name: event_log_y2026m05_pkey; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_pkey ATTACH PARTITION dev.event_log_y2026m05_pkey;
--
-- Name: event_log_y2026m05_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_table_name_index ATTACH PARTITION dev.event_log_y2026m05_table_name_action_timestamp_idx;
--
-- Name: event_log_y2026m05_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_timestamp_index ATTACH PARTITION dev.event_log_y2026m05_timestamp_action_table_name_idx;
--
-- Name: event_log_y2026m06_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_action_index ATTACH PARTITION dev.event_log_y2026m06_action_timestamp_table_name_idx;
--
-- Name: event_log_y2026m06_pkey; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_pkey ATTACH PARTITION dev.event_log_y2026m06_pkey;
--
-- Name: event_log_y2026m06_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_table_name_index ATTACH PARTITION dev.event_log_y2026m06_table_name_action_timestamp_idx;
--
-- Name: event_log_y2026m06_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_timestamp_index ATTACH PARTITION dev.event_log_y2026m06_timestamp_action_table_name_idx;
--
-- Name: event_log_y2026m07_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_action_index ATTACH PARTITION dev.event_log_y2026m07_action_timestamp_table_name_idx;
--
-- Name: event_log_y2026m07_pkey; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_pkey ATTACH PARTITION dev.event_log_y2026m07_pkey;
--
-- Name: event_log_y2026m07_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_table_name_index ATTACH PARTITION dev.event_log_y2026m07_table_name_action_timestamp_idx;
--
-- Name: event_log_y2026m07_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_timestamp_index ATTACH PARTITION dev.event_log_y2026m07_timestamp_action_table_name_idx;
--
-- Name: event_log_y2026m08_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_action_index ATTACH PARTITION dev.event_log_y2026m08_action_timestamp_table_name_idx;
--
-- Name: event_log_y2026m08_pkey; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_pkey ATTACH PARTITION dev.event_log_y2026m08_pkey;
--
-- Name: event_log_y2026m08_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_table_name_index ATTACH PARTITION dev.event_log_y2026m08_table_name_action_timestamp_idx;
--
-- Name: event_log_y2026m08_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_timestamp_index ATTACH PARTITION dev.event_log_y2026m08_timestamp_action_table_name_idx;
--
-- Name: event_log_y2026m09_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_action_index ATTACH PARTITION dev.event_log_y2026m09_action_timestamp_table_name_idx;
--
-- Name: event_log_y2026m09_pkey; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_pkey ATTACH PARTITION dev.event_log_y2026m09_pkey;
--
-- Name: event_log_y2026m09_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_table_name_index ATTACH PARTITION dev.event_log_y2026m09_table_name_action_timestamp_idx;
--
-- Name: event_log_y2026m09_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_timestamp_index ATTACH PARTITION dev.event_log_y2026m09_timestamp_action_table_name_idx;
--
-- Name: event_log_y2026m10_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_action_index ATTACH PARTITION dev.event_log_y2026m10_action_timestamp_table_name_idx;
--
-- Name: event_log_y2026m10_pkey; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_pkey ATTACH PARTITION dev.event_log_y2026m10_pkey;
--
-- Name: event_log_y2026m10_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_table_name_index ATTACH PARTITION dev.event_log_y2026m10_table_name_action_timestamp_idx;
--
-- Name: event_log_y2026m10_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_timestamp_index ATTACH PARTITION dev.event_log_y2026m10_timestamp_action_table_name_idx;
--
-- Name: event_log_y2026m11_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_action_index ATTACH PARTITION dev.event_log_y2026m11_action_timestamp_table_name_idx;
--
-- Name: event_log_y2026m11_pkey; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_pkey ATTACH PARTITION dev.event_log_y2026m11_pkey;
--
-- Name: event_log_y2026m11_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_table_name_index ATTACH PARTITION dev.event_log_y2026m11_table_name_action_timestamp_idx;
--
-- Name: event_log_y2026m11_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_timestamp_index ATTACH PARTITION dev.event_log_y2026m11_timestamp_action_table_name_idx;
--
-- Name: event_log_y2026m12_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_action_index ATTACH PARTITION dev.event_log_y2026m12_action_timestamp_table_name_idx;
--
-- Name: event_log_y2026m12_pkey; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_pkey ATTACH PARTITION dev.event_log_y2026m12_pkey;
--
-- Name: event_log_y2026m12_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_table_name_index ATTACH PARTITION dev.event_log_y2026m12_table_name_action_timestamp_idx;
--
-- Name: event_log_y2026m12_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev; Owner: postgres
--
ALTER INDEX dev.event_log_timestamp_index ATTACH PARTITION dev.event_log_y2026m12_timestamp_action_table_name_idx;
--
-- Name: event_log_y2023m08_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_action_index ATTACH PARTITION test.event_log_y2023m08_action_timestamp_table_name_idx;
--
-- Name: event_log_y2023m08_pkey; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_pkey ATTACH PARTITION test.event_log_y2023m08_pkey;
--
-- Name: event_log_y2023m08_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_table_name_index ATTACH PARTITION test.event_log_y2023m08_table_name_action_timestamp_idx;
--
-- Name: event_log_y2023m08_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_timestamp_index ATTACH PARTITION test.event_log_y2023m08_timestamp_action_table_name_idx;
--
-- Name: event_log_y2023m09_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_action_index ATTACH PARTITION test.event_log_y2023m09_action_timestamp_table_name_idx;
--
-- Name: event_log_y2023m09_pkey; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_pkey ATTACH PARTITION test.event_log_y2023m09_pkey;
--
-- Name: event_log_y2023m09_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_table_name_index ATTACH PARTITION test.event_log_y2023m09_table_name_action_timestamp_idx;
--
-- Name: event_log_y2023m09_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_timestamp_index ATTACH PARTITION test.event_log_y2023m09_timestamp_action_table_name_idx;
--
-- Name: event_log_y2023m10_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_action_index ATTACH PARTITION test.event_log_y2023m10_action_timestamp_table_name_idx;
--
-- Name: event_log_y2023m10_pkey; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_pkey ATTACH PARTITION test.event_log_y2023m10_pkey;
--
-- Name: event_log_y2023m10_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_table_name_index ATTACH PARTITION test.event_log_y2023m10_table_name_action_timestamp_idx;
--
-- Name: event_log_y2023m10_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_timestamp_index ATTACH PARTITION test.event_log_y2023m10_timestamp_action_table_name_idx;
--
-- Name: event_log_y2023m11_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_action_index ATTACH PARTITION test.event_log_y2023m11_action_timestamp_table_name_idx;
--
-- Name: event_log_y2023m11_pkey; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_pkey ATTACH PARTITION test.event_log_y2023m11_pkey;
--
-- Name: event_log_y2023m11_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_table_name_index ATTACH PARTITION test.event_log_y2023m11_table_name_action_timestamp_idx;
--
-- Name: event_log_y2023m11_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_timestamp_index ATTACH PARTITION test.event_log_y2023m11_timestamp_action_table_name_idx;
--
-- Name: event_log_y2023m12_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_action_index ATTACH PARTITION test.event_log_y2023m12_action_timestamp_table_name_idx;
--
-- Name: event_log_y2023m12_pkey; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_pkey ATTACH PARTITION test.event_log_y2023m12_pkey;
--
-- Name: event_log_y2023m12_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_table_name_index ATTACH PARTITION test.event_log_y2023m12_table_name_action_timestamp_idx;
--
-- Name: event_log_y2023m12_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_timestamp_index ATTACH PARTITION test.event_log_y2023m12_timestamp_action_table_name_idx;
--
-- Name: event_log_y2024m01_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_action_index ATTACH PARTITION test.event_log_y2024m01_action_timestamp_table_name_idx;
--
-- Name: event_log_y2024m01_pkey; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_pkey ATTACH PARTITION test.event_log_y2024m01_pkey;
--
-- Name: event_log_y2024m01_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_table_name_index ATTACH PARTITION test.event_log_y2024m01_table_name_action_timestamp_idx;
--
-- Name: event_log_y2024m01_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_timestamp_index ATTACH PARTITION test.event_log_y2024m01_timestamp_action_table_name_idx;
--
-- Name: event_log_y2024m02_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_action_index ATTACH PARTITION test.event_log_y2024m02_action_timestamp_table_name_idx;
--
-- Name: event_log_y2024m02_pkey; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_pkey ATTACH PARTITION test.event_log_y2024m02_pkey;
--
-- Name: event_log_y2024m02_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_table_name_index ATTACH PARTITION test.event_log_y2024m02_table_name_action_timestamp_idx;
--
-- Name: event_log_y2024m02_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_timestamp_index ATTACH PARTITION test.event_log_y2024m02_timestamp_action_table_name_idx;
--
-- Name: event_log_y2024m03_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_action_index ATTACH PARTITION test.event_log_y2024m03_action_timestamp_table_name_idx;
--
-- Name: event_log_y2024m03_pkey; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_pkey ATTACH PARTITION test.event_log_y2024m03_pkey;
--
-- Name: event_log_y2024m03_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_table_name_index ATTACH PARTITION test.event_log_y2024m03_table_name_action_timestamp_idx;
--
-- Name: event_log_y2024m03_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_timestamp_index ATTACH PARTITION test.event_log_y2024m03_timestamp_action_table_name_idx;
--
-- Name: event_log_y2024m04_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_action_index ATTACH PARTITION test.event_log_y2024m04_action_timestamp_table_name_idx;
--
-- Name: event_log_y2024m04_pkey; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_pkey ATTACH PARTITION test.event_log_y2024m04_pkey;
--
-- Name: event_log_y2024m04_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_table_name_index ATTACH PARTITION test.event_log_y2024m04_table_name_action_timestamp_idx;
--
-- Name: event_log_y2024m04_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_timestamp_index ATTACH PARTITION test.event_log_y2024m04_timestamp_action_table_name_idx;
--
-- Name: event_log_y2024m05_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_action_index ATTACH PARTITION test.event_log_y2024m05_action_timestamp_table_name_idx;
--
-- Name: event_log_y2024m05_pkey; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_pkey ATTACH PARTITION test.event_log_y2024m05_pkey;
--
-- Name: event_log_y2024m05_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_table_name_index ATTACH PARTITION test.event_log_y2024m05_table_name_action_timestamp_idx;
--
-- Name: event_log_y2024m05_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_timestamp_index ATTACH PARTITION test.event_log_y2024m05_timestamp_action_table_name_idx;
--
-- Name: event_log_y2024m06_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_action_index ATTACH PARTITION test.event_log_y2024m06_action_timestamp_table_name_idx;
--
-- Name: event_log_y2024m06_pkey; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_pkey ATTACH PARTITION test.event_log_y2024m06_pkey;
--
-- Name: event_log_y2024m06_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_table_name_index ATTACH PARTITION test.event_log_y2024m06_table_name_action_timestamp_idx;
--
-- Name: event_log_y2024m06_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_timestamp_index ATTACH PARTITION test.event_log_y2024m06_timestamp_action_table_name_idx;
--
-- Name: event_log_y2024m07_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_action_index ATTACH PARTITION test.event_log_y2024m07_action_timestamp_table_name_idx;
--
-- Name: event_log_y2024m07_pkey; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_pkey ATTACH PARTITION test.event_log_y2024m07_pkey;
--
-- Name: event_log_y2024m07_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_table_name_index ATTACH PARTITION test.event_log_y2024m07_table_name_action_timestamp_idx;
--
-- Name: event_log_y2024m07_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_timestamp_index ATTACH PARTITION test.event_log_y2024m07_timestamp_action_table_name_idx;
--
-- Name: event_log_y2024m08_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_action_index ATTACH PARTITION test.event_log_y2024m08_action_timestamp_table_name_idx;
--
-- Name: event_log_y2024m08_pkey; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_pkey ATTACH PARTITION test.event_log_y2024m08_pkey;
--
-- Name: event_log_y2024m08_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_table_name_index ATTACH PARTITION test.event_log_y2024m08_table_name_action_timestamp_idx;
--
-- Name: event_log_y2024m08_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_timestamp_index ATTACH PARTITION test.event_log_y2024m08_timestamp_action_table_name_idx;
--
-- Name: event_log_y2024m09_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_action_index ATTACH PARTITION test.event_log_y2024m09_action_timestamp_table_name_idx;
--
-- Name: event_log_y2024m09_pkey; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_pkey ATTACH PARTITION test.event_log_y2024m09_pkey;
--
-- Name: event_log_y2024m09_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_table_name_index ATTACH PARTITION test.event_log_y2024m09_table_name_action_timestamp_idx;
--
-- Name: event_log_y2024m09_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_timestamp_index ATTACH PARTITION test.event_log_y2024m09_timestamp_action_table_name_idx;
--
-- Name: event_log_y2024m10_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_action_index ATTACH PARTITION test.event_log_y2024m10_action_timestamp_table_name_idx;
--
-- Name: event_log_y2024m10_pkey; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_pkey ATTACH PARTITION test.event_log_y2024m10_pkey;
--
-- Name: event_log_y2024m10_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_table_name_index ATTACH PARTITION test.event_log_y2024m10_table_name_action_timestamp_idx;
--
-- Name: event_log_y2024m10_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_timestamp_index ATTACH PARTITION test.event_log_y2024m10_timestamp_action_table_name_idx;
--
-- Name: event_log_y2024m11_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_action_index ATTACH PARTITION test.event_log_y2024m11_action_timestamp_table_name_idx;
--
-- Name: event_log_y2024m11_pkey; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_pkey ATTACH PARTITION test.event_log_y2024m11_pkey;
--
-- Name: event_log_y2024m11_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_table_name_index ATTACH PARTITION test.event_log_y2024m11_table_name_action_timestamp_idx;
--
-- Name: event_log_y2024m11_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_timestamp_index ATTACH PARTITION test.event_log_y2024m11_timestamp_action_table_name_idx;
--
-- Name: event_log_y2024m12_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_action_index ATTACH PARTITION test.event_log_y2024m12_action_timestamp_table_name_idx;
--
-- Name: event_log_y2024m12_pkey; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_pkey ATTACH PARTITION test.event_log_y2024m12_pkey;
--
-- Name: event_log_y2024m12_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_table_name_index ATTACH PARTITION test.event_log_y2024m12_table_name_action_timestamp_idx;
--
-- Name: event_log_y2024m12_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_timestamp_index ATTACH PARTITION test.event_log_y2024m12_timestamp_action_table_name_idx;
--
-- Name: event_log_y2025m01_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_action_index ATTACH PARTITION test.event_log_y2025m01_action_timestamp_table_name_idx;
--
-- Name: event_log_y2025m01_pkey; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_pkey ATTACH PARTITION test.event_log_y2025m01_pkey;
--
-- Name: event_log_y2025m01_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_table_name_index ATTACH PARTITION test.event_log_y2025m01_table_name_action_timestamp_idx;
--
-- Name: event_log_y2025m01_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_timestamp_index ATTACH PARTITION test.event_log_y2025m01_timestamp_action_table_name_idx;
--
-- Name: event_log_y2025m02_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_action_index ATTACH PARTITION test.event_log_y2025m02_action_timestamp_table_name_idx;
--
-- Name: event_log_y2025m02_pkey; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_pkey ATTACH PARTITION test.event_log_y2025m02_pkey;
--
-- Name: event_log_y2025m02_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_table_name_index ATTACH PARTITION test.event_log_y2025m02_table_name_action_timestamp_idx;
--
-- Name: event_log_y2025m02_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_timestamp_index ATTACH PARTITION test.event_log_y2025m02_timestamp_action_table_name_idx;
--
-- Name: event_log_y2025m03_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_action_index ATTACH PARTITION test.event_log_y2025m03_action_timestamp_table_name_idx;
--
-- Name: event_log_y2025m03_pkey; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_pkey ATTACH PARTITION test.event_log_y2025m03_pkey;
--
-- Name: event_log_y2025m03_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_table_name_index ATTACH PARTITION test.event_log_y2025m03_table_name_action_timestamp_idx;
--
-- Name: event_log_y2025m03_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_timestamp_index ATTACH PARTITION test.event_log_y2025m03_timestamp_action_table_name_idx;
--
-- Name: event_log_y2025m04_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_action_index ATTACH PARTITION test.event_log_y2025m04_action_timestamp_table_name_idx;
--
-- Name: event_log_y2025m04_pkey; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_pkey ATTACH PARTITION test.event_log_y2025m04_pkey;
--
-- Name: event_log_y2025m04_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_table_name_index ATTACH PARTITION test.event_log_y2025m04_table_name_action_timestamp_idx;
--
-- Name: event_log_y2025m04_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_timestamp_index ATTACH PARTITION test.event_log_y2025m04_timestamp_action_table_name_idx;
--
-- Name: event_log_y2025m05_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_action_index ATTACH PARTITION test.event_log_y2025m05_action_timestamp_table_name_idx;
--
-- Name: event_log_y2025m05_pkey; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_pkey ATTACH PARTITION test.event_log_y2025m05_pkey;
--
-- Name: event_log_y2025m05_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_table_name_index ATTACH PARTITION test.event_log_y2025m05_table_name_action_timestamp_idx;
--
-- Name: event_log_y2025m05_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_timestamp_index ATTACH PARTITION test.event_log_y2025m05_timestamp_action_table_name_idx;
--
-- Name: event_log_y2025m06_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_action_index ATTACH PARTITION test.event_log_y2025m06_action_timestamp_table_name_idx;
--
-- Name: event_log_y2025m06_pkey; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_pkey ATTACH PARTITION test.event_log_y2025m06_pkey;
--
-- Name: event_log_y2025m06_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_table_name_index ATTACH PARTITION test.event_log_y2025m06_table_name_action_timestamp_idx;
--
-- Name: event_log_y2025m06_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_timestamp_index ATTACH PARTITION test.event_log_y2025m06_timestamp_action_table_name_idx;
--
-- Name: event_log_y2025m07_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_action_index ATTACH PARTITION test.event_log_y2025m07_action_timestamp_table_name_idx;
--
-- Name: event_log_y2025m07_pkey; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_pkey ATTACH PARTITION test.event_log_y2025m07_pkey;
--
-- Name: event_log_y2025m07_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_table_name_index ATTACH PARTITION test.event_log_y2025m07_table_name_action_timestamp_idx;
--
-- Name: event_log_y2025m07_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_timestamp_index ATTACH PARTITION test.event_log_y2025m07_timestamp_action_table_name_idx;
--
-- Name: event_log_y2025m08_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_action_index ATTACH PARTITION test.event_log_y2025m08_action_timestamp_table_name_idx;
--
-- Name: event_log_y2025m08_pkey; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_pkey ATTACH PARTITION test.event_log_y2025m08_pkey;
--
-- Name: event_log_y2025m08_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_table_name_index ATTACH PARTITION test.event_log_y2025m08_table_name_action_timestamp_idx;
--
-- Name: event_log_y2025m08_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_timestamp_index ATTACH PARTITION test.event_log_y2025m08_timestamp_action_table_name_idx;
--
-- Name: event_log_y2025m09_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_action_index ATTACH PARTITION test.event_log_y2025m09_action_timestamp_table_name_idx;
--
-- Name: event_log_y2025m09_pkey; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_pkey ATTACH PARTITION test.event_log_y2025m09_pkey;
--
-- Name: event_log_y2025m09_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_table_name_index ATTACH PARTITION test.event_log_y2025m09_table_name_action_timestamp_idx;
--
-- Name: event_log_y2025m09_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_timestamp_index ATTACH PARTITION test.event_log_y2025m09_timestamp_action_table_name_idx;
--
-- Name: event_log_y2025m10_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_action_index ATTACH PARTITION test.event_log_y2025m10_action_timestamp_table_name_idx;
--
-- Name: event_log_y2025m10_pkey; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_pkey ATTACH PARTITION test.event_log_y2025m10_pkey;
--
-- Name: event_log_y2025m10_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_table_name_index ATTACH PARTITION test.event_log_y2025m10_table_name_action_timestamp_idx;
--
-- Name: event_log_y2025m10_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_timestamp_index ATTACH PARTITION test.event_log_y2025m10_timestamp_action_table_name_idx;
--
-- Name: event_log_y2025m11_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_action_index ATTACH PARTITION test.event_log_y2025m11_action_timestamp_table_name_idx;
--
-- Name: event_log_y2025m11_pkey; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_pkey ATTACH PARTITION test.event_log_y2025m11_pkey;
--
-- Name: event_log_y2025m11_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_table_name_index ATTACH PARTITION test.event_log_y2025m11_table_name_action_timestamp_idx;
--
-- Name: event_log_y2025m11_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_timestamp_index ATTACH PARTITION test.event_log_y2025m11_timestamp_action_table_name_idx;
--
-- Name: event_log_y2025m12_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_action_index ATTACH PARTITION test.event_log_y2025m12_action_timestamp_table_name_idx;
--
-- Name: event_log_y2025m12_pkey; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_pkey ATTACH PARTITION test.event_log_y2025m12_pkey;
--
-- Name: event_log_y2025m12_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_table_name_index ATTACH PARTITION test.event_log_y2025m12_table_name_action_timestamp_idx;
--
-- Name: event_log_y2025m12_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_timestamp_index ATTACH PARTITION test.event_log_y2025m12_timestamp_action_table_name_idx;
--
-- Name: event_log_y2026m01_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_action_index ATTACH PARTITION test.event_log_y2026m01_action_timestamp_table_name_idx;
--
-- Name: event_log_y2026m01_pkey; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_pkey ATTACH PARTITION test.event_log_y2026m01_pkey;
--
-- Name: event_log_y2026m01_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_table_name_index ATTACH PARTITION test.event_log_y2026m01_table_name_action_timestamp_idx;
--
-- Name: event_log_y2026m01_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_timestamp_index ATTACH PARTITION test.event_log_y2026m01_timestamp_action_table_name_idx;
--
-- Name: event_log_y2026m02_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_action_index ATTACH PARTITION test.event_log_y2026m02_action_timestamp_table_name_idx;
--
-- Name: event_log_y2026m02_pkey; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_pkey ATTACH PARTITION test.event_log_y2026m02_pkey;
--
-- Name: event_log_y2026m02_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_table_name_index ATTACH PARTITION test.event_log_y2026m02_table_name_action_timestamp_idx;
--
-- Name: event_log_y2026m02_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_timestamp_index ATTACH PARTITION test.event_log_y2026m02_timestamp_action_table_name_idx;
--
-- Name: event_log_y2026m03_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_action_index ATTACH PARTITION test.event_log_y2026m03_action_timestamp_table_name_idx;
--
-- Name: event_log_y2026m03_pkey; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_pkey ATTACH PARTITION test.event_log_y2026m03_pkey;
--
-- Name: event_log_y2026m03_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_table_name_index ATTACH PARTITION test.event_log_y2026m03_table_name_action_timestamp_idx;
--
-- Name: event_log_y2026m03_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_timestamp_index ATTACH PARTITION test.event_log_y2026m03_timestamp_action_table_name_idx;
--
-- Name: event_log_y2026m04_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_action_index ATTACH PARTITION test.event_log_y2026m04_action_timestamp_table_name_idx;
--
-- Name: event_log_y2026m04_pkey; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_pkey ATTACH PARTITION test.event_log_y2026m04_pkey;
--
-- Name: event_log_y2026m04_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_table_name_index ATTACH PARTITION test.event_log_y2026m04_table_name_action_timestamp_idx;
--
-- Name: event_log_y2026m04_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_timestamp_index ATTACH PARTITION test.event_log_y2026m04_timestamp_action_table_name_idx;
--
-- Name: event_log_y2026m05_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_action_index ATTACH PARTITION test.event_log_y2026m05_action_timestamp_table_name_idx;
--
-- Name: event_log_y2026m05_pkey; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_pkey ATTACH PARTITION test.event_log_y2026m05_pkey;
--
-- Name: event_log_y2026m05_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_table_name_index ATTACH PARTITION test.event_log_y2026m05_table_name_action_timestamp_idx;
--
-- Name: event_log_y2026m05_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_timestamp_index ATTACH PARTITION test.event_log_y2026m05_timestamp_action_table_name_idx;
--
-- Name: event_log_y2026m06_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_action_index ATTACH PARTITION test.event_log_y2026m06_action_timestamp_table_name_idx;
--
-- Name: event_log_y2026m06_pkey; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_pkey ATTACH PARTITION test.event_log_y2026m06_pkey;
--
-- Name: event_log_y2026m06_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_table_name_index ATTACH PARTITION test.event_log_y2026m06_table_name_action_timestamp_idx;
--
-- Name: event_log_y2026m06_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_timestamp_index ATTACH PARTITION test.event_log_y2026m06_timestamp_action_table_name_idx;
--
-- Name: event_log_y2026m07_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_action_index ATTACH PARTITION test.event_log_y2026m07_action_timestamp_table_name_idx;
--
-- Name: event_log_y2026m07_pkey; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_pkey ATTACH PARTITION test.event_log_y2026m07_pkey;
--
-- Name: event_log_y2026m07_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_table_name_index ATTACH PARTITION test.event_log_y2026m07_table_name_action_timestamp_idx;
--
-- Name: event_log_y2026m07_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_timestamp_index ATTACH PARTITION test.event_log_y2026m07_timestamp_action_table_name_idx;
--
-- Name: event_log_y2026m08_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_action_index ATTACH PARTITION test.event_log_y2026m08_action_timestamp_table_name_idx;
--
-- Name: event_log_y2026m08_pkey; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_pkey ATTACH PARTITION test.event_log_y2026m08_pkey;
--
-- Name: event_log_y2026m08_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_table_name_index ATTACH PARTITION test.event_log_y2026m08_table_name_action_timestamp_idx;
--
-- Name: event_log_y2026m08_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_timestamp_index ATTACH PARTITION test.event_log_y2026m08_timestamp_action_table_name_idx;
--
-- Name: event_log_y2026m09_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_action_index ATTACH PARTITION test.event_log_y2026m09_action_timestamp_table_name_idx;
--
-- Name: event_log_y2026m09_pkey; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_pkey ATTACH PARTITION test.event_log_y2026m09_pkey;
--
-- Name: event_log_y2026m09_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_table_name_index ATTACH PARTITION test.event_log_y2026m09_table_name_action_timestamp_idx;
--
-- Name: event_log_y2026m09_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_timestamp_index ATTACH PARTITION test.event_log_y2026m09_timestamp_action_table_name_idx;
--
-- Name: event_log_y2026m10_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_action_index ATTACH PARTITION test.event_log_y2026m10_action_timestamp_table_name_idx;
--
-- Name: event_log_y2026m10_pkey; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_pkey ATTACH PARTITION test.event_log_y2026m10_pkey;
--
-- Name: event_log_y2026m10_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_table_name_index ATTACH PARTITION test.event_log_y2026m10_table_name_action_timestamp_idx;
--
-- Name: event_log_y2026m10_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_timestamp_index ATTACH PARTITION test.event_log_y2026m10_timestamp_action_table_name_idx;
--
-- Name: event_log_y2026m11_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_action_index ATTACH PARTITION test.event_log_y2026m11_action_timestamp_table_name_idx;
--
-- Name: event_log_y2026m11_pkey; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_pkey ATTACH PARTITION test.event_log_y2026m11_pkey;
--
-- Name: event_log_y2026m11_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_table_name_index ATTACH PARTITION test.event_log_y2026m11_table_name_action_timestamp_idx;
--
-- Name: event_log_y2026m11_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_timestamp_index ATTACH PARTITION test.event_log_y2026m11_timestamp_action_table_name_idx;
--
-- Name: event_log_y2026m12_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_action_index ATTACH PARTITION test.event_log_y2026m12_action_timestamp_table_name_idx;
--
-- Name: event_log_y2026m12_pkey; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_pkey ATTACH PARTITION test.event_log_y2026m12_pkey;
--
-- Name: event_log_y2026m12_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_table_name_index ATTACH PARTITION test.event_log_y2026m12_table_name_action_timestamp_idx;
--
-- Name: event_log_y2026m12_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test; Owner: postgres
--
ALTER INDEX test.event_log_timestamp_index ATTACH PARTITION test.event_log_y2026m12_timestamp_action_table_name_idx;
--
-- Name: contexts contexts_audit; Type: TRIGGER; Schema: dev; Owner: postgres
--
CREATE TRIGGER contexts_audit
AFTER
INSERT
    OR DELETE
    OR
UPDATE ON dev.contexts FOR EACH ROW EXECUTE FUNCTION dev.event_logger();
--
-- Name: default_configs default_configs_audit; Type: TRIGGER; Schema: dev; Owner: postgres
--
CREATE TRIGGER default_configs_audit
AFTER
INSERT
    OR DELETE
    OR
UPDATE ON dev.default_configs FOR EACH ROW EXECUTE FUNCTION dev.event_logger();
--
-- Name: dimensions dimensions_audit; Type: TRIGGER; Schema: dev; Owner: postgres
--
CREATE TRIGGER dimensions_audit
AFTER
INSERT
    OR DELETE
    OR
UPDATE ON dev.dimensions FOR EACH ROW EXECUTE FUNCTION dev.event_logger();
--
-- Name: experiments experiments_audit; Type: TRIGGER; Schema: dev; Owner: postgres
--
CREATE TRIGGER experiments_audit
AFTER
INSERT
    OR DELETE
    OR
UPDATE ON dev.experiments FOR EACH ROW EXECUTE FUNCTION dev.event_logger();
--
-- Name: functions functions_audit; Type: TRIGGER; Schema: dev; Owner: postgres
--
CREATE TRIGGER functions_audit
AFTER
INSERT
    OR DELETE
    OR
UPDATE ON dev.functions FOR EACH ROW EXECUTE FUNCTION dev.event_logger();
--
-- Name: contexts contexts_audit; Type: TRIGGER; Schema: test; Owner: postgres
--
CREATE TRIGGER contexts_audit
AFTER
INSERT
    OR DELETE
    OR
UPDATE ON test.contexts FOR EACH ROW EXECUTE FUNCTION test.event_logger();
--
-- Name: default_configs default_configs_audit; Type: TRIGGER; Schema: test; Owner: postgres
--
CREATE TRIGGER default_configs_audit
AFTER
INSERT
    OR DELETE
    OR
UPDATE ON test.default_configs FOR EACH ROW EXECUTE FUNCTION test.event_logger();
--
-- Name: dimensions dimensions_audit; Type: TRIGGER; Schema: test; Owner: postgres
--
CREATE TRIGGER dimensions_audit
AFTER
INSERT
    OR DELETE
    OR
UPDATE ON test.dimensions FOR EACH ROW EXECUTE FUNCTION test.event_logger();
--
-- Name: experiments experiments_audit; Type: TRIGGER; Schema: test; Owner: postgres
--
CREATE TRIGGER experiments_audit
AFTER
INSERT
    OR DELETE
    OR
UPDATE ON test.experiments FOR EACH ROW EXECUTE FUNCTION test.event_logger();
--
-- Name: functions functions_audit; Type: TRIGGER; Schema: test; Owner: postgres
--
CREATE TRIGGER functions_audit
AFTER
INSERT
    OR DELETE
    OR
UPDATE ON test.functions FOR EACH ROW EXECUTE FUNCTION test.event_logger();
--
-- Name: default_configs default_configs_function_name_fkey; Type: FK CONSTRAINT; Schema: dev; Owner: postgres
--
ALTER TABLE ONLY dev.default_configs
ADD CONSTRAINT default_configs_function_name_fkey FOREIGN KEY (function_name) REFERENCES dev.functions(function_name);
--
-- Name: dimensions dimensions_function_name_fkey; Type: FK CONSTRAINT; Schema: dev; Owner: postgres
--
ALTER TABLE ONLY dev.dimensions
ADD CONSTRAINT dimensions_function_name_fkey FOREIGN KEY (function_name) REFERENCES dev.functions(function_name);
--
-- Name: default_configs default_configs_function_name_fkey; Type: FK CONSTRAINT; Schema: test; Owner: postgres
--
ALTER TABLE ONLY test.default_configs
ADD CONSTRAINT default_configs_function_name_fkey FOREIGN KEY (function_name) REFERENCES test.functions(function_name);
--
-- Name: dimensions dimensions_function_name_fkey; Type: FK CONSTRAINT; Schema: test; Owner: postgres
--
ALTER TABLE ONLY test.dimensions
ADD CONSTRAINT dimensions_function_name_fkey FOREIGN KEY (function_name) REFERENCES test.functions(function_name);
--
-- PostgreSQL database dump complete
--
