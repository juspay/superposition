--
-- PostgreSQL database dump
--

-- Dumped from database version 12.18
-- Dumped by pg_dump version 12.18

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
-- Name: dev_cac; Type: SCHEMA; Schema: -; Owner: postgres
--

CREATE SCHEMA dev_cac;


ALTER SCHEMA dev_cac OWNER TO postgres;

--
-- Name: dev_experimentation; Type: SCHEMA; Schema: -; Owner: postgres
--

CREATE SCHEMA dev_experimentation;


ALTER SCHEMA dev_experimentation OWNER TO postgres;

--
-- Name: test_cac; Type: SCHEMA; Schema: -; Owner: postgres
--

CREATE SCHEMA test_cac;


ALTER SCHEMA test_cac OWNER TO postgres;

--
-- Name: test_experimentation; Type: SCHEMA; Schema: -; Owner: postgres
--

CREATE SCHEMA test_experimentation;


ALTER SCHEMA test_experimentation OWNER TO postgres;

--
-- Name: uuid-ossp; Type: EXTENSION; Schema: -; Owner: -
--

CREATE EXTENSION IF NOT EXISTS "uuid-ossp" WITH SCHEMA public;


--
-- Name: EXTENSION "uuid-ossp"; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION "uuid-ossp" IS 'generate universally unique identifiers (UUIDs)';


--
-- Name: dimension_type; Type: TYPE; Schema: dev_cac; Owner: postgres
--

CREATE TYPE dev_cac.dimension_type AS ENUM (
    'NULL',
    'BOOL',
    'NUMBER',
    'STRING',
    'ARRAY',
    'OBJECT'
);


ALTER TYPE dev_cac.dimension_type OWNER TO postgres;

--
-- Name: experiment_status_type; Type: TYPE; Schema: dev_experimentation; Owner: postgres
--

CREATE TYPE dev_experimentation.experiment_status_type AS ENUM (
    'CREATED',
    'CONCLUDED',
    'INPROGRESS'
);


ALTER TYPE dev_experimentation.experiment_status_type OWNER TO postgres;

--
-- Name: not_null_text; Type: DOMAIN; Schema: dev_experimentation; Owner: postgres
--

CREATE DOMAIN dev_experimentation.not_null_text AS text NOT NULL;


ALTER DOMAIN dev_experimentation.not_null_text OWNER TO postgres;

--
-- Name: dimension_type; Type: TYPE; Schema: test_cac; Owner: postgres
--

CREATE TYPE test_cac.dimension_type AS ENUM (
    'NULL',
    'BOOL',
    'NUMBER',
    'STRING',
    'ARRAY',
    'OBJECT'
);


ALTER TYPE test_cac.dimension_type OWNER TO postgres;

--
-- Name: experiment_status_type; Type: TYPE; Schema: test_experimentation; Owner: postgres
--

CREATE TYPE test_experimentation.experiment_status_type AS ENUM (
    'CREATED',
    'CONCLUDED',
    'INPROGRESS'
);


ALTER TYPE test_experimentation.experiment_status_type OWNER TO postgres;

--
-- Name: not_null_text; Type: DOMAIN; Schema: test_experimentation; Owner: postgres
--

CREATE DOMAIN test_experimentation.not_null_text AS text NOT NULL;


ALTER DOMAIN test_experimentation.not_null_text OWNER TO postgres;

--
-- Name: event_logger(); Type: FUNCTION; Schema: dev_cac; Owner: postgres
--

CREATE FUNCTION dev_cac.event_logger() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
    old_data json;
    new_data json;
BEGIN
    IF (TG_OP = 'UPDATE') THEN
        old_data := row_to_json(OLD);
        new_data := row_to_json(NEW);
        INSERT INTO dev_cac.event_log
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
        INSERT INTO dev_cac.event_log
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
        INSERT INTO dev_cac.event_log
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


ALTER FUNCTION dev_cac.event_logger() OWNER TO postgres;

--
-- Name: event_logger(); Type: FUNCTION; Schema: dev_experimentation; Owner: postgres
--

CREATE FUNCTION dev_experimentation.event_logger() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
    old_data json;
    new_data json;
BEGIN
    IF (TG_OP = 'UPDATE') THEN
        old_data := row_to_json(OLD);
        new_data := row_to_json(NEW);
        INSERT INTO dev_experimentation.event_log
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
        INSERT INTO dev_experimentation.event_log
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
        INSERT INTO dev_experimentation.event_log
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


ALTER FUNCTION dev_experimentation.event_logger() OWNER TO postgres;

--
-- Name: event_logger(); Type: FUNCTION; Schema: test_cac; Owner: postgres
--

CREATE FUNCTION test_cac.event_logger() RETURNS trigger
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


ALTER FUNCTION test_cac.event_logger() OWNER TO postgres;

--
-- Name: event_logger(); Type: FUNCTION; Schema: test_experimentation; Owner: postgres
--

CREATE FUNCTION test_experimentation.event_logger() RETURNS trigger
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


ALTER FUNCTION test_experimentation.event_logger() OWNER TO postgres;

SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: contexts; Type: TABLE; Schema: dev_cac; Owner: postgres
--

CREATE TABLE dev_cac.contexts (
    id character varying NOT NULL,
    value json NOT NULL,
    override_id character varying NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_by character varying NOT NULL,
    priority integer DEFAULT 1 NOT NULL,
    override json DEFAULT '{}'::json NOT NULL
);


ALTER TABLE dev_cac.contexts OWNER TO postgres;

--
-- Name: default_configs; Type: TABLE; Schema: dev_cac; Owner: postgres
--

CREATE TABLE dev_cac.default_configs (
    key character varying NOT NULL,
    value json NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_by character varying NOT NULL,
    schema json DEFAULT '{}'::json NOT NULL,
    function_name text
);


ALTER TABLE dev_cac.default_configs OWNER TO postgres;

--
-- Name: dimensions; Type: TABLE; Schema: dev_cac; Owner: postgres
--

CREATE TABLE dev_cac.dimensions (
    dimension character varying NOT NULL,
    priority integer NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_by character varying NOT NULL,
    schema json DEFAULT '{}'::json NOT NULL,
    function_name text
);


ALTER TABLE dev_cac.dimensions OWNER TO postgres;

--
-- Name: event_log; Type: TABLE; Schema: dev_cac; Owner: postgres
--

CREATE TABLE dev_cac.event_log (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
)
PARTITION BY RANGE ("timestamp");


ALTER TABLE dev_cac.event_log OWNER TO postgres;

--
-- Name: event_log_y2023m08; Type: TABLE; Schema: dev_cac; Owner: postgres
--

CREATE TABLE dev_cac.event_log_y2023m08 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_cac.event_log ATTACH PARTITION dev_cac.event_log_y2023m08 FOR VALUES FROM ('2023-08-01 00:00:00') TO ('2023-09-01 00:00:00');


ALTER TABLE dev_cac.event_log_y2023m08 OWNER TO postgres;

--
-- Name: event_log_y2023m09; Type: TABLE; Schema: dev_cac; Owner: postgres
--

CREATE TABLE dev_cac.event_log_y2023m09 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_cac.event_log ATTACH PARTITION dev_cac.event_log_y2023m09 FOR VALUES FROM ('2023-09-01 00:00:00') TO ('2023-10-01 00:00:00');


ALTER TABLE dev_cac.event_log_y2023m09 OWNER TO postgres;

--
-- Name: event_log_y2023m10; Type: TABLE; Schema: dev_cac; Owner: postgres
--

CREATE TABLE dev_cac.event_log_y2023m10 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_cac.event_log ATTACH PARTITION dev_cac.event_log_y2023m10 FOR VALUES FROM ('2023-10-01 00:00:00') TO ('2023-11-01 00:00:00');


ALTER TABLE dev_cac.event_log_y2023m10 OWNER TO postgres;

--
-- Name: event_log_y2023m11; Type: TABLE; Schema: dev_cac; Owner: postgres
--

CREATE TABLE dev_cac.event_log_y2023m11 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_cac.event_log ATTACH PARTITION dev_cac.event_log_y2023m11 FOR VALUES FROM ('2023-11-01 00:00:00') TO ('2023-12-01 00:00:00');


ALTER TABLE dev_cac.event_log_y2023m11 OWNER TO postgres;

--
-- Name: event_log_y2023m12; Type: TABLE; Schema: dev_cac; Owner: postgres
--

CREATE TABLE dev_cac.event_log_y2023m12 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_cac.event_log ATTACH PARTITION dev_cac.event_log_y2023m12 FOR VALUES FROM ('2023-12-01 00:00:00') TO ('2024-01-01 00:00:00');


ALTER TABLE dev_cac.event_log_y2023m12 OWNER TO postgres;

--
-- Name: event_log_y2024m01; Type: TABLE; Schema: dev_cac; Owner: postgres
--

CREATE TABLE dev_cac.event_log_y2024m01 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_cac.event_log ATTACH PARTITION dev_cac.event_log_y2024m01 FOR VALUES FROM ('2024-01-01 00:00:00') TO ('2024-02-01 00:00:00');


ALTER TABLE dev_cac.event_log_y2024m01 OWNER TO postgres;

--
-- Name: event_log_y2024m02; Type: TABLE; Schema: dev_cac; Owner: postgres
--

CREATE TABLE dev_cac.event_log_y2024m02 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_cac.event_log ATTACH PARTITION dev_cac.event_log_y2024m02 FOR VALUES FROM ('2024-02-01 00:00:00') TO ('2024-03-01 00:00:00');


ALTER TABLE dev_cac.event_log_y2024m02 OWNER TO postgres;

--
-- Name: event_log_y2024m03; Type: TABLE; Schema: dev_cac; Owner: postgres
--

CREATE TABLE dev_cac.event_log_y2024m03 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_cac.event_log ATTACH PARTITION dev_cac.event_log_y2024m03 FOR VALUES FROM ('2024-03-01 00:00:00') TO ('2024-04-01 00:00:00');


ALTER TABLE dev_cac.event_log_y2024m03 OWNER TO postgres;

--
-- Name: event_log_y2024m04; Type: TABLE; Schema: dev_cac; Owner: postgres
--

CREATE TABLE dev_cac.event_log_y2024m04 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_cac.event_log ATTACH PARTITION dev_cac.event_log_y2024m04 FOR VALUES FROM ('2024-04-01 00:00:00') TO ('2024-05-01 00:00:00');


ALTER TABLE dev_cac.event_log_y2024m04 OWNER TO postgres;

--
-- Name: event_log_y2024m05; Type: TABLE; Schema: dev_cac; Owner: postgres
--

CREATE TABLE dev_cac.event_log_y2024m05 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_cac.event_log ATTACH PARTITION dev_cac.event_log_y2024m05 FOR VALUES FROM ('2024-05-01 00:00:00') TO ('2024-06-01 00:00:00');


ALTER TABLE dev_cac.event_log_y2024m05 OWNER TO postgres;

--
-- Name: event_log_y2024m06; Type: TABLE; Schema: dev_cac; Owner: postgres
--

CREATE TABLE dev_cac.event_log_y2024m06 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_cac.event_log ATTACH PARTITION dev_cac.event_log_y2024m06 FOR VALUES FROM ('2024-06-01 00:00:00') TO ('2024-07-01 00:00:00');


ALTER TABLE dev_cac.event_log_y2024m06 OWNER TO postgres;

--
-- Name: event_log_y2024m07; Type: TABLE; Schema: dev_cac; Owner: postgres
--

CREATE TABLE dev_cac.event_log_y2024m07 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_cac.event_log ATTACH PARTITION dev_cac.event_log_y2024m07 FOR VALUES FROM ('2024-07-01 00:00:00') TO ('2024-08-01 00:00:00');


ALTER TABLE dev_cac.event_log_y2024m07 OWNER TO postgres;

--
-- Name: event_log_y2024m08; Type: TABLE; Schema: dev_cac; Owner: postgres
--

CREATE TABLE dev_cac.event_log_y2024m08 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_cac.event_log ATTACH PARTITION dev_cac.event_log_y2024m08 FOR VALUES FROM ('2024-08-01 00:00:00') TO ('2024-09-01 00:00:00');


ALTER TABLE dev_cac.event_log_y2024m08 OWNER TO postgres;

--
-- Name: event_log_y2024m09; Type: TABLE; Schema: dev_cac; Owner: postgres
--

CREATE TABLE dev_cac.event_log_y2024m09 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_cac.event_log ATTACH PARTITION dev_cac.event_log_y2024m09 FOR VALUES FROM ('2024-09-01 00:00:00') TO ('2024-10-01 00:00:00');


ALTER TABLE dev_cac.event_log_y2024m09 OWNER TO postgres;

--
-- Name: event_log_y2024m10; Type: TABLE; Schema: dev_cac; Owner: postgres
--

CREATE TABLE dev_cac.event_log_y2024m10 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_cac.event_log ATTACH PARTITION dev_cac.event_log_y2024m10 FOR VALUES FROM ('2024-10-01 00:00:00') TO ('2024-11-01 00:00:00');


ALTER TABLE dev_cac.event_log_y2024m10 OWNER TO postgres;

--
-- Name: event_log_y2024m11; Type: TABLE; Schema: dev_cac; Owner: postgres
--

CREATE TABLE dev_cac.event_log_y2024m11 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_cac.event_log ATTACH PARTITION dev_cac.event_log_y2024m11 FOR VALUES FROM ('2024-11-01 00:00:00') TO ('2024-12-01 00:00:00');


ALTER TABLE dev_cac.event_log_y2024m11 OWNER TO postgres;

--
-- Name: event_log_y2024m12; Type: TABLE; Schema: dev_cac; Owner: postgres
--

CREATE TABLE dev_cac.event_log_y2024m12 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_cac.event_log ATTACH PARTITION dev_cac.event_log_y2024m12 FOR VALUES FROM ('2024-12-01 00:00:00') TO ('2025-01-01 00:00:00');


ALTER TABLE dev_cac.event_log_y2024m12 OWNER TO postgres;

--
-- Name: event_log_y2025m01; Type: TABLE; Schema: dev_cac; Owner: postgres
--

CREATE TABLE dev_cac.event_log_y2025m01 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_cac.event_log ATTACH PARTITION dev_cac.event_log_y2025m01 FOR VALUES FROM ('2025-01-01 00:00:00') TO ('2025-02-01 00:00:00');


ALTER TABLE dev_cac.event_log_y2025m01 OWNER TO postgres;

--
-- Name: event_log_y2025m02; Type: TABLE; Schema: dev_cac; Owner: postgres
--

CREATE TABLE dev_cac.event_log_y2025m02 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_cac.event_log ATTACH PARTITION dev_cac.event_log_y2025m02 FOR VALUES FROM ('2025-02-01 00:00:00') TO ('2025-03-01 00:00:00');


ALTER TABLE dev_cac.event_log_y2025m02 OWNER TO postgres;

--
-- Name: event_log_y2025m03; Type: TABLE; Schema: dev_cac; Owner: postgres
--

CREATE TABLE dev_cac.event_log_y2025m03 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_cac.event_log ATTACH PARTITION dev_cac.event_log_y2025m03 FOR VALUES FROM ('2025-03-01 00:00:00') TO ('2025-04-01 00:00:00');


ALTER TABLE dev_cac.event_log_y2025m03 OWNER TO postgres;

--
-- Name: event_log_y2025m04; Type: TABLE; Schema: dev_cac; Owner: postgres
--

CREATE TABLE dev_cac.event_log_y2025m04 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_cac.event_log ATTACH PARTITION dev_cac.event_log_y2025m04 FOR VALUES FROM ('2025-04-01 00:00:00') TO ('2025-05-01 00:00:00');


ALTER TABLE dev_cac.event_log_y2025m04 OWNER TO postgres;

--
-- Name: event_log_y2025m05; Type: TABLE; Schema: dev_cac; Owner: postgres
--

CREATE TABLE dev_cac.event_log_y2025m05 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_cac.event_log ATTACH PARTITION dev_cac.event_log_y2025m05 FOR VALUES FROM ('2025-05-01 00:00:00') TO ('2025-06-01 00:00:00');


ALTER TABLE dev_cac.event_log_y2025m05 OWNER TO postgres;

--
-- Name: event_log_y2025m06; Type: TABLE; Schema: dev_cac; Owner: postgres
--

CREATE TABLE dev_cac.event_log_y2025m06 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_cac.event_log ATTACH PARTITION dev_cac.event_log_y2025m06 FOR VALUES FROM ('2025-06-01 00:00:00') TO ('2025-07-01 00:00:00');


ALTER TABLE dev_cac.event_log_y2025m06 OWNER TO postgres;

--
-- Name: event_log_y2025m07; Type: TABLE; Schema: dev_cac; Owner: postgres
--

CREATE TABLE dev_cac.event_log_y2025m07 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_cac.event_log ATTACH PARTITION dev_cac.event_log_y2025m07 FOR VALUES FROM ('2025-07-01 00:00:00') TO ('2025-08-01 00:00:00');


ALTER TABLE dev_cac.event_log_y2025m07 OWNER TO postgres;

--
-- Name: event_log_y2025m08; Type: TABLE; Schema: dev_cac; Owner: postgres
--

CREATE TABLE dev_cac.event_log_y2025m08 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_cac.event_log ATTACH PARTITION dev_cac.event_log_y2025m08 FOR VALUES FROM ('2025-08-01 00:00:00') TO ('2025-09-01 00:00:00');


ALTER TABLE dev_cac.event_log_y2025m08 OWNER TO postgres;

--
-- Name: event_log_y2025m09; Type: TABLE; Schema: dev_cac; Owner: postgres
--

CREATE TABLE dev_cac.event_log_y2025m09 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_cac.event_log ATTACH PARTITION dev_cac.event_log_y2025m09 FOR VALUES FROM ('2025-09-01 00:00:00') TO ('2025-10-01 00:00:00');


ALTER TABLE dev_cac.event_log_y2025m09 OWNER TO postgres;

--
-- Name: event_log_y2025m10; Type: TABLE; Schema: dev_cac; Owner: postgres
--

CREATE TABLE dev_cac.event_log_y2025m10 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_cac.event_log ATTACH PARTITION dev_cac.event_log_y2025m10 FOR VALUES FROM ('2025-10-01 00:00:00') TO ('2025-11-01 00:00:00');


ALTER TABLE dev_cac.event_log_y2025m10 OWNER TO postgres;

--
-- Name: event_log_y2025m11; Type: TABLE; Schema: dev_cac; Owner: postgres
--

CREATE TABLE dev_cac.event_log_y2025m11 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_cac.event_log ATTACH PARTITION dev_cac.event_log_y2025m11 FOR VALUES FROM ('2025-11-01 00:00:00') TO ('2025-12-01 00:00:00');


ALTER TABLE dev_cac.event_log_y2025m11 OWNER TO postgres;

--
-- Name: event_log_y2025m12; Type: TABLE; Schema: dev_cac; Owner: postgres
--

CREATE TABLE dev_cac.event_log_y2025m12 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_cac.event_log ATTACH PARTITION dev_cac.event_log_y2025m12 FOR VALUES FROM ('2025-12-01 00:00:00') TO ('2026-01-01 00:00:00');


ALTER TABLE dev_cac.event_log_y2025m12 OWNER TO postgres;

--
-- Name: event_log_y2026m01; Type: TABLE; Schema: dev_cac; Owner: postgres
--

CREATE TABLE dev_cac.event_log_y2026m01 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_cac.event_log ATTACH PARTITION dev_cac.event_log_y2026m01 FOR VALUES FROM ('2026-01-01 00:00:00') TO ('2026-02-01 00:00:00');


ALTER TABLE dev_cac.event_log_y2026m01 OWNER TO postgres;

--
-- Name: event_log_y2026m02; Type: TABLE; Schema: dev_cac; Owner: postgres
--

CREATE TABLE dev_cac.event_log_y2026m02 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_cac.event_log ATTACH PARTITION dev_cac.event_log_y2026m02 FOR VALUES FROM ('2026-02-01 00:00:00') TO ('2026-03-01 00:00:00');


ALTER TABLE dev_cac.event_log_y2026m02 OWNER TO postgres;

--
-- Name: event_log_y2026m03; Type: TABLE; Schema: dev_cac; Owner: postgres
--

CREATE TABLE dev_cac.event_log_y2026m03 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_cac.event_log ATTACH PARTITION dev_cac.event_log_y2026m03 FOR VALUES FROM ('2026-03-01 00:00:00') TO ('2026-04-01 00:00:00');


ALTER TABLE dev_cac.event_log_y2026m03 OWNER TO postgres;

--
-- Name: event_log_y2026m04; Type: TABLE; Schema: dev_cac; Owner: postgres
--

CREATE TABLE dev_cac.event_log_y2026m04 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_cac.event_log ATTACH PARTITION dev_cac.event_log_y2026m04 FOR VALUES FROM ('2026-04-01 00:00:00') TO ('2026-05-01 00:00:00');


ALTER TABLE dev_cac.event_log_y2026m04 OWNER TO postgres;

--
-- Name: event_log_y2026m05; Type: TABLE; Schema: dev_cac; Owner: postgres
--

CREATE TABLE dev_cac.event_log_y2026m05 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_cac.event_log ATTACH PARTITION dev_cac.event_log_y2026m05 FOR VALUES FROM ('2026-05-01 00:00:00') TO ('2026-06-01 00:00:00');


ALTER TABLE dev_cac.event_log_y2026m05 OWNER TO postgres;

--
-- Name: event_log_y2026m06; Type: TABLE; Schema: dev_cac; Owner: postgres
--

CREATE TABLE dev_cac.event_log_y2026m06 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_cac.event_log ATTACH PARTITION dev_cac.event_log_y2026m06 FOR VALUES FROM ('2026-06-01 00:00:00') TO ('2026-07-01 00:00:00');


ALTER TABLE dev_cac.event_log_y2026m06 OWNER TO postgres;

--
-- Name: event_log_y2026m07; Type: TABLE; Schema: dev_cac; Owner: postgres
--

CREATE TABLE dev_cac.event_log_y2026m07 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_cac.event_log ATTACH PARTITION dev_cac.event_log_y2026m07 FOR VALUES FROM ('2026-07-01 00:00:00') TO ('2026-08-01 00:00:00');


ALTER TABLE dev_cac.event_log_y2026m07 OWNER TO postgres;

--
-- Name: event_log_y2026m08; Type: TABLE; Schema: dev_cac; Owner: postgres
--

CREATE TABLE dev_cac.event_log_y2026m08 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_cac.event_log ATTACH PARTITION dev_cac.event_log_y2026m08 FOR VALUES FROM ('2026-08-01 00:00:00') TO ('2026-09-01 00:00:00');


ALTER TABLE dev_cac.event_log_y2026m08 OWNER TO postgres;

--
-- Name: event_log_y2026m09; Type: TABLE; Schema: dev_cac; Owner: postgres
--

CREATE TABLE dev_cac.event_log_y2026m09 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_cac.event_log ATTACH PARTITION dev_cac.event_log_y2026m09 FOR VALUES FROM ('2026-09-01 00:00:00') TO ('2026-10-01 00:00:00');


ALTER TABLE dev_cac.event_log_y2026m09 OWNER TO postgres;

--
-- Name: event_log_y2026m10; Type: TABLE; Schema: dev_cac; Owner: postgres
--

CREATE TABLE dev_cac.event_log_y2026m10 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_cac.event_log ATTACH PARTITION dev_cac.event_log_y2026m10 FOR VALUES FROM ('2026-10-01 00:00:00') TO ('2026-11-01 00:00:00');


ALTER TABLE dev_cac.event_log_y2026m10 OWNER TO postgres;

--
-- Name: event_log_y2026m11; Type: TABLE; Schema: dev_cac; Owner: postgres
--

CREATE TABLE dev_cac.event_log_y2026m11 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_cac.event_log ATTACH PARTITION dev_cac.event_log_y2026m11 FOR VALUES FROM ('2026-11-01 00:00:00') TO ('2026-12-01 00:00:00');


ALTER TABLE dev_cac.event_log_y2026m11 OWNER TO postgres;

--
-- Name: event_log_y2026m12; Type: TABLE; Schema: dev_cac; Owner: postgres
--

CREATE TABLE dev_cac.event_log_y2026m12 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_cac.event_log ATTACH PARTITION dev_cac.event_log_y2026m12 FOR VALUES FROM ('2026-12-01 00:00:00') TO ('2027-01-01 00:00:00');


ALTER TABLE dev_cac.event_log_y2026m12 OWNER TO postgres;

--
-- Name: functions; Type: TABLE; Schema: dev_cac; Owner: postgres
--

CREATE TABLE dev_cac.functions (
    function_name text NOT NULL,
    published_code text,
    draft_code text NOT NULL,
    function_description text NOT NULL,
    published_runtime_version character varying(16),
    draft_runtime_version character varying(16) NOT NULL,
    published_at timestamp without time zone,
    draft_edited_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    published_by text,
    draft_edited_by text NOT NULL
);


ALTER TABLE dev_cac.functions OWNER TO postgres;

--
-- Name: event_log; Type: TABLE; Schema: dev_experimentation; Owner: postgres
--

CREATE TABLE dev_experimentation.event_log (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
)
PARTITION BY RANGE ("timestamp");


ALTER TABLE dev_experimentation.event_log OWNER TO postgres;

--
-- Name: event_log_y2023m08; Type: TABLE; Schema: dev_experimentation; Owner: postgres
--

CREATE TABLE dev_experimentation.event_log_y2023m08 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_experimentation.event_log ATTACH PARTITION dev_experimentation.event_log_y2023m08 FOR VALUES FROM ('2023-08-01 00:00:00') TO ('2023-09-01 00:00:00');


ALTER TABLE dev_experimentation.event_log_y2023m08 OWNER TO postgres;

--
-- Name: event_log_y2023m09; Type: TABLE; Schema: dev_experimentation; Owner: postgres
--

CREATE TABLE dev_experimentation.event_log_y2023m09 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_experimentation.event_log ATTACH PARTITION dev_experimentation.event_log_y2023m09 FOR VALUES FROM ('2023-09-01 00:00:00') TO ('2023-10-01 00:00:00');


ALTER TABLE dev_experimentation.event_log_y2023m09 OWNER TO postgres;

--
-- Name: event_log_y2023m10; Type: TABLE; Schema: dev_experimentation; Owner: postgres
--

CREATE TABLE dev_experimentation.event_log_y2023m10 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_experimentation.event_log ATTACH PARTITION dev_experimentation.event_log_y2023m10 FOR VALUES FROM ('2023-10-01 00:00:00') TO ('2023-11-01 00:00:00');


ALTER TABLE dev_experimentation.event_log_y2023m10 OWNER TO postgres;

--
-- Name: event_log_y2023m11; Type: TABLE; Schema: dev_experimentation; Owner: postgres
--

CREATE TABLE dev_experimentation.event_log_y2023m11 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_experimentation.event_log ATTACH PARTITION dev_experimentation.event_log_y2023m11 FOR VALUES FROM ('2023-11-01 00:00:00') TO ('2023-12-01 00:00:00');


ALTER TABLE dev_experimentation.event_log_y2023m11 OWNER TO postgres;

--
-- Name: event_log_y2023m12; Type: TABLE; Schema: dev_experimentation; Owner: postgres
--

CREATE TABLE dev_experimentation.event_log_y2023m12 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_experimentation.event_log ATTACH PARTITION dev_experimentation.event_log_y2023m12 FOR VALUES FROM ('2023-12-01 00:00:00') TO ('2024-01-01 00:00:00');


ALTER TABLE dev_experimentation.event_log_y2023m12 OWNER TO postgres;

--
-- Name: event_log_y2024m01; Type: TABLE; Schema: dev_experimentation; Owner: postgres
--

CREATE TABLE dev_experimentation.event_log_y2024m01 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_experimentation.event_log ATTACH PARTITION dev_experimentation.event_log_y2024m01 FOR VALUES FROM ('2024-01-01 00:00:00') TO ('2024-02-01 00:00:00');


ALTER TABLE dev_experimentation.event_log_y2024m01 OWNER TO postgres;

--
-- Name: event_log_y2024m02; Type: TABLE; Schema: dev_experimentation; Owner: postgres
--

CREATE TABLE dev_experimentation.event_log_y2024m02 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_experimentation.event_log ATTACH PARTITION dev_experimentation.event_log_y2024m02 FOR VALUES FROM ('2024-02-01 00:00:00') TO ('2024-03-01 00:00:00');


ALTER TABLE dev_experimentation.event_log_y2024m02 OWNER TO postgres;

--
-- Name: event_log_y2024m03; Type: TABLE; Schema: dev_experimentation; Owner: postgres
--

CREATE TABLE dev_experimentation.event_log_y2024m03 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_experimentation.event_log ATTACH PARTITION dev_experimentation.event_log_y2024m03 FOR VALUES FROM ('2024-03-01 00:00:00') TO ('2024-04-01 00:00:00');


ALTER TABLE dev_experimentation.event_log_y2024m03 OWNER TO postgres;

--
-- Name: event_log_y2024m04; Type: TABLE; Schema: dev_experimentation; Owner: postgres
--

CREATE TABLE dev_experimentation.event_log_y2024m04 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_experimentation.event_log ATTACH PARTITION dev_experimentation.event_log_y2024m04 FOR VALUES FROM ('2024-04-01 00:00:00') TO ('2024-05-01 00:00:00');


ALTER TABLE dev_experimentation.event_log_y2024m04 OWNER TO postgres;

--
-- Name: event_log_y2024m05; Type: TABLE; Schema: dev_experimentation; Owner: postgres
--

CREATE TABLE dev_experimentation.event_log_y2024m05 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_experimentation.event_log ATTACH PARTITION dev_experimentation.event_log_y2024m05 FOR VALUES FROM ('2024-05-01 00:00:00') TO ('2024-06-01 00:00:00');


ALTER TABLE dev_experimentation.event_log_y2024m05 OWNER TO postgres;

--
-- Name: event_log_y2024m06; Type: TABLE; Schema: dev_experimentation; Owner: postgres
--

CREATE TABLE dev_experimentation.event_log_y2024m06 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_experimentation.event_log ATTACH PARTITION dev_experimentation.event_log_y2024m06 FOR VALUES FROM ('2024-06-01 00:00:00') TO ('2024-07-01 00:00:00');


ALTER TABLE dev_experimentation.event_log_y2024m06 OWNER TO postgres;

--
-- Name: event_log_y2024m07; Type: TABLE; Schema: dev_experimentation; Owner: postgres
--

CREATE TABLE dev_experimentation.event_log_y2024m07 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_experimentation.event_log ATTACH PARTITION dev_experimentation.event_log_y2024m07 FOR VALUES FROM ('2024-07-01 00:00:00') TO ('2024-08-01 00:00:00');


ALTER TABLE dev_experimentation.event_log_y2024m07 OWNER TO postgres;

--
-- Name: event_log_y2024m08; Type: TABLE; Schema: dev_experimentation; Owner: postgres
--

CREATE TABLE dev_experimentation.event_log_y2024m08 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_experimentation.event_log ATTACH PARTITION dev_experimentation.event_log_y2024m08 FOR VALUES FROM ('2024-08-01 00:00:00') TO ('2024-09-01 00:00:00');


ALTER TABLE dev_experimentation.event_log_y2024m08 OWNER TO postgres;

--
-- Name: event_log_y2024m09; Type: TABLE; Schema: dev_experimentation; Owner: postgres
--

CREATE TABLE dev_experimentation.event_log_y2024m09 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_experimentation.event_log ATTACH PARTITION dev_experimentation.event_log_y2024m09 FOR VALUES FROM ('2024-09-01 00:00:00') TO ('2024-10-01 00:00:00');


ALTER TABLE dev_experimentation.event_log_y2024m09 OWNER TO postgres;

--
-- Name: event_log_y2024m10; Type: TABLE; Schema: dev_experimentation; Owner: postgres
--

CREATE TABLE dev_experimentation.event_log_y2024m10 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_experimentation.event_log ATTACH PARTITION dev_experimentation.event_log_y2024m10 FOR VALUES FROM ('2024-10-01 00:00:00') TO ('2024-11-01 00:00:00');


ALTER TABLE dev_experimentation.event_log_y2024m10 OWNER TO postgres;

--
-- Name: event_log_y2024m11; Type: TABLE; Schema: dev_experimentation; Owner: postgres
--

CREATE TABLE dev_experimentation.event_log_y2024m11 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_experimentation.event_log ATTACH PARTITION dev_experimentation.event_log_y2024m11 FOR VALUES FROM ('2024-11-01 00:00:00') TO ('2024-12-01 00:00:00');


ALTER TABLE dev_experimentation.event_log_y2024m11 OWNER TO postgres;

--
-- Name: event_log_y2024m12; Type: TABLE; Schema: dev_experimentation; Owner: postgres
--

CREATE TABLE dev_experimentation.event_log_y2024m12 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_experimentation.event_log ATTACH PARTITION dev_experimentation.event_log_y2024m12 FOR VALUES FROM ('2024-12-01 00:00:00') TO ('2025-01-01 00:00:00');


ALTER TABLE dev_experimentation.event_log_y2024m12 OWNER TO postgres;

--
-- Name: event_log_y2025m01; Type: TABLE; Schema: dev_experimentation; Owner: postgres
--

CREATE TABLE dev_experimentation.event_log_y2025m01 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_experimentation.event_log ATTACH PARTITION dev_experimentation.event_log_y2025m01 FOR VALUES FROM ('2025-01-01 00:00:00') TO ('2025-02-01 00:00:00');


ALTER TABLE dev_experimentation.event_log_y2025m01 OWNER TO postgres;

--
-- Name: event_log_y2025m02; Type: TABLE; Schema: dev_experimentation; Owner: postgres
--

CREATE TABLE dev_experimentation.event_log_y2025m02 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_experimentation.event_log ATTACH PARTITION dev_experimentation.event_log_y2025m02 FOR VALUES FROM ('2025-02-01 00:00:00') TO ('2025-03-01 00:00:00');


ALTER TABLE dev_experimentation.event_log_y2025m02 OWNER TO postgres;

--
-- Name: event_log_y2025m03; Type: TABLE; Schema: dev_experimentation; Owner: postgres
--

CREATE TABLE dev_experimentation.event_log_y2025m03 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_experimentation.event_log ATTACH PARTITION dev_experimentation.event_log_y2025m03 FOR VALUES FROM ('2025-03-01 00:00:00') TO ('2025-04-01 00:00:00');


ALTER TABLE dev_experimentation.event_log_y2025m03 OWNER TO postgres;

--
-- Name: event_log_y2025m04; Type: TABLE; Schema: dev_experimentation; Owner: postgres
--

CREATE TABLE dev_experimentation.event_log_y2025m04 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_experimentation.event_log ATTACH PARTITION dev_experimentation.event_log_y2025m04 FOR VALUES FROM ('2025-04-01 00:00:00') TO ('2025-05-01 00:00:00');


ALTER TABLE dev_experimentation.event_log_y2025m04 OWNER TO postgres;

--
-- Name: event_log_y2025m05; Type: TABLE; Schema: dev_experimentation; Owner: postgres
--

CREATE TABLE dev_experimentation.event_log_y2025m05 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_experimentation.event_log ATTACH PARTITION dev_experimentation.event_log_y2025m05 FOR VALUES FROM ('2025-05-01 00:00:00') TO ('2025-06-01 00:00:00');


ALTER TABLE dev_experimentation.event_log_y2025m05 OWNER TO postgres;

--
-- Name: event_log_y2025m06; Type: TABLE; Schema: dev_experimentation; Owner: postgres
--

CREATE TABLE dev_experimentation.event_log_y2025m06 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_experimentation.event_log ATTACH PARTITION dev_experimentation.event_log_y2025m06 FOR VALUES FROM ('2025-06-01 00:00:00') TO ('2025-07-01 00:00:00');


ALTER TABLE dev_experimentation.event_log_y2025m06 OWNER TO postgres;

--
-- Name: event_log_y2025m07; Type: TABLE; Schema: dev_experimentation; Owner: postgres
--

CREATE TABLE dev_experimentation.event_log_y2025m07 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_experimentation.event_log ATTACH PARTITION dev_experimentation.event_log_y2025m07 FOR VALUES FROM ('2025-07-01 00:00:00') TO ('2025-08-01 00:00:00');


ALTER TABLE dev_experimentation.event_log_y2025m07 OWNER TO postgres;

--
-- Name: event_log_y2025m08; Type: TABLE; Schema: dev_experimentation; Owner: postgres
--

CREATE TABLE dev_experimentation.event_log_y2025m08 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_experimentation.event_log ATTACH PARTITION dev_experimentation.event_log_y2025m08 FOR VALUES FROM ('2025-08-01 00:00:00') TO ('2025-09-01 00:00:00');


ALTER TABLE dev_experimentation.event_log_y2025m08 OWNER TO postgres;

--
-- Name: event_log_y2025m09; Type: TABLE; Schema: dev_experimentation; Owner: postgres
--

CREATE TABLE dev_experimentation.event_log_y2025m09 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_experimentation.event_log ATTACH PARTITION dev_experimentation.event_log_y2025m09 FOR VALUES FROM ('2025-09-01 00:00:00') TO ('2025-10-01 00:00:00');


ALTER TABLE dev_experimentation.event_log_y2025m09 OWNER TO postgres;

--
-- Name: event_log_y2025m10; Type: TABLE; Schema: dev_experimentation; Owner: postgres
--

CREATE TABLE dev_experimentation.event_log_y2025m10 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_experimentation.event_log ATTACH PARTITION dev_experimentation.event_log_y2025m10 FOR VALUES FROM ('2025-10-01 00:00:00') TO ('2025-11-01 00:00:00');


ALTER TABLE dev_experimentation.event_log_y2025m10 OWNER TO postgres;

--
-- Name: event_log_y2025m11; Type: TABLE; Schema: dev_experimentation; Owner: postgres
--

CREATE TABLE dev_experimentation.event_log_y2025m11 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_experimentation.event_log ATTACH PARTITION dev_experimentation.event_log_y2025m11 FOR VALUES FROM ('2025-11-01 00:00:00') TO ('2025-12-01 00:00:00');


ALTER TABLE dev_experimentation.event_log_y2025m11 OWNER TO postgres;

--
-- Name: event_log_y2025m12; Type: TABLE; Schema: dev_experimentation; Owner: postgres
--

CREATE TABLE dev_experimentation.event_log_y2025m12 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_experimentation.event_log ATTACH PARTITION dev_experimentation.event_log_y2025m12 FOR VALUES FROM ('2025-12-01 00:00:00') TO ('2026-01-01 00:00:00');


ALTER TABLE dev_experimentation.event_log_y2025m12 OWNER TO postgres;

--
-- Name: event_log_y2026m01; Type: TABLE; Schema: dev_experimentation; Owner: postgres
--

CREATE TABLE dev_experimentation.event_log_y2026m01 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_experimentation.event_log ATTACH PARTITION dev_experimentation.event_log_y2026m01 FOR VALUES FROM ('2026-01-01 00:00:00') TO ('2026-02-01 00:00:00');


ALTER TABLE dev_experimentation.event_log_y2026m01 OWNER TO postgres;

--
-- Name: event_log_y2026m02; Type: TABLE; Schema: dev_experimentation; Owner: postgres
--

CREATE TABLE dev_experimentation.event_log_y2026m02 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_experimentation.event_log ATTACH PARTITION dev_experimentation.event_log_y2026m02 FOR VALUES FROM ('2026-02-01 00:00:00') TO ('2026-03-01 00:00:00');


ALTER TABLE dev_experimentation.event_log_y2026m02 OWNER TO postgres;

--
-- Name: event_log_y2026m03; Type: TABLE; Schema: dev_experimentation; Owner: postgres
--

CREATE TABLE dev_experimentation.event_log_y2026m03 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_experimentation.event_log ATTACH PARTITION dev_experimentation.event_log_y2026m03 FOR VALUES FROM ('2026-03-01 00:00:00') TO ('2026-04-01 00:00:00');


ALTER TABLE dev_experimentation.event_log_y2026m03 OWNER TO postgres;

--
-- Name: event_log_y2026m04; Type: TABLE; Schema: dev_experimentation; Owner: postgres
--

CREATE TABLE dev_experimentation.event_log_y2026m04 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_experimentation.event_log ATTACH PARTITION dev_experimentation.event_log_y2026m04 FOR VALUES FROM ('2026-04-01 00:00:00') TO ('2026-05-01 00:00:00');


ALTER TABLE dev_experimentation.event_log_y2026m04 OWNER TO postgres;

--
-- Name: event_log_y2026m05; Type: TABLE; Schema: dev_experimentation; Owner: postgres
--

CREATE TABLE dev_experimentation.event_log_y2026m05 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_experimentation.event_log ATTACH PARTITION dev_experimentation.event_log_y2026m05 FOR VALUES FROM ('2026-05-01 00:00:00') TO ('2026-06-01 00:00:00');


ALTER TABLE dev_experimentation.event_log_y2026m05 OWNER TO postgres;

--
-- Name: event_log_y2026m06; Type: TABLE; Schema: dev_experimentation; Owner: postgres
--

CREATE TABLE dev_experimentation.event_log_y2026m06 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_experimentation.event_log ATTACH PARTITION dev_experimentation.event_log_y2026m06 FOR VALUES FROM ('2026-06-01 00:00:00') TO ('2026-07-01 00:00:00');


ALTER TABLE dev_experimentation.event_log_y2026m06 OWNER TO postgres;

--
-- Name: event_log_y2026m07; Type: TABLE; Schema: dev_experimentation; Owner: postgres
--

CREATE TABLE dev_experimentation.event_log_y2026m07 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_experimentation.event_log ATTACH PARTITION dev_experimentation.event_log_y2026m07 FOR VALUES FROM ('2026-07-01 00:00:00') TO ('2026-08-01 00:00:00');


ALTER TABLE dev_experimentation.event_log_y2026m07 OWNER TO postgres;

--
-- Name: event_log_y2026m08; Type: TABLE; Schema: dev_experimentation; Owner: postgres
--

CREATE TABLE dev_experimentation.event_log_y2026m08 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_experimentation.event_log ATTACH PARTITION dev_experimentation.event_log_y2026m08 FOR VALUES FROM ('2026-08-01 00:00:00') TO ('2026-09-01 00:00:00');


ALTER TABLE dev_experimentation.event_log_y2026m08 OWNER TO postgres;

--
-- Name: event_log_y2026m09; Type: TABLE; Schema: dev_experimentation; Owner: postgres
--

CREATE TABLE dev_experimentation.event_log_y2026m09 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_experimentation.event_log ATTACH PARTITION dev_experimentation.event_log_y2026m09 FOR VALUES FROM ('2026-09-01 00:00:00') TO ('2026-10-01 00:00:00');


ALTER TABLE dev_experimentation.event_log_y2026m09 OWNER TO postgres;

--
-- Name: event_log_y2026m10; Type: TABLE; Schema: dev_experimentation; Owner: postgres
--

CREATE TABLE dev_experimentation.event_log_y2026m10 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_experimentation.event_log ATTACH PARTITION dev_experimentation.event_log_y2026m10 FOR VALUES FROM ('2026-10-01 00:00:00') TO ('2026-11-01 00:00:00');


ALTER TABLE dev_experimentation.event_log_y2026m10 OWNER TO postgres;

--
-- Name: event_log_y2026m11; Type: TABLE; Schema: dev_experimentation; Owner: postgres
--

CREATE TABLE dev_experimentation.event_log_y2026m11 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_experimentation.event_log ATTACH PARTITION dev_experimentation.event_log_y2026m11 FOR VALUES FROM ('2026-11-01 00:00:00') TO ('2026-12-01 00:00:00');


ALTER TABLE dev_experimentation.event_log_y2026m11 OWNER TO postgres;

--
-- Name: event_log_y2026m12; Type: TABLE; Schema: dev_experimentation; Owner: postgres
--

CREATE TABLE dev_experimentation.event_log_y2026m12 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY dev_experimentation.event_log ATTACH PARTITION dev_experimentation.event_log_y2026m12 FOR VALUES FROM ('2026-12-01 00:00:00') TO ('2027-01-01 00:00:00');


ALTER TABLE dev_experimentation.event_log_y2026m12 OWNER TO postgres;

--
-- Name: experiments; Type: TABLE; Schema: dev_experimentation; Owner: postgres
--

CREATE TABLE dev_experimentation.experiments (
    id bigint NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_by text NOT NULL,
    last_modified timestamp with time zone DEFAULT now() NOT NULL,
    name text NOT NULL,
    override_keys dev_experimentation.not_null_text[] NOT NULL,
    status dev_experimentation.experiment_status_type NOT NULL,
    traffic_percentage integer NOT NULL,
    context json NOT NULL,
    variants json NOT NULL,
    last_modified_by text DEFAULT 'Null'::text NOT NULL,
    chosen_variant text,
    CONSTRAINT experiments_traffic_percentage_check CHECK ((traffic_percentage >= 0))
);


ALTER TABLE dev_experimentation.experiments OWNER TO postgres;

--
-- Name: contexts; Type: TABLE; Schema: test_cac; Owner: postgres
--

CREATE TABLE test_cac.contexts (
    id character varying NOT NULL,
    value json NOT NULL,
    override_id character varying NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_by character varying NOT NULL,
    priority integer DEFAULT 1 NOT NULL,
    override json DEFAULT '{}'::json NOT NULL
);


ALTER TABLE test_cac.contexts OWNER TO postgres;

--
-- Name: default_configs; Type: TABLE; Schema: test_cac; Owner: postgres
--

CREATE TABLE test_cac.default_configs (
    key character varying NOT NULL,
    value json NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_by character varying NOT NULL,
    schema json DEFAULT '{}'::json NOT NULL,
    function_name text
);


ALTER TABLE test_cac.default_configs OWNER TO postgres;

--
-- Name: dimensions; Type: TABLE; Schema: test_cac; Owner: postgres
--

CREATE TABLE test_cac.dimensions (
    dimension character varying NOT NULL,
    priority integer NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_by character varying NOT NULL,
    schema json DEFAULT '{}'::json NOT NULL,
    function_name text
);


ALTER TABLE test_cac.dimensions OWNER TO postgres;

--
-- Name: event_log; Type: TABLE; Schema: test_cac; Owner: postgres
--

CREATE TABLE test_cac.event_log (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
)
PARTITION BY RANGE ("timestamp");


ALTER TABLE test_cac.event_log OWNER TO postgres;

--
-- Name: event_log_y2023m08; Type: TABLE; Schema: test_cac; Owner: postgres
--

CREATE TABLE test_cac.event_log_y2023m08 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_cac.event_log ATTACH PARTITION test_cac.event_log_y2023m08 FOR VALUES FROM ('2023-08-01 00:00:00') TO ('2023-09-01 00:00:00');


ALTER TABLE test_cac.event_log_y2023m08 OWNER TO postgres;

--
-- Name: event_log_y2023m09; Type: TABLE; Schema: test_cac; Owner: postgres
--

CREATE TABLE test_cac.event_log_y2023m09 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_cac.event_log ATTACH PARTITION test_cac.event_log_y2023m09 FOR VALUES FROM ('2023-09-01 00:00:00') TO ('2023-10-01 00:00:00');


ALTER TABLE test_cac.event_log_y2023m09 OWNER TO postgres;

--
-- Name: event_log_y2023m10; Type: TABLE; Schema: test_cac; Owner: postgres
--

CREATE TABLE test_cac.event_log_y2023m10 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_cac.event_log ATTACH PARTITION test_cac.event_log_y2023m10 FOR VALUES FROM ('2023-10-01 00:00:00') TO ('2023-11-01 00:00:00');


ALTER TABLE test_cac.event_log_y2023m10 OWNER TO postgres;

--
-- Name: event_log_y2023m11; Type: TABLE; Schema: test_cac; Owner: postgres
--

CREATE TABLE test_cac.event_log_y2023m11 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_cac.event_log ATTACH PARTITION test_cac.event_log_y2023m11 FOR VALUES FROM ('2023-11-01 00:00:00') TO ('2023-12-01 00:00:00');


ALTER TABLE test_cac.event_log_y2023m11 OWNER TO postgres;

--
-- Name: event_log_y2023m12; Type: TABLE; Schema: test_cac; Owner: postgres
--

CREATE TABLE test_cac.event_log_y2023m12 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_cac.event_log ATTACH PARTITION test_cac.event_log_y2023m12 FOR VALUES FROM ('2023-12-01 00:00:00') TO ('2024-01-01 00:00:00');


ALTER TABLE test_cac.event_log_y2023m12 OWNER TO postgres;

--
-- Name: event_log_y2024m01; Type: TABLE; Schema: test_cac; Owner: postgres
--

CREATE TABLE test_cac.event_log_y2024m01 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_cac.event_log ATTACH PARTITION test_cac.event_log_y2024m01 FOR VALUES FROM ('2024-01-01 00:00:00') TO ('2024-02-01 00:00:00');


ALTER TABLE test_cac.event_log_y2024m01 OWNER TO postgres;

--
-- Name: event_log_y2024m02; Type: TABLE; Schema: test_cac; Owner: postgres
--

CREATE TABLE test_cac.event_log_y2024m02 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_cac.event_log ATTACH PARTITION test_cac.event_log_y2024m02 FOR VALUES FROM ('2024-02-01 00:00:00') TO ('2024-03-01 00:00:00');


ALTER TABLE test_cac.event_log_y2024m02 OWNER TO postgres;

--
-- Name: event_log_y2024m03; Type: TABLE; Schema: test_cac; Owner: postgres
--

CREATE TABLE test_cac.event_log_y2024m03 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_cac.event_log ATTACH PARTITION test_cac.event_log_y2024m03 FOR VALUES FROM ('2024-03-01 00:00:00') TO ('2024-04-01 00:00:00');


ALTER TABLE test_cac.event_log_y2024m03 OWNER TO postgres;

--
-- Name: event_log_y2024m04; Type: TABLE; Schema: test_cac; Owner: postgres
--

CREATE TABLE test_cac.event_log_y2024m04 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_cac.event_log ATTACH PARTITION test_cac.event_log_y2024m04 FOR VALUES FROM ('2024-04-01 00:00:00') TO ('2024-05-01 00:00:00');


ALTER TABLE test_cac.event_log_y2024m04 OWNER TO postgres;

--
-- Name: event_log_y2024m05; Type: TABLE; Schema: test_cac; Owner: postgres
--

CREATE TABLE test_cac.event_log_y2024m05 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_cac.event_log ATTACH PARTITION test_cac.event_log_y2024m05 FOR VALUES FROM ('2024-05-01 00:00:00') TO ('2024-06-01 00:00:00');


ALTER TABLE test_cac.event_log_y2024m05 OWNER TO postgres;

--
-- Name: event_log_y2024m06; Type: TABLE; Schema: test_cac; Owner: postgres
--

CREATE TABLE test_cac.event_log_y2024m06 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_cac.event_log ATTACH PARTITION test_cac.event_log_y2024m06 FOR VALUES FROM ('2024-06-01 00:00:00') TO ('2024-07-01 00:00:00');


ALTER TABLE test_cac.event_log_y2024m06 OWNER TO postgres;

--
-- Name: event_log_y2024m07; Type: TABLE; Schema: test_cac; Owner: postgres
--

CREATE TABLE test_cac.event_log_y2024m07 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_cac.event_log ATTACH PARTITION test_cac.event_log_y2024m07 FOR VALUES FROM ('2024-07-01 00:00:00') TO ('2024-08-01 00:00:00');


ALTER TABLE test_cac.event_log_y2024m07 OWNER TO postgres;

--
-- Name: event_log_y2024m08; Type: TABLE; Schema: test_cac; Owner: postgres
--

CREATE TABLE test_cac.event_log_y2024m08 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_cac.event_log ATTACH PARTITION test_cac.event_log_y2024m08 FOR VALUES FROM ('2024-08-01 00:00:00') TO ('2024-09-01 00:00:00');


ALTER TABLE test_cac.event_log_y2024m08 OWNER TO postgres;

--
-- Name: event_log_y2024m09; Type: TABLE; Schema: test_cac; Owner: postgres
--

CREATE TABLE test_cac.event_log_y2024m09 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_cac.event_log ATTACH PARTITION test_cac.event_log_y2024m09 FOR VALUES FROM ('2024-09-01 00:00:00') TO ('2024-10-01 00:00:00');


ALTER TABLE test_cac.event_log_y2024m09 OWNER TO postgres;

--
-- Name: event_log_y2024m10; Type: TABLE; Schema: test_cac; Owner: postgres
--

CREATE TABLE test_cac.event_log_y2024m10 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_cac.event_log ATTACH PARTITION test_cac.event_log_y2024m10 FOR VALUES FROM ('2024-10-01 00:00:00') TO ('2024-11-01 00:00:00');


ALTER TABLE test_cac.event_log_y2024m10 OWNER TO postgres;

--
-- Name: event_log_y2024m11; Type: TABLE; Schema: test_cac; Owner: postgres
--

CREATE TABLE test_cac.event_log_y2024m11 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_cac.event_log ATTACH PARTITION test_cac.event_log_y2024m11 FOR VALUES FROM ('2024-11-01 00:00:00') TO ('2024-12-01 00:00:00');


ALTER TABLE test_cac.event_log_y2024m11 OWNER TO postgres;

--
-- Name: event_log_y2024m12; Type: TABLE; Schema: test_cac; Owner: postgres
--

CREATE TABLE test_cac.event_log_y2024m12 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_cac.event_log ATTACH PARTITION test_cac.event_log_y2024m12 FOR VALUES FROM ('2024-12-01 00:00:00') TO ('2025-01-01 00:00:00');


ALTER TABLE test_cac.event_log_y2024m12 OWNER TO postgres;

--
-- Name: event_log_y2025m01; Type: TABLE; Schema: test_cac; Owner: postgres
--

CREATE TABLE test_cac.event_log_y2025m01 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_cac.event_log ATTACH PARTITION test_cac.event_log_y2025m01 FOR VALUES FROM ('2025-01-01 00:00:00') TO ('2025-02-01 00:00:00');


ALTER TABLE test_cac.event_log_y2025m01 OWNER TO postgres;

--
-- Name: event_log_y2025m02; Type: TABLE; Schema: test_cac; Owner: postgres
--

CREATE TABLE test_cac.event_log_y2025m02 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_cac.event_log ATTACH PARTITION test_cac.event_log_y2025m02 FOR VALUES FROM ('2025-02-01 00:00:00') TO ('2025-03-01 00:00:00');


ALTER TABLE test_cac.event_log_y2025m02 OWNER TO postgres;

--
-- Name: event_log_y2025m03; Type: TABLE; Schema: test_cac; Owner: postgres
--

CREATE TABLE test_cac.event_log_y2025m03 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_cac.event_log ATTACH PARTITION test_cac.event_log_y2025m03 FOR VALUES FROM ('2025-03-01 00:00:00') TO ('2025-04-01 00:00:00');


ALTER TABLE test_cac.event_log_y2025m03 OWNER TO postgres;

--
-- Name: event_log_y2025m04; Type: TABLE; Schema: test_cac; Owner: postgres
--

CREATE TABLE test_cac.event_log_y2025m04 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_cac.event_log ATTACH PARTITION test_cac.event_log_y2025m04 FOR VALUES FROM ('2025-04-01 00:00:00') TO ('2025-05-01 00:00:00');


ALTER TABLE test_cac.event_log_y2025m04 OWNER TO postgres;

--
-- Name: event_log_y2025m05; Type: TABLE; Schema: test_cac; Owner: postgres
--

CREATE TABLE test_cac.event_log_y2025m05 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_cac.event_log ATTACH PARTITION test_cac.event_log_y2025m05 FOR VALUES FROM ('2025-05-01 00:00:00') TO ('2025-06-01 00:00:00');


ALTER TABLE test_cac.event_log_y2025m05 OWNER TO postgres;

--
-- Name: event_log_y2025m06; Type: TABLE; Schema: test_cac; Owner: postgres
--

CREATE TABLE test_cac.event_log_y2025m06 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_cac.event_log ATTACH PARTITION test_cac.event_log_y2025m06 FOR VALUES FROM ('2025-06-01 00:00:00') TO ('2025-07-01 00:00:00');


ALTER TABLE test_cac.event_log_y2025m06 OWNER TO postgres;

--
-- Name: event_log_y2025m07; Type: TABLE; Schema: test_cac; Owner: postgres
--

CREATE TABLE test_cac.event_log_y2025m07 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_cac.event_log ATTACH PARTITION test_cac.event_log_y2025m07 FOR VALUES FROM ('2025-07-01 00:00:00') TO ('2025-08-01 00:00:00');


ALTER TABLE test_cac.event_log_y2025m07 OWNER TO postgres;

--
-- Name: event_log_y2025m08; Type: TABLE; Schema: test_cac; Owner: postgres
--

CREATE TABLE test_cac.event_log_y2025m08 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_cac.event_log ATTACH PARTITION test_cac.event_log_y2025m08 FOR VALUES FROM ('2025-08-01 00:00:00') TO ('2025-09-01 00:00:00');


ALTER TABLE test_cac.event_log_y2025m08 OWNER TO postgres;

--
-- Name: event_log_y2025m09; Type: TABLE; Schema: test_cac; Owner: postgres
--

CREATE TABLE test_cac.event_log_y2025m09 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_cac.event_log ATTACH PARTITION test_cac.event_log_y2025m09 FOR VALUES FROM ('2025-09-01 00:00:00') TO ('2025-10-01 00:00:00');


ALTER TABLE test_cac.event_log_y2025m09 OWNER TO postgres;

--
-- Name: event_log_y2025m10; Type: TABLE; Schema: test_cac; Owner: postgres
--

CREATE TABLE test_cac.event_log_y2025m10 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_cac.event_log ATTACH PARTITION test_cac.event_log_y2025m10 FOR VALUES FROM ('2025-10-01 00:00:00') TO ('2025-11-01 00:00:00');


ALTER TABLE test_cac.event_log_y2025m10 OWNER TO postgres;

--
-- Name: event_log_y2025m11; Type: TABLE; Schema: test_cac; Owner: postgres
--

CREATE TABLE test_cac.event_log_y2025m11 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_cac.event_log ATTACH PARTITION test_cac.event_log_y2025m11 FOR VALUES FROM ('2025-11-01 00:00:00') TO ('2025-12-01 00:00:00');


ALTER TABLE test_cac.event_log_y2025m11 OWNER TO postgres;

--
-- Name: event_log_y2025m12; Type: TABLE; Schema: test_cac; Owner: postgres
--

CREATE TABLE test_cac.event_log_y2025m12 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_cac.event_log ATTACH PARTITION test_cac.event_log_y2025m12 FOR VALUES FROM ('2025-12-01 00:00:00') TO ('2026-01-01 00:00:00');


ALTER TABLE test_cac.event_log_y2025m12 OWNER TO postgres;

--
-- Name: event_log_y2026m01; Type: TABLE; Schema: test_cac; Owner: postgres
--

CREATE TABLE test_cac.event_log_y2026m01 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_cac.event_log ATTACH PARTITION test_cac.event_log_y2026m01 FOR VALUES FROM ('2026-01-01 00:00:00') TO ('2026-02-01 00:00:00');


ALTER TABLE test_cac.event_log_y2026m01 OWNER TO postgres;

--
-- Name: event_log_y2026m02; Type: TABLE; Schema: test_cac; Owner: postgres
--

CREATE TABLE test_cac.event_log_y2026m02 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_cac.event_log ATTACH PARTITION test_cac.event_log_y2026m02 FOR VALUES FROM ('2026-02-01 00:00:00') TO ('2026-03-01 00:00:00');


ALTER TABLE test_cac.event_log_y2026m02 OWNER TO postgres;

--
-- Name: event_log_y2026m03; Type: TABLE; Schema: test_cac; Owner: postgres
--

CREATE TABLE test_cac.event_log_y2026m03 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_cac.event_log ATTACH PARTITION test_cac.event_log_y2026m03 FOR VALUES FROM ('2026-03-01 00:00:00') TO ('2026-04-01 00:00:00');


ALTER TABLE test_cac.event_log_y2026m03 OWNER TO postgres;

--
-- Name: event_log_y2026m04; Type: TABLE; Schema: test_cac; Owner: postgres
--

CREATE TABLE test_cac.event_log_y2026m04 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_cac.event_log ATTACH PARTITION test_cac.event_log_y2026m04 FOR VALUES FROM ('2026-04-01 00:00:00') TO ('2026-05-01 00:00:00');


ALTER TABLE test_cac.event_log_y2026m04 OWNER TO postgres;

--
-- Name: event_log_y2026m05; Type: TABLE; Schema: test_cac; Owner: postgres
--

CREATE TABLE test_cac.event_log_y2026m05 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_cac.event_log ATTACH PARTITION test_cac.event_log_y2026m05 FOR VALUES FROM ('2026-05-01 00:00:00') TO ('2026-06-01 00:00:00');


ALTER TABLE test_cac.event_log_y2026m05 OWNER TO postgres;

--
-- Name: event_log_y2026m06; Type: TABLE; Schema: test_cac; Owner: postgres
--

CREATE TABLE test_cac.event_log_y2026m06 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_cac.event_log ATTACH PARTITION test_cac.event_log_y2026m06 FOR VALUES FROM ('2026-06-01 00:00:00') TO ('2026-07-01 00:00:00');


ALTER TABLE test_cac.event_log_y2026m06 OWNER TO postgres;

--
-- Name: event_log_y2026m07; Type: TABLE; Schema: test_cac; Owner: postgres
--

CREATE TABLE test_cac.event_log_y2026m07 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_cac.event_log ATTACH PARTITION test_cac.event_log_y2026m07 FOR VALUES FROM ('2026-07-01 00:00:00') TO ('2026-08-01 00:00:00');


ALTER TABLE test_cac.event_log_y2026m07 OWNER TO postgres;

--
-- Name: event_log_y2026m08; Type: TABLE; Schema: test_cac; Owner: postgres
--

CREATE TABLE test_cac.event_log_y2026m08 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_cac.event_log ATTACH PARTITION test_cac.event_log_y2026m08 FOR VALUES FROM ('2026-08-01 00:00:00') TO ('2026-09-01 00:00:00');


ALTER TABLE test_cac.event_log_y2026m08 OWNER TO postgres;

--
-- Name: event_log_y2026m09; Type: TABLE; Schema: test_cac; Owner: postgres
--

CREATE TABLE test_cac.event_log_y2026m09 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_cac.event_log ATTACH PARTITION test_cac.event_log_y2026m09 FOR VALUES FROM ('2026-09-01 00:00:00') TO ('2026-10-01 00:00:00');


ALTER TABLE test_cac.event_log_y2026m09 OWNER TO postgres;

--
-- Name: event_log_y2026m10; Type: TABLE; Schema: test_cac; Owner: postgres
--

CREATE TABLE test_cac.event_log_y2026m10 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_cac.event_log ATTACH PARTITION test_cac.event_log_y2026m10 FOR VALUES FROM ('2026-10-01 00:00:00') TO ('2026-11-01 00:00:00');


ALTER TABLE test_cac.event_log_y2026m10 OWNER TO postgres;

--
-- Name: event_log_y2026m11; Type: TABLE; Schema: test_cac; Owner: postgres
--

CREATE TABLE test_cac.event_log_y2026m11 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_cac.event_log ATTACH PARTITION test_cac.event_log_y2026m11 FOR VALUES FROM ('2026-11-01 00:00:00') TO ('2026-12-01 00:00:00');


ALTER TABLE test_cac.event_log_y2026m11 OWNER TO postgres;

--
-- Name: event_log_y2026m12; Type: TABLE; Schema: test_cac; Owner: postgres
--

CREATE TABLE test_cac.event_log_y2026m12 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_cac.event_log ATTACH PARTITION test_cac.event_log_y2026m12 FOR VALUES FROM ('2026-12-01 00:00:00') TO ('2027-01-01 00:00:00');


ALTER TABLE test_cac.event_log_y2026m12 OWNER TO postgres;

--
-- Name: functions; Type: TABLE; Schema: test_cac; Owner: postgres
--

CREATE TABLE test_cac.functions (
    function_name text NOT NULL,
    published_code text,
    draft_code text NOT NULL,
    function_description text NOT NULL,
    published_runtime_version character varying(16),
    draft_runtime_version character varying(16) NOT NULL,
    published_at timestamp without time zone,
    draft_edited_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    published_by text,
    draft_edited_by text NOT NULL
);


ALTER TABLE test_cac.functions OWNER TO postgres;

--
-- Name: event_log; Type: TABLE; Schema: test_experimentation; Owner: postgres
--

CREATE TABLE test_experimentation.event_log (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
)
PARTITION BY RANGE ("timestamp");


ALTER TABLE test_experimentation.event_log OWNER TO postgres;

--
-- Name: event_log_y2023m08; Type: TABLE; Schema: test_experimentation; Owner: postgres
--

CREATE TABLE test_experimentation.event_log_y2023m08 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_experimentation.event_log ATTACH PARTITION test_experimentation.event_log_y2023m08 FOR VALUES FROM ('2023-08-01 00:00:00') TO ('2023-09-01 00:00:00');


ALTER TABLE test_experimentation.event_log_y2023m08 OWNER TO postgres;

--
-- Name: event_log_y2023m09; Type: TABLE; Schema: test_experimentation; Owner: postgres
--

CREATE TABLE test_experimentation.event_log_y2023m09 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_experimentation.event_log ATTACH PARTITION test_experimentation.event_log_y2023m09 FOR VALUES FROM ('2023-09-01 00:00:00') TO ('2023-10-01 00:00:00');


ALTER TABLE test_experimentation.event_log_y2023m09 OWNER TO postgres;

--
-- Name: event_log_y2023m10; Type: TABLE; Schema: test_experimentation; Owner: postgres
--

CREATE TABLE test_experimentation.event_log_y2023m10 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_experimentation.event_log ATTACH PARTITION test_experimentation.event_log_y2023m10 FOR VALUES FROM ('2023-10-01 00:00:00') TO ('2023-11-01 00:00:00');


ALTER TABLE test_experimentation.event_log_y2023m10 OWNER TO postgres;

--
-- Name: event_log_y2023m11; Type: TABLE; Schema: test_experimentation; Owner: postgres
--

CREATE TABLE test_experimentation.event_log_y2023m11 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_experimentation.event_log ATTACH PARTITION test_experimentation.event_log_y2023m11 FOR VALUES FROM ('2023-11-01 00:00:00') TO ('2023-12-01 00:00:00');


ALTER TABLE test_experimentation.event_log_y2023m11 OWNER TO postgres;

--
-- Name: event_log_y2023m12; Type: TABLE; Schema: test_experimentation; Owner: postgres
--

CREATE TABLE test_experimentation.event_log_y2023m12 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_experimentation.event_log ATTACH PARTITION test_experimentation.event_log_y2023m12 FOR VALUES FROM ('2023-12-01 00:00:00') TO ('2024-01-01 00:00:00');


ALTER TABLE test_experimentation.event_log_y2023m12 OWNER TO postgres;

--
-- Name: event_log_y2024m01; Type: TABLE; Schema: test_experimentation; Owner: postgres
--

CREATE TABLE test_experimentation.event_log_y2024m01 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_experimentation.event_log ATTACH PARTITION test_experimentation.event_log_y2024m01 FOR VALUES FROM ('2024-01-01 00:00:00') TO ('2024-02-01 00:00:00');


ALTER TABLE test_experimentation.event_log_y2024m01 OWNER TO postgres;

--
-- Name: event_log_y2024m02; Type: TABLE; Schema: test_experimentation; Owner: postgres
--

CREATE TABLE test_experimentation.event_log_y2024m02 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_experimentation.event_log ATTACH PARTITION test_experimentation.event_log_y2024m02 FOR VALUES FROM ('2024-02-01 00:00:00') TO ('2024-03-01 00:00:00');


ALTER TABLE test_experimentation.event_log_y2024m02 OWNER TO postgres;

--
-- Name: event_log_y2024m03; Type: TABLE; Schema: test_experimentation; Owner: postgres
--

CREATE TABLE test_experimentation.event_log_y2024m03 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_experimentation.event_log ATTACH PARTITION test_experimentation.event_log_y2024m03 FOR VALUES FROM ('2024-03-01 00:00:00') TO ('2024-04-01 00:00:00');


ALTER TABLE test_experimentation.event_log_y2024m03 OWNER TO postgres;

--
-- Name: event_log_y2024m04; Type: TABLE; Schema: test_experimentation; Owner: postgres
--

CREATE TABLE test_experimentation.event_log_y2024m04 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_experimentation.event_log ATTACH PARTITION test_experimentation.event_log_y2024m04 FOR VALUES FROM ('2024-04-01 00:00:00') TO ('2024-05-01 00:00:00');


ALTER TABLE test_experimentation.event_log_y2024m04 OWNER TO postgres;

--
-- Name: event_log_y2024m05; Type: TABLE; Schema: test_experimentation; Owner: postgres
--

CREATE TABLE test_experimentation.event_log_y2024m05 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_experimentation.event_log ATTACH PARTITION test_experimentation.event_log_y2024m05 FOR VALUES FROM ('2024-05-01 00:00:00') TO ('2024-06-01 00:00:00');


ALTER TABLE test_experimentation.event_log_y2024m05 OWNER TO postgres;

--
-- Name: event_log_y2024m06; Type: TABLE; Schema: test_experimentation; Owner: postgres
--

CREATE TABLE test_experimentation.event_log_y2024m06 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_experimentation.event_log ATTACH PARTITION test_experimentation.event_log_y2024m06 FOR VALUES FROM ('2024-06-01 00:00:00') TO ('2024-07-01 00:00:00');


ALTER TABLE test_experimentation.event_log_y2024m06 OWNER TO postgres;

--
-- Name: event_log_y2024m07; Type: TABLE; Schema: test_experimentation; Owner: postgres
--

CREATE TABLE test_experimentation.event_log_y2024m07 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_experimentation.event_log ATTACH PARTITION test_experimentation.event_log_y2024m07 FOR VALUES FROM ('2024-07-01 00:00:00') TO ('2024-08-01 00:00:00');


ALTER TABLE test_experimentation.event_log_y2024m07 OWNER TO postgres;

--
-- Name: event_log_y2024m08; Type: TABLE; Schema: test_experimentation; Owner: postgres
--

CREATE TABLE test_experimentation.event_log_y2024m08 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_experimentation.event_log ATTACH PARTITION test_experimentation.event_log_y2024m08 FOR VALUES FROM ('2024-08-01 00:00:00') TO ('2024-09-01 00:00:00');


ALTER TABLE test_experimentation.event_log_y2024m08 OWNER TO postgres;

--
-- Name: event_log_y2024m09; Type: TABLE; Schema: test_experimentation; Owner: postgres
--

CREATE TABLE test_experimentation.event_log_y2024m09 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_experimentation.event_log ATTACH PARTITION test_experimentation.event_log_y2024m09 FOR VALUES FROM ('2024-09-01 00:00:00') TO ('2024-10-01 00:00:00');


ALTER TABLE test_experimentation.event_log_y2024m09 OWNER TO postgres;

--
-- Name: event_log_y2024m10; Type: TABLE; Schema: test_experimentation; Owner: postgres
--

CREATE TABLE test_experimentation.event_log_y2024m10 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_experimentation.event_log ATTACH PARTITION test_experimentation.event_log_y2024m10 FOR VALUES FROM ('2024-10-01 00:00:00') TO ('2024-11-01 00:00:00');


ALTER TABLE test_experimentation.event_log_y2024m10 OWNER TO postgres;

--
-- Name: event_log_y2024m11; Type: TABLE; Schema: test_experimentation; Owner: postgres
--

CREATE TABLE test_experimentation.event_log_y2024m11 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_experimentation.event_log ATTACH PARTITION test_experimentation.event_log_y2024m11 FOR VALUES FROM ('2024-11-01 00:00:00') TO ('2024-12-01 00:00:00');


ALTER TABLE test_experimentation.event_log_y2024m11 OWNER TO postgres;

--
-- Name: event_log_y2024m12; Type: TABLE; Schema: test_experimentation; Owner: postgres
--

CREATE TABLE test_experimentation.event_log_y2024m12 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_experimentation.event_log ATTACH PARTITION test_experimentation.event_log_y2024m12 FOR VALUES FROM ('2024-12-01 00:00:00') TO ('2025-01-01 00:00:00');


ALTER TABLE test_experimentation.event_log_y2024m12 OWNER TO postgres;

--
-- Name: event_log_y2025m01; Type: TABLE; Schema: test_experimentation; Owner: postgres
--

CREATE TABLE test_experimentation.event_log_y2025m01 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_experimentation.event_log ATTACH PARTITION test_experimentation.event_log_y2025m01 FOR VALUES FROM ('2025-01-01 00:00:00') TO ('2025-02-01 00:00:00');


ALTER TABLE test_experimentation.event_log_y2025m01 OWNER TO postgres;

--
-- Name: event_log_y2025m02; Type: TABLE; Schema: test_experimentation; Owner: postgres
--

CREATE TABLE test_experimentation.event_log_y2025m02 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_experimentation.event_log ATTACH PARTITION test_experimentation.event_log_y2025m02 FOR VALUES FROM ('2025-02-01 00:00:00') TO ('2025-03-01 00:00:00');


ALTER TABLE test_experimentation.event_log_y2025m02 OWNER TO postgres;

--
-- Name: event_log_y2025m03; Type: TABLE; Schema: test_experimentation; Owner: postgres
--

CREATE TABLE test_experimentation.event_log_y2025m03 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_experimentation.event_log ATTACH PARTITION test_experimentation.event_log_y2025m03 FOR VALUES FROM ('2025-03-01 00:00:00') TO ('2025-04-01 00:00:00');


ALTER TABLE test_experimentation.event_log_y2025m03 OWNER TO postgres;

--
-- Name: event_log_y2025m04; Type: TABLE; Schema: test_experimentation; Owner: postgres
--

CREATE TABLE test_experimentation.event_log_y2025m04 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_experimentation.event_log ATTACH PARTITION test_experimentation.event_log_y2025m04 FOR VALUES FROM ('2025-04-01 00:00:00') TO ('2025-05-01 00:00:00');


ALTER TABLE test_experimentation.event_log_y2025m04 OWNER TO postgres;

--
-- Name: event_log_y2025m05; Type: TABLE; Schema: test_experimentation; Owner: postgres
--

CREATE TABLE test_experimentation.event_log_y2025m05 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_experimentation.event_log ATTACH PARTITION test_experimentation.event_log_y2025m05 FOR VALUES FROM ('2025-05-01 00:00:00') TO ('2025-06-01 00:00:00');


ALTER TABLE test_experimentation.event_log_y2025m05 OWNER TO postgres;

--
-- Name: event_log_y2025m06; Type: TABLE; Schema: test_experimentation; Owner: postgres
--

CREATE TABLE test_experimentation.event_log_y2025m06 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_experimentation.event_log ATTACH PARTITION test_experimentation.event_log_y2025m06 FOR VALUES FROM ('2025-06-01 00:00:00') TO ('2025-07-01 00:00:00');


ALTER TABLE test_experimentation.event_log_y2025m06 OWNER TO postgres;

--
-- Name: event_log_y2025m07; Type: TABLE; Schema: test_experimentation; Owner: postgres
--

CREATE TABLE test_experimentation.event_log_y2025m07 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_experimentation.event_log ATTACH PARTITION test_experimentation.event_log_y2025m07 FOR VALUES FROM ('2025-07-01 00:00:00') TO ('2025-08-01 00:00:00');


ALTER TABLE test_experimentation.event_log_y2025m07 OWNER TO postgres;

--
-- Name: event_log_y2025m08; Type: TABLE; Schema: test_experimentation; Owner: postgres
--

CREATE TABLE test_experimentation.event_log_y2025m08 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_experimentation.event_log ATTACH PARTITION test_experimentation.event_log_y2025m08 FOR VALUES FROM ('2025-08-01 00:00:00') TO ('2025-09-01 00:00:00');


ALTER TABLE test_experimentation.event_log_y2025m08 OWNER TO postgres;

--
-- Name: event_log_y2025m09; Type: TABLE; Schema: test_experimentation; Owner: postgres
--

CREATE TABLE test_experimentation.event_log_y2025m09 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_experimentation.event_log ATTACH PARTITION test_experimentation.event_log_y2025m09 FOR VALUES FROM ('2025-09-01 00:00:00') TO ('2025-10-01 00:00:00');


ALTER TABLE test_experimentation.event_log_y2025m09 OWNER TO postgres;

--
-- Name: event_log_y2025m10; Type: TABLE; Schema: test_experimentation; Owner: postgres
--

CREATE TABLE test_experimentation.event_log_y2025m10 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_experimentation.event_log ATTACH PARTITION test_experimentation.event_log_y2025m10 FOR VALUES FROM ('2025-10-01 00:00:00') TO ('2025-11-01 00:00:00');


ALTER TABLE test_experimentation.event_log_y2025m10 OWNER TO postgres;

--
-- Name: event_log_y2025m11; Type: TABLE; Schema: test_experimentation; Owner: postgres
--

CREATE TABLE test_experimentation.event_log_y2025m11 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_experimentation.event_log ATTACH PARTITION test_experimentation.event_log_y2025m11 FOR VALUES FROM ('2025-11-01 00:00:00') TO ('2025-12-01 00:00:00');


ALTER TABLE test_experimentation.event_log_y2025m11 OWNER TO postgres;

--
-- Name: event_log_y2025m12; Type: TABLE; Schema: test_experimentation; Owner: postgres
--

CREATE TABLE test_experimentation.event_log_y2025m12 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_experimentation.event_log ATTACH PARTITION test_experimentation.event_log_y2025m12 FOR VALUES FROM ('2025-12-01 00:00:00') TO ('2026-01-01 00:00:00');


ALTER TABLE test_experimentation.event_log_y2025m12 OWNER TO postgres;

--
-- Name: event_log_y2026m01; Type: TABLE; Schema: test_experimentation; Owner: postgres
--

CREATE TABLE test_experimentation.event_log_y2026m01 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_experimentation.event_log ATTACH PARTITION test_experimentation.event_log_y2026m01 FOR VALUES FROM ('2026-01-01 00:00:00') TO ('2026-02-01 00:00:00');


ALTER TABLE test_experimentation.event_log_y2026m01 OWNER TO postgres;

--
-- Name: event_log_y2026m02; Type: TABLE; Schema: test_experimentation; Owner: postgres
--

CREATE TABLE test_experimentation.event_log_y2026m02 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_experimentation.event_log ATTACH PARTITION test_experimentation.event_log_y2026m02 FOR VALUES FROM ('2026-02-01 00:00:00') TO ('2026-03-01 00:00:00');


ALTER TABLE test_experimentation.event_log_y2026m02 OWNER TO postgres;

--
-- Name: event_log_y2026m03; Type: TABLE; Schema: test_experimentation; Owner: postgres
--

CREATE TABLE test_experimentation.event_log_y2026m03 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_experimentation.event_log ATTACH PARTITION test_experimentation.event_log_y2026m03 FOR VALUES FROM ('2026-03-01 00:00:00') TO ('2026-04-01 00:00:00');


ALTER TABLE test_experimentation.event_log_y2026m03 OWNER TO postgres;

--
-- Name: event_log_y2026m04; Type: TABLE; Schema: test_experimentation; Owner: postgres
--

CREATE TABLE test_experimentation.event_log_y2026m04 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_experimentation.event_log ATTACH PARTITION test_experimentation.event_log_y2026m04 FOR VALUES FROM ('2026-04-01 00:00:00') TO ('2026-05-01 00:00:00');


ALTER TABLE test_experimentation.event_log_y2026m04 OWNER TO postgres;

--
-- Name: event_log_y2026m05; Type: TABLE; Schema: test_experimentation; Owner: postgres
--

CREATE TABLE test_experimentation.event_log_y2026m05 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_experimentation.event_log ATTACH PARTITION test_experimentation.event_log_y2026m05 FOR VALUES FROM ('2026-05-01 00:00:00') TO ('2026-06-01 00:00:00');


ALTER TABLE test_experimentation.event_log_y2026m05 OWNER TO postgres;

--
-- Name: event_log_y2026m06; Type: TABLE; Schema: test_experimentation; Owner: postgres
--

CREATE TABLE test_experimentation.event_log_y2026m06 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_experimentation.event_log ATTACH PARTITION test_experimentation.event_log_y2026m06 FOR VALUES FROM ('2026-06-01 00:00:00') TO ('2026-07-01 00:00:00');


ALTER TABLE test_experimentation.event_log_y2026m06 OWNER TO postgres;

--
-- Name: event_log_y2026m07; Type: TABLE; Schema: test_experimentation; Owner: postgres
--

CREATE TABLE test_experimentation.event_log_y2026m07 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_experimentation.event_log ATTACH PARTITION test_experimentation.event_log_y2026m07 FOR VALUES FROM ('2026-07-01 00:00:00') TO ('2026-08-01 00:00:00');


ALTER TABLE test_experimentation.event_log_y2026m07 OWNER TO postgres;

--
-- Name: event_log_y2026m08; Type: TABLE; Schema: test_experimentation; Owner: postgres
--

CREATE TABLE test_experimentation.event_log_y2026m08 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_experimentation.event_log ATTACH PARTITION test_experimentation.event_log_y2026m08 FOR VALUES FROM ('2026-08-01 00:00:00') TO ('2026-09-01 00:00:00');


ALTER TABLE test_experimentation.event_log_y2026m08 OWNER TO postgres;

--
-- Name: event_log_y2026m09; Type: TABLE; Schema: test_experimentation; Owner: postgres
--

CREATE TABLE test_experimentation.event_log_y2026m09 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_experimentation.event_log ATTACH PARTITION test_experimentation.event_log_y2026m09 FOR VALUES FROM ('2026-09-01 00:00:00') TO ('2026-10-01 00:00:00');


ALTER TABLE test_experimentation.event_log_y2026m09 OWNER TO postgres;

--
-- Name: event_log_y2026m10; Type: TABLE; Schema: test_experimentation; Owner: postgres
--

CREATE TABLE test_experimentation.event_log_y2026m10 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_experimentation.event_log ATTACH PARTITION test_experimentation.event_log_y2026m10 FOR VALUES FROM ('2026-10-01 00:00:00') TO ('2026-11-01 00:00:00');


ALTER TABLE test_experimentation.event_log_y2026m10 OWNER TO postgres;

--
-- Name: event_log_y2026m11; Type: TABLE; Schema: test_experimentation; Owner: postgres
--

CREATE TABLE test_experimentation.event_log_y2026m11 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_experimentation.event_log ATTACH PARTITION test_experimentation.event_log_y2026m11 FOR VALUES FROM ('2026-11-01 00:00:00') TO ('2026-12-01 00:00:00');


ALTER TABLE test_experimentation.event_log_y2026m11 OWNER TO postgres;

--
-- Name: event_log_y2026m12; Type: TABLE; Schema: test_experimentation; Owner: postgres
--

CREATE TABLE test_experimentation.event_log_y2026m12 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY test_experimentation.event_log ATTACH PARTITION test_experimentation.event_log_y2026m12 FOR VALUES FROM ('2026-12-01 00:00:00') TO ('2027-01-01 00:00:00');


ALTER TABLE test_experimentation.event_log_y2026m12 OWNER TO postgres;

--
-- Name: experiments; Type: TABLE; Schema: test_experimentation; Owner: postgres
--

CREATE TABLE test_experimentation.experiments (
    id bigint NOT NULL,
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


ALTER TABLE test_experimentation.experiments OWNER TO postgres;

--
-- Data for Name: contexts; Type: TABLE DATA; Schema: dev_cac; Owner: postgres
--

COPY dev_cac.contexts (id, value, override_id, created_at, created_by, priority, override) FROM stdin;
9fb61ecfb662467e9036ec79bfd8c02ac244c1836f113242aec5fe6d9265d8dc	{"and":[{"==":[{"var":"city"},"Bangalore"]},{"==":[{"var":"vehicle_type"},"cab"]}]}	e2411e811b927824f910f9edd75b984a3ad226c46a1216ffa735b7bda60c1ede	2024-05-07 17:51:39.330747+00	superposition@juspay.in	6	{"base_rate":12}
d8256cd0c9fbbfae460057ed14f0630f4dd0b050cf271832b0c9bfb5e31ca43b	{"and":[{"==":[{"var":"city"},"Bangalore"]},{"==":[{"var":"vehicle_type"},"cab"]},{"==":[{"var":"hour_of_day"},9]}]}	204dc317acb302d38289cbfbc3227461d45bdde8dc5bfc0f6f961a8bb476e910	2024-05-08 08:02:50.144267+00	superposition@juspay.in	38	{"base_rate":100,"per_distance_unit_rate":50}
11c6bd932b8888a9e50be8f822b76116fbaa20fd58b059f98b9e7179543bb9a4	{"==":[{"var":"city"},"Bangalore"]}	932a2b3502038c2e975d9ace00c90f016860ecb14564cf13085b0d40d1959153	2024-05-10 14:45:07.769368+00	user@superposition.io	4	{"base_rate":20,"currency":"INR","distance_unit":"Km","hello_message":"ನಮಸ್ಕಾರ ಬೆಂಗಳೂರು","hello_message_color":"red","logo":"https://www.shutterstock.com/image-vector/bangalore-logo-vidhana-soudha-600nw-1506258893.jpg","per_distance_unit_rate":15}
e47edbe36bf3333f42ca2ded0a4604997adc4f6e0bf6176079272597c01353d8	{"==":[{"var":"city"},"Chennai"]}	645b12a924823be87eb423791f3c42393ab19103afcd8ce923c4a1112408b32b	2024-05-10 14:48:42.122682+00	user@superposition.io	4	{"hello_message":"வணக்கம் சென்னை","hello_message_color":"green","logo":"https://www.shutterstock.com/image-vector/chennai-skyline-color-landmarks-blue-260nw-515862346.jpg"}
3f0e5fe95e2e758151d4cef712185767a137c926a9e7156b6face3a98b167d35	{"==":[{"var":"city"},"Seattle"]}	f73412831793a34df7df95a79bc65754b1eebf31534e4ccfa9c79681fcf8ac69	2024-05-10 14:50:19.471563+00	user@superposition.io	4	{"base_rate":5,"currency":"USD","distance_unit":"Miles","hello_message":"Hello Seattle","hello_message_color":"blue","logo":"https://t4.ftcdn.net/jpg/04/24/15/07/360_F_424150716_kDZIgUONDIKpIhHqsxlDcVIiglyjIOQs.jpg","per_distance_unit_rate":2.5}
\.


--
-- Data for Name: default_configs; Type: TABLE DATA; Schema: dev_cac; Owner: postgres
--

COPY dev_cac.default_configs (key, value, created_at, created_by, schema, function_name) FROM stdin;
base_rate	10.0	2024-05-05 10:53:22.654911+00	superposition@juspay.in	{"type":"number"}	\N
currency	"INR"	2024-05-05 10:54:07.595371+00	superposition@juspay.in	{"enum":["INR","USD","EUR"],"type":"string"}	\N
per_distance_unit_rate	15.0	2024-05-05 10:53:38.957119+00	superposition@juspay.in	{"type":"number"}	\N
distance_unit	"Km"	2024-05-06 07:01:59.163885+00	superposition@juspay.in	{"enum":["Km","Miles"],"type":"string"}	\N
bar.com	10	2024-05-07 15:02:11.504644+00	superposition@juspay.in	{"type":"number"}	\N
bar.foo	10.0	2024-05-07 15:02:31.131308+00	superposition@juspay.in	{"type":"number"}	\N
hello_message	"Hello World !!!"	2024-05-08 09:13:14.009736+00	superposition@juspay.in	{"pattern":".*","type":"string"}	\N
hello_message_color	"black"	2024-05-08 10:25:50.551915+00	superposition@juspay.in	{"enum":["black","red","blue","green","pink"],"type":"string"}	\N
logo	"https://cdn1.vectorstock.com/i/1000x1000/03/20/big-city-logo-vector-20480320.jpg"	2024-05-10 14:44:29.391516+00	user@superposition.io	{"pattern":"https://.*","type":"string"}	\N
\.


--
-- Data for Name: dimensions; Type: TABLE DATA; Schema: dev_cac; Owner: postgres
--

COPY dev_cac.dimensions (dimension, priority, created_at, created_by, schema, function_name) FROM stdin;
variantIds	1	2024-05-01 11:25:39.025892+00	anon@juspay.in	{"type": "string","pattern": ".*"}	\N
vehicle_type	2	2024-05-07 17:44:05.136115+00	superposition@juspay.in	{"enum":["cab","auto"],"type":"string"}	\N
city	4	2024-05-07 17:44:16.716277+00	superposition@juspay.in	{"pattern":"[a-zA-Z ]+","type":"string"}	\N
hour_of_day	32	2024-05-08 08:01:36.617931+00	superposition@juspay.in	{"type":"number"}	\N
\.


--
-- Data for Name: event_log_y2023m08; Type: TABLE DATA; Schema: dev_cac; Owner: postgres
--

COPY dev_cac.event_log_y2023m08 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2023m09; Type: TABLE DATA; Schema: dev_cac; Owner: postgres
--

COPY dev_cac.event_log_y2023m09 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2023m10; Type: TABLE DATA; Schema: dev_cac; Owner: postgres
--

COPY dev_cac.event_log_y2023m10 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2023m11; Type: TABLE DATA; Schema: dev_cac; Owner: postgres
--

COPY dev_cac.event_log_y2023m11 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2023m12; Type: TABLE DATA; Schema: dev_cac; Owner: postgres
--

COPY dev_cac.event_log_y2023m12 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2024m01; Type: TABLE DATA; Schema: dev_cac; Owner: postgres
--

COPY dev_cac.event_log_y2024m01 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2024m02; Type: TABLE DATA; Schema: dev_cac; Owner: postgres
--

COPY dev_cac.event_log_y2024m02 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2024m03; Type: TABLE DATA; Schema: dev_cac; Owner: postgres
--

COPY dev_cac.event_log_y2024m03 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2024m04; Type: TABLE DATA; Schema: dev_cac; Owner: postgres
--

COPY dev_cac.event_log_y2024m04 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2024m05; Type: TABLE DATA; Schema: dev_cac; Owner: postgres
--

COPY dev_cac.event_log_y2024m05 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
afb2d44c-cce3-4757-b3c9-10e37347bf79	dimensions	postgres	2024-05-01 11:25:39.025892	INSERT	\N	{"dimension":"variantIds","priority":1,"created_at":"2024-05-01T11:25:39.025892+00:00","created_by":"anon@juspay.in","schema":{"type": "string","pattern": ".*"},"function_name":null}	INSERT INTO dev_cac.dimensions (dimension, priority, created_at, created_by, schema, function_name) VALUES ('variantIds', 1, CURRENT_TIMESTAMP, 'anon@juspay.in', '{"type": "string","pattern": ".*"}'::json, null);
f32e1804-dcec-4643-b039-ea69afcd4e85	default_configs	postgres	2024-05-01 12:16:09.940256	INSERT	\N	{"key":"foo","value":10,"created_at":"2024-05-01T12:16:09.93811+00:00","created_by":"superposition@juspay.in","schema":{"type":"number"},"function_name":null}	INSERT INTO "default_configs" ("key", "value", "created_at", "created_by", "schema", "function_name") VALUES ($1, $2, $3, $4, $5, DEFAULT) ON CONFLICT ("key") DO UPDATE SET "value" = $6, "created_at" = $7, "created_by" = $8, "schema" = $9, "function_name" = $10
ffd9d778-8b8d-47be-8c04-86fa95bae0e4	dimensions	postgres	2024-05-01 12:16:59.244634	INSERT	\N	{"dimension":"city","priority":1,"created_at":"2024-05-01T12:16:59.242405+00:00","created_by":"superposition@juspay.in","schema":{"pattern":"[a-zA-Z]+","type":"string"},"function_name":null}	INSERT INTO "dimensions" ("dimension", "priority", "created_at", "created_by", "schema", "function_name") VALUES ($1, $2, $3, $4, $5, DEFAULT) ON CONFLICT ("dimension") DO UPDATE SET "priority" = $6, "created_at" = $7, "created_by" = $8, "schema" = $9, "function_name" = $10 RETURNING "dimensions"."dimension", "dimensions"."priority", "dimensions"."created_at", "dimensions"."created_by", "dimensions"."schema", "dimensions"."function_name"
1a65d9bc-bd07-4bd9-acb1-5f17272860ac	contexts	postgres	2024-05-01 12:17:16.16277	INSERT	\N	{"id":"11c6bd932b8888a9e50be8f822b76116fbaa20fd58b059f98b9e7179543bb9a4","value":{"==":[{"var":"city"},"Bangalore"]},"override_id":"7faf7ac21a000f465344c4c4b403fa62cd9f9a0caecd684fe566f06260677503","created_at":"2024-05-01T12:17:16.160015+00:00","created_by":"superposition@juspay.in","priority":1,"override":{"foo":12}}	INSERT INTO "contexts" ("id", "value", "override_id", "created_at", "created_by", "priority", "override") VALUES ($1, $2, $3, $4, $5, $6, $7)
64be2b08-5844-491d-ab47-241244a2cfa1	contexts	postgres	2024-05-01 12:59:26.485692	INSERT	\N	{"id":"fb6c3f081a7d44d73f603c3c1e6ea2b4f6c823c33316aa2629a3864d68d98b66","value":{"and":[{"in":["7191420957775499264-control",{"var":"variantIds"}]}]},"override_id":"4045fc51982e1424d81e02e3e78c68e7a91a3b2077a27beefb443edd05deba78","created_at":"2024-05-01T12:59:26.49421+00:00","created_by":"superposition@juspay.in","priority":1,"override":{"foo":10}}	INSERT INTO "contexts" ("id", "value", "override_id", "created_at", "created_by", "priority", "override") VALUES ($1, $2, $3, $4, $5, $6, $7)
9892302c-87bd-4e67-baba-a098c0062a53	contexts	postgres	2024-05-01 12:59:26.485692	INSERT	\N	{"id":"1e4087f940e168cafcff6e2a693eab9ad5ffba71b98d2463af04a9be3ab8fc9a","value":{"and":[{"in":["7191420957775499264-experimental",{"var":"variantIds"}]}]},"override_id":"bbbea2aa79b2e69bbe6905fa9045f29ad4205621b5b4adf16c7e26d01a5a318e","created_at":"2024-05-01T12:59:26.521789+00:00","created_by":"superposition@juspay.in","priority":1,"override":{"foo":14}}	INSERT INTO "contexts" ("id", "value", "override_id", "created_at", "created_by", "priority", "override") VALUES ($1, $2, $3, $4, $5, $6, $7)
90085d39-b9a6-4d0a-b083-b1cbec778151	contexts	postgres	2024-05-02 04:13:27.450921	INSERT	\N	{"id":"89c076836990b7b9fbe6386b513712e0532516242165d04b8a286b7fcd6e637c","value":{"and":[{"":[{"var":"city"},"Bangalore"]},{"in":["7191650977466945536-control",{"var":"variantIds"}]}]},"override_id":"4045fc51982e1424d81e02e3e78c68e7a91a3b2077a27beefb443edd05deba78","created_at":"2024-05-02T04:13:27.464399+00:00","created_by":"superposition@juspay.in","priority":2,"override":{"foo":10}}	INSERT INTO "contexts" ("id", "value", "override_id", "created_at", "created_by", "priority", "override") VALUES ($1, $2, $3, $4, $5, $6, $7)
03ca94ac-fed9-4ffa-8c9e-585a580b5d8a	contexts	postgres	2024-05-02 04:13:27.450921	INSERT	\N	{"id":"00ab50b29e06b77448320034a82bfbb411ad014bbc8bef5835347320e7e1427a","value":{"and":[{"":[{"var":"city"},"Bangalore"]},{"in":["7191650977466945536-experimental",{"var":"variantIds"}]}]},"override_id":"655941b5b4116a70e7ab0fdc4fb38d8d2479ade758f605b79347ff60e31f22cb","created_at":"2024-05-02T04:13:27.485522+00:00","created_by":"superposition@juspay.in","priority":2,"override":{"foo":17}}	INSERT INTO "contexts" ("id", "value", "override_id", "created_at", "created_by", "priority", "override") VALUES ($1, $2, $3, $4, $5, $6, $7)
49061f20-f09b-4145-8362-5bc71ee9132d	contexts	postgres	2024-05-02 04:13:53.806162	DELETE	{"id":"89c076836990b7b9fbe6386b513712e0532516242165d04b8a286b7fcd6e637c","value":{"and":[{"":[{"var":"city"},"Bangalore"]},{"in":["7191650977466945536-control",{"var":"variantIds"}]}]},"override_id":"4045fc51982e1424d81e02e3e78c68e7a91a3b2077a27beefb443edd05deba78","created_at":"2024-05-02T04:13:27.464399+00:00","created_by":"superposition@juspay.in","priority":2,"override":{"foo":10}}	\N	DELETE  FROM "contexts" WHERE ("contexts"."id" = $1)
363597f0-0242-4f41-acec-a4dc9f66f81b	contexts	postgres	2024-05-02 04:13:53.806162	UPDATE	{"id":"00ab50b29e06b77448320034a82bfbb411ad014bbc8bef5835347320e7e1427a","value":{"and":[{"":[{"var":"city"},"Bangalore"]},{"in":["7191650977466945536-experimental",{"var":"variantIds"}]}]},"override_id":"655941b5b4116a70e7ab0fdc4fb38d8d2479ade758f605b79347ff60e31f22cb","created_at":"2024-05-02T04:13:27.485522+00:00","created_by":"superposition@juspay.in","priority":2,"override":{"foo":17}}	{"id":"8d2d84ffd2187715f75e43bbc058d8eafef7d1854639818567caef29daf875d8","value":{"":[{"var":"city"},"Bangalore"]},"override_id":"655941b5b4116a70e7ab0fdc4fb38d8d2479ade758f605b79347ff60e31f22cb","created_at":"2024-05-02T04:13:27.485522+00:00","created_by":"superposition@juspay.in","priority":1,"override":{"foo":17}}	UPDATE "contexts" SET "id" = $1, "value" = $2, "priority" = $3 WHERE ("contexts"."id" = $4) RETURNING "contexts"."id", "contexts"."value", "contexts"."override_id", "contexts"."created_at", "contexts"."created_by", "contexts"."priority", "contexts"."override"
5cd6abbd-eb6d-42dc-be10-9fb8a1b81695	contexts	postgres	2024-05-02 04:15:15.380716	INSERT	\N	{"id":"3162da98fc4912c835341a77c1fc72cfb491a61832662e0493510e28ac3bb3fb","value":{"and":[{"":[{"var":"city"},"Bangalore"]},{"in":["7191651430124621824-control",{"var":"variantIds"}]}]},"override_id":"4045fc51982e1424d81e02e3e78c68e7a91a3b2077a27beefb443edd05deba78","created_at":"2024-05-02T04:15:15.386908+00:00","created_by":"superposition@juspay.in","priority":2,"override":{"foo":10}}	INSERT INTO "contexts" ("id", "value", "override_id", "created_at", "created_by", "priority", "override") VALUES ($1, $2, $3, $4, $5, $6, $7)
3dbdb8a8-68e8-4152-9f77-1fe4bdadfee6	contexts	postgres	2024-05-02 04:15:15.380716	INSERT	\N	{"id":"525064795060ba63a14b63dcb0b7ecfa55001c490b9d8cff53eb8fc636c030be","value":{"and":[{"":[{"var":"city"},"Bangalore"]},{"in":["7191651430124621824-experimental",{"var":"variantIds"}]}]},"override_id":"b5be3e559306b2f3ecada2fd438bdd42e8ed9104576f25ebe66a88d007d38772","created_at":"2024-05-02T04:15:15.397603+00:00","created_by":"superposition@juspay.in","priority":2,"override":{"foo":18}}	INSERT INTO "contexts" ("id", "value", "override_id", "created_at", "created_by", "priority", "override") VALUES ($1, $2, $3, $4, $5, $6, $7)
1cd9b110-892f-45c3-ba7a-bf9a18c107a4	contexts	postgres	2024-05-02 04:15:39.33219	DELETE	{"id":"3162da98fc4912c835341a77c1fc72cfb491a61832662e0493510e28ac3bb3fb","value":{"and":[{"":[{"var":"city"},"Bangalore"]},{"in":["7191651430124621824-control",{"var":"variantIds"}]}]},"override_id":"4045fc51982e1424d81e02e3e78c68e7a91a3b2077a27beefb443edd05deba78","created_at":"2024-05-02T04:15:15.386908+00:00","created_by":"superposition@juspay.in","priority":2,"override":{"foo":10}}	\N	DELETE  FROM "contexts" WHERE ("contexts"."id" = $1)
3f1a6ce5-03c5-421a-9de5-050670ba57db	contexts	postgres	2024-05-02 04:15:39.33219	DELETE	{"id":"525064795060ba63a14b63dcb0b7ecfa55001c490b9d8cff53eb8fc636c030be","value":{"and":[{"":[{"var":"city"},"Bangalore"]},{"in":["7191651430124621824-experimental",{"var":"variantIds"}]}]},"override_id":"b5be3e559306b2f3ecada2fd438bdd42e8ed9104576f25ebe66a88d007d38772","created_at":"2024-05-02T04:15:15.397603+00:00","created_by":"superposition@juspay.in","priority":2,"override":{"foo":18}}	\N	DELETE  FROM "contexts" WHERE ("contexts"."id" = $1) RETURNING "contexts"."id", "contexts"."value", "contexts"."override_id", "contexts"."created_at", "contexts"."created_by", "contexts"."priority", "contexts"."override"
a50f253c-979e-43e4-808d-1c0b3948a608	contexts	postgres	2024-05-02 04:15:39.33219	UPDATE	{"id":"8d2d84ffd2187715f75e43bbc058d8eafef7d1854639818567caef29daf875d8","value":{"":[{"var":"city"},"Bangalore"]},"override_id":"655941b5b4116a70e7ab0fdc4fb38d8d2479ade758f605b79347ff60e31f22cb","created_at":"2024-05-02T04:13:27.485522+00:00","created_by":"superposition@juspay.in","priority":1,"override":{"foo":17}}	{"id":"8d2d84ffd2187715f75e43bbc058d8eafef7d1854639818567caef29daf875d8","value":{"":[{"var":"city"},"Bangalore"]},"override_id":"b5be3e559306b2f3ecada2fd438bdd42e8ed9104576f25ebe66a88d007d38772","created_at":"2024-05-02T04:15:39.347124+00:00","created_by":"superposition@juspay.in","priority":1,"override":{"foo":18}}	UPDATE "contexts" SET "value" = $1, "override_id" = $2, "created_at" = $3, "created_by" = $4, "priority" = $5, "override" = $6 WHERE ("contexts"."id" = $7)
ad709096-5bd7-4654-b674-8531af20b83c	contexts	postgres	2024-05-02 07:23:51.387798	INSERT	\N	{"id":"5bf54e4783b83f91b2ae1e1d905a2897b007a657bc235475c3881b0f06b6f7a2","value":{"and":[{"":[{"var":"city"},"FooBar"]},{"in":["7191698892931600384-control",{"var":"variantIds"}]}]},"override_id":"4045fc51982e1424d81e02e3e78c68e7a91a3b2077a27beefb443edd05deba78","created_at":"2024-05-02T07:23:51.400605+00:00","created_by":"superposition@juspay.in","priority":2,"override":{"foo":10}}	INSERT INTO "contexts" ("id", "value", "override_id", "created_at", "created_by", "priority", "override") VALUES ($1, $2, $3, $4, $5, $6, $7)
757c203b-5b04-4e6d-9825-68e077ea8eb4	contexts	postgres	2024-05-02 07:23:51.387798	INSERT	\N	{"id":"81b9cf1f4ae22d89f5d3118d1019698abe46cfe481ca3593d61f2acc04bc498c","value":{"and":[{"":[{"var":"city"},"FooBar"]},{"in":["7191698892931600384-experimental",{"var":"variantIds"}]}]},"override_id":"16d291cd4d3204baecba99cd4e9b4a7978719df8265774f13679db03b17c0698","created_at":"2024-05-02T07:23:51.424615+00:00","created_by":"superposition@juspay.in","priority":2,"override":{"foo":43}}	INSERT INTO "contexts" ("id", "value", "override_id", "created_at", "created_by", "priority", "override") VALUES ($1, $2, $3, $4, $5, $6, $7)
11c7f09b-1e2b-439c-8446-140e298415e3	default_configs	postgres	2024-05-03 16:17:49.368894	INSERT	\N	{"key":"bar.com","value":12,"created_at":"2024-05-03T16:17:49.36615+00:00","created_by":"superposition@juspay.in","schema":{"type":"number"},"function_name":null}	INSERT INTO "default_configs" ("key", "value", "created_at", "created_by", "schema", "function_name") VALUES ($1, $2, $3, $4, $5, DEFAULT) ON CONFLICT ("key") DO UPDATE SET "value" = $6, "created_at" = $7, "created_by" = $8, "schema" = $9, "function_name" = $10
827262a3-424e-423a-8c3f-60066be8f968	default_configs	postgres	2024-05-03 16:18:05.361893	INSERT	\N	{"key":"bar.baz","value":43,"created_at":"2024-05-03T16:18:05.358005+00:00","created_by":"superposition@juspay.in","schema":{"type":"number"},"function_name":null}	INSERT INTO "default_configs" ("key", "value", "created_at", "created_by", "schema", "function_name") VALUES ($1, $2, $3, $4, $5, DEFAULT) ON CONFLICT ("key") DO UPDATE SET "value" = $6, "created_at" = $7, "created_by" = $8, "schema" = $9, "function_name" = $10
f1c192b4-2c2f-45b2-a008-c42420f7280b	default_configs	postgres	2024-05-05 07:54:40.99486	INSERT	\N	{"key":"base-fare","value":25.0,"created_at":"2024-05-05T07:54:40.987208+00:00","created_by":"superposition@juspay.in","schema":{"type":"number"},"function_name":null}	INSERT INTO "default_configs" ("key", "value", "created_at", "created_by", "schema", "function_name") VALUES ($1, $2, $3, $4, $5, DEFAULT) ON CONFLICT ("key") DO UPDATE SET "value" = $6, "created_at" = $7, "created_by" = $8, "schema" = $9, "function_name" = $10
5fd5b38f-a9b7-4d91-a14c-86df8c754936	default_configs	postgres	2024-05-05 07:54:57.222327	INSERT	\N	{"key":"per-km-rate","value":15.0,"created_at":"2024-05-05T07:54:57.218143+00:00","created_by":"superposition@juspay.in","schema":{"type":"number"},"function_name":null}	INSERT INTO "default_configs" ("key", "value", "created_at", "created_by", "schema", "function_name") VALUES ($1, $2, $3, $4, $5, DEFAULT) ON CONFLICT ("key") DO UPDATE SET "value" = $6, "created_at" = $7, "created_by" = $8, "schema" = $9, "function_name" = $10
a9c308ca-c1ab-47d3-aa87-cd28f3d0a58d	default_configs	postgres	2024-05-05 07:55:53.35989	INSERT	\N	{"key":"currency","value":"INR","created_at":"2024-05-05T07:55:53.354787+00:00","created_by":"superposition@juspay.in","schema":{"enum":["INR","USD","EUR"],"type":"string"},"function_name":null}	INSERT INTO "default_configs" ("key", "value", "created_at", "created_by", "schema", "function_name") VALUES ($1, $2, $3, $4, $5, DEFAULT) ON CONFLICT ("key") DO UPDATE SET "value" = $6, "created_at" = $7, "created_by" = $8, "schema" = $9, "function_name" = $10
01a44fb6-9481-47cc-91a0-8e32dd2dc5e5	default_configs	postgres	2024-05-05 10:48:53.407338	DELETE	{"key":"bar.com","value":12,"created_at":"2024-05-03T16:17:49.36615+00:00","created_by":"superposition@juspay.in","schema":{"type":"number"},"function_name":null}	\N	delete from dev_cac.default_configs where key = 'bar.com';
9860722e-a8f9-44eb-93f7-2087206323e8	default_configs	postgres	2024-05-05 10:48:58.297119	DELETE	{"key":"bar.baz","value":43,"created_at":"2024-05-03T16:18:05.358005+00:00","created_by":"superposition@juspay.in","schema":{"type":"number"},"function_name":null}	\N	delete from dev_cac.default_configs where key = 'bar.baz';
4c937453-61f7-4c14-b079-9f7140a00ffc	default_configs	postgres	2024-05-05 10:49:01.223898	DELETE	{"key":"foo","value":10,"created_at":"2024-05-01T12:16:09.93811+00:00","created_by":"superposition@juspay.in","schema":{"type":"number"},"function_name":null}	\N	delete from dev_cac.default_configs where key = 'foo';
df9a6e18-06a9-48b1-99a9-4032f6ce69f0	contexts	postgres	2024-05-05 10:49:25.904006	DELETE	{"id":"11c6bd932b8888a9e50be8f822b76116fbaa20fd58b059f98b9e7179543bb9a4","value":{"==":[{"var":"city"},"Bangalore"]},"override_id":"7faf7ac21a000f465344c4c4b403fa62cd9f9a0caecd684fe566f06260677503","created_at":"2024-05-01T12:17:16.160015+00:00","created_by":"superposition@juspay.in","priority":1,"override":{"foo":12}}	\N	delete from dev_cac.contexts ;
4c0cc8dd-866e-4405-8947-cf9362d339af	contexts	postgres	2024-05-05 10:49:25.904006	DELETE	{"id":"fb6c3f081a7d44d73f603c3c1e6ea2b4f6c823c33316aa2629a3864d68d98b66","value":{"and":[{"in":["7191420957775499264-control",{"var":"variantIds"}]}]},"override_id":"4045fc51982e1424d81e02e3e78c68e7a91a3b2077a27beefb443edd05deba78","created_at":"2024-05-01T12:59:26.49421+00:00","created_by":"superposition@juspay.in","priority":1,"override":{"foo":10}}	\N	delete from dev_cac.contexts ;
3ffa7086-85cc-4914-af07-4290e8a5ecd1	contexts	postgres	2024-05-05 10:49:25.904006	DELETE	{"id":"1e4087f940e168cafcff6e2a693eab9ad5ffba71b98d2463af04a9be3ab8fc9a","value":{"and":[{"in":["7191420957775499264-experimental",{"var":"variantIds"}]}]},"override_id":"bbbea2aa79b2e69bbe6905fa9045f29ad4205621b5b4adf16c7e26d01a5a318e","created_at":"2024-05-01T12:59:26.521789+00:00","created_by":"superposition@juspay.in","priority":1,"override":{"foo":14}}	\N	delete from dev_cac.contexts ;
567e1d3a-9592-434e-be39-fd6f2ef2ce4d	contexts	postgres	2024-05-05 10:49:25.904006	DELETE	{"id":"8d2d84ffd2187715f75e43bbc058d8eafef7d1854639818567caef29daf875d8","value":{"":[{"var":"city"},"Bangalore"]},"override_id":"b5be3e559306b2f3ecada2fd438bdd42e8ed9104576f25ebe66a88d007d38772","created_at":"2024-05-02T04:15:39.347124+00:00","created_by":"superposition@juspay.in","priority":1,"override":{"foo":18}}	\N	delete from dev_cac.contexts ;
b4500fb1-e921-49ab-8a2d-139933a00d5c	contexts	postgres	2024-05-05 10:49:25.904006	DELETE	{"id":"5bf54e4783b83f91b2ae1e1d905a2897b007a657bc235475c3881b0f06b6f7a2","value":{"and":[{"":[{"var":"city"},"FooBar"]},{"in":["7191698892931600384-control",{"var":"variantIds"}]}]},"override_id":"4045fc51982e1424d81e02e3e78c68e7a91a3b2077a27beefb443edd05deba78","created_at":"2024-05-02T07:23:51.400605+00:00","created_by":"superposition@juspay.in","priority":2,"override":{"foo":10}}	\N	delete from dev_cac.contexts ;
8448eddf-726a-4745-a7ed-909ecd2489b1	contexts	postgres	2024-05-05 10:49:25.904006	DELETE	{"id":"81b9cf1f4ae22d89f5d3118d1019698abe46cfe481ca3593d61f2acc04bc498c","value":{"and":[{"":[{"var":"city"},"FooBar"]},{"in":["7191698892931600384-experimental",{"var":"variantIds"}]}]},"override_id":"16d291cd4d3204baecba99cd4e9b4a7978719df8265774f13679db03b17c0698","created_at":"2024-05-02T07:23:51.424615+00:00","created_by":"superposition@juspay.in","priority":2,"override":{"foo":43}}	\N	delete from dev_cac.contexts ;
7b36c78f-09a3-42f0-a073-0b9936be9654	default_configs	postgres	2024-05-05 10:53:08.167611	DELETE	{"key":"base-fare","value":25.0,"created_at":"2024-05-05T07:54:40.987208+00:00","created_by":"superposition@juspay.in","schema":{"type":"number"},"function_name":null}	\N	delete from dev_cac.default_configs ;
4efe88b1-61db-4642-b63d-14d4ad5d7eac	default_configs	postgres	2024-05-05 10:53:08.167611	DELETE	{"key":"per-km-rate","value":15.0,"created_at":"2024-05-05T07:54:57.218143+00:00","created_by":"superposition@juspay.in","schema":{"type":"number"},"function_name":null}	\N	delete from dev_cac.default_configs ;
322d902e-449b-4f90-b5c6-f7ca6982ac25	default_configs	postgres	2024-05-05 10:53:08.167611	DELETE	{"key":"currency","value":"INR","created_at":"2024-05-05T07:55:53.354787+00:00","created_by":"superposition@juspay.in","schema":{"enum":["INR","USD","EUR"],"type":"string"},"function_name":null}	\N	delete from dev_cac.default_configs ;
bb85d46f-1707-49d1-b043-a3c27e0aad75	default_configs	postgres	2024-05-05 10:53:22.663514	INSERT	\N	{"key":"base_rate","value":10.0,"created_at":"2024-05-05T10:53:22.654911+00:00","created_by":"superposition@juspay.in","schema":{"type":"number"},"function_name":null}	INSERT INTO "default_configs" ("key", "value", "created_at", "created_by", "schema", "function_name") VALUES ($1, $2, $3, $4, $5, DEFAULT) ON CONFLICT ("key") DO UPDATE SET "value" = $6, "created_at" = $7, "created_by" = $8, "schema" = $9, "function_name" = $10
271c1ba9-68f4-4b17-a32a-03ee035f2f08	default_configs	postgres	2024-05-05 10:53:38.959564	INSERT	\N	{"key":"per_km_rate","value":15.0,"created_at":"2024-05-05T10:53:38.957119+00:00","created_by":"superposition@juspay.in","schema":{"type":"number"},"function_name":null}	INSERT INTO "default_configs" ("key", "value", "created_at", "created_by", "schema", "function_name") VALUES ($1, $2, $3, $4, $5, DEFAULT) ON CONFLICT ("key") DO UPDATE SET "value" = $6, "created_at" = $7, "created_by" = $8, "schema" = $9, "function_name" = $10
423b5a40-026d-481d-92ea-39441b553c64	default_configs	postgres	2024-05-05 10:54:07.600067	INSERT	\N	{"key":"currency","value":"INR","created_at":"2024-05-05T10:54:07.595371+00:00","created_by":"superposition@juspay.in","schema":{"enum":["INR","USD","EUR"],"type":"string"},"function_name":null}	INSERT INTO "default_configs" ("key", "value", "created_at", "created_by", "schema", "function_name") VALUES ($1, $2, $3, $4, $5, DEFAULT) ON CONFLICT ("key") DO UPDATE SET "value" = $6, "created_at" = $7, "created_by" = $8, "schema" = $9, "function_name" = $10
e01a8e7b-7895-413c-a0f2-871f4e1bfd3b	contexts	postgres	2024-05-05 14:56:12.253026	INSERT	\N	{"id":"11c6bd932b8888a9e50be8f822b76116fbaa20fd58b059f98b9e7179543bb9a4","value":{"==":[{"var":"city"},"Bangalore"]},"override_id":"5dfab853b336d9bcac2b24af9690060c730f185cc3709b030a76afc1ce7261ad","created_at":"2024-05-05T14:56:12.2485+00:00","created_by":"superposition@juspay.in","priority":1,"override":{"base_rate":25,"currency":"INR","per_km_rate":5}}	INSERT INTO "contexts" ("id", "value", "override_id", "created_at", "created_by", "priority", "override") VALUES ($1, $2, $3, $4, $5, $6, $7)
83f2719a-fbe9-4123-923c-844857e4f300	contexts	postgres	2024-05-05 15:02:22.520244	INSERT	\N	{"id":"e47edbe36bf3333f42ca2ded0a4604997adc4f6e0bf6176079272597c01353d8","value":{"==":[{"var":"city"},"Chennai"]},"override_id":"5152412bb4f8df280529d1db745b776ecf7503a7217da8f92e3c954c5bd41389","created_at":"2024-05-05T15:02:22.516258+00:00","created_by":"superposition@juspay.in","priority":1,"override":{"base_rate":40,"per_km_rate":25}}	INSERT INTO "contexts" ("id", "value", "override_id", "created_at", "created_by", "priority", "override") VALUES ($1, $2, $3, $4, $5, $6, $7)
5df4e77c-4925-4257-b7ec-fa1874d50365	contexts	postgres	2024-05-05 17:19:50.72273	INSERT	\N	{"id":"f12e3bcd17b3166d8d2cf98bbc0d65ed84057975f11bd3894a4310b900c52c9f","value":{"==":[{"var":"city"},"Paris"]},"override_id":"392ac624c4647cc2d67cc85e0aacabb571f74400290bd729ba905ac70ca47451","created_at":"2024-05-05T17:19:50.719846+00:00","created_by":"superposition@juspay.in","priority":1,"override":{"base_rate":5,"currency":"EUR","per_km_rate":3}}	INSERT INTO "contexts" ("id", "value", "override_id", "created_at", "created_by", "priority", "override") VALUES ($1, $2, $3, $4, $5, $6, $7)
0f4aa355-9d99-4816-b62d-c3bc9f2510b8	default_configs	postgres	2024-05-06 06:37:10.686609	INSERT	\N	{"key":"distance_unit","value":"kilometres","created_at":"2024-05-06T06:37:10.674057+00:00","created_by":"superposition@juspay.in","schema":{"enum":["kilometres","miles"],"type":"string"},"function_name":null}	INSERT INTO "default_configs" ("key", "value", "created_at", "created_by", "schema", "function_name") VALUES ($1, $2, $3, $4, $5, DEFAULT) ON CONFLICT ("key") DO UPDATE SET "value" = $6, "created_at" = $7, "created_by" = $8, "schema" = $9, "function_name" = $10
7f776f86-e31d-4aa2-b3a1-e0ae844c3156	default_configs	postgres	2024-05-06 06:40:16.765211	UPDATE	{"key":"per_km_rate","value":15.0,"created_at":"2024-05-05T10:53:38.957119+00:00","created_by":"superposition@juspay.in","schema":{"type":"number"},"function_name":null}	{"key":"per_distance_unit_rate","value":15.0,"created_at":"2024-05-05T10:53:38.957119+00:00","created_by":"superposition@juspay.in","schema":{"type":"number"},"function_name":null}	update dev_cac.default_configs  set key = 'per_distance_unit_rate' where key = 'per_km_rate';
7a90eb5f-9812-402b-bb37-a870bfe41a7c	contexts	postgres	2024-05-06 06:40:29.521695	DELETE	{"id":"11c6bd932b8888a9e50be8f822b76116fbaa20fd58b059f98b9e7179543bb9a4","value":{"==":[{"var":"city"},"Bangalore"]},"override_id":"5dfab853b336d9bcac2b24af9690060c730f185cc3709b030a76afc1ce7261ad","created_at":"2024-05-05T14:56:12.2485+00:00","created_by":"superposition@juspay.in","priority":1,"override":{"base_rate":25,"currency":"INR","per_km_rate":5}}	\N	delete from dev_cac.contexts ;
acbc9088-4b1f-4b40-8a93-22a9a0446d4f	contexts	postgres	2024-05-06 06:40:29.521695	DELETE	{"id":"e47edbe36bf3333f42ca2ded0a4604997adc4f6e0bf6176079272597c01353d8","value":{"==":[{"var":"city"},"Chennai"]},"override_id":"5152412bb4f8df280529d1db745b776ecf7503a7217da8f92e3c954c5bd41389","created_at":"2024-05-05T15:02:22.516258+00:00","created_by":"superposition@juspay.in","priority":1,"override":{"base_rate":40,"per_km_rate":25}}	\N	delete from dev_cac.contexts ;
b0ddc4b4-fdb4-4df6-ba9e-1d23847feeca	contexts	postgres	2024-05-06 06:40:29.521695	DELETE	{"id":"f12e3bcd17b3166d8d2cf98bbc0d65ed84057975f11bd3894a4310b900c52c9f","value":{"==":[{"var":"city"},"Paris"]},"override_id":"392ac624c4647cc2d67cc85e0aacabb571f74400290bd729ba905ac70ca47451","created_at":"2024-05-05T17:19:50.719846+00:00","created_by":"superposition@juspay.in","priority":1,"override":{"base_rate":5,"currency":"EUR","per_km_rate":3}}	\N	delete from dev_cac.contexts ;
ea6ca8b3-a075-4f1a-a7dc-dccb98bdff03	contexts	postgres	2024-05-06 06:41:33.312274	INSERT	\N	{"id":"11c6bd932b8888a9e50be8f822b76116fbaa20fd58b059f98b9e7179543bb9a4","value":{"==":[{"var":"city"},"Bangalore"]},"override_id":"71687fff7d7e42f6d3962999344f1bc8a5aed12785a2d5851f63298896c56392","created_at":"2024-05-06T06:41:33.309596+00:00","created_by":"superposition@juspay.in","priority":1,"override":{"base_rate":20,"currency":"INR","distance_unit":"kilometres","per_distance_unit_rate":15}}	INSERT INTO "contexts" ("id", "value", "override_id", "created_at", "created_by", "priority", "override") VALUES ($1, $2, $3, $4, $5, $6, $7)
23936b31-e631-464e-aeb6-f6686ea5d1a6	contexts	postgres	2024-05-06 06:41:45.596988	INSERT	\N	{"id":"e47edbe36bf3333f42ca2ded0a4604997adc4f6e0bf6176079272597c01353d8","value":{"==":[{"var":"city"},"Chennai"]},"override_id":"19339f445abe140836795d6068a887c9e1697496269a1486da02f73f59d93821","created_at":"2024-05-06T06:41:45.593764+00:00","created_by":"superposition@juspay.in","priority":1,"override":{"base_rate":25,"currency":"INR","distance_unit":"kilometres","per_distance_unit_rate":20}}	INSERT INTO "contexts" ("id", "value", "override_id", "created_at", "created_by", "priority", "override") VALUES ($1, $2, $3, $4, $5, $6, $7)
2bf7ebd6-1c9b-4b3c-880e-4184ab90f5ed	contexts	postgres	2024-05-06 06:58:48.727481	INSERT	\N	{"id":"1672b4b6f0a3e9729cce608d8cb013e5cdffcbca59464e85114f4b4731eb8a35","value":{"==":[{"var":"city"},"New York"]},"override_id":"15c70cfe4602e1df69ce5353f590f6a1de2d10df8bce991a28e32e5f7ba0e026","created_at":"2024-05-06T06:58:48.723229+00:00","created_by":"superposition@juspay.in","priority":1,"override":{"base_rate":5,"currency":"USD","distance_unit":"miles","per_distance_unit_rate":2.5}}	INSERT INTO "contexts" ("id", "value", "override_id", "created_at", "created_by", "priority", "override") VALUES ($1, $2, $3, $4, $5, $6, $7)
ad992a6a-ae69-4f74-bcea-453ef8b17e97	contexts	postgres	2024-05-06 07:02:08.017873	DELETE	{"id":"11c6bd932b8888a9e50be8f822b76116fbaa20fd58b059f98b9e7179543bb9a4","value":{"==":[{"var":"city"},"Bangalore"]},"override_id":"71687fff7d7e42f6d3962999344f1bc8a5aed12785a2d5851f63298896c56392","created_at":"2024-05-06T06:41:33.309596+00:00","created_by":"superposition@juspay.in","priority":1,"override":{"base_rate":20,"currency":"INR","distance_unit":"kilometres","per_distance_unit_rate":15}}	\N	delete from dev_cac.contexts ;
2c2f518f-3473-4c03-bc37-01a903ce945c	contexts	postgres	2024-05-06 07:02:08.017873	DELETE	{"id":"e47edbe36bf3333f42ca2ded0a4604997adc4f6e0bf6176079272597c01353d8","value":{"==":[{"var":"city"},"Chennai"]},"override_id":"19339f445abe140836795d6068a887c9e1697496269a1486da02f73f59d93821","created_at":"2024-05-06T06:41:45.593764+00:00","created_by":"superposition@juspay.in","priority":1,"override":{"base_rate":25,"currency":"INR","distance_unit":"kilometres","per_distance_unit_rate":20}}	\N	delete from dev_cac.contexts ;
bc8b7490-c840-4a20-b0fb-a2bc9ce33df8	contexts	postgres	2024-05-06 07:02:08.017873	DELETE	{"id":"1672b4b6f0a3e9729cce608d8cb013e5cdffcbca59464e85114f4b4731eb8a35","value":{"==":[{"var":"city"},"New York"]},"override_id":"15c70cfe4602e1df69ce5353f590f6a1de2d10df8bce991a28e32e5f7ba0e026","created_at":"2024-05-06T06:58:48.723229+00:00","created_by":"superposition@juspay.in","priority":1,"override":{"base_rate":5,"currency":"USD","distance_unit":"miles","per_distance_unit_rate":2.5}}	\N	delete from dev_cac.contexts ;
960edc22-3a48-4111-87bc-550f72c0ef39	dimensions	postgres	2024-05-06 06:42:47.707076	UPDATE	{"dimension":"city","priority":1,"created_at":"2024-05-01T12:16:59.242405+00:00","created_by":"superposition@juspay.in","schema":{"pattern":"[a-zA-Z]+","type":"string"},"function_name":null}	{"dimension":"city","priority":1,"created_at":"2024-05-06T06:42:47.70463+00:00","created_by":"superposition@juspay.in","schema":{"pattern":"[a-zA-Z ]+","type":"string"},"function_name":null}	INSERT INTO "dimensions" ("dimension", "priority", "created_at", "created_by", "schema", "function_name") VALUES ($1, $2, $3, $4, $5, DEFAULT) ON CONFLICT ("dimension") DO UPDATE SET "priority" = $6, "created_at" = $7, "created_by" = $8, "schema" = $9, "function_name" = $10 RETURNING "dimensions"."dimension", "dimensions"."priority", "dimensions"."created_at", "dimensions"."created_by", "dimensions"."schema", "dimensions"."function_name"
39752f99-b8dc-4685-b9cb-9166682bf434	default_configs	postgres	2024-05-06 07:01:59.166724	UPDATE	{"key":"distance_unit","value":"kilometres","created_at":"2024-05-06T06:37:10.674057+00:00","created_by":"superposition@juspay.in","schema":{"enum":["kilometres","miles"],"type":"string"},"function_name":null}	{"key":"distance_unit","value":"Km","created_at":"2024-05-06T07:01:59.163885+00:00","created_by":"superposition@juspay.in","schema":{"enum":["Km","Miles"],"type":"string"},"function_name":null}	INSERT INTO "default_configs" ("key", "value", "created_at", "created_by", "schema", "function_name") VALUES ($1, $2, $3, $4, $5, DEFAULT) ON CONFLICT ("key") DO UPDATE SET "value" = $6, "created_at" = $7, "created_by" = $8, "schema" = $9, "function_name" = $10
19c9977a-470a-4215-91b4-1771daed6d3d	contexts	postgres	2024-05-06 07:02:48.724584	INSERT	\N	{"id":"11c6bd932b8888a9e50be8f822b76116fbaa20fd58b059f98b9e7179543bb9a4","value":{"==":[{"var":"city"},"Bangalore"]},"override_id":"795dc46ccb2b513104f7e7f1688b7f20a04502b6afaa7dbf68ae7a4588c94fde","created_at":"2024-05-06T07:02:48.718942+00:00","created_by":"superposition@juspay.in","priority":1,"override":{"base_rate":20,"currency":"INR","distance_unit":"Km","per_distance_unit_rate":15}}	INSERT INTO "contexts" ("id", "value", "override_id", "created_at", "created_by", "priority", "override") VALUES ($1, $2, $3, $4, $5, $6, $7)
ca009789-9c9e-4c84-8c3e-bb9a2766a744	contexts	postgres	2024-05-06 07:05:52.998888	INSERT	\N	{"id":"1672b4b6f0a3e9729cce608d8cb013e5cdffcbca59464e85114f4b4731eb8a35","value":{"==":[{"var":"city"},"New York"]},"override_id":"00e985580ef9edb57d54e30de21acca2449cecda239e23be60d9bae6270f847c","created_at":"2024-05-06T07:05:52.994267+00:00","created_by":"superposition@juspay.in","priority":1,"override":{"base_rate":5,"currency":"USD","distance_unit":"Miles","per_distance_unit_rate":2.5}}	INSERT INTO "contexts" ("id", "value", "override_id", "created_at", "created_by", "priority", "override") VALUES ($1, $2, $3, $4, $5, $6, $7)
806ac0e5-6f89-4f2d-9a0e-45330b939ae3	contexts	postgres	2024-05-06 07:07:34.312018	DELETE	{"id":"1672b4b6f0a3e9729cce608d8cb013e5cdffcbca59464e85114f4b4731eb8a35","value":{"==":[{"var":"city"},"New York"]},"override_id":"00e985580ef9edb57d54e30de21acca2449cecda239e23be60d9bae6270f847c","created_at":"2024-05-06T07:05:52.994267+00:00","created_by":"superposition@juspay.in","priority":1,"override":{"base_rate":5,"currency":"USD","distance_unit":"Miles","per_distance_unit_rate":2.5}}	\N	delete from dev_cac.contexts where id = '1672b4b6f0a3e9729cce608d8cb013e5cdffcbca59464e85114f4b4731eb8a35';
45a494b6-09d2-4c9c-adbc-151118ca019c	contexts	postgres	2024-05-06 07:08:11.416527	INSERT	\N	{"id":"3f0e5fe95e2e758151d4cef712185767a137c926a9e7156b6face3a98b167d35","value":{"==":[{"var":"city"},"Seattle"]},"override_id":"00e985580ef9edb57d54e30de21acca2449cecda239e23be60d9bae6270f847c","created_at":"2024-05-06T07:08:11.41364+00:00","created_by":"superposition@juspay.in","priority":1,"override":{"base_rate":5,"currency":"USD","distance_unit":"Miles","per_distance_unit_rate":2.5}}	INSERT INTO "contexts" ("id", "value", "override_id", "created_at", "created_by", "priority", "override") VALUES ($1, $2, $3, $4, $5, $6, $7)
a0ae41dc-f30a-467d-a8eb-b1af161d5f7c	default_configs	postgres	2024-05-07 15:02:11.517048	INSERT	\N	{"key":"bar.com","value":10,"created_at":"2024-05-07T15:02:11.504644+00:00","created_by":"superposition@juspay.in","schema":{"type":"number"},"function_name":null}	INSERT INTO "default_configs" ("key", "value", "created_at", "created_by", "schema", "function_name") VALUES ($1, $2, $3, $4, $5, DEFAULT) ON CONFLICT ("key") DO UPDATE SET "value" = $6, "created_at" = $7, "created_by" = $8, "schema" = $9, "function_name" = $10
eb7b97c5-e11a-40e3-b4e6-e926ad926f83	default_configs	postgres	2024-05-07 15:02:31.134231	INSERT	\N	{"key":"bar.foo","value":10.0,"created_at":"2024-05-07T15:02:31.131308+00:00","created_by":"superposition@juspay.in","schema":{"type":"number"},"function_name":null}	INSERT INTO "default_configs" ("key", "value", "created_at", "created_by", "schema", "function_name") VALUES ($1, $2, $3, $4, $5, DEFAULT) ON CONFLICT ("key") DO UPDATE SET "value" = $6, "created_at" = $7, "created_by" = $8, "schema" = $9, "function_name" = $10
15a84e44-06e9-4373-b3e7-03b890d25322	dimensions	postgres	2024-05-07 17:44:05.1437	INSERT	\N	{"dimension":"vehicle_type","priority":2,"created_at":"2024-05-07T17:44:05.136115+00:00","created_by":"superposition@juspay.in","schema":{"enum":["cab","auto"],"type":"string"},"function_name":null}	INSERT INTO "dimensions" ("dimension", "priority", "created_at", "created_by", "schema", "function_name") VALUES ($1, $2, $3, $4, $5, DEFAULT) ON CONFLICT ("dimension") DO UPDATE SET "priority" = $6, "created_at" = $7, "created_by" = $8, "schema" = $9, "function_name" = $10 RETURNING "dimensions"."dimension", "dimensions"."priority", "dimensions"."created_at", "dimensions"."created_by", "dimensions"."schema", "dimensions"."function_name"
d586c6ad-5c07-4b9f-94b6-621579663e74	dimensions	postgres	2024-05-07 17:44:11.247902	UPDATE	{"dimension":"city","priority":1,"created_at":"2024-05-06T06:42:47.70463+00:00","created_by":"superposition@juspay.in","schema":{"pattern":"[a-zA-Z ]+","type":"string"},"function_name":null}	{"dimension":"city","priority":2,"created_at":"2024-05-07T17:44:11.244241+00:00","created_by":"superposition@juspay.in","schema":{"pattern":"[a-zA-Z ]+","type":"string"},"function_name":null}	INSERT INTO "dimensions" ("dimension", "priority", "created_at", "created_by", "schema", "function_name") VALUES ($1, $2, $3, $4, $5, DEFAULT) ON CONFLICT ("dimension") DO UPDATE SET "priority" = $6, "created_at" = $7, "created_by" = $8, "schema" = $9, "function_name" = $10 RETURNING "dimensions"."dimension", "dimensions"."priority", "dimensions"."created_at", "dimensions"."created_by", "dimensions"."schema", "dimensions"."function_name"
e5b90531-ad8c-4157-934c-f25fd13b3bd2	dimensions	postgres	2024-05-07 17:44:16.718568	UPDATE	{"dimension":"city","priority":2,"created_at":"2024-05-07T17:44:11.244241+00:00","created_by":"superposition@juspay.in","schema":{"pattern":"[a-zA-Z ]+","type":"string"},"function_name":null}	{"dimension":"city","priority":4,"created_at":"2024-05-07T17:44:16.716277+00:00","created_by":"superposition@juspay.in","schema":{"pattern":"[a-zA-Z ]+","type":"string"},"function_name":null}	INSERT INTO "dimensions" ("dimension", "priority", "created_at", "created_by", "schema", "function_name") VALUES ($1, $2, $3, $4, $5, DEFAULT) ON CONFLICT ("dimension") DO UPDATE SET "priority" = $6, "created_at" = $7, "created_by" = $8, "schema" = $9, "function_name" = $10 RETURNING "dimensions"."dimension", "dimensions"."priority", "dimensions"."created_at", "dimensions"."created_by", "dimensions"."schema", "dimensions"."function_name"
6df10707-60bc-4de5-bf4d-48ea4270c7be	contexts	postgres	2024-05-07 17:51:39.33541	INSERT	\N	{"id":"9fb61ecfb662467e9036ec79bfd8c02ac244c1836f113242aec5fe6d9265d8dc","value":{"and":[{"==":[{"var":"city"},"Bangalore"]},{"==":[{"var":"vehicle_type"},"cab"]}]},"override_id":"e2411e811b927824f910f9edd75b984a3ad226c46a1216ffa735b7bda60c1ede","created_at":"2024-05-07T17:51:39.330747+00:00","created_by":"superposition@juspay.in","priority":6,"override":{"base_rate":12}}	INSERT INTO "contexts" ("id", "value", "override_id", "created_at", "created_by", "priority", "override") VALUES ($1, $2, $3, $4, $5, $6, $7)
dd1bbf71-7a83-4940-bb55-28ac59747e21	dimensions	postgres	2024-05-08 08:01:36.622328	INSERT	\N	{"dimension":"hour_of_day","priority":32,"created_at":"2024-05-08T08:01:36.617931+00:00","created_by":"superposition@juspay.in","schema":{"type":"number"},"function_name":null}	INSERT INTO "dimensions" ("dimension", "priority", "created_at", "created_by", "schema", "function_name") VALUES ($1, $2, $3, $4, $5, DEFAULT) ON CONFLICT ("dimension") DO UPDATE SET "priority" = $6, "created_at" = $7, "created_by" = $8, "schema" = $9, "function_name" = $10 RETURNING "dimensions"."dimension", "dimensions"."priority", "dimensions"."created_at", "dimensions"."created_by", "dimensions"."schema", "dimensions"."function_name"
2874b23f-39ff-480a-ba8c-ab4bbf382aa2	contexts	postgres	2024-05-08 08:02:50.14613	INSERT	\N	{"id":"d8256cd0c9fbbfae460057ed14f0630f4dd0b050cf271832b0c9bfb5e31ca43b","value":{"and":[{"==":[{"var":"city"},"Bangalore"]},{"==":[{"var":"vehicle_type"},"cab"]},{"==":[{"var":"hour_of_day"},9]}]},"override_id":"204dc317acb302d38289cbfbc3227461d45bdde8dc5bfc0f6f961a8bb476e910","created_at":"2024-05-08T08:02:50.144267+00:00","created_by":"superposition@juspay.in","priority":38,"override":{"base_rate":100,"per_distance_unit_rate":50}}	INSERT INTO "contexts" ("id", "value", "override_id", "created_at", "created_by", "priority", "override") VALUES ($1, $2, $3, $4, $5, $6, $7)
c78499a0-2a7f-4cd5-baa7-172397d46069	default_configs	postgres	2024-05-08 09:12:55.795319	INSERT	\N	{"key":"hello_message","value":"","created_at":"2024-05-08T09:12:55.781855+00:00","created_by":"superposition@juspay.in","schema":{"pattern":".*","type":"string"},"function_name":null}	INSERT INTO "default_configs" ("key", "value", "created_at", "created_by", "schema", "function_name") VALUES ($1, $2, $3, $4, $5, DEFAULT) ON CONFLICT ("key") DO UPDATE SET "value" = $6, "created_at" = $7, "created_by" = $8, "schema" = $9, "function_name" = $10
bb762a1c-fb3a-462c-a4fa-c948152ff6cc	default_configs	postgres	2024-05-08 09:13:14.016507	UPDATE	{"key":"hello_message","value":"","created_at":"2024-05-08T09:12:55.781855+00:00","created_by":"superposition@juspay.in","schema":{"pattern":".*","type":"string"},"function_name":null}	{"key":"hello_message","value":"Hello World !!!","created_at":"2024-05-08T09:13:14.009736+00:00","created_by":"superposition@juspay.in","schema":{"pattern":".*","type":"string"},"function_name":null}	INSERT INTO "default_configs" ("key", "value", "created_at", "created_by", "schema", "function_name") VALUES ($1, $2, $3, $4, $5, DEFAULT) ON CONFLICT ("key") DO UPDATE SET "value" = $6, "created_at" = $7, "created_by" = $8, "schema" = $9, "function_name" = $10
806af1ae-8580-445d-b8d1-7ba092d4ece6	contexts	postgres	2024-05-08 09:15:03.014944	UPDATE	{"id":"11c6bd932b8888a9e50be8f822b76116fbaa20fd58b059f98b9e7179543bb9a4","value":{"==":[{"var":"city"},"Bangalore"]},"override_id":"795dc46ccb2b513104f7e7f1688b7f20a04502b6afaa7dbf68ae7a4588c94fde","created_at":"2024-05-06T07:02:48.718942+00:00","created_by":"superposition@juspay.in","priority":1,"override":{"base_rate":20,"currency":"INR","distance_unit":"Km","per_distance_unit_rate":15}}	{"id":"11c6bd932b8888a9e50be8f822b76116fbaa20fd58b059f98b9e7179543bb9a4","value":{"==":[{"var":"city"},"Bangalore"]},"override_id":"4da0308c37eaa8bd32f4b857d6fac95eb7305f38610f13ff048217f741af4381","created_at":"2024-05-08T09:15:03.004255+00:00","created_by":"superposition@juspay.in","priority":4,"override":{"base_rate":20,"currency":"INR","distance_unit":"Km","hello_message":"ನಮ್ಮ ಬೆಂಗಳೂರು","per_distance_unit_rate":15}}	UPDATE "contexts" SET "value" = $1, "override_id" = $2, "created_at" = $3, "created_by" = $4, "priority" = $5, "override" = $6 WHERE ("contexts"."id" = $7)
f2a1d7ad-7ed2-4513-8065-8a0ab2e127bc	contexts	postgres	2024-05-08 09:38:24.978965	INSERT	\N	{"id":"e47edbe36bf3333f42ca2ded0a4604997adc4f6e0bf6176079272597c01353d8","value":{"==":[{"var":"city"},"Chennai"]},"override_id":"024af06619749624e28e725a062ae988c3827ab8fa439565dcac218a4d6f2df8","created_at":"2024-05-08T09:38:24.97809+00:00","created_by":"superposition@juspay.in","priority":4,"override":{"hello_message":"வணக்கம் சென்னை"}}	INSERT INTO "contexts" ("id", "value", "override_id", "created_at", "created_by", "priority", "override") VALUES ($1, $2, $3, $4, $5, $6, $7)
1d607447-5bea-4d65-abbe-aee152606137	default_configs	postgres	2024-05-08 10:25:50.555039	INSERT	\N	{"key":"hello_message_color","value":"black","created_at":"2024-05-08T10:25:50.551915+00:00","created_by":"superposition@juspay.in","schema":{"enum":["black","red","blue","green","pink"],"type":"string"},"function_name":null}	INSERT INTO "default_configs" ("key", "value", "created_at", "created_by", "schema", "function_name") VALUES ($1, $2, $3, $4, $5, DEFAULT) ON CONFLICT ("key") DO UPDATE SET "value" = $6, "created_at" = $7, "created_by" = $8, "schema" = $9, "function_name" = $10
48884ece-d45f-459c-b066-3c6bd6a93a02	contexts	postgres	2024-05-08 10:28:06.68553	UPDATE	{"id":"11c6bd932b8888a9e50be8f822b76116fbaa20fd58b059f98b9e7179543bb9a4","value":{"==":[{"var":"city"},"Bangalore"]},"override_id":"4da0308c37eaa8bd32f4b857d6fac95eb7305f38610f13ff048217f741af4381","created_at":"2024-05-08T09:15:03.004255+00:00","created_by":"superposition@juspay.in","priority":4,"override":{"base_rate":20,"currency":"INR","distance_unit":"Km","hello_message":"ನಮ್ಮ ಬೆಂಗಳೂರು","per_distance_unit_rate":15}}	{"id":"11c6bd932b8888a9e50be8f822b76116fbaa20fd58b059f98b9e7179543bb9a4","value":{"==":[{"var":"city"},"Bangalore"]},"override_id":"f2e29b63c08871a60ee3c2f4c66cc6772ee596f462d6f4bf1bd8ab6198da457b","created_at":"2024-05-08T10:28:06.674859+00:00","created_by":"superposition@juspay.in","priority":4,"override":{"base_rate":20,"currency":"INR","distance_unit":"Km","hello_message":"ನಮ್ಮ ಬೆಂಗಳೂರು","hello_message_color":"pink","per_distance_unit_rate":15}}	UPDATE "contexts" SET "value" = $1, "override_id" = $2, "created_at" = $3, "created_by" = $4, "priority" = $5, "override" = $6 WHERE ("contexts"."id" = $7)
67c1882d-c15d-45bf-99f2-39eedc7e323a	contexts	postgres	2024-05-08 10:28:33.152792	UPDATE	{"id":"11c6bd932b8888a9e50be8f822b76116fbaa20fd58b059f98b9e7179543bb9a4","value":{"==":[{"var":"city"},"Bangalore"]},"override_id":"f2e29b63c08871a60ee3c2f4c66cc6772ee596f462d6f4bf1bd8ab6198da457b","created_at":"2024-05-08T10:28:06.674859+00:00","created_by":"superposition@juspay.in","priority":4,"override":{"base_rate":20,"currency":"INR","distance_unit":"Km","hello_message":"ನಮ್ಮ ಬೆಂಗಳೂರು","hello_message_color":"pink","per_distance_unit_rate":15}}	{"id":"11c6bd932b8888a9e50be8f822b76116fbaa20fd58b059f98b9e7179543bb9a4","value":{"==":[{"var":"city"},"Bangalore"]},"override_id":"a11cf3b9993dcc9e9676b42b18f1a2a88e76a4708096423eb5299f542eee30f9","created_at":"2024-05-08T10:28:33.140039+00:00","created_by":"superposition@juspay.in","priority":4,"override":{"base_rate":20,"currency":"INR","distance_unit":"Km","hello_message":"ನಮ್ಮ ಬೆಂಗಳೂರು","hello_message_color":"red","per_distance_unit_rate":15}}	UPDATE "contexts" SET "value" = $1, "override_id" = $2, "created_at" = $3, "created_by" = $4, "priority" = $5, "override" = $6 WHERE ("contexts"."id" = $7)
8810fc14-28cb-4b83-8da3-35d09665cc58	contexts	postgres	2024-05-08 10:28:51.965506	UPDATE	{"id":"e47edbe36bf3333f42ca2ded0a4604997adc4f6e0bf6176079272597c01353d8","value":{"==":[{"var":"city"},"Chennai"]},"override_id":"024af06619749624e28e725a062ae988c3827ab8fa439565dcac218a4d6f2df8","created_at":"2024-05-08T09:38:24.97809+00:00","created_by":"superposition@juspay.in","priority":4,"override":{"hello_message":"வணக்கம் சென்னை"}}	{"id":"e47edbe36bf3333f42ca2ded0a4604997adc4f6e0bf6176079272597c01353d8","value":{"==":[{"var":"city"},"Chennai"]},"override_id":"f861adc6d826c313d638bfee24e531a10bf4da060a2499c29772e9d9e80f4da2","created_at":"2024-05-08T10:28:51.959772+00:00","created_by":"superposition@juspay.in","priority":4,"override":{"hello_message":"வணக்கம் சென்னை","hello_message_color":"green"}}	UPDATE "contexts" SET "value" = $1, "override_id" = $2, "created_at" = $3, "created_by" = $4, "priority" = $5, "override" = $6 WHERE ("contexts"."id" = $7)
756bde30-813f-47a1-854e-cc1a0de88313	contexts	postgres	2024-05-08 10:29:52.294663	UPDATE	{"id":"11c6bd932b8888a9e50be8f822b76116fbaa20fd58b059f98b9e7179543bb9a4","value":{"==":[{"var":"city"},"Bangalore"]},"override_id":"a11cf3b9993dcc9e9676b42b18f1a2a88e76a4708096423eb5299f542eee30f9","created_at":"2024-05-08T10:28:33.140039+00:00","created_by":"superposition@juspay.in","priority":4,"override":{"base_rate":20,"currency":"INR","distance_unit":"Km","hello_message":"ನಮ್ಮ ಬೆಂಗಳೂರು","hello_message_color":"red","per_distance_unit_rate":15}}	{"id":"11c6bd932b8888a9e50be8f822b76116fbaa20fd58b059f98b9e7179543bb9a4","value":{"==":[{"var":"city"},"Bangalore"]},"override_id":"eead504894c48cc5d1c03ceee57097fba335116ae9cbad2e84fce9cf10797d8c","created_at":"2024-05-08T10:29:52.286889+00:00","created_by":"superposition@juspay.in","priority":4,"override":{"base_rate":20,"currency":"INR","distance_unit":"Km","hello_message":"ನಮಸ್ಕಾರ ಬೆಂಗಳೂರು","hello_message_color":"red","per_distance_unit_rate":15}}	UPDATE "contexts" SET "value" = $1, "override_id" = $2, "created_at" = $3, "created_by" = $4, "priority" = $5, "override" = $6 WHERE ("contexts"."id" = $7)
63ef0485-fbf9-4b58-81b4-79bc86f953b0	contexts	postgres	2024-05-08 10:33:30.533295	UPDATE	{"id":"3f0e5fe95e2e758151d4cef712185767a137c926a9e7156b6face3a98b167d35","value":{"==":[{"var":"city"},"Seattle"]},"override_id":"00e985580ef9edb57d54e30de21acca2449cecda239e23be60d9bae6270f847c","created_at":"2024-05-06T07:08:11.41364+00:00","created_by":"superposition@juspay.in","priority":1,"override":{"base_rate":5,"currency":"USD","distance_unit":"Miles","per_distance_unit_rate":2.5}}	{"id":"3f0e5fe95e2e758151d4cef712185767a137c926a9e7156b6face3a98b167d35","value":{"==":[{"var":"city"},"Seattle"]},"override_id":"014160b8e1f1c1f80c4e9c2e2c6614e72fb0917e2c9109602788b2ecaf64ef0f","created_at":"2024-05-08T10:33:30.518223+00:00","created_by":"superposition@juspay.in","priority":4,"override":{"base_rate":5,"currency":"USD","distance_unit":"Miles","hello_message":"Hello Seattle","per_distance_unit_rate":2.5}}	UPDATE "contexts" SET "value" = $1, "override_id" = $2, "created_at" = $3, "created_by" = $4, "priority" = $5, "override" = $6 WHERE ("contexts"."id" = $7)
d2fc0f9a-4631-4631-a5fa-3276ca3ed2fe	contexts	postgres	2024-05-08 10:34:25.099774	UPDATE	{"id":"3f0e5fe95e2e758151d4cef712185767a137c926a9e7156b6face3a98b167d35","value":{"==":[{"var":"city"},"Seattle"]},"override_id":"014160b8e1f1c1f80c4e9c2e2c6614e72fb0917e2c9109602788b2ecaf64ef0f","created_at":"2024-05-08T10:33:30.518223+00:00","created_by":"superposition@juspay.in","priority":4,"override":{"base_rate":5,"currency":"USD","distance_unit":"Miles","hello_message":"Hello Seattle","per_distance_unit_rate":2.5}}	{"id":"3f0e5fe95e2e758151d4cef712185767a137c926a9e7156b6face3a98b167d35","value":{"==":[{"var":"city"},"Seattle"]},"override_id":"de147cd0db5fb97de6a60fe45c54aec01ef1cc7c02659110d5899486b8c95257","created_at":"2024-05-08T10:34:25.089476+00:00","created_by":"superposition@juspay.in","priority":4,"override":{"base_rate":5,"currency":"USD","distance_unit":"Miles","hello_message":"Hello Seattle","hello_message_color":"blue","per_distance_unit_rate":2.5}}	UPDATE "contexts" SET "value" = $1, "override_id" = $2, "created_at" = $3, "created_by" = $4, "priority" = $5, "override" = $6 WHERE ("contexts"."id" = $7)
a2ae8276-20fa-489a-8d30-2e09f116bd21	default_configs	postgres	2024-05-10 14:44:29.40222	INSERT	\N	{"key":"logo","value":"https://cdn1.vectorstock.com/i/1000x1000/03/20/big-city-logo-vector-20480320.jpg","created_at":"2024-05-10T14:44:29.391516+00:00","created_by":"user@superposition.io","schema":{"pattern":"https://.*","type":"string"},"function_name":null}	INSERT INTO "default_configs" ("key", "value", "created_at", "created_by", "schema", "function_name") VALUES ($1, $2, $3, $4, $5, DEFAULT) ON CONFLICT ("key") DO UPDATE SET "value" = $6, "created_at" = $7, "created_by" = $8, "schema" = $9, "function_name" = $10
f29d4303-fb76-4b25-912c-e673142b175e	contexts	postgres	2024-05-10 14:45:07.776303	UPDATE	{"id":"11c6bd932b8888a9e50be8f822b76116fbaa20fd58b059f98b9e7179543bb9a4","value":{"==":[{"var":"city"},"Bangalore"]},"override_id":"eead504894c48cc5d1c03ceee57097fba335116ae9cbad2e84fce9cf10797d8c","created_at":"2024-05-08T10:29:52.286889+00:00","created_by":"superposition@juspay.in","priority":4,"override":{"base_rate":20,"currency":"INR","distance_unit":"Km","hello_message":"ನಮಸ್ಕಾರ ಬೆಂಗಳೂರು","hello_message_color":"red","per_distance_unit_rate":15}}	{"id":"11c6bd932b8888a9e50be8f822b76116fbaa20fd58b059f98b9e7179543bb9a4","value":{"==":[{"var":"city"},"Bangalore"]},"override_id":"932a2b3502038c2e975d9ace00c90f016860ecb14564cf13085b0d40d1959153","created_at":"2024-05-10T14:45:07.769368+00:00","created_by":"user@superposition.io","priority":4,"override":{"base_rate":20,"currency":"INR","distance_unit":"Km","hello_message":"ನಮಸ್ಕಾರ ಬೆಂಗಳೂರು","hello_message_color":"red","logo":"https://www.shutterstock.com/image-vector/bangalore-logo-vidhana-soudha-600nw-1506258893.jpg","per_distance_unit_rate":15}}	UPDATE "contexts" SET "value" = $1, "override_id" = $2, "created_at" = $3, "created_by" = $4, "priority" = $5, "override" = $6 WHERE ("contexts"."id" = $7)
755bce98-ea99-4ec8-aff9-835f1b73dd91	contexts	postgres	2024-05-10 14:45:34.793726	UPDATE	{"id":"e47edbe36bf3333f42ca2ded0a4604997adc4f6e0bf6176079272597c01353d8","value":{"==":[{"var":"city"},"Chennai"]},"override_id":"f861adc6d826c313d638bfee24e531a10bf4da060a2499c29772e9d9e80f4da2","created_at":"2024-05-08T10:28:51.959772+00:00","created_by":"superposition@juspay.in","priority":4,"override":{"hello_message":"வணக்கம் சென்னை","hello_message_color":"green"}}	{"id":"e47edbe36bf3333f42ca2ded0a4604997adc4f6e0bf6176079272597c01353d8","value":{"==":[{"var":"city"},"Chennai"]},"override_id":"d6ee14a723773959add518b640becd77978f93799a878a6d2ead04a578902849","created_at":"2024-05-10T14:45:34.785525+00:00","created_by":"user@superposition.io","priority":4,"override":{"hello_message":"வணக்கம் சென்னை","hello_message_color":"green","logo":"https://upload.wikimedia.org/wikipedia/commons/d/d9/Inside_Chennai_logo.png"}}	UPDATE "contexts" SET "value" = $1, "override_id" = $2, "created_at" = $3, "created_by" = $4, "priority" = $5, "override" = $6 WHERE ("contexts"."id" = $7)
3e17da47-d882-4de8-8504-8eeb5f5f9abd	contexts	postgres	2024-05-10 14:45:47.949706	UPDATE	{"id":"3f0e5fe95e2e758151d4cef712185767a137c926a9e7156b6face3a98b167d35","value":{"==":[{"var":"city"},"Seattle"]},"override_id":"de147cd0db5fb97de6a60fe45c54aec01ef1cc7c02659110d5899486b8c95257","created_at":"2024-05-08T10:34:25.089476+00:00","created_by":"superposition@juspay.in","priority":4,"override":{"base_rate":5,"currency":"USD","distance_unit":"Miles","hello_message":"Hello Seattle","hello_message_color":"blue","per_distance_unit_rate":2.5}}	{"id":"3f0e5fe95e2e758151d4cef712185767a137c926a9e7156b6face3a98b167d35","value":{"==":[{"var":"city"},"Seattle"]},"override_id":"e425ad03b83e0ee850581973a7da762e93f12e607eed79ab7d84648bc7507fa5","created_at":"2024-05-10T14:45:47.946646+00:00","created_by":"user@superposition.io","priority":4,"override":{"base_rate":5,"currency":"USD","distance_unit":"Miles","hello_message":"Hello Seattle","hello_message_color":"blue","logo":"https://i.pinimg.com/originals/1e/5a/f4/1e5af416b9756d32cf1c1daa87f2682e.jpg","per_distance_unit_rate":2.5}}	UPDATE "contexts" SET "value" = $1, "override_id" = $2, "created_at" = $3, "created_by" = $4, "priority" = $5, "override" = $6 WHERE ("contexts"."id" = $7)
7bf4fb23-cc1e-4f70-aa5c-926f6ce687b4	contexts	postgres	2024-05-10 14:48:42.132738	UPDATE	{"id":"e47edbe36bf3333f42ca2ded0a4604997adc4f6e0bf6176079272597c01353d8","value":{"==":[{"var":"city"},"Chennai"]},"override_id":"d6ee14a723773959add518b640becd77978f93799a878a6d2ead04a578902849","created_at":"2024-05-10T14:45:34.785525+00:00","created_by":"user@superposition.io","priority":4,"override":{"hello_message":"வணக்கம் சென்னை","hello_message_color":"green","logo":"https://upload.wikimedia.org/wikipedia/commons/d/d9/Inside_Chennai_logo.png"}}	{"id":"e47edbe36bf3333f42ca2ded0a4604997adc4f6e0bf6176079272597c01353d8","value":{"==":[{"var":"city"},"Chennai"]},"override_id":"645b12a924823be87eb423791f3c42393ab19103afcd8ce923c4a1112408b32b","created_at":"2024-05-10T14:48:42.122682+00:00","created_by":"user@superposition.io","priority":4,"override":{"hello_message":"வணக்கம் சென்னை","hello_message_color":"green","logo":"https://www.shutterstock.com/image-vector/chennai-skyline-color-landmarks-blue-260nw-515862346.jpg"}}	UPDATE "contexts" SET "value" = $1, "override_id" = $2, "created_at" = $3, "created_by" = $4, "priority" = $5, "override" = $6 WHERE ("contexts"."id" = $7)
96ce4c3f-3a08-4c44-b9b3-adfcc7f3ae7b	contexts	postgres	2024-05-10 14:50:19.475281	UPDATE	{"id":"3f0e5fe95e2e758151d4cef712185767a137c926a9e7156b6face3a98b167d35","value":{"==":[{"var":"city"},"Seattle"]},"override_id":"e425ad03b83e0ee850581973a7da762e93f12e607eed79ab7d84648bc7507fa5","created_at":"2024-05-10T14:45:47.946646+00:00","created_by":"user@superposition.io","priority":4,"override":{"base_rate":5,"currency":"USD","distance_unit":"Miles","hello_message":"Hello Seattle","hello_message_color":"blue","logo":"https://i.pinimg.com/originals/1e/5a/f4/1e5af416b9756d32cf1c1daa87f2682e.jpg","per_distance_unit_rate":2.5}}	{"id":"3f0e5fe95e2e758151d4cef712185767a137c926a9e7156b6face3a98b167d35","value":{"==":[{"var":"city"},"Seattle"]},"override_id":"f73412831793a34df7df95a79bc65754b1eebf31534e4ccfa9c79681fcf8ac69","created_at":"2024-05-10T14:50:19.471563+00:00","created_by":"user@superposition.io","priority":4,"override":{"base_rate":5,"currency":"USD","distance_unit":"Miles","hello_message":"Hello Seattle","hello_message_color":"blue","logo":"https://t4.ftcdn.net/jpg/04/24/15/07/360_F_424150716_kDZIgUONDIKpIhHqsxlDcVIiglyjIOQs.jpg","per_distance_unit_rate":2.5}}	UPDATE "contexts" SET "value" = $1, "override_id" = $2, "created_at" = $3, "created_by" = $4, "priority" = $5, "override" = $6 WHERE ("contexts"."id" = $7)
\.


--
-- Data for Name: event_log_y2024m06; Type: TABLE DATA; Schema: dev_cac; Owner: postgres
--

COPY dev_cac.event_log_y2024m06 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2024m07; Type: TABLE DATA; Schema: dev_cac; Owner: postgres
--

COPY dev_cac.event_log_y2024m07 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2024m08; Type: TABLE DATA; Schema: dev_cac; Owner: postgres
--

COPY dev_cac.event_log_y2024m08 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2024m09; Type: TABLE DATA; Schema: dev_cac; Owner: postgres
--

COPY dev_cac.event_log_y2024m09 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2024m10; Type: TABLE DATA; Schema: dev_cac; Owner: postgres
--

COPY dev_cac.event_log_y2024m10 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2024m11; Type: TABLE DATA; Schema: dev_cac; Owner: postgres
--

COPY dev_cac.event_log_y2024m11 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2024m12; Type: TABLE DATA; Schema: dev_cac; Owner: postgres
--

COPY dev_cac.event_log_y2024m12 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2025m01; Type: TABLE DATA; Schema: dev_cac; Owner: postgres
--

COPY dev_cac.event_log_y2025m01 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2025m02; Type: TABLE DATA; Schema: dev_cac; Owner: postgres
--

COPY dev_cac.event_log_y2025m02 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2025m03; Type: TABLE DATA; Schema: dev_cac; Owner: postgres
--

COPY dev_cac.event_log_y2025m03 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2025m04; Type: TABLE DATA; Schema: dev_cac; Owner: postgres
--

COPY dev_cac.event_log_y2025m04 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2025m05; Type: TABLE DATA; Schema: dev_cac; Owner: postgres
--

COPY dev_cac.event_log_y2025m05 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2025m06; Type: TABLE DATA; Schema: dev_cac; Owner: postgres
--

COPY dev_cac.event_log_y2025m06 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2025m07; Type: TABLE DATA; Schema: dev_cac; Owner: postgres
--

COPY dev_cac.event_log_y2025m07 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2025m08; Type: TABLE DATA; Schema: dev_cac; Owner: postgres
--

COPY dev_cac.event_log_y2025m08 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2025m09; Type: TABLE DATA; Schema: dev_cac; Owner: postgres
--

COPY dev_cac.event_log_y2025m09 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2025m10; Type: TABLE DATA; Schema: dev_cac; Owner: postgres
--

COPY dev_cac.event_log_y2025m10 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2025m11; Type: TABLE DATA; Schema: dev_cac; Owner: postgres
--

COPY dev_cac.event_log_y2025m11 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2025m12; Type: TABLE DATA; Schema: dev_cac; Owner: postgres
--

COPY dev_cac.event_log_y2025m12 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2026m01; Type: TABLE DATA; Schema: dev_cac; Owner: postgres
--

COPY dev_cac.event_log_y2026m01 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2026m02; Type: TABLE DATA; Schema: dev_cac; Owner: postgres
--

COPY dev_cac.event_log_y2026m02 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2026m03; Type: TABLE DATA; Schema: dev_cac; Owner: postgres
--

COPY dev_cac.event_log_y2026m03 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2026m04; Type: TABLE DATA; Schema: dev_cac; Owner: postgres
--

COPY dev_cac.event_log_y2026m04 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2026m05; Type: TABLE DATA; Schema: dev_cac; Owner: postgres
--

COPY dev_cac.event_log_y2026m05 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2026m06; Type: TABLE DATA; Schema: dev_cac; Owner: postgres
--

COPY dev_cac.event_log_y2026m06 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2026m07; Type: TABLE DATA; Schema: dev_cac; Owner: postgres
--

COPY dev_cac.event_log_y2026m07 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2026m08; Type: TABLE DATA; Schema: dev_cac; Owner: postgres
--

COPY dev_cac.event_log_y2026m08 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2026m09; Type: TABLE DATA; Schema: dev_cac; Owner: postgres
--

COPY dev_cac.event_log_y2026m09 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2026m10; Type: TABLE DATA; Schema: dev_cac; Owner: postgres
--

COPY dev_cac.event_log_y2026m10 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2026m11; Type: TABLE DATA; Schema: dev_cac; Owner: postgres
--

COPY dev_cac.event_log_y2026m11 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2026m12; Type: TABLE DATA; Schema: dev_cac; Owner: postgres
--

COPY dev_cac.event_log_y2026m12 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: functions; Type: TABLE DATA; Schema: dev_cac; Owner: postgres
--

COPY dev_cac.functions (function_name, published_code, draft_code, function_description, published_runtime_version, draft_runtime_version, published_at, draft_edited_at, published_by, draft_edited_by) FROM stdin;
\.


--
-- Data for Name: event_log_y2023m08; Type: TABLE DATA; Schema: dev_experimentation; Owner: postgres
--

COPY dev_experimentation.event_log_y2023m08 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2023m09; Type: TABLE DATA; Schema: dev_experimentation; Owner: postgres
--

COPY dev_experimentation.event_log_y2023m09 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2023m10; Type: TABLE DATA; Schema: dev_experimentation; Owner: postgres
--

COPY dev_experimentation.event_log_y2023m10 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2023m11; Type: TABLE DATA; Schema: dev_experimentation; Owner: postgres
--

COPY dev_experimentation.event_log_y2023m11 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2023m12; Type: TABLE DATA; Schema: dev_experimentation; Owner: postgres
--

COPY dev_experimentation.event_log_y2023m12 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2024m01; Type: TABLE DATA; Schema: dev_experimentation; Owner: postgres
--

COPY dev_experimentation.event_log_y2024m01 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2024m02; Type: TABLE DATA; Schema: dev_experimentation; Owner: postgres
--

COPY dev_experimentation.event_log_y2024m02 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2024m03; Type: TABLE DATA; Schema: dev_experimentation; Owner: postgres
--

COPY dev_experimentation.event_log_y2024m03 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2024m04; Type: TABLE DATA; Schema: dev_experimentation; Owner: postgres
--

COPY dev_experimentation.event_log_y2024m04 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2024m05; Type: TABLE DATA; Schema: dev_experimentation; Owner: postgres
--

COPY dev_experimentation.event_log_y2024m05 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
c37717e1-2e92-4116-9763-e2f7f2acbf55	experiments	postgres	2024-05-01 12:59:26.52675	INSERT	\N	{"id":7191420957775499264,"created_at":"2024-05-01T12:59:26.52598+00:00","created_by":"superposition@juspay.in","last_modified":"2024-05-01T12:59:26.525982+00:00","name":"testing foo increment","override_keys":["foo"],"status":"CREATED","traffic_percentage":0,"context":{"and":[]},"variants":[{"context_id":"fb6c3f081a7d44d73f603c3c1e6ea2b4f6c823c33316aa2629a3864d68d98b66","id":"7191420957775499264-control","override_id":"4045fc51982e1424d81e02e3e78c68e7a91a3b2077a27beefb443edd05deba78","overrides":{"foo":10},"variant_type":"CONTROL"},{"context_id":"1e4087f940e168cafcff6e2a693eab9ad5ffba71b98d2463af04a9be3ab8fc9a","id":"7191420957775499264-experimental","override_id":"bbbea2aa79b2e69bbe6905fa9045f29ad4205621b5b4adf16c7e26d01a5a318e","overrides":{"foo":14},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"superposition@juspay.in","chosen_variant":null}	INSERT INTO "experiments" ("id", "created_at", "created_by", "last_modified", "name", "override_keys", "status", "traffic_percentage", "context", "variants", "last_modified_by", "chosen_variant") VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, DEFAULT) RETURNING "experiments"."id", "experiments"."created_at", "experiments"."created_by", "experiments"."last_modified", "experiments"."name", "experiments"."override_keys", "experiments"."status", "experiments"."traffic_percentage", "experiments"."context", "experiments"."variants", "experiments"."last_modified_by", "experiments"."chosen_variant"
aa21af7f-126a-4bb4-9d0b-e14e9c21b69f	experiments	postgres	2024-05-01 13:00:18.033204	UPDATE	{"id":7191420957775499264,"created_at":"2024-05-01T12:59:26.52598+00:00","created_by":"superposition@juspay.in","last_modified":"2024-05-01T12:59:26.525982+00:00","name":"testing foo increment","override_keys":["foo"],"status":"CREATED","traffic_percentage":0,"context":{"and":[]},"variants":[{"context_id":"fb6c3f081a7d44d73f603c3c1e6ea2b4f6c823c33316aa2629a3864d68d98b66","id":"7191420957775499264-control","override_id":"4045fc51982e1424d81e02e3e78c68e7a91a3b2077a27beefb443edd05deba78","overrides":{"foo":10},"variant_type":"CONTROL"},{"context_id":"1e4087f940e168cafcff6e2a693eab9ad5ffba71b98d2463af04a9be3ab8fc9a","id":"7191420957775499264-experimental","override_id":"bbbea2aa79b2e69bbe6905fa9045f29ad4205621b5b4adf16c7e26d01a5a318e","overrides":{"foo":14},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"superposition@juspay.in","chosen_variant":null}	{"id":7191420957775499264,"created_at":"2024-05-01T12:59:26.52598+00:00","created_by":"superposition@juspay.in","last_modified":"2024-05-01T13:00:18.026442+00:00","name":"testing foo increment","override_keys":["foo"],"status":"INPROGRESS","traffic_percentage":0,"context":{"and":[]},"variants":[{"context_id":"fb6c3f081a7d44d73f603c3c1e6ea2b4f6c823c33316aa2629a3864d68d98b66","id":"7191420957775499264-control","override_id":"4045fc51982e1424d81e02e3e78c68e7a91a3b2077a27beefb443edd05deba78","overrides":{"foo":10},"variant_type":"CONTROL"},{"context_id":"1e4087f940e168cafcff6e2a693eab9ad5ffba71b98d2463af04a9be3ab8fc9a","id":"7191420957775499264-experimental","override_id":"bbbea2aa79b2e69bbe6905fa9045f29ad4205621b5b4adf16c7e26d01a5a318e","overrides":{"foo":14},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"superposition@juspay.in","chosen_variant":null}	UPDATE "experiments" SET "traffic_percentage" = $1, "last_modified" = $2, "last_modified_by" = $3, "status" = $4 WHERE ("experiments"."id" = $5) RETURNING "experiments"."id", "experiments"."created_at", "experiments"."created_by", "experiments"."last_modified", "experiments"."name", "experiments"."override_keys", "experiments"."status", "experiments"."traffic_percentage", "experiments"."context", "experiments"."variants", "experiments"."last_modified_by", "experiments"."chosen_variant"
392bac24-3098-4cd5-972b-f72762677e51	experiments	postgres	2024-05-01 13:00:23.273963	UPDATE	{"id":7191420957775499264,"created_at":"2024-05-01T12:59:26.52598+00:00","created_by":"superposition@juspay.in","last_modified":"2024-05-01T13:00:18.026442+00:00","name":"testing foo increment","override_keys":["foo"],"status":"INPROGRESS","traffic_percentage":0,"context":{"and":[]},"variants":[{"context_id":"fb6c3f081a7d44d73f603c3c1e6ea2b4f6c823c33316aa2629a3864d68d98b66","id":"7191420957775499264-control","override_id":"4045fc51982e1424d81e02e3e78c68e7a91a3b2077a27beefb443edd05deba78","overrides":{"foo":10},"variant_type":"CONTROL"},{"context_id":"1e4087f940e168cafcff6e2a693eab9ad5ffba71b98d2463af04a9be3ab8fc9a","id":"7191420957775499264-experimental","override_id":"bbbea2aa79b2e69bbe6905fa9045f29ad4205621b5b4adf16c7e26d01a5a318e","overrides":{"foo":14},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"superposition@juspay.in","chosen_variant":null}	{"id":7191420957775499264,"created_at":"2024-05-01T12:59:26.52598+00:00","created_by":"superposition@juspay.in","last_modified":"2024-05-01T13:00:23.272822+00:00","name":"testing foo increment","override_keys":["foo"],"status":"INPROGRESS","traffic_percentage":12,"context":{"and":[]},"variants":[{"context_id":"fb6c3f081a7d44d73f603c3c1e6ea2b4f6c823c33316aa2629a3864d68d98b66","id":"7191420957775499264-control","override_id":"4045fc51982e1424d81e02e3e78c68e7a91a3b2077a27beefb443edd05deba78","overrides":{"foo":10},"variant_type":"CONTROL"},{"context_id":"1e4087f940e168cafcff6e2a693eab9ad5ffba71b98d2463af04a9be3ab8fc9a","id":"7191420957775499264-experimental","override_id":"bbbea2aa79b2e69bbe6905fa9045f29ad4205621b5b4adf16c7e26d01a5a318e","overrides":{"foo":14},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"superposition@juspay.in","chosen_variant":null}	UPDATE "experiments" SET "traffic_percentage" = $1, "last_modified" = $2, "last_modified_by" = $3, "status" = $4 WHERE ("experiments"."id" = $5) RETURNING "experiments"."id", "experiments"."created_at", "experiments"."created_by", "experiments"."last_modified", "experiments"."name", "experiments"."override_keys", "experiments"."status", "experiments"."traffic_percentage", "experiments"."context", "experiments"."variants", "experiments"."last_modified_by", "experiments"."chosen_variant"
797e2c2c-314c-443e-90e5-9b80f25340ee	experiments	postgres	2024-05-02 04:13:27.491769	INSERT	\N	{"id":7191650977466945536,"created_at":"2024-05-02T04:13:27.489552+00:00","created_by":"superposition@juspay.in","last_modified":"2024-05-02T04:13:27.489553+00:00","name":"bangalore experiment","override_keys":["foo"],"status":"CREATED","traffic_percentage":0,"context":{"":[{"var":"city"},"Bangalore"]},"variants":[{"context_id":"89c076836990b7b9fbe6386b513712e0532516242165d04b8a286b7fcd6e637c","id":"7191650977466945536-control","override_id":"4045fc51982e1424d81e02e3e78c68e7a91a3b2077a27beefb443edd05deba78","overrides":{"foo":10},"variant_type":"CONTROL"},{"context_id":"00ab50b29e06b77448320034a82bfbb411ad014bbc8bef5835347320e7e1427a","id":"7191650977466945536-experimental","override_id":"655941b5b4116a70e7ab0fdc4fb38d8d2479ade758f605b79347ff60e31f22cb","overrides":{"foo":17},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"superposition@juspay.in","chosen_variant":null}	INSERT INTO "experiments" ("id", "created_at", "created_by", "last_modified", "name", "override_keys", "status", "traffic_percentage", "context", "variants", "last_modified_by", "chosen_variant") VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, DEFAULT) RETURNING "experiments"."id", "experiments"."created_at", "experiments"."created_by", "experiments"."last_modified", "experiments"."name", "experiments"."override_keys", "experiments"."status", "experiments"."traffic_percentage", "experiments"."context", "experiments"."variants", "experiments"."last_modified_by", "experiments"."chosen_variant"
409f81fe-a577-48ab-a097-b51757aeb57f	experiments	postgres	2024-05-02 04:13:31.99882	UPDATE	{"id":7191650977466945536,"created_at":"2024-05-02T04:13:27.489552+00:00","created_by":"superposition@juspay.in","last_modified":"2024-05-02T04:13:27.489553+00:00","name":"bangalore experiment","override_keys":["foo"],"status":"CREATED","traffic_percentage":0,"context":{"":[{"var":"city"},"Bangalore"]},"variants":[{"context_id":"89c076836990b7b9fbe6386b513712e0532516242165d04b8a286b7fcd6e637c","id":"7191650977466945536-control","override_id":"4045fc51982e1424d81e02e3e78c68e7a91a3b2077a27beefb443edd05deba78","overrides":{"foo":10},"variant_type":"CONTROL"},{"context_id":"00ab50b29e06b77448320034a82bfbb411ad014bbc8bef5835347320e7e1427a","id":"7191650977466945536-experimental","override_id":"655941b5b4116a70e7ab0fdc4fb38d8d2479ade758f605b79347ff60e31f22cb","overrides":{"foo":17},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"superposition@juspay.in","chosen_variant":null}	{"id":7191650977466945536,"created_at":"2024-05-02T04:13:27.489552+00:00","created_by":"superposition@juspay.in","last_modified":"2024-05-02T04:13:31.995893+00:00","name":"bangalore experiment","override_keys":["foo"],"status":"INPROGRESS","traffic_percentage":0,"context":{"":[{"var":"city"},"Bangalore"]},"variants":[{"context_id":"89c076836990b7b9fbe6386b513712e0532516242165d04b8a286b7fcd6e637c","id":"7191650977466945536-control","override_id":"4045fc51982e1424d81e02e3e78c68e7a91a3b2077a27beefb443edd05deba78","overrides":{"foo":10},"variant_type":"CONTROL"},{"context_id":"00ab50b29e06b77448320034a82bfbb411ad014bbc8bef5835347320e7e1427a","id":"7191650977466945536-experimental","override_id":"655941b5b4116a70e7ab0fdc4fb38d8d2479ade758f605b79347ff60e31f22cb","overrides":{"foo":17},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"superposition@juspay.in","chosen_variant":null}	UPDATE "experiments" SET "traffic_percentage" = $1, "last_modified" = $2, "last_modified_by" = $3, "status" = $4 WHERE ("experiments"."id" = $5) RETURNING "experiments"."id", "experiments"."created_at", "experiments"."created_by", "experiments"."last_modified", "experiments"."name", "experiments"."override_keys", "experiments"."status", "experiments"."traffic_percentage", "experiments"."context", "experiments"."variants", "experiments"."last_modified_by", "experiments"."chosen_variant"
2cc57cf5-6f46-4356-9f65-938f1a78b427	experiments	postgres	2024-05-02 04:13:45.956356	UPDATE	{"id":7191650977466945536,"created_at":"2024-05-02T04:13:27.489552+00:00","created_by":"superposition@juspay.in","last_modified":"2024-05-02T04:13:31.995893+00:00","name":"bangalore experiment","override_keys":["foo"],"status":"INPROGRESS","traffic_percentage":0,"context":{"":[{"var":"city"},"Bangalore"]},"variants":[{"context_id":"89c076836990b7b9fbe6386b513712e0532516242165d04b8a286b7fcd6e637c","id":"7191650977466945536-control","override_id":"4045fc51982e1424d81e02e3e78c68e7a91a3b2077a27beefb443edd05deba78","overrides":{"foo":10},"variant_type":"CONTROL"},{"context_id":"00ab50b29e06b77448320034a82bfbb411ad014bbc8bef5835347320e7e1427a","id":"7191650977466945536-experimental","override_id":"655941b5b4116a70e7ab0fdc4fb38d8d2479ade758f605b79347ff60e31f22cb","overrides":{"foo":17},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"superposition@juspay.in","chosen_variant":null}	{"id":7191650977466945536,"created_at":"2024-05-02T04:13:27.489552+00:00","created_by":"superposition@juspay.in","last_modified":"2024-05-02T04:13:45.952817+00:00","name":"bangalore experiment","override_keys":["foo"],"status":"INPROGRESS","traffic_percentage":34,"context":{"":[{"var":"city"},"Bangalore"]},"variants":[{"context_id":"89c076836990b7b9fbe6386b513712e0532516242165d04b8a286b7fcd6e637c","id":"7191650977466945536-control","override_id":"4045fc51982e1424d81e02e3e78c68e7a91a3b2077a27beefb443edd05deba78","overrides":{"foo":10},"variant_type":"CONTROL"},{"context_id":"00ab50b29e06b77448320034a82bfbb411ad014bbc8bef5835347320e7e1427a","id":"7191650977466945536-experimental","override_id":"655941b5b4116a70e7ab0fdc4fb38d8d2479ade758f605b79347ff60e31f22cb","overrides":{"foo":17},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"superposition@juspay.in","chosen_variant":null}	UPDATE "experiments" SET "traffic_percentage" = $1, "last_modified" = $2, "last_modified_by" = $3, "status" = $4 WHERE ("experiments"."id" = $5) RETURNING "experiments"."id", "experiments"."created_at", "experiments"."created_by", "experiments"."last_modified", "experiments"."name", "experiments"."override_keys", "experiments"."status", "experiments"."traffic_percentage", "experiments"."context", "experiments"."variants", "experiments"."last_modified_by", "experiments"."chosen_variant"
6232259e-6b9c-45be-a06f-2945525e60cd	experiments	postgres	2024-05-02 04:13:53.829414	UPDATE	{"id":7191650977466945536,"created_at":"2024-05-02T04:13:27.489552+00:00","created_by":"superposition@juspay.in","last_modified":"2024-05-02T04:13:45.952817+00:00","name":"bangalore experiment","override_keys":["foo"],"status":"INPROGRESS","traffic_percentage":34,"context":{"":[{"var":"city"},"Bangalore"]},"variants":[{"context_id":"89c076836990b7b9fbe6386b513712e0532516242165d04b8a286b7fcd6e637c","id":"7191650977466945536-control","override_id":"4045fc51982e1424d81e02e3e78c68e7a91a3b2077a27beefb443edd05deba78","overrides":{"foo":10},"variant_type":"CONTROL"},{"context_id":"00ab50b29e06b77448320034a82bfbb411ad014bbc8bef5835347320e7e1427a","id":"7191650977466945536-experimental","override_id":"655941b5b4116a70e7ab0fdc4fb38d8d2479ade758f605b79347ff60e31f22cb","overrides":{"foo":17},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"superposition@juspay.in","chosen_variant":null}	{"id":7191650977466945536,"created_at":"2024-05-02T04:13:27.489552+00:00","created_by":"superposition@juspay.in","last_modified":"2024-05-02T04:13:53.828349+00:00","name":"bangalore experiment","override_keys":["foo"],"status":"CONCLUDED","traffic_percentage":34,"context":{"":[{"var":"city"},"Bangalore"]},"variants":[{"context_id":"89c076836990b7b9fbe6386b513712e0532516242165d04b8a286b7fcd6e637c","id":"7191650977466945536-control","override_id":"4045fc51982e1424d81e02e3e78c68e7a91a3b2077a27beefb443edd05deba78","overrides":{"foo":10},"variant_type":"CONTROL"},{"context_id":"00ab50b29e06b77448320034a82bfbb411ad014bbc8bef5835347320e7e1427a","id":"7191650977466945536-experimental","override_id":"655941b5b4116a70e7ab0fdc4fb38d8d2479ade758f605b79347ff60e31f22cb","overrides":{"foo":17},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"superposition@juspay.in","chosen_variant":"7191650977466945536-experimental"}	UPDATE "experiments" SET "status" = $1, "last_modified" = $2, "last_modified_by" = $3, "chosen_variant" = $4 WHERE ("experiments"."id" = $5) RETURNING "experiments"."id", "experiments"."created_at", "experiments"."created_by", "experiments"."last_modified", "experiments"."name", "experiments"."override_keys", "experiments"."status", "experiments"."traffic_percentage", "experiments"."context", "experiments"."variants", "experiments"."last_modified_by", "experiments"."chosen_variant"
191b8d64-a6cd-475d-84f0-6cc140741785	experiments	postgres	2024-05-02 04:15:15.404911	INSERT	\N	{"id":7191651430124621824,"created_at":"2024-05-02T04:15:15.403953+00:00","created_by":"superposition@juspay.in","last_modified":"2024-05-02T04:15:15.403956+00:00","name":"bangalore experiment 2","override_keys":["foo"],"status":"CREATED","traffic_percentage":0,"context":{"":[{"var":"city"},"Bangalore"]},"variants":[{"context_id":"3162da98fc4912c835341a77c1fc72cfb491a61832662e0493510e28ac3bb3fb","id":"7191651430124621824-control","override_id":"4045fc51982e1424d81e02e3e78c68e7a91a3b2077a27beefb443edd05deba78","overrides":{"foo":10},"variant_type":"CONTROL"},{"context_id":"525064795060ba63a14b63dcb0b7ecfa55001c490b9d8cff53eb8fc636c030be","id":"7191651430124621824-experimental","override_id":"b5be3e559306b2f3ecada2fd438bdd42e8ed9104576f25ebe66a88d007d38772","overrides":{"foo":18},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"superposition@juspay.in","chosen_variant":null}	INSERT INTO "experiments" ("id", "created_at", "created_by", "last_modified", "name", "override_keys", "status", "traffic_percentage", "context", "variants", "last_modified_by", "chosen_variant") VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, DEFAULT) RETURNING "experiments"."id", "experiments"."created_at", "experiments"."created_by", "experiments"."last_modified", "experiments"."name", "experiments"."override_keys", "experiments"."status", "experiments"."traffic_percentage", "experiments"."context", "experiments"."variants", "experiments"."last_modified_by", "experiments"."chosen_variant"
9ee8cb5e-a8c5-42b2-a178-07d112f4ba6b	experiments	postgres	2024-05-02 04:15:36.623989	UPDATE	{"id":7191651430124621824,"created_at":"2024-05-02T04:15:15.403953+00:00","created_by":"superposition@juspay.in","last_modified":"2024-05-02T04:15:15.403956+00:00","name":"bangalore experiment 2","override_keys":["foo"],"status":"CREATED","traffic_percentage":0,"context":{"":[{"var":"city"},"Bangalore"]},"variants":[{"context_id":"3162da98fc4912c835341a77c1fc72cfb491a61832662e0493510e28ac3bb3fb","id":"7191651430124621824-control","override_id":"4045fc51982e1424d81e02e3e78c68e7a91a3b2077a27beefb443edd05deba78","overrides":{"foo":10},"variant_type":"CONTROL"},{"context_id":"525064795060ba63a14b63dcb0b7ecfa55001c490b9d8cff53eb8fc636c030be","id":"7191651430124621824-experimental","override_id":"b5be3e559306b2f3ecada2fd438bdd42e8ed9104576f25ebe66a88d007d38772","overrides":{"foo":18},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"superposition@juspay.in","chosen_variant":null}	{"id":7191651430124621824,"created_at":"2024-05-02T04:15:15.403953+00:00","created_by":"superposition@juspay.in","last_modified":"2024-05-02T04:15:36.621412+00:00","name":"bangalore experiment 2","override_keys":["foo"],"status":"INPROGRESS","traffic_percentage":0,"context":{"":[{"var":"city"},"Bangalore"]},"variants":[{"context_id":"3162da98fc4912c835341a77c1fc72cfb491a61832662e0493510e28ac3bb3fb","id":"7191651430124621824-control","override_id":"4045fc51982e1424d81e02e3e78c68e7a91a3b2077a27beefb443edd05deba78","overrides":{"foo":10},"variant_type":"CONTROL"},{"context_id":"525064795060ba63a14b63dcb0b7ecfa55001c490b9d8cff53eb8fc636c030be","id":"7191651430124621824-experimental","override_id":"b5be3e559306b2f3ecada2fd438bdd42e8ed9104576f25ebe66a88d007d38772","overrides":{"foo":18},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"superposition@juspay.in","chosen_variant":null}	UPDATE "experiments" SET "traffic_percentage" = $1, "last_modified" = $2, "last_modified_by" = $3, "status" = $4 WHERE ("experiments"."id" = $5) RETURNING "experiments"."id", "experiments"."created_at", "experiments"."created_by", "experiments"."last_modified", "experiments"."name", "experiments"."override_keys", "experiments"."status", "experiments"."traffic_percentage", "experiments"."context", "experiments"."variants", "experiments"."last_modified_by", "experiments"."chosen_variant"
e007bcc0-bb11-4dc0-a35c-00c620dee970	experiments	postgres	2024-05-02 04:15:39.353909	UPDATE	{"id":7191651430124621824,"created_at":"2024-05-02T04:15:15.403953+00:00","created_by":"superposition@juspay.in","last_modified":"2024-05-02T04:15:36.621412+00:00","name":"bangalore experiment 2","override_keys":["foo"],"status":"INPROGRESS","traffic_percentage":0,"context":{"":[{"var":"city"},"Bangalore"]},"variants":[{"context_id":"3162da98fc4912c835341a77c1fc72cfb491a61832662e0493510e28ac3bb3fb","id":"7191651430124621824-control","override_id":"4045fc51982e1424d81e02e3e78c68e7a91a3b2077a27beefb443edd05deba78","overrides":{"foo":10},"variant_type":"CONTROL"},{"context_id":"525064795060ba63a14b63dcb0b7ecfa55001c490b9d8cff53eb8fc636c030be","id":"7191651430124621824-experimental","override_id":"b5be3e559306b2f3ecada2fd438bdd42e8ed9104576f25ebe66a88d007d38772","overrides":{"foo":18},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"superposition@juspay.in","chosen_variant":null}	{"id":7191651430124621824,"created_at":"2024-05-02T04:15:15.403953+00:00","created_by":"superposition@juspay.in","last_modified":"2024-05-02T04:15:39.351579+00:00","name":"bangalore experiment 2","override_keys":["foo"],"status":"CONCLUDED","traffic_percentage":0,"context":{"":[{"var":"city"},"Bangalore"]},"variants":[{"context_id":"3162da98fc4912c835341a77c1fc72cfb491a61832662e0493510e28ac3bb3fb","id":"7191651430124621824-control","override_id":"4045fc51982e1424d81e02e3e78c68e7a91a3b2077a27beefb443edd05deba78","overrides":{"foo":10},"variant_type":"CONTROL"},{"context_id":"525064795060ba63a14b63dcb0b7ecfa55001c490b9d8cff53eb8fc636c030be","id":"7191651430124621824-experimental","override_id":"b5be3e559306b2f3ecada2fd438bdd42e8ed9104576f25ebe66a88d007d38772","overrides":{"foo":18},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"superposition@juspay.in","chosen_variant":"7191651430124621824-experimental"}	UPDATE "experiments" SET "status" = $1, "last_modified" = $2, "last_modified_by" = $3, "chosen_variant" = $4 WHERE ("experiments"."id" = $5) RETURNING "experiments"."id", "experiments"."created_at", "experiments"."created_by", "experiments"."last_modified", "experiments"."name", "experiments"."override_keys", "experiments"."status", "experiments"."traffic_percentage", "experiments"."context", "experiments"."variants", "experiments"."last_modified_by", "experiments"."chosen_variant"
9601ff8b-9b66-4c1e-8072-433c4efeb0e6	experiments	postgres	2024-05-02 07:23:51.433091	INSERT	\N	{"id":7191698892931600384,"created_at":"2024-05-02T07:23:51.432422+00:00","created_by":"superposition@juspay.in","last_modified":"2024-05-02T07:23:51.432424+00:00","name":"default operator check","override_keys":["foo"],"status":"CREATED","traffic_percentage":0,"context":{"":[{"var":"city"},"FooBar"]},"variants":[{"context_id":"5bf54e4783b83f91b2ae1e1d905a2897b007a657bc235475c3881b0f06b6f7a2","id":"7191698892931600384-control","override_id":"4045fc51982e1424d81e02e3e78c68e7a91a3b2077a27beefb443edd05deba78","overrides":{"foo":10},"variant_type":"CONTROL"},{"context_id":"81b9cf1f4ae22d89f5d3118d1019698abe46cfe481ca3593d61f2acc04bc498c","id":"7191698892931600384-experimental","override_id":"16d291cd4d3204baecba99cd4e9b4a7978719df8265774f13679db03b17c0698","overrides":{"foo":43},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"superposition@juspay.in","chosen_variant":null}	INSERT INTO "experiments" ("id", "created_at", "created_by", "last_modified", "name", "override_keys", "status", "traffic_percentage", "context", "variants", "last_modified_by", "chosen_variant") VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, DEFAULT) RETURNING "experiments"."id", "experiments"."created_at", "experiments"."created_by", "experiments"."last_modified", "experiments"."name", "experiments"."override_keys", "experiments"."status", "experiments"."traffic_percentage", "experiments"."context", "experiments"."variants", "experiments"."last_modified_by", "experiments"."chosen_variant"
\.


--
-- Data for Name: event_log_y2024m06; Type: TABLE DATA; Schema: dev_experimentation; Owner: postgres
--

COPY dev_experimentation.event_log_y2024m06 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2024m07; Type: TABLE DATA; Schema: dev_experimentation; Owner: postgres
--

COPY dev_experimentation.event_log_y2024m07 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2024m08; Type: TABLE DATA; Schema: dev_experimentation; Owner: postgres
--

COPY dev_experimentation.event_log_y2024m08 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2024m09; Type: TABLE DATA; Schema: dev_experimentation; Owner: postgres
--

COPY dev_experimentation.event_log_y2024m09 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2024m10; Type: TABLE DATA; Schema: dev_experimentation; Owner: postgres
--

COPY dev_experimentation.event_log_y2024m10 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2024m11; Type: TABLE DATA; Schema: dev_experimentation; Owner: postgres
--

COPY dev_experimentation.event_log_y2024m11 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2024m12; Type: TABLE DATA; Schema: dev_experimentation; Owner: postgres
--

COPY dev_experimentation.event_log_y2024m12 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2025m01; Type: TABLE DATA; Schema: dev_experimentation; Owner: postgres
--

COPY dev_experimentation.event_log_y2025m01 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2025m02; Type: TABLE DATA; Schema: dev_experimentation; Owner: postgres
--

COPY dev_experimentation.event_log_y2025m02 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2025m03; Type: TABLE DATA; Schema: dev_experimentation; Owner: postgres
--

COPY dev_experimentation.event_log_y2025m03 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2025m04; Type: TABLE DATA; Schema: dev_experimentation; Owner: postgres
--

COPY dev_experimentation.event_log_y2025m04 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2025m05; Type: TABLE DATA; Schema: dev_experimentation; Owner: postgres
--

COPY dev_experimentation.event_log_y2025m05 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2025m06; Type: TABLE DATA; Schema: dev_experimentation; Owner: postgres
--

COPY dev_experimentation.event_log_y2025m06 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2025m07; Type: TABLE DATA; Schema: dev_experimentation; Owner: postgres
--

COPY dev_experimentation.event_log_y2025m07 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2025m08; Type: TABLE DATA; Schema: dev_experimentation; Owner: postgres
--

COPY dev_experimentation.event_log_y2025m08 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2025m09; Type: TABLE DATA; Schema: dev_experimentation; Owner: postgres
--

COPY dev_experimentation.event_log_y2025m09 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2025m10; Type: TABLE DATA; Schema: dev_experimentation; Owner: postgres
--

COPY dev_experimentation.event_log_y2025m10 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2025m11; Type: TABLE DATA; Schema: dev_experimentation; Owner: postgres
--

COPY dev_experimentation.event_log_y2025m11 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2025m12; Type: TABLE DATA; Schema: dev_experimentation; Owner: postgres
--

COPY dev_experimentation.event_log_y2025m12 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2026m01; Type: TABLE DATA; Schema: dev_experimentation; Owner: postgres
--

COPY dev_experimentation.event_log_y2026m01 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2026m02; Type: TABLE DATA; Schema: dev_experimentation; Owner: postgres
--

COPY dev_experimentation.event_log_y2026m02 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2026m03; Type: TABLE DATA; Schema: dev_experimentation; Owner: postgres
--

COPY dev_experimentation.event_log_y2026m03 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2026m04; Type: TABLE DATA; Schema: dev_experimentation; Owner: postgres
--

COPY dev_experimentation.event_log_y2026m04 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2026m05; Type: TABLE DATA; Schema: dev_experimentation; Owner: postgres
--

COPY dev_experimentation.event_log_y2026m05 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2026m06; Type: TABLE DATA; Schema: dev_experimentation; Owner: postgres
--

COPY dev_experimentation.event_log_y2026m06 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2026m07; Type: TABLE DATA; Schema: dev_experimentation; Owner: postgres
--

COPY dev_experimentation.event_log_y2026m07 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2026m08; Type: TABLE DATA; Schema: dev_experimentation; Owner: postgres
--

COPY dev_experimentation.event_log_y2026m08 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2026m09; Type: TABLE DATA; Schema: dev_experimentation; Owner: postgres
--

COPY dev_experimentation.event_log_y2026m09 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2026m10; Type: TABLE DATA; Schema: dev_experimentation; Owner: postgres
--

COPY dev_experimentation.event_log_y2026m10 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2026m11; Type: TABLE DATA; Schema: dev_experimentation; Owner: postgres
--

COPY dev_experimentation.event_log_y2026m11 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2026m12; Type: TABLE DATA; Schema: dev_experimentation; Owner: postgres
--

COPY dev_experimentation.event_log_y2026m12 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: experiments; Type: TABLE DATA; Schema: dev_experimentation; Owner: postgres
--

COPY dev_experimentation.experiments (id, created_at, created_by, last_modified, name, override_keys, status, traffic_percentage, context, variants, last_modified_by, chosen_variant) FROM stdin;
7191420957775499264	2024-05-01 12:59:26.52598+00	superposition@juspay.in	2024-05-01 13:00:23.272822+00	testing foo increment	{foo}	INPROGRESS	12	{"and":[]}	[{"context_id":"fb6c3f081a7d44d73f603c3c1e6ea2b4f6c823c33316aa2629a3864d68d98b66","id":"7191420957775499264-control","override_id":"4045fc51982e1424d81e02e3e78c68e7a91a3b2077a27beefb443edd05deba78","overrides":{"foo":10},"variant_type":"CONTROL"},{"context_id":"1e4087f940e168cafcff6e2a693eab9ad5ffba71b98d2463af04a9be3ab8fc9a","id":"7191420957775499264-experimental","override_id":"bbbea2aa79b2e69bbe6905fa9045f29ad4205621b5b4adf16c7e26d01a5a318e","overrides":{"foo":14},"variant_type":"EXPERIMENTAL"}]	superposition@juspay.in	\N
7191650977466945536	2024-05-02 04:13:27.489552+00	superposition@juspay.in	2024-05-02 04:13:53.828349+00	bangalore experiment	{foo}	CONCLUDED	34	{"":[{"var":"city"},"Bangalore"]}	[{"context_id":"89c076836990b7b9fbe6386b513712e0532516242165d04b8a286b7fcd6e637c","id":"7191650977466945536-control","override_id":"4045fc51982e1424d81e02e3e78c68e7a91a3b2077a27beefb443edd05deba78","overrides":{"foo":10},"variant_type":"CONTROL"},{"context_id":"00ab50b29e06b77448320034a82bfbb411ad014bbc8bef5835347320e7e1427a","id":"7191650977466945536-experimental","override_id":"655941b5b4116a70e7ab0fdc4fb38d8d2479ade758f605b79347ff60e31f22cb","overrides":{"foo":17},"variant_type":"EXPERIMENTAL"}]	superposition@juspay.in	7191650977466945536-experimental
7191651430124621824	2024-05-02 04:15:15.403953+00	superposition@juspay.in	2024-05-02 04:15:39.351579+00	bangalore experiment 2	{foo}	CONCLUDED	0	{"":[{"var":"city"},"Bangalore"]}	[{"context_id":"3162da98fc4912c835341a77c1fc72cfb491a61832662e0493510e28ac3bb3fb","id":"7191651430124621824-control","override_id":"4045fc51982e1424d81e02e3e78c68e7a91a3b2077a27beefb443edd05deba78","overrides":{"foo":10},"variant_type":"CONTROL"},{"context_id":"525064795060ba63a14b63dcb0b7ecfa55001c490b9d8cff53eb8fc636c030be","id":"7191651430124621824-experimental","override_id":"b5be3e559306b2f3ecada2fd438bdd42e8ed9104576f25ebe66a88d007d38772","overrides":{"foo":18},"variant_type":"EXPERIMENTAL"}]	superposition@juspay.in	7191651430124621824-experimental
7191698892931600384	2024-05-02 07:23:51.432422+00	superposition@juspay.in	2024-05-02 07:23:51.432424+00	default operator check	{foo}	CREATED	0	{"":[{"var":"city"},"FooBar"]}	[{"context_id":"5bf54e4783b83f91b2ae1e1d905a2897b007a657bc235475c3881b0f06b6f7a2","id":"7191698892931600384-control","override_id":"4045fc51982e1424d81e02e3e78c68e7a91a3b2077a27beefb443edd05deba78","overrides":{"foo":10},"variant_type":"CONTROL"},{"context_id":"81b9cf1f4ae22d89f5d3118d1019698abe46cfe481ca3593d61f2acc04bc498c","id":"7191698892931600384-experimental","override_id":"16d291cd4d3204baecba99cd4e9b4a7978719df8265774f13679db03b17c0698","overrides":{"foo":43},"variant_type":"EXPERIMENTAL"}]	superposition@juspay.in	\N
\.


--
-- Data for Name: contexts; Type: TABLE DATA; Schema: test_cac; Owner: postgres
--

COPY test_cac.contexts (id, value, override_id, created_at, created_by, priority, override) FROM stdin;
\.


--
-- Data for Name: default_configs; Type: TABLE DATA; Schema: test_cac; Owner: postgres
--

COPY test_cac.default_configs (key, value, created_at, created_by, schema, function_name) FROM stdin;
\.


--
-- Data for Name: dimensions; Type: TABLE DATA; Schema: test_cac; Owner: postgres
--

COPY test_cac.dimensions (dimension, priority, created_at, created_by, schema, function_name) FROM stdin;
variantIds	1	2024-05-01 11:25:36.713795+00	anon@juspay.in	{"type": "string","pattern": ".*"}	\N
\.


--
-- Data for Name: event_log_y2023m08; Type: TABLE DATA; Schema: test_cac; Owner: postgres
--

COPY test_cac.event_log_y2023m08 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2023m09; Type: TABLE DATA; Schema: test_cac; Owner: postgres
--

COPY test_cac.event_log_y2023m09 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2023m10; Type: TABLE DATA; Schema: test_cac; Owner: postgres
--

COPY test_cac.event_log_y2023m10 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2023m11; Type: TABLE DATA; Schema: test_cac; Owner: postgres
--

COPY test_cac.event_log_y2023m11 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2023m12; Type: TABLE DATA; Schema: test_cac; Owner: postgres
--

COPY test_cac.event_log_y2023m12 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2024m01; Type: TABLE DATA; Schema: test_cac; Owner: postgres
--

COPY test_cac.event_log_y2024m01 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2024m02; Type: TABLE DATA; Schema: test_cac; Owner: postgres
--

COPY test_cac.event_log_y2024m02 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2024m03; Type: TABLE DATA; Schema: test_cac; Owner: postgres
--

COPY test_cac.event_log_y2024m03 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2024m04; Type: TABLE DATA; Schema: test_cac; Owner: postgres
--

COPY test_cac.event_log_y2024m04 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2024m05; Type: TABLE DATA; Schema: test_cac; Owner: postgres
--

COPY test_cac.event_log_y2024m05 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
2ab8a93a-8329-4af2-823b-908d1e3fbd50	dimensions	postgres	2024-05-01 11:25:36.713795	INSERT	\N	{"dimension":"variantIds","priority":1,"created_at":"2024-05-01T11:25:36.713795+00:00","created_by":"anon@juspay.in","schema":{"type": "string","pattern": ".*"},"function_name":null}	INSERT INTO test_cac.dimensions (dimension, priority, created_at, created_by, schema, function_name) VALUES ('variantIds', 1, CURRENT_TIMESTAMP, 'anon@juspay.in', '{"type": "string","pattern": ".*"}'::json, null);
\.


--
-- Data for Name: event_log_y2024m06; Type: TABLE DATA; Schema: test_cac; Owner: postgres
--

COPY test_cac.event_log_y2024m06 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2024m07; Type: TABLE DATA; Schema: test_cac; Owner: postgres
--

COPY test_cac.event_log_y2024m07 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2024m08; Type: TABLE DATA; Schema: test_cac; Owner: postgres
--

COPY test_cac.event_log_y2024m08 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2024m09; Type: TABLE DATA; Schema: test_cac; Owner: postgres
--

COPY test_cac.event_log_y2024m09 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2024m10; Type: TABLE DATA; Schema: test_cac; Owner: postgres
--

COPY test_cac.event_log_y2024m10 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2024m11; Type: TABLE DATA; Schema: test_cac; Owner: postgres
--

COPY test_cac.event_log_y2024m11 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2024m12; Type: TABLE DATA; Schema: test_cac; Owner: postgres
--

COPY test_cac.event_log_y2024m12 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2025m01; Type: TABLE DATA; Schema: test_cac; Owner: postgres
--

COPY test_cac.event_log_y2025m01 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2025m02; Type: TABLE DATA; Schema: test_cac; Owner: postgres
--

COPY test_cac.event_log_y2025m02 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2025m03; Type: TABLE DATA; Schema: test_cac; Owner: postgres
--

COPY test_cac.event_log_y2025m03 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2025m04; Type: TABLE DATA; Schema: test_cac; Owner: postgres
--

COPY test_cac.event_log_y2025m04 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2025m05; Type: TABLE DATA; Schema: test_cac; Owner: postgres
--

COPY test_cac.event_log_y2025m05 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2025m06; Type: TABLE DATA; Schema: test_cac; Owner: postgres
--

COPY test_cac.event_log_y2025m06 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2025m07; Type: TABLE DATA; Schema: test_cac; Owner: postgres
--

COPY test_cac.event_log_y2025m07 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2025m08; Type: TABLE DATA; Schema: test_cac; Owner: postgres
--

COPY test_cac.event_log_y2025m08 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2025m09; Type: TABLE DATA; Schema: test_cac; Owner: postgres
--

COPY test_cac.event_log_y2025m09 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2025m10; Type: TABLE DATA; Schema: test_cac; Owner: postgres
--

COPY test_cac.event_log_y2025m10 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2025m11; Type: TABLE DATA; Schema: test_cac; Owner: postgres
--

COPY test_cac.event_log_y2025m11 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2025m12; Type: TABLE DATA; Schema: test_cac; Owner: postgres
--

COPY test_cac.event_log_y2025m12 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2026m01; Type: TABLE DATA; Schema: test_cac; Owner: postgres
--

COPY test_cac.event_log_y2026m01 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2026m02; Type: TABLE DATA; Schema: test_cac; Owner: postgres
--

COPY test_cac.event_log_y2026m02 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2026m03; Type: TABLE DATA; Schema: test_cac; Owner: postgres
--

COPY test_cac.event_log_y2026m03 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2026m04; Type: TABLE DATA; Schema: test_cac; Owner: postgres
--

COPY test_cac.event_log_y2026m04 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2026m05; Type: TABLE DATA; Schema: test_cac; Owner: postgres
--

COPY test_cac.event_log_y2026m05 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2026m06; Type: TABLE DATA; Schema: test_cac; Owner: postgres
--

COPY test_cac.event_log_y2026m06 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2026m07; Type: TABLE DATA; Schema: test_cac; Owner: postgres
--

COPY test_cac.event_log_y2026m07 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2026m08; Type: TABLE DATA; Schema: test_cac; Owner: postgres
--

COPY test_cac.event_log_y2026m08 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2026m09; Type: TABLE DATA; Schema: test_cac; Owner: postgres
--

COPY test_cac.event_log_y2026m09 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2026m10; Type: TABLE DATA; Schema: test_cac; Owner: postgres
--

COPY test_cac.event_log_y2026m10 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2026m11; Type: TABLE DATA; Schema: test_cac; Owner: postgres
--

COPY test_cac.event_log_y2026m11 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2026m12; Type: TABLE DATA; Schema: test_cac; Owner: postgres
--

COPY test_cac.event_log_y2026m12 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: functions; Type: TABLE DATA; Schema: test_cac; Owner: postgres
--

COPY test_cac.functions (function_name, published_code, draft_code, function_description, published_runtime_version, draft_runtime_version, published_at, draft_edited_at, published_by, draft_edited_by) FROM stdin;
\.


--
-- Data for Name: event_log_y2023m08; Type: TABLE DATA; Schema: test_experimentation; Owner: postgres
--

COPY test_experimentation.event_log_y2023m08 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2023m09; Type: TABLE DATA; Schema: test_experimentation; Owner: postgres
--

COPY test_experimentation.event_log_y2023m09 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2023m10; Type: TABLE DATA; Schema: test_experimentation; Owner: postgres
--

COPY test_experimentation.event_log_y2023m10 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2023m11; Type: TABLE DATA; Schema: test_experimentation; Owner: postgres
--

COPY test_experimentation.event_log_y2023m11 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2023m12; Type: TABLE DATA; Schema: test_experimentation; Owner: postgres
--

COPY test_experimentation.event_log_y2023m12 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2024m01; Type: TABLE DATA; Schema: test_experimentation; Owner: postgres
--

COPY test_experimentation.event_log_y2024m01 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2024m02; Type: TABLE DATA; Schema: test_experimentation; Owner: postgres
--

COPY test_experimentation.event_log_y2024m02 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2024m03; Type: TABLE DATA; Schema: test_experimentation; Owner: postgres
--

COPY test_experimentation.event_log_y2024m03 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2024m04; Type: TABLE DATA; Schema: test_experimentation; Owner: postgres
--

COPY test_experimentation.event_log_y2024m04 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2024m05; Type: TABLE DATA; Schema: test_experimentation; Owner: postgres
--

COPY test_experimentation.event_log_y2024m05 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2024m06; Type: TABLE DATA; Schema: test_experimentation; Owner: postgres
--

COPY test_experimentation.event_log_y2024m06 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2024m07; Type: TABLE DATA; Schema: test_experimentation; Owner: postgres
--

COPY test_experimentation.event_log_y2024m07 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2024m08; Type: TABLE DATA; Schema: test_experimentation; Owner: postgres
--

COPY test_experimentation.event_log_y2024m08 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2024m09; Type: TABLE DATA; Schema: test_experimentation; Owner: postgres
--

COPY test_experimentation.event_log_y2024m09 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2024m10; Type: TABLE DATA; Schema: test_experimentation; Owner: postgres
--

COPY test_experimentation.event_log_y2024m10 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2024m11; Type: TABLE DATA; Schema: test_experimentation; Owner: postgres
--

COPY test_experimentation.event_log_y2024m11 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2024m12; Type: TABLE DATA; Schema: test_experimentation; Owner: postgres
--

COPY test_experimentation.event_log_y2024m12 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2025m01; Type: TABLE DATA; Schema: test_experimentation; Owner: postgres
--

COPY test_experimentation.event_log_y2025m01 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2025m02; Type: TABLE DATA; Schema: test_experimentation; Owner: postgres
--

COPY test_experimentation.event_log_y2025m02 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2025m03; Type: TABLE DATA; Schema: test_experimentation; Owner: postgres
--

COPY test_experimentation.event_log_y2025m03 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2025m04; Type: TABLE DATA; Schema: test_experimentation; Owner: postgres
--

COPY test_experimentation.event_log_y2025m04 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2025m05; Type: TABLE DATA; Schema: test_experimentation; Owner: postgres
--

COPY test_experimentation.event_log_y2025m05 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2025m06; Type: TABLE DATA; Schema: test_experimentation; Owner: postgres
--

COPY test_experimentation.event_log_y2025m06 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2025m07; Type: TABLE DATA; Schema: test_experimentation; Owner: postgres
--

COPY test_experimentation.event_log_y2025m07 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2025m08; Type: TABLE DATA; Schema: test_experimentation; Owner: postgres
--

COPY test_experimentation.event_log_y2025m08 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2025m09; Type: TABLE DATA; Schema: test_experimentation; Owner: postgres
--

COPY test_experimentation.event_log_y2025m09 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2025m10; Type: TABLE DATA; Schema: test_experimentation; Owner: postgres
--

COPY test_experimentation.event_log_y2025m10 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2025m11; Type: TABLE DATA; Schema: test_experimentation; Owner: postgres
--

COPY test_experimentation.event_log_y2025m11 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2025m12; Type: TABLE DATA; Schema: test_experimentation; Owner: postgres
--

COPY test_experimentation.event_log_y2025m12 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2026m01; Type: TABLE DATA; Schema: test_experimentation; Owner: postgres
--

COPY test_experimentation.event_log_y2026m01 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2026m02; Type: TABLE DATA; Schema: test_experimentation; Owner: postgres
--

COPY test_experimentation.event_log_y2026m02 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2026m03; Type: TABLE DATA; Schema: test_experimentation; Owner: postgres
--

COPY test_experimentation.event_log_y2026m03 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2026m04; Type: TABLE DATA; Schema: test_experimentation; Owner: postgres
--

COPY test_experimentation.event_log_y2026m04 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2026m05; Type: TABLE DATA; Schema: test_experimentation; Owner: postgres
--

COPY test_experimentation.event_log_y2026m05 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2026m06; Type: TABLE DATA; Schema: test_experimentation; Owner: postgres
--

COPY test_experimentation.event_log_y2026m06 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2026m07; Type: TABLE DATA; Schema: test_experimentation; Owner: postgres
--

COPY test_experimentation.event_log_y2026m07 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2026m08; Type: TABLE DATA; Schema: test_experimentation; Owner: postgres
--

COPY test_experimentation.event_log_y2026m08 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2026m09; Type: TABLE DATA; Schema: test_experimentation; Owner: postgres
--

COPY test_experimentation.event_log_y2026m09 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2026m10; Type: TABLE DATA; Schema: test_experimentation; Owner: postgres
--

COPY test_experimentation.event_log_y2026m10 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2026m11; Type: TABLE DATA; Schema: test_experimentation; Owner: postgres
--

COPY test_experimentation.event_log_y2026m11 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2026m12; Type: TABLE DATA; Schema: test_experimentation; Owner: postgres
--

COPY test_experimentation.event_log_y2026m12 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: experiments; Type: TABLE DATA; Schema: test_experimentation; Owner: postgres
--

COPY test_experimentation.experiments (id, created_at, created_by, last_modified, name, override_keys, status, traffic_percentage, context, variants, last_modified_by, chosen_variant) FROM stdin;
\.


--
-- Name: contexts contexts_pkey; Type: CONSTRAINT; Schema: dev_cac; Owner: postgres
--

ALTER TABLE ONLY dev_cac.contexts
    ADD CONSTRAINT contexts_pkey PRIMARY KEY (id);


--
-- Name: default_configs default_configs_pkey; Type: CONSTRAINT; Schema: dev_cac; Owner: postgres
--

ALTER TABLE ONLY dev_cac.default_configs
    ADD CONSTRAINT default_configs_pkey PRIMARY KEY (key);


--
-- Name: dimensions dimensions_pkey; Type: CONSTRAINT; Schema: dev_cac; Owner: postgres
--

ALTER TABLE ONLY dev_cac.dimensions
    ADD CONSTRAINT dimensions_pkey PRIMARY KEY (dimension);


--
-- Name: event_log event_log_pkey; Type: CONSTRAINT; Schema: dev_cac; Owner: postgres
--

ALTER TABLE ONLY dev_cac.event_log
    ADD CONSTRAINT event_log_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2023m08 event_log_y2023m08_pkey; Type: CONSTRAINT; Schema: dev_cac; Owner: postgres
--

ALTER TABLE ONLY dev_cac.event_log_y2023m08
    ADD CONSTRAINT event_log_y2023m08_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2023m09 event_log_y2023m09_pkey; Type: CONSTRAINT; Schema: dev_cac; Owner: postgres
--

ALTER TABLE ONLY dev_cac.event_log_y2023m09
    ADD CONSTRAINT event_log_y2023m09_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2023m10 event_log_y2023m10_pkey; Type: CONSTRAINT; Schema: dev_cac; Owner: postgres
--

ALTER TABLE ONLY dev_cac.event_log_y2023m10
    ADD CONSTRAINT event_log_y2023m10_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2023m11 event_log_y2023m11_pkey; Type: CONSTRAINT; Schema: dev_cac; Owner: postgres
--

ALTER TABLE ONLY dev_cac.event_log_y2023m11
    ADD CONSTRAINT event_log_y2023m11_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2023m12 event_log_y2023m12_pkey; Type: CONSTRAINT; Schema: dev_cac; Owner: postgres
--

ALTER TABLE ONLY dev_cac.event_log_y2023m12
    ADD CONSTRAINT event_log_y2023m12_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2024m01 event_log_y2024m01_pkey; Type: CONSTRAINT; Schema: dev_cac; Owner: postgres
--

ALTER TABLE ONLY dev_cac.event_log_y2024m01
    ADD CONSTRAINT event_log_y2024m01_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2024m02 event_log_y2024m02_pkey; Type: CONSTRAINT; Schema: dev_cac; Owner: postgres
--

ALTER TABLE ONLY dev_cac.event_log_y2024m02
    ADD CONSTRAINT event_log_y2024m02_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2024m03 event_log_y2024m03_pkey; Type: CONSTRAINT; Schema: dev_cac; Owner: postgres
--

ALTER TABLE ONLY dev_cac.event_log_y2024m03
    ADD CONSTRAINT event_log_y2024m03_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2024m04 event_log_y2024m04_pkey; Type: CONSTRAINT; Schema: dev_cac; Owner: postgres
--

ALTER TABLE ONLY dev_cac.event_log_y2024m04
    ADD CONSTRAINT event_log_y2024m04_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2024m05 event_log_y2024m05_pkey; Type: CONSTRAINT; Schema: dev_cac; Owner: postgres
--

ALTER TABLE ONLY dev_cac.event_log_y2024m05
    ADD CONSTRAINT event_log_y2024m05_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2024m06 event_log_y2024m06_pkey; Type: CONSTRAINT; Schema: dev_cac; Owner: postgres
--

ALTER TABLE ONLY dev_cac.event_log_y2024m06
    ADD CONSTRAINT event_log_y2024m06_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2024m07 event_log_y2024m07_pkey; Type: CONSTRAINT; Schema: dev_cac; Owner: postgres
--

ALTER TABLE ONLY dev_cac.event_log_y2024m07
    ADD CONSTRAINT event_log_y2024m07_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2024m08 event_log_y2024m08_pkey; Type: CONSTRAINT; Schema: dev_cac; Owner: postgres
--

ALTER TABLE ONLY dev_cac.event_log_y2024m08
    ADD CONSTRAINT event_log_y2024m08_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2024m09 event_log_y2024m09_pkey; Type: CONSTRAINT; Schema: dev_cac; Owner: postgres
--

ALTER TABLE ONLY dev_cac.event_log_y2024m09
    ADD CONSTRAINT event_log_y2024m09_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2024m10 event_log_y2024m10_pkey; Type: CONSTRAINT; Schema: dev_cac; Owner: postgres
--

ALTER TABLE ONLY dev_cac.event_log_y2024m10
    ADD CONSTRAINT event_log_y2024m10_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2024m11 event_log_y2024m11_pkey; Type: CONSTRAINT; Schema: dev_cac; Owner: postgres
--

ALTER TABLE ONLY dev_cac.event_log_y2024m11
    ADD CONSTRAINT event_log_y2024m11_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2024m12 event_log_y2024m12_pkey; Type: CONSTRAINT; Schema: dev_cac; Owner: postgres
--

ALTER TABLE ONLY dev_cac.event_log_y2024m12
    ADD CONSTRAINT event_log_y2024m12_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2025m01 event_log_y2025m01_pkey; Type: CONSTRAINT; Schema: dev_cac; Owner: postgres
--

ALTER TABLE ONLY dev_cac.event_log_y2025m01
    ADD CONSTRAINT event_log_y2025m01_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2025m02 event_log_y2025m02_pkey; Type: CONSTRAINT; Schema: dev_cac; Owner: postgres
--

ALTER TABLE ONLY dev_cac.event_log_y2025m02
    ADD CONSTRAINT event_log_y2025m02_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2025m03 event_log_y2025m03_pkey; Type: CONSTRAINT; Schema: dev_cac; Owner: postgres
--

ALTER TABLE ONLY dev_cac.event_log_y2025m03
    ADD CONSTRAINT event_log_y2025m03_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2025m04 event_log_y2025m04_pkey; Type: CONSTRAINT; Schema: dev_cac; Owner: postgres
--

ALTER TABLE ONLY dev_cac.event_log_y2025m04
    ADD CONSTRAINT event_log_y2025m04_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2025m05 event_log_y2025m05_pkey; Type: CONSTRAINT; Schema: dev_cac; Owner: postgres
--

ALTER TABLE ONLY dev_cac.event_log_y2025m05
    ADD CONSTRAINT event_log_y2025m05_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2025m06 event_log_y2025m06_pkey; Type: CONSTRAINT; Schema: dev_cac; Owner: postgres
--

ALTER TABLE ONLY dev_cac.event_log_y2025m06
    ADD CONSTRAINT event_log_y2025m06_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2025m07 event_log_y2025m07_pkey; Type: CONSTRAINT; Schema: dev_cac; Owner: postgres
--

ALTER TABLE ONLY dev_cac.event_log_y2025m07
    ADD CONSTRAINT event_log_y2025m07_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2025m08 event_log_y2025m08_pkey; Type: CONSTRAINT; Schema: dev_cac; Owner: postgres
--

ALTER TABLE ONLY dev_cac.event_log_y2025m08
    ADD CONSTRAINT event_log_y2025m08_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2025m09 event_log_y2025m09_pkey; Type: CONSTRAINT; Schema: dev_cac; Owner: postgres
--

ALTER TABLE ONLY dev_cac.event_log_y2025m09
    ADD CONSTRAINT event_log_y2025m09_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2025m10 event_log_y2025m10_pkey; Type: CONSTRAINT; Schema: dev_cac; Owner: postgres
--

ALTER TABLE ONLY dev_cac.event_log_y2025m10
    ADD CONSTRAINT event_log_y2025m10_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2025m11 event_log_y2025m11_pkey; Type: CONSTRAINT; Schema: dev_cac; Owner: postgres
--

ALTER TABLE ONLY dev_cac.event_log_y2025m11
    ADD CONSTRAINT event_log_y2025m11_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2025m12 event_log_y2025m12_pkey; Type: CONSTRAINT; Schema: dev_cac; Owner: postgres
--

ALTER TABLE ONLY dev_cac.event_log_y2025m12
    ADD CONSTRAINT event_log_y2025m12_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2026m01 event_log_y2026m01_pkey; Type: CONSTRAINT; Schema: dev_cac; Owner: postgres
--

ALTER TABLE ONLY dev_cac.event_log_y2026m01
    ADD CONSTRAINT event_log_y2026m01_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2026m02 event_log_y2026m02_pkey; Type: CONSTRAINT; Schema: dev_cac; Owner: postgres
--

ALTER TABLE ONLY dev_cac.event_log_y2026m02
    ADD CONSTRAINT event_log_y2026m02_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2026m03 event_log_y2026m03_pkey; Type: CONSTRAINT; Schema: dev_cac; Owner: postgres
--

ALTER TABLE ONLY dev_cac.event_log_y2026m03
    ADD CONSTRAINT event_log_y2026m03_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2026m04 event_log_y2026m04_pkey; Type: CONSTRAINT; Schema: dev_cac; Owner: postgres
--

ALTER TABLE ONLY dev_cac.event_log_y2026m04
    ADD CONSTRAINT event_log_y2026m04_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2026m05 event_log_y2026m05_pkey; Type: CONSTRAINT; Schema: dev_cac; Owner: postgres
--

ALTER TABLE ONLY dev_cac.event_log_y2026m05
    ADD CONSTRAINT event_log_y2026m05_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2026m06 event_log_y2026m06_pkey; Type: CONSTRAINT; Schema: dev_cac; Owner: postgres
--

ALTER TABLE ONLY dev_cac.event_log_y2026m06
    ADD CONSTRAINT event_log_y2026m06_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2026m07 event_log_y2026m07_pkey; Type: CONSTRAINT; Schema: dev_cac; Owner: postgres
--

ALTER TABLE ONLY dev_cac.event_log_y2026m07
    ADD CONSTRAINT event_log_y2026m07_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2026m08 event_log_y2026m08_pkey; Type: CONSTRAINT; Schema: dev_cac; Owner: postgres
--

ALTER TABLE ONLY dev_cac.event_log_y2026m08
    ADD CONSTRAINT event_log_y2026m08_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2026m09 event_log_y2026m09_pkey; Type: CONSTRAINT; Schema: dev_cac; Owner: postgres
--

ALTER TABLE ONLY dev_cac.event_log_y2026m09
    ADD CONSTRAINT event_log_y2026m09_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2026m10 event_log_y2026m10_pkey; Type: CONSTRAINT; Schema: dev_cac; Owner: postgres
--

ALTER TABLE ONLY dev_cac.event_log_y2026m10
    ADD CONSTRAINT event_log_y2026m10_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2026m11 event_log_y2026m11_pkey; Type: CONSTRAINT; Schema: dev_cac; Owner: postgres
--

ALTER TABLE ONLY dev_cac.event_log_y2026m11
    ADD CONSTRAINT event_log_y2026m11_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2026m12 event_log_y2026m12_pkey; Type: CONSTRAINT; Schema: dev_cac; Owner: postgres
--

ALTER TABLE ONLY dev_cac.event_log_y2026m12
    ADD CONSTRAINT event_log_y2026m12_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: functions functions_pkey; Type: CONSTRAINT; Schema: dev_cac; Owner: postgres
--

ALTER TABLE ONLY dev_cac.functions
    ADD CONSTRAINT functions_pkey PRIMARY KEY (function_name);


--
-- Name: event_log event_log_pkey; Type: CONSTRAINT; Schema: dev_experimentation; Owner: postgres
--

ALTER TABLE ONLY dev_experimentation.event_log
    ADD CONSTRAINT event_log_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2023m08 event_log_y2023m08_pkey; Type: CONSTRAINT; Schema: dev_experimentation; Owner: postgres
--

ALTER TABLE ONLY dev_experimentation.event_log_y2023m08
    ADD CONSTRAINT event_log_y2023m08_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2023m09 event_log_y2023m09_pkey; Type: CONSTRAINT; Schema: dev_experimentation; Owner: postgres
--

ALTER TABLE ONLY dev_experimentation.event_log_y2023m09
    ADD CONSTRAINT event_log_y2023m09_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2023m10 event_log_y2023m10_pkey; Type: CONSTRAINT; Schema: dev_experimentation; Owner: postgres
--

ALTER TABLE ONLY dev_experimentation.event_log_y2023m10
    ADD CONSTRAINT event_log_y2023m10_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2023m11 event_log_y2023m11_pkey; Type: CONSTRAINT; Schema: dev_experimentation; Owner: postgres
--

ALTER TABLE ONLY dev_experimentation.event_log_y2023m11
    ADD CONSTRAINT event_log_y2023m11_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2023m12 event_log_y2023m12_pkey; Type: CONSTRAINT; Schema: dev_experimentation; Owner: postgres
--

ALTER TABLE ONLY dev_experimentation.event_log_y2023m12
    ADD CONSTRAINT event_log_y2023m12_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2024m01 event_log_y2024m01_pkey; Type: CONSTRAINT; Schema: dev_experimentation; Owner: postgres
--

ALTER TABLE ONLY dev_experimentation.event_log_y2024m01
    ADD CONSTRAINT event_log_y2024m01_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2024m02 event_log_y2024m02_pkey; Type: CONSTRAINT; Schema: dev_experimentation; Owner: postgres
--

ALTER TABLE ONLY dev_experimentation.event_log_y2024m02
    ADD CONSTRAINT event_log_y2024m02_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2024m03 event_log_y2024m03_pkey; Type: CONSTRAINT; Schema: dev_experimentation; Owner: postgres
--

ALTER TABLE ONLY dev_experimentation.event_log_y2024m03
    ADD CONSTRAINT event_log_y2024m03_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2024m04 event_log_y2024m04_pkey; Type: CONSTRAINT; Schema: dev_experimentation; Owner: postgres
--

ALTER TABLE ONLY dev_experimentation.event_log_y2024m04
    ADD CONSTRAINT event_log_y2024m04_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2024m05 event_log_y2024m05_pkey; Type: CONSTRAINT; Schema: dev_experimentation; Owner: postgres
--

ALTER TABLE ONLY dev_experimentation.event_log_y2024m05
    ADD CONSTRAINT event_log_y2024m05_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2024m06 event_log_y2024m06_pkey; Type: CONSTRAINT; Schema: dev_experimentation; Owner: postgres
--

ALTER TABLE ONLY dev_experimentation.event_log_y2024m06
    ADD CONSTRAINT event_log_y2024m06_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2024m07 event_log_y2024m07_pkey; Type: CONSTRAINT; Schema: dev_experimentation; Owner: postgres
--

ALTER TABLE ONLY dev_experimentation.event_log_y2024m07
    ADD CONSTRAINT event_log_y2024m07_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2024m08 event_log_y2024m08_pkey; Type: CONSTRAINT; Schema: dev_experimentation; Owner: postgres
--

ALTER TABLE ONLY dev_experimentation.event_log_y2024m08
    ADD CONSTRAINT event_log_y2024m08_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2024m09 event_log_y2024m09_pkey; Type: CONSTRAINT; Schema: dev_experimentation; Owner: postgres
--

ALTER TABLE ONLY dev_experimentation.event_log_y2024m09
    ADD CONSTRAINT event_log_y2024m09_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2024m10 event_log_y2024m10_pkey; Type: CONSTRAINT; Schema: dev_experimentation; Owner: postgres
--

ALTER TABLE ONLY dev_experimentation.event_log_y2024m10
    ADD CONSTRAINT event_log_y2024m10_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2024m11 event_log_y2024m11_pkey; Type: CONSTRAINT; Schema: dev_experimentation; Owner: postgres
--

ALTER TABLE ONLY dev_experimentation.event_log_y2024m11
    ADD CONSTRAINT event_log_y2024m11_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2024m12 event_log_y2024m12_pkey; Type: CONSTRAINT; Schema: dev_experimentation; Owner: postgres
--

ALTER TABLE ONLY dev_experimentation.event_log_y2024m12
    ADD CONSTRAINT event_log_y2024m12_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2025m01 event_log_y2025m01_pkey; Type: CONSTRAINT; Schema: dev_experimentation; Owner: postgres
--

ALTER TABLE ONLY dev_experimentation.event_log_y2025m01
    ADD CONSTRAINT event_log_y2025m01_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2025m02 event_log_y2025m02_pkey; Type: CONSTRAINT; Schema: dev_experimentation; Owner: postgres
--

ALTER TABLE ONLY dev_experimentation.event_log_y2025m02
    ADD CONSTRAINT event_log_y2025m02_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2025m03 event_log_y2025m03_pkey; Type: CONSTRAINT; Schema: dev_experimentation; Owner: postgres
--

ALTER TABLE ONLY dev_experimentation.event_log_y2025m03
    ADD CONSTRAINT event_log_y2025m03_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2025m04 event_log_y2025m04_pkey; Type: CONSTRAINT; Schema: dev_experimentation; Owner: postgres
--

ALTER TABLE ONLY dev_experimentation.event_log_y2025m04
    ADD CONSTRAINT event_log_y2025m04_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2025m05 event_log_y2025m05_pkey; Type: CONSTRAINT; Schema: dev_experimentation; Owner: postgres
--

ALTER TABLE ONLY dev_experimentation.event_log_y2025m05
    ADD CONSTRAINT event_log_y2025m05_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2025m06 event_log_y2025m06_pkey; Type: CONSTRAINT; Schema: dev_experimentation; Owner: postgres
--

ALTER TABLE ONLY dev_experimentation.event_log_y2025m06
    ADD CONSTRAINT event_log_y2025m06_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2025m07 event_log_y2025m07_pkey; Type: CONSTRAINT; Schema: dev_experimentation; Owner: postgres
--

ALTER TABLE ONLY dev_experimentation.event_log_y2025m07
    ADD CONSTRAINT event_log_y2025m07_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2025m08 event_log_y2025m08_pkey; Type: CONSTRAINT; Schema: dev_experimentation; Owner: postgres
--

ALTER TABLE ONLY dev_experimentation.event_log_y2025m08
    ADD CONSTRAINT event_log_y2025m08_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2025m09 event_log_y2025m09_pkey; Type: CONSTRAINT; Schema: dev_experimentation; Owner: postgres
--

ALTER TABLE ONLY dev_experimentation.event_log_y2025m09
    ADD CONSTRAINT event_log_y2025m09_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2025m10 event_log_y2025m10_pkey; Type: CONSTRAINT; Schema: dev_experimentation; Owner: postgres
--

ALTER TABLE ONLY dev_experimentation.event_log_y2025m10
    ADD CONSTRAINT event_log_y2025m10_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2025m11 event_log_y2025m11_pkey; Type: CONSTRAINT; Schema: dev_experimentation; Owner: postgres
--

ALTER TABLE ONLY dev_experimentation.event_log_y2025m11
    ADD CONSTRAINT event_log_y2025m11_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2025m12 event_log_y2025m12_pkey; Type: CONSTRAINT; Schema: dev_experimentation; Owner: postgres
--

ALTER TABLE ONLY dev_experimentation.event_log_y2025m12
    ADD CONSTRAINT event_log_y2025m12_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2026m01 event_log_y2026m01_pkey; Type: CONSTRAINT; Schema: dev_experimentation; Owner: postgres
--

ALTER TABLE ONLY dev_experimentation.event_log_y2026m01
    ADD CONSTRAINT event_log_y2026m01_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2026m02 event_log_y2026m02_pkey; Type: CONSTRAINT; Schema: dev_experimentation; Owner: postgres
--

ALTER TABLE ONLY dev_experimentation.event_log_y2026m02
    ADD CONSTRAINT event_log_y2026m02_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2026m03 event_log_y2026m03_pkey; Type: CONSTRAINT; Schema: dev_experimentation; Owner: postgres
--

ALTER TABLE ONLY dev_experimentation.event_log_y2026m03
    ADD CONSTRAINT event_log_y2026m03_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2026m04 event_log_y2026m04_pkey; Type: CONSTRAINT; Schema: dev_experimentation; Owner: postgres
--

ALTER TABLE ONLY dev_experimentation.event_log_y2026m04
    ADD CONSTRAINT event_log_y2026m04_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2026m05 event_log_y2026m05_pkey; Type: CONSTRAINT; Schema: dev_experimentation; Owner: postgres
--

ALTER TABLE ONLY dev_experimentation.event_log_y2026m05
    ADD CONSTRAINT event_log_y2026m05_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2026m06 event_log_y2026m06_pkey; Type: CONSTRAINT; Schema: dev_experimentation; Owner: postgres
--

ALTER TABLE ONLY dev_experimentation.event_log_y2026m06
    ADD CONSTRAINT event_log_y2026m06_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2026m07 event_log_y2026m07_pkey; Type: CONSTRAINT; Schema: dev_experimentation; Owner: postgres
--

ALTER TABLE ONLY dev_experimentation.event_log_y2026m07
    ADD CONSTRAINT event_log_y2026m07_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2026m08 event_log_y2026m08_pkey; Type: CONSTRAINT; Schema: dev_experimentation; Owner: postgres
--

ALTER TABLE ONLY dev_experimentation.event_log_y2026m08
    ADD CONSTRAINT event_log_y2026m08_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2026m09 event_log_y2026m09_pkey; Type: CONSTRAINT; Schema: dev_experimentation; Owner: postgres
--

ALTER TABLE ONLY dev_experimentation.event_log_y2026m09
    ADD CONSTRAINT event_log_y2026m09_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2026m10 event_log_y2026m10_pkey; Type: CONSTRAINT; Schema: dev_experimentation; Owner: postgres
--

ALTER TABLE ONLY dev_experimentation.event_log_y2026m10
    ADD CONSTRAINT event_log_y2026m10_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2026m11 event_log_y2026m11_pkey; Type: CONSTRAINT; Schema: dev_experimentation; Owner: postgres
--

ALTER TABLE ONLY dev_experimentation.event_log_y2026m11
    ADD CONSTRAINT event_log_y2026m11_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2026m12 event_log_y2026m12_pkey; Type: CONSTRAINT; Schema: dev_experimentation; Owner: postgres
--

ALTER TABLE ONLY dev_experimentation.event_log_y2026m12
    ADD CONSTRAINT event_log_y2026m12_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: experiments experiments_pkey; Type: CONSTRAINT; Schema: dev_experimentation; Owner: postgres
--

ALTER TABLE ONLY dev_experimentation.experiments
    ADD CONSTRAINT experiments_pkey PRIMARY KEY (id);


--
-- Name: contexts contexts_pkey; Type: CONSTRAINT; Schema: test_cac; Owner: postgres
--

ALTER TABLE ONLY test_cac.contexts
    ADD CONSTRAINT contexts_pkey PRIMARY KEY (id);


--
-- Name: default_configs default_configs_pkey; Type: CONSTRAINT; Schema: test_cac; Owner: postgres
--

ALTER TABLE ONLY test_cac.default_configs
    ADD CONSTRAINT default_configs_pkey PRIMARY KEY (key);


--
-- Name: dimensions dimensions_pkey; Type: CONSTRAINT; Schema: test_cac; Owner: postgres
--

ALTER TABLE ONLY test_cac.dimensions
    ADD CONSTRAINT dimensions_pkey PRIMARY KEY (dimension);


--
-- Name: event_log event_log_pkey; Type: CONSTRAINT; Schema: test_cac; Owner: postgres
--

ALTER TABLE ONLY test_cac.event_log
    ADD CONSTRAINT event_log_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2023m08 event_log_y2023m08_pkey; Type: CONSTRAINT; Schema: test_cac; Owner: postgres
--

ALTER TABLE ONLY test_cac.event_log_y2023m08
    ADD CONSTRAINT event_log_y2023m08_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2023m09 event_log_y2023m09_pkey; Type: CONSTRAINT; Schema: test_cac; Owner: postgres
--

ALTER TABLE ONLY test_cac.event_log_y2023m09
    ADD CONSTRAINT event_log_y2023m09_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2023m10 event_log_y2023m10_pkey; Type: CONSTRAINT; Schema: test_cac; Owner: postgres
--

ALTER TABLE ONLY test_cac.event_log_y2023m10
    ADD CONSTRAINT event_log_y2023m10_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2023m11 event_log_y2023m11_pkey; Type: CONSTRAINT; Schema: test_cac; Owner: postgres
--

ALTER TABLE ONLY test_cac.event_log_y2023m11
    ADD CONSTRAINT event_log_y2023m11_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2023m12 event_log_y2023m12_pkey; Type: CONSTRAINT; Schema: test_cac; Owner: postgres
--

ALTER TABLE ONLY test_cac.event_log_y2023m12
    ADD CONSTRAINT event_log_y2023m12_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2024m01 event_log_y2024m01_pkey; Type: CONSTRAINT; Schema: test_cac; Owner: postgres
--

ALTER TABLE ONLY test_cac.event_log_y2024m01
    ADD CONSTRAINT event_log_y2024m01_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2024m02 event_log_y2024m02_pkey; Type: CONSTRAINT; Schema: test_cac; Owner: postgres
--

ALTER TABLE ONLY test_cac.event_log_y2024m02
    ADD CONSTRAINT event_log_y2024m02_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2024m03 event_log_y2024m03_pkey; Type: CONSTRAINT; Schema: test_cac; Owner: postgres
--

ALTER TABLE ONLY test_cac.event_log_y2024m03
    ADD CONSTRAINT event_log_y2024m03_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2024m04 event_log_y2024m04_pkey; Type: CONSTRAINT; Schema: test_cac; Owner: postgres
--

ALTER TABLE ONLY test_cac.event_log_y2024m04
    ADD CONSTRAINT event_log_y2024m04_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2024m05 event_log_y2024m05_pkey; Type: CONSTRAINT; Schema: test_cac; Owner: postgres
--

ALTER TABLE ONLY test_cac.event_log_y2024m05
    ADD CONSTRAINT event_log_y2024m05_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2024m06 event_log_y2024m06_pkey; Type: CONSTRAINT; Schema: test_cac; Owner: postgres
--

ALTER TABLE ONLY test_cac.event_log_y2024m06
    ADD CONSTRAINT event_log_y2024m06_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2024m07 event_log_y2024m07_pkey; Type: CONSTRAINT; Schema: test_cac; Owner: postgres
--

ALTER TABLE ONLY test_cac.event_log_y2024m07
    ADD CONSTRAINT event_log_y2024m07_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2024m08 event_log_y2024m08_pkey; Type: CONSTRAINT; Schema: test_cac; Owner: postgres
--

ALTER TABLE ONLY test_cac.event_log_y2024m08
    ADD CONSTRAINT event_log_y2024m08_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2024m09 event_log_y2024m09_pkey; Type: CONSTRAINT; Schema: test_cac; Owner: postgres
--

ALTER TABLE ONLY test_cac.event_log_y2024m09
    ADD CONSTRAINT event_log_y2024m09_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2024m10 event_log_y2024m10_pkey; Type: CONSTRAINT; Schema: test_cac; Owner: postgres
--

ALTER TABLE ONLY test_cac.event_log_y2024m10
    ADD CONSTRAINT event_log_y2024m10_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2024m11 event_log_y2024m11_pkey; Type: CONSTRAINT; Schema: test_cac; Owner: postgres
--

ALTER TABLE ONLY test_cac.event_log_y2024m11
    ADD CONSTRAINT event_log_y2024m11_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2024m12 event_log_y2024m12_pkey; Type: CONSTRAINT; Schema: test_cac; Owner: postgres
--

ALTER TABLE ONLY test_cac.event_log_y2024m12
    ADD CONSTRAINT event_log_y2024m12_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2025m01 event_log_y2025m01_pkey; Type: CONSTRAINT; Schema: test_cac; Owner: postgres
--

ALTER TABLE ONLY test_cac.event_log_y2025m01
    ADD CONSTRAINT event_log_y2025m01_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2025m02 event_log_y2025m02_pkey; Type: CONSTRAINT; Schema: test_cac; Owner: postgres
--

ALTER TABLE ONLY test_cac.event_log_y2025m02
    ADD CONSTRAINT event_log_y2025m02_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2025m03 event_log_y2025m03_pkey; Type: CONSTRAINT; Schema: test_cac; Owner: postgres
--

ALTER TABLE ONLY test_cac.event_log_y2025m03
    ADD CONSTRAINT event_log_y2025m03_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2025m04 event_log_y2025m04_pkey; Type: CONSTRAINT; Schema: test_cac; Owner: postgres
--

ALTER TABLE ONLY test_cac.event_log_y2025m04
    ADD CONSTRAINT event_log_y2025m04_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2025m05 event_log_y2025m05_pkey; Type: CONSTRAINT; Schema: test_cac; Owner: postgres
--

ALTER TABLE ONLY test_cac.event_log_y2025m05
    ADD CONSTRAINT event_log_y2025m05_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2025m06 event_log_y2025m06_pkey; Type: CONSTRAINT; Schema: test_cac; Owner: postgres
--

ALTER TABLE ONLY test_cac.event_log_y2025m06
    ADD CONSTRAINT event_log_y2025m06_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2025m07 event_log_y2025m07_pkey; Type: CONSTRAINT; Schema: test_cac; Owner: postgres
--

ALTER TABLE ONLY test_cac.event_log_y2025m07
    ADD CONSTRAINT event_log_y2025m07_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2025m08 event_log_y2025m08_pkey; Type: CONSTRAINT; Schema: test_cac; Owner: postgres
--

ALTER TABLE ONLY test_cac.event_log_y2025m08
    ADD CONSTRAINT event_log_y2025m08_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2025m09 event_log_y2025m09_pkey; Type: CONSTRAINT; Schema: test_cac; Owner: postgres
--

ALTER TABLE ONLY test_cac.event_log_y2025m09
    ADD CONSTRAINT event_log_y2025m09_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2025m10 event_log_y2025m10_pkey; Type: CONSTRAINT; Schema: test_cac; Owner: postgres
--

ALTER TABLE ONLY test_cac.event_log_y2025m10
    ADD CONSTRAINT event_log_y2025m10_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2025m11 event_log_y2025m11_pkey; Type: CONSTRAINT; Schema: test_cac; Owner: postgres
--

ALTER TABLE ONLY test_cac.event_log_y2025m11
    ADD CONSTRAINT event_log_y2025m11_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2025m12 event_log_y2025m12_pkey; Type: CONSTRAINT; Schema: test_cac; Owner: postgres
--

ALTER TABLE ONLY test_cac.event_log_y2025m12
    ADD CONSTRAINT event_log_y2025m12_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2026m01 event_log_y2026m01_pkey; Type: CONSTRAINT; Schema: test_cac; Owner: postgres
--

ALTER TABLE ONLY test_cac.event_log_y2026m01
    ADD CONSTRAINT event_log_y2026m01_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2026m02 event_log_y2026m02_pkey; Type: CONSTRAINT; Schema: test_cac; Owner: postgres
--

ALTER TABLE ONLY test_cac.event_log_y2026m02
    ADD CONSTRAINT event_log_y2026m02_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2026m03 event_log_y2026m03_pkey; Type: CONSTRAINT; Schema: test_cac; Owner: postgres
--

ALTER TABLE ONLY test_cac.event_log_y2026m03
    ADD CONSTRAINT event_log_y2026m03_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2026m04 event_log_y2026m04_pkey; Type: CONSTRAINT; Schema: test_cac; Owner: postgres
--

ALTER TABLE ONLY test_cac.event_log_y2026m04
    ADD CONSTRAINT event_log_y2026m04_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2026m05 event_log_y2026m05_pkey; Type: CONSTRAINT; Schema: test_cac; Owner: postgres
--

ALTER TABLE ONLY test_cac.event_log_y2026m05
    ADD CONSTRAINT event_log_y2026m05_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2026m06 event_log_y2026m06_pkey; Type: CONSTRAINT; Schema: test_cac; Owner: postgres
--

ALTER TABLE ONLY test_cac.event_log_y2026m06
    ADD CONSTRAINT event_log_y2026m06_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2026m07 event_log_y2026m07_pkey; Type: CONSTRAINT; Schema: test_cac; Owner: postgres
--

ALTER TABLE ONLY test_cac.event_log_y2026m07
    ADD CONSTRAINT event_log_y2026m07_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2026m08 event_log_y2026m08_pkey; Type: CONSTRAINT; Schema: test_cac; Owner: postgres
--

ALTER TABLE ONLY test_cac.event_log_y2026m08
    ADD CONSTRAINT event_log_y2026m08_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2026m09 event_log_y2026m09_pkey; Type: CONSTRAINT; Schema: test_cac; Owner: postgres
--

ALTER TABLE ONLY test_cac.event_log_y2026m09
    ADD CONSTRAINT event_log_y2026m09_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2026m10 event_log_y2026m10_pkey; Type: CONSTRAINT; Schema: test_cac; Owner: postgres
--

ALTER TABLE ONLY test_cac.event_log_y2026m10
    ADD CONSTRAINT event_log_y2026m10_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2026m11 event_log_y2026m11_pkey; Type: CONSTRAINT; Schema: test_cac; Owner: postgres
--

ALTER TABLE ONLY test_cac.event_log_y2026m11
    ADD CONSTRAINT event_log_y2026m11_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2026m12 event_log_y2026m12_pkey; Type: CONSTRAINT; Schema: test_cac; Owner: postgres
--

ALTER TABLE ONLY test_cac.event_log_y2026m12
    ADD CONSTRAINT event_log_y2026m12_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: functions functions_pkey; Type: CONSTRAINT; Schema: test_cac; Owner: postgres
--

ALTER TABLE ONLY test_cac.functions
    ADD CONSTRAINT functions_pkey PRIMARY KEY (function_name);


--
-- Name: event_log event_log_pkey; Type: CONSTRAINT; Schema: test_experimentation; Owner: postgres
--

ALTER TABLE ONLY test_experimentation.event_log
    ADD CONSTRAINT event_log_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2023m08 event_log_y2023m08_pkey; Type: CONSTRAINT; Schema: test_experimentation; Owner: postgres
--

ALTER TABLE ONLY test_experimentation.event_log_y2023m08
    ADD CONSTRAINT event_log_y2023m08_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2023m09 event_log_y2023m09_pkey; Type: CONSTRAINT; Schema: test_experimentation; Owner: postgres
--

ALTER TABLE ONLY test_experimentation.event_log_y2023m09
    ADD CONSTRAINT event_log_y2023m09_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2023m10 event_log_y2023m10_pkey; Type: CONSTRAINT; Schema: test_experimentation; Owner: postgres
--

ALTER TABLE ONLY test_experimentation.event_log_y2023m10
    ADD CONSTRAINT event_log_y2023m10_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2023m11 event_log_y2023m11_pkey; Type: CONSTRAINT; Schema: test_experimentation; Owner: postgres
--

ALTER TABLE ONLY test_experimentation.event_log_y2023m11
    ADD CONSTRAINT event_log_y2023m11_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2023m12 event_log_y2023m12_pkey; Type: CONSTRAINT; Schema: test_experimentation; Owner: postgres
--

ALTER TABLE ONLY test_experimentation.event_log_y2023m12
    ADD CONSTRAINT event_log_y2023m12_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2024m01 event_log_y2024m01_pkey; Type: CONSTRAINT; Schema: test_experimentation; Owner: postgres
--

ALTER TABLE ONLY test_experimentation.event_log_y2024m01
    ADD CONSTRAINT event_log_y2024m01_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2024m02 event_log_y2024m02_pkey; Type: CONSTRAINT; Schema: test_experimentation; Owner: postgres
--

ALTER TABLE ONLY test_experimentation.event_log_y2024m02
    ADD CONSTRAINT event_log_y2024m02_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2024m03 event_log_y2024m03_pkey; Type: CONSTRAINT; Schema: test_experimentation; Owner: postgres
--

ALTER TABLE ONLY test_experimentation.event_log_y2024m03
    ADD CONSTRAINT event_log_y2024m03_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2024m04 event_log_y2024m04_pkey; Type: CONSTRAINT; Schema: test_experimentation; Owner: postgres
--

ALTER TABLE ONLY test_experimentation.event_log_y2024m04
    ADD CONSTRAINT event_log_y2024m04_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2024m05 event_log_y2024m05_pkey; Type: CONSTRAINT; Schema: test_experimentation; Owner: postgres
--

ALTER TABLE ONLY test_experimentation.event_log_y2024m05
    ADD CONSTRAINT event_log_y2024m05_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2024m06 event_log_y2024m06_pkey; Type: CONSTRAINT; Schema: test_experimentation; Owner: postgres
--

ALTER TABLE ONLY test_experimentation.event_log_y2024m06
    ADD CONSTRAINT event_log_y2024m06_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2024m07 event_log_y2024m07_pkey; Type: CONSTRAINT; Schema: test_experimentation; Owner: postgres
--

ALTER TABLE ONLY test_experimentation.event_log_y2024m07
    ADD CONSTRAINT event_log_y2024m07_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2024m08 event_log_y2024m08_pkey; Type: CONSTRAINT; Schema: test_experimentation; Owner: postgres
--

ALTER TABLE ONLY test_experimentation.event_log_y2024m08
    ADD CONSTRAINT event_log_y2024m08_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2024m09 event_log_y2024m09_pkey; Type: CONSTRAINT; Schema: test_experimentation; Owner: postgres
--

ALTER TABLE ONLY test_experimentation.event_log_y2024m09
    ADD CONSTRAINT event_log_y2024m09_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2024m10 event_log_y2024m10_pkey; Type: CONSTRAINT; Schema: test_experimentation; Owner: postgres
--

ALTER TABLE ONLY test_experimentation.event_log_y2024m10
    ADD CONSTRAINT event_log_y2024m10_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2024m11 event_log_y2024m11_pkey; Type: CONSTRAINT; Schema: test_experimentation; Owner: postgres
--

ALTER TABLE ONLY test_experimentation.event_log_y2024m11
    ADD CONSTRAINT event_log_y2024m11_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2024m12 event_log_y2024m12_pkey; Type: CONSTRAINT; Schema: test_experimentation; Owner: postgres
--

ALTER TABLE ONLY test_experimentation.event_log_y2024m12
    ADD CONSTRAINT event_log_y2024m12_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2025m01 event_log_y2025m01_pkey; Type: CONSTRAINT; Schema: test_experimentation; Owner: postgres
--

ALTER TABLE ONLY test_experimentation.event_log_y2025m01
    ADD CONSTRAINT event_log_y2025m01_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2025m02 event_log_y2025m02_pkey; Type: CONSTRAINT; Schema: test_experimentation; Owner: postgres
--

ALTER TABLE ONLY test_experimentation.event_log_y2025m02
    ADD CONSTRAINT event_log_y2025m02_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2025m03 event_log_y2025m03_pkey; Type: CONSTRAINT; Schema: test_experimentation; Owner: postgres
--

ALTER TABLE ONLY test_experimentation.event_log_y2025m03
    ADD CONSTRAINT event_log_y2025m03_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2025m04 event_log_y2025m04_pkey; Type: CONSTRAINT; Schema: test_experimentation; Owner: postgres
--

ALTER TABLE ONLY test_experimentation.event_log_y2025m04
    ADD CONSTRAINT event_log_y2025m04_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2025m05 event_log_y2025m05_pkey; Type: CONSTRAINT; Schema: test_experimentation; Owner: postgres
--

ALTER TABLE ONLY test_experimentation.event_log_y2025m05
    ADD CONSTRAINT event_log_y2025m05_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2025m06 event_log_y2025m06_pkey; Type: CONSTRAINT; Schema: test_experimentation; Owner: postgres
--

ALTER TABLE ONLY test_experimentation.event_log_y2025m06
    ADD CONSTRAINT event_log_y2025m06_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2025m07 event_log_y2025m07_pkey; Type: CONSTRAINT; Schema: test_experimentation; Owner: postgres
--

ALTER TABLE ONLY test_experimentation.event_log_y2025m07
    ADD CONSTRAINT event_log_y2025m07_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2025m08 event_log_y2025m08_pkey; Type: CONSTRAINT; Schema: test_experimentation; Owner: postgres
--

ALTER TABLE ONLY test_experimentation.event_log_y2025m08
    ADD CONSTRAINT event_log_y2025m08_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2025m09 event_log_y2025m09_pkey; Type: CONSTRAINT; Schema: test_experimentation; Owner: postgres
--

ALTER TABLE ONLY test_experimentation.event_log_y2025m09
    ADD CONSTRAINT event_log_y2025m09_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2025m10 event_log_y2025m10_pkey; Type: CONSTRAINT; Schema: test_experimentation; Owner: postgres
--

ALTER TABLE ONLY test_experimentation.event_log_y2025m10
    ADD CONSTRAINT event_log_y2025m10_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2025m11 event_log_y2025m11_pkey; Type: CONSTRAINT; Schema: test_experimentation; Owner: postgres
--

ALTER TABLE ONLY test_experimentation.event_log_y2025m11
    ADD CONSTRAINT event_log_y2025m11_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2025m12 event_log_y2025m12_pkey; Type: CONSTRAINT; Schema: test_experimentation; Owner: postgres
--

ALTER TABLE ONLY test_experimentation.event_log_y2025m12
    ADD CONSTRAINT event_log_y2025m12_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2026m01 event_log_y2026m01_pkey; Type: CONSTRAINT; Schema: test_experimentation; Owner: postgres
--

ALTER TABLE ONLY test_experimentation.event_log_y2026m01
    ADD CONSTRAINT event_log_y2026m01_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2026m02 event_log_y2026m02_pkey; Type: CONSTRAINT; Schema: test_experimentation; Owner: postgres
--

ALTER TABLE ONLY test_experimentation.event_log_y2026m02
    ADD CONSTRAINT event_log_y2026m02_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2026m03 event_log_y2026m03_pkey; Type: CONSTRAINT; Schema: test_experimentation; Owner: postgres
--

ALTER TABLE ONLY test_experimentation.event_log_y2026m03
    ADD CONSTRAINT event_log_y2026m03_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2026m04 event_log_y2026m04_pkey; Type: CONSTRAINT; Schema: test_experimentation; Owner: postgres
--

ALTER TABLE ONLY test_experimentation.event_log_y2026m04
    ADD CONSTRAINT event_log_y2026m04_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2026m05 event_log_y2026m05_pkey; Type: CONSTRAINT; Schema: test_experimentation; Owner: postgres
--

ALTER TABLE ONLY test_experimentation.event_log_y2026m05
    ADD CONSTRAINT event_log_y2026m05_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2026m06 event_log_y2026m06_pkey; Type: CONSTRAINT; Schema: test_experimentation; Owner: postgres
--

ALTER TABLE ONLY test_experimentation.event_log_y2026m06
    ADD CONSTRAINT event_log_y2026m06_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2026m07 event_log_y2026m07_pkey; Type: CONSTRAINT; Schema: test_experimentation; Owner: postgres
--

ALTER TABLE ONLY test_experimentation.event_log_y2026m07
    ADD CONSTRAINT event_log_y2026m07_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2026m08 event_log_y2026m08_pkey; Type: CONSTRAINT; Schema: test_experimentation; Owner: postgres
--

ALTER TABLE ONLY test_experimentation.event_log_y2026m08
    ADD CONSTRAINT event_log_y2026m08_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2026m09 event_log_y2026m09_pkey; Type: CONSTRAINT; Schema: test_experimentation; Owner: postgres
--

ALTER TABLE ONLY test_experimentation.event_log_y2026m09
    ADD CONSTRAINT event_log_y2026m09_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2026m10 event_log_y2026m10_pkey; Type: CONSTRAINT; Schema: test_experimentation; Owner: postgres
--

ALTER TABLE ONLY test_experimentation.event_log_y2026m10
    ADD CONSTRAINT event_log_y2026m10_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2026m11 event_log_y2026m11_pkey; Type: CONSTRAINT; Schema: test_experimentation; Owner: postgres
--

ALTER TABLE ONLY test_experimentation.event_log_y2026m11
    ADD CONSTRAINT event_log_y2026m11_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2026m12 event_log_y2026m12_pkey; Type: CONSTRAINT; Schema: test_experimentation; Owner: postgres
--

ALTER TABLE ONLY test_experimentation.event_log_y2026m12
    ADD CONSTRAINT event_log_y2026m12_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: experiments experiments_pkey; Type: CONSTRAINT; Schema: test_experimentation; Owner: postgres
--

ALTER TABLE ONLY test_experimentation.experiments
    ADD CONSTRAINT experiments_pkey PRIMARY KEY (id);


--
-- Name: event_log_action_index; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_action_index ON ONLY dev_cac.event_log USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_table_name_index; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_table_name_index ON ONLY dev_cac.event_log USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_timestamp_index; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_timestamp_index ON ONLY dev_cac.event_log USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2023m08_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2023m08_action_timestamp_table_name_idx ON dev_cac.event_log_y2023m08 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2023m08_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2023m08_table_name_action_timestamp_idx ON dev_cac.event_log_y2023m08 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2023m08_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2023m08_timestamp_action_table_name_idx ON dev_cac.event_log_y2023m08 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2023m09_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2023m09_action_timestamp_table_name_idx ON dev_cac.event_log_y2023m09 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2023m09_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2023m09_table_name_action_timestamp_idx ON dev_cac.event_log_y2023m09 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2023m09_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2023m09_timestamp_action_table_name_idx ON dev_cac.event_log_y2023m09 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2023m10_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2023m10_action_timestamp_table_name_idx ON dev_cac.event_log_y2023m10 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2023m10_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2023m10_table_name_action_timestamp_idx ON dev_cac.event_log_y2023m10 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2023m10_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2023m10_timestamp_action_table_name_idx ON dev_cac.event_log_y2023m10 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2023m11_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2023m11_action_timestamp_table_name_idx ON dev_cac.event_log_y2023m11 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2023m11_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2023m11_table_name_action_timestamp_idx ON dev_cac.event_log_y2023m11 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2023m11_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2023m11_timestamp_action_table_name_idx ON dev_cac.event_log_y2023m11 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2023m12_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2023m12_action_timestamp_table_name_idx ON dev_cac.event_log_y2023m12 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2023m12_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2023m12_table_name_action_timestamp_idx ON dev_cac.event_log_y2023m12 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2023m12_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2023m12_timestamp_action_table_name_idx ON dev_cac.event_log_y2023m12 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2024m01_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m01_action_timestamp_table_name_idx ON dev_cac.event_log_y2024m01 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2024m01_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m01_table_name_action_timestamp_idx ON dev_cac.event_log_y2024m01 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2024m01_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m01_timestamp_action_table_name_idx ON dev_cac.event_log_y2024m01 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2024m02_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m02_action_timestamp_table_name_idx ON dev_cac.event_log_y2024m02 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2024m02_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m02_table_name_action_timestamp_idx ON dev_cac.event_log_y2024m02 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2024m02_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m02_timestamp_action_table_name_idx ON dev_cac.event_log_y2024m02 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2024m03_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m03_action_timestamp_table_name_idx ON dev_cac.event_log_y2024m03 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2024m03_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m03_table_name_action_timestamp_idx ON dev_cac.event_log_y2024m03 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2024m03_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m03_timestamp_action_table_name_idx ON dev_cac.event_log_y2024m03 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2024m04_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m04_action_timestamp_table_name_idx ON dev_cac.event_log_y2024m04 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2024m04_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m04_table_name_action_timestamp_idx ON dev_cac.event_log_y2024m04 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2024m04_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m04_timestamp_action_table_name_idx ON dev_cac.event_log_y2024m04 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2024m05_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m05_action_timestamp_table_name_idx ON dev_cac.event_log_y2024m05 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2024m05_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m05_table_name_action_timestamp_idx ON dev_cac.event_log_y2024m05 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2024m05_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m05_timestamp_action_table_name_idx ON dev_cac.event_log_y2024m05 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2024m06_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m06_action_timestamp_table_name_idx ON dev_cac.event_log_y2024m06 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2024m06_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m06_table_name_action_timestamp_idx ON dev_cac.event_log_y2024m06 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2024m06_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m06_timestamp_action_table_name_idx ON dev_cac.event_log_y2024m06 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2024m07_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m07_action_timestamp_table_name_idx ON dev_cac.event_log_y2024m07 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2024m07_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m07_table_name_action_timestamp_idx ON dev_cac.event_log_y2024m07 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2024m07_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m07_timestamp_action_table_name_idx ON dev_cac.event_log_y2024m07 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2024m08_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m08_action_timestamp_table_name_idx ON dev_cac.event_log_y2024m08 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2024m08_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m08_table_name_action_timestamp_idx ON dev_cac.event_log_y2024m08 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2024m08_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m08_timestamp_action_table_name_idx ON dev_cac.event_log_y2024m08 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2024m09_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m09_action_timestamp_table_name_idx ON dev_cac.event_log_y2024m09 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2024m09_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m09_table_name_action_timestamp_idx ON dev_cac.event_log_y2024m09 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2024m09_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m09_timestamp_action_table_name_idx ON dev_cac.event_log_y2024m09 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2024m10_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m10_action_timestamp_table_name_idx ON dev_cac.event_log_y2024m10 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2024m10_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m10_table_name_action_timestamp_idx ON dev_cac.event_log_y2024m10 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2024m10_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m10_timestamp_action_table_name_idx ON dev_cac.event_log_y2024m10 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2024m11_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m11_action_timestamp_table_name_idx ON dev_cac.event_log_y2024m11 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2024m11_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m11_table_name_action_timestamp_idx ON dev_cac.event_log_y2024m11 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2024m11_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m11_timestamp_action_table_name_idx ON dev_cac.event_log_y2024m11 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2024m12_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m12_action_timestamp_table_name_idx ON dev_cac.event_log_y2024m12 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2024m12_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m12_table_name_action_timestamp_idx ON dev_cac.event_log_y2024m12 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2024m12_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m12_timestamp_action_table_name_idx ON dev_cac.event_log_y2024m12 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2025m01_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m01_action_timestamp_table_name_idx ON dev_cac.event_log_y2025m01 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2025m01_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m01_table_name_action_timestamp_idx ON dev_cac.event_log_y2025m01 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2025m01_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m01_timestamp_action_table_name_idx ON dev_cac.event_log_y2025m01 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2025m02_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m02_action_timestamp_table_name_idx ON dev_cac.event_log_y2025m02 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2025m02_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m02_table_name_action_timestamp_idx ON dev_cac.event_log_y2025m02 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2025m02_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m02_timestamp_action_table_name_idx ON dev_cac.event_log_y2025m02 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2025m03_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m03_action_timestamp_table_name_idx ON dev_cac.event_log_y2025m03 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2025m03_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m03_table_name_action_timestamp_idx ON dev_cac.event_log_y2025m03 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2025m03_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m03_timestamp_action_table_name_idx ON dev_cac.event_log_y2025m03 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2025m04_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m04_action_timestamp_table_name_idx ON dev_cac.event_log_y2025m04 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2025m04_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m04_table_name_action_timestamp_idx ON dev_cac.event_log_y2025m04 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2025m04_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m04_timestamp_action_table_name_idx ON dev_cac.event_log_y2025m04 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2025m05_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m05_action_timestamp_table_name_idx ON dev_cac.event_log_y2025m05 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2025m05_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m05_table_name_action_timestamp_idx ON dev_cac.event_log_y2025m05 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2025m05_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m05_timestamp_action_table_name_idx ON dev_cac.event_log_y2025m05 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2025m06_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m06_action_timestamp_table_name_idx ON dev_cac.event_log_y2025m06 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2025m06_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m06_table_name_action_timestamp_idx ON dev_cac.event_log_y2025m06 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2025m06_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m06_timestamp_action_table_name_idx ON dev_cac.event_log_y2025m06 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2025m07_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m07_action_timestamp_table_name_idx ON dev_cac.event_log_y2025m07 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2025m07_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m07_table_name_action_timestamp_idx ON dev_cac.event_log_y2025m07 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2025m07_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m07_timestamp_action_table_name_idx ON dev_cac.event_log_y2025m07 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2025m08_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m08_action_timestamp_table_name_idx ON dev_cac.event_log_y2025m08 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2025m08_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m08_table_name_action_timestamp_idx ON dev_cac.event_log_y2025m08 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2025m08_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m08_timestamp_action_table_name_idx ON dev_cac.event_log_y2025m08 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2025m09_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m09_action_timestamp_table_name_idx ON dev_cac.event_log_y2025m09 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2025m09_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m09_table_name_action_timestamp_idx ON dev_cac.event_log_y2025m09 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2025m09_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m09_timestamp_action_table_name_idx ON dev_cac.event_log_y2025m09 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2025m10_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m10_action_timestamp_table_name_idx ON dev_cac.event_log_y2025m10 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2025m10_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m10_table_name_action_timestamp_idx ON dev_cac.event_log_y2025m10 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2025m10_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m10_timestamp_action_table_name_idx ON dev_cac.event_log_y2025m10 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2025m11_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m11_action_timestamp_table_name_idx ON dev_cac.event_log_y2025m11 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2025m11_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m11_table_name_action_timestamp_idx ON dev_cac.event_log_y2025m11 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2025m11_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m11_timestamp_action_table_name_idx ON dev_cac.event_log_y2025m11 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2025m12_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m12_action_timestamp_table_name_idx ON dev_cac.event_log_y2025m12 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2025m12_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m12_table_name_action_timestamp_idx ON dev_cac.event_log_y2025m12 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2025m12_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m12_timestamp_action_table_name_idx ON dev_cac.event_log_y2025m12 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2026m01_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m01_action_timestamp_table_name_idx ON dev_cac.event_log_y2026m01 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2026m01_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m01_table_name_action_timestamp_idx ON dev_cac.event_log_y2026m01 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2026m01_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m01_timestamp_action_table_name_idx ON dev_cac.event_log_y2026m01 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2026m02_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m02_action_timestamp_table_name_idx ON dev_cac.event_log_y2026m02 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2026m02_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m02_table_name_action_timestamp_idx ON dev_cac.event_log_y2026m02 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2026m02_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m02_timestamp_action_table_name_idx ON dev_cac.event_log_y2026m02 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2026m03_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m03_action_timestamp_table_name_idx ON dev_cac.event_log_y2026m03 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2026m03_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m03_table_name_action_timestamp_idx ON dev_cac.event_log_y2026m03 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2026m03_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m03_timestamp_action_table_name_idx ON dev_cac.event_log_y2026m03 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2026m04_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m04_action_timestamp_table_name_idx ON dev_cac.event_log_y2026m04 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2026m04_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m04_table_name_action_timestamp_idx ON dev_cac.event_log_y2026m04 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2026m04_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m04_timestamp_action_table_name_idx ON dev_cac.event_log_y2026m04 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2026m05_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m05_action_timestamp_table_name_idx ON dev_cac.event_log_y2026m05 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2026m05_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m05_table_name_action_timestamp_idx ON dev_cac.event_log_y2026m05 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2026m05_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m05_timestamp_action_table_name_idx ON dev_cac.event_log_y2026m05 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2026m06_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m06_action_timestamp_table_name_idx ON dev_cac.event_log_y2026m06 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2026m06_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m06_table_name_action_timestamp_idx ON dev_cac.event_log_y2026m06 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2026m06_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m06_timestamp_action_table_name_idx ON dev_cac.event_log_y2026m06 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2026m07_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m07_action_timestamp_table_name_idx ON dev_cac.event_log_y2026m07 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2026m07_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m07_table_name_action_timestamp_idx ON dev_cac.event_log_y2026m07 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2026m07_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m07_timestamp_action_table_name_idx ON dev_cac.event_log_y2026m07 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2026m08_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m08_action_timestamp_table_name_idx ON dev_cac.event_log_y2026m08 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2026m08_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m08_table_name_action_timestamp_idx ON dev_cac.event_log_y2026m08 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2026m08_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m08_timestamp_action_table_name_idx ON dev_cac.event_log_y2026m08 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2026m09_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m09_action_timestamp_table_name_idx ON dev_cac.event_log_y2026m09 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2026m09_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m09_table_name_action_timestamp_idx ON dev_cac.event_log_y2026m09 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2026m09_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m09_timestamp_action_table_name_idx ON dev_cac.event_log_y2026m09 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2026m10_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m10_action_timestamp_table_name_idx ON dev_cac.event_log_y2026m10 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2026m10_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m10_table_name_action_timestamp_idx ON dev_cac.event_log_y2026m10 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2026m10_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m10_timestamp_action_table_name_idx ON dev_cac.event_log_y2026m10 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2026m11_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m11_action_timestamp_table_name_idx ON dev_cac.event_log_y2026m11 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2026m11_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m11_table_name_action_timestamp_idx ON dev_cac.event_log_y2026m11 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2026m11_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m11_timestamp_action_table_name_idx ON dev_cac.event_log_y2026m11 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2026m12_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m12_action_timestamp_table_name_idx ON dev_cac.event_log_y2026m12 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2026m12_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m12_table_name_action_timestamp_idx ON dev_cac.event_log_y2026m12 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2026m12_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m12_timestamp_action_table_name_idx ON dev_cac.event_log_y2026m12 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_action_index; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_action_index ON ONLY dev_experimentation.event_log USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_table_name_index; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_table_name_index ON ONLY dev_experimentation.event_log USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_timestamp_index; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_timestamp_index ON ONLY dev_experimentation.event_log USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2023m08_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2023m08_action_timestamp_table_name_idx ON dev_experimentation.event_log_y2023m08 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2023m08_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2023m08_table_name_action_timestamp_idx ON dev_experimentation.event_log_y2023m08 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2023m08_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2023m08_timestamp_action_table_name_idx ON dev_experimentation.event_log_y2023m08 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2023m09_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2023m09_action_timestamp_table_name_idx ON dev_experimentation.event_log_y2023m09 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2023m09_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2023m09_table_name_action_timestamp_idx ON dev_experimentation.event_log_y2023m09 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2023m09_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2023m09_timestamp_action_table_name_idx ON dev_experimentation.event_log_y2023m09 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2023m10_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2023m10_action_timestamp_table_name_idx ON dev_experimentation.event_log_y2023m10 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2023m10_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2023m10_table_name_action_timestamp_idx ON dev_experimentation.event_log_y2023m10 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2023m10_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2023m10_timestamp_action_table_name_idx ON dev_experimentation.event_log_y2023m10 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2023m11_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2023m11_action_timestamp_table_name_idx ON dev_experimentation.event_log_y2023m11 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2023m11_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2023m11_table_name_action_timestamp_idx ON dev_experimentation.event_log_y2023m11 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2023m11_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2023m11_timestamp_action_table_name_idx ON dev_experimentation.event_log_y2023m11 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2023m12_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2023m12_action_timestamp_table_name_idx ON dev_experimentation.event_log_y2023m12 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2023m12_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2023m12_table_name_action_timestamp_idx ON dev_experimentation.event_log_y2023m12 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2023m12_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2023m12_timestamp_action_table_name_idx ON dev_experimentation.event_log_y2023m12 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2024m01_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m01_action_timestamp_table_name_idx ON dev_experimentation.event_log_y2024m01 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2024m01_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m01_table_name_action_timestamp_idx ON dev_experimentation.event_log_y2024m01 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2024m01_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m01_timestamp_action_table_name_idx ON dev_experimentation.event_log_y2024m01 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2024m02_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m02_action_timestamp_table_name_idx ON dev_experimentation.event_log_y2024m02 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2024m02_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m02_table_name_action_timestamp_idx ON dev_experimentation.event_log_y2024m02 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2024m02_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m02_timestamp_action_table_name_idx ON dev_experimentation.event_log_y2024m02 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2024m03_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m03_action_timestamp_table_name_idx ON dev_experimentation.event_log_y2024m03 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2024m03_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m03_table_name_action_timestamp_idx ON dev_experimentation.event_log_y2024m03 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2024m03_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m03_timestamp_action_table_name_idx ON dev_experimentation.event_log_y2024m03 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2024m04_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m04_action_timestamp_table_name_idx ON dev_experimentation.event_log_y2024m04 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2024m04_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m04_table_name_action_timestamp_idx ON dev_experimentation.event_log_y2024m04 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2024m04_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m04_timestamp_action_table_name_idx ON dev_experimentation.event_log_y2024m04 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2024m05_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m05_action_timestamp_table_name_idx ON dev_experimentation.event_log_y2024m05 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2024m05_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m05_table_name_action_timestamp_idx ON dev_experimentation.event_log_y2024m05 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2024m05_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m05_timestamp_action_table_name_idx ON dev_experimentation.event_log_y2024m05 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2024m06_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m06_action_timestamp_table_name_idx ON dev_experimentation.event_log_y2024m06 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2024m06_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m06_table_name_action_timestamp_idx ON dev_experimentation.event_log_y2024m06 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2024m06_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m06_timestamp_action_table_name_idx ON dev_experimentation.event_log_y2024m06 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2024m07_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m07_action_timestamp_table_name_idx ON dev_experimentation.event_log_y2024m07 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2024m07_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m07_table_name_action_timestamp_idx ON dev_experimentation.event_log_y2024m07 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2024m07_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m07_timestamp_action_table_name_idx ON dev_experimentation.event_log_y2024m07 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2024m08_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m08_action_timestamp_table_name_idx ON dev_experimentation.event_log_y2024m08 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2024m08_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m08_table_name_action_timestamp_idx ON dev_experimentation.event_log_y2024m08 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2024m08_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m08_timestamp_action_table_name_idx ON dev_experimentation.event_log_y2024m08 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2024m09_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m09_action_timestamp_table_name_idx ON dev_experimentation.event_log_y2024m09 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2024m09_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m09_table_name_action_timestamp_idx ON dev_experimentation.event_log_y2024m09 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2024m09_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m09_timestamp_action_table_name_idx ON dev_experimentation.event_log_y2024m09 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2024m10_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m10_action_timestamp_table_name_idx ON dev_experimentation.event_log_y2024m10 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2024m10_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m10_table_name_action_timestamp_idx ON dev_experimentation.event_log_y2024m10 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2024m10_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m10_timestamp_action_table_name_idx ON dev_experimentation.event_log_y2024m10 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2024m11_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m11_action_timestamp_table_name_idx ON dev_experimentation.event_log_y2024m11 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2024m11_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m11_table_name_action_timestamp_idx ON dev_experimentation.event_log_y2024m11 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2024m11_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m11_timestamp_action_table_name_idx ON dev_experimentation.event_log_y2024m11 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2024m12_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m12_action_timestamp_table_name_idx ON dev_experimentation.event_log_y2024m12 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2024m12_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m12_table_name_action_timestamp_idx ON dev_experimentation.event_log_y2024m12 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2024m12_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m12_timestamp_action_table_name_idx ON dev_experimentation.event_log_y2024m12 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2025m01_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m01_action_timestamp_table_name_idx ON dev_experimentation.event_log_y2025m01 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2025m01_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m01_table_name_action_timestamp_idx ON dev_experimentation.event_log_y2025m01 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2025m01_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m01_timestamp_action_table_name_idx ON dev_experimentation.event_log_y2025m01 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2025m02_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m02_action_timestamp_table_name_idx ON dev_experimentation.event_log_y2025m02 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2025m02_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m02_table_name_action_timestamp_idx ON dev_experimentation.event_log_y2025m02 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2025m02_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m02_timestamp_action_table_name_idx ON dev_experimentation.event_log_y2025m02 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2025m03_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m03_action_timestamp_table_name_idx ON dev_experimentation.event_log_y2025m03 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2025m03_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m03_table_name_action_timestamp_idx ON dev_experimentation.event_log_y2025m03 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2025m03_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m03_timestamp_action_table_name_idx ON dev_experimentation.event_log_y2025m03 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2025m04_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m04_action_timestamp_table_name_idx ON dev_experimentation.event_log_y2025m04 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2025m04_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m04_table_name_action_timestamp_idx ON dev_experimentation.event_log_y2025m04 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2025m04_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m04_timestamp_action_table_name_idx ON dev_experimentation.event_log_y2025m04 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2025m05_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m05_action_timestamp_table_name_idx ON dev_experimentation.event_log_y2025m05 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2025m05_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m05_table_name_action_timestamp_idx ON dev_experimentation.event_log_y2025m05 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2025m05_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m05_timestamp_action_table_name_idx ON dev_experimentation.event_log_y2025m05 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2025m06_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m06_action_timestamp_table_name_idx ON dev_experimentation.event_log_y2025m06 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2025m06_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m06_table_name_action_timestamp_idx ON dev_experimentation.event_log_y2025m06 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2025m06_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m06_timestamp_action_table_name_idx ON dev_experimentation.event_log_y2025m06 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2025m07_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m07_action_timestamp_table_name_idx ON dev_experimentation.event_log_y2025m07 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2025m07_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m07_table_name_action_timestamp_idx ON dev_experimentation.event_log_y2025m07 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2025m07_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m07_timestamp_action_table_name_idx ON dev_experimentation.event_log_y2025m07 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2025m08_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m08_action_timestamp_table_name_idx ON dev_experimentation.event_log_y2025m08 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2025m08_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m08_table_name_action_timestamp_idx ON dev_experimentation.event_log_y2025m08 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2025m08_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m08_timestamp_action_table_name_idx ON dev_experimentation.event_log_y2025m08 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2025m09_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m09_action_timestamp_table_name_idx ON dev_experimentation.event_log_y2025m09 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2025m09_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m09_table_name_action_timestamp_idx ON dev_experimentation.event_log_y2025m09 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2025m09_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m09_timestamp_action_table_name_idx ON dev_experimentation.event_log_y2025m09 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2025m10_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m10_action_timestamp_table_name_idx ON dev_experimentation.event_log_y2025m10 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2025m10_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m10_table_name_action_timestamp_idx ON dev_experimentation.event_log_y2025m10 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2025m10_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m10_timestamp_action_table_name_idx ON dev_experimentation.event_log_y2025m10 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2025m11_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m11_action_timestamp_table_name_idx ON dev_experimentation.event_log_y2025m11 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2025m11_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m11_table_name_action_timestamp_idx ON dev_experimentation.event_log_y2025m11 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2025m11_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m11_timestamp_action_table_name_idx ON dev_experimentation.event_log_y2025m11 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2025m12_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m12_action_timestamp_table_name_idx ON dev_experimentation.event_log_y2025m12 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2025m12_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m12_table_name_action_timestamp_idx ON dev_experimentation.event_log_y2025m12 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2025m12_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m12_timestamp_action_table_name_idx ON dev_experimentation.event_log_y2025m12 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2026m01_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m01_action_timestamp_table_name_idx ON dev_experimentation.event_log_y2026m01 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2026m01_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m01_table_name_action_timestamp_idx ON dev_experimentation.event_log_y2026m01 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2026m01_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m01_timestamp_action_table_name_idx ON dev_experimentation.event_log_y2026m01 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2026m02_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m02_action_timestamp_table_name_idx ON dev_experimentation.event_log_y2026m02 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2026m02_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m02_table_name_action_timestamp_idx ON dev_experimentation.event_log_y2026m02 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2026m02_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m02_timestamp_action_table_name_idx ON dev_experimentation.event_log_y2026m02 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2026m03_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m03_action_timestamp_table_name_idx ON dev_experimentation.event_log_y2026m03 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2026m03_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m03_table_name_action_timestamp_idx ON dev_experimentation.event_log_y2026m03 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2026m03_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m03_timestamp_action_table_name_idx ON dev_experimentation.event_log_y2026m03 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2026m04_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m04_action_timestamp_table_name_idx ON dev_experimentation.event_log_y2026m04 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2026m04_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m04_table_name_action_timestamp_idx ON dev_experimentation.event_log_y2026m04 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2026m04_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m04_timestamp_action_table_name_idx ON dev_experimentation.event_log_y2026m04 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2026m05_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m05_action_timestamp_table_name_idx ON dev_experimentation.event_log_y2026m05 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2026m05_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m05_table_name_action_timestamp_idx ON dev_experimentation.event_log_y2026m05 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2026m05_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m05_timestamp_action_table_name_idx ON dev_experimentation.event_log_y2026m05 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2026m06_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m06_action_timestamp_table_name_idx ON dev_experimentation.event_log_y2026m06 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2026m06_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m06_table_name_action_timestamp_idx ON dev_experimentation.event_log_y2026m06 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2026m06_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m06_timestamp_action_table_name_idx ON dev_experimentation.event_log_y2026m06 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2026m07_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m07_action_timestamp_table_name_idx ON dev_experimentation.event_log_y2026m07 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2026m07_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m07_table_name_action_timestamp_idx ON dev_experimentation.event_log_y2026m07 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2026m07_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m07_timestamp_action_table_name_idx ON dev_experimentation.event_log_y2026m07 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2026m08_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m08_action_timestamp_table_name_idx ON dev_experimentation.event_log_y2026m08 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2026m08_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m08_table_name_action_timestamp_idx ON dev_experimentation.event_log_y2026m08 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2026m08_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m08_timestamp_action_table_name_idx ON dev_experimentation.event_log_y2026m08 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2026m09_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m09_action_timestamp_table_name_idx ON dev_experimentation.event_log_y2026m09 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2026m09_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m09_table_name_action_timestamp_idx ON dev_experimentation.event_log_y2026m09 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2026m09_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m09_timestamp_action_table_name_idx ON dev_experimentation.event_log_y2026m09 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2026m10_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m10_action_timestamp_table_name_idx ON dev_experimentation.event_log_y2026m10 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2026m10_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m10_table_name_action_timestamp_idx ON dev_experimentation.event_log_y2026m10 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2026m10_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m10_timestamp_action_table_name_idx ON dev_experimentation.event_log_y2026m10 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2026m11_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m11_action_timestamp_table_name_idx ON dev_experimentation.event_log_y2026m11 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2026m11_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m11_table_name_action_timestamp_idx ON dev_experimentation.event_log_y2026m11 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2026m11_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m11_timestamp_action_table_name_idx ON dev_experimentation.event_log_y2026m11 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2026m12_action_timestamp_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m12_action_timestamp_table_name_idx ON dev_experimentation.event_log_y2026m12 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2026m12_table_name_action_timestamp_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m12_table_name_action_timestamp_idx ON dev_experimentation.event_log_y2026m12 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2026m12_timestamp_action_table_name_idx; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m12_timestamp_action_table_name_idx ON dev_experimentation.event_log_y2026m12 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: experiment_created_date_index; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX experiment_created_date_index ON dev_experimentation.experiments USING btree (created_at) INCLUDE (id);


--
-- Name: experiment_last_modified_index; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX experiment_last_modified_index ON dev_experimentation.experiments USING btree (last_modified) INCLUDE (id, created_at);


--
-- Name: experiment_status_index; Type: INDEX; Schema: dev_experimentation; Owner: postgres
--

CREATE INDEX experiment_status_index ON dev_experimentation.experiments USING btree (status) INCLUDE (created_at, last_modified);


--
-- Name: event_log_action_index; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_action_index ON ONLY test_cac.event_log USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_table_name_index; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_table_name_index ON ONLY test_cac.event_log USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_timestamp_index; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_timestamp_index ON ONLY test_cac.event_log USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2023m08_action_timestamp_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2023m08_action_timestamp_table_name_idx ON test_cac.event_log_y2023m08 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2023m08_table_name_action_timestamp_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2023m08_table_name_action_timestamp_idx ON test_cac.event_log_y2023m08 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2023m08_timestamp_action_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2023m08_timestamp_action_table_name_idx ON test_cac.event_log_y2023m08 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2023m09_action_timestamp_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2023m09_action_timestamp_table_name_idx ON test_cac.event_log_y2023m09 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2023m09_table_name_action_timestamp_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2023m09_table_name_action_timestamp_idx ON test_cac.event_log_y2023m09 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2023m09_timestamp_action_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2023m09_timestamp_action_table_name_idx ON test_cac.event_log_y2023m09 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2023m10_action_timestamp_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2023m10_action_timestamp_table_name_idx ON test_cac.event_log_y2023m10 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2023m10_table_name_action_timestamp_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2023m10_table_name_action_timestamp_idx ON test_cac.event_log_y2023m10 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2023m10_timestamp_action_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2023m10_timestamp_action_table_name_idx ON test_cac.event_log_y2023m10 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2023m11_action_timestamp_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2023m11_action_timestamp_table_name_idx ON test_cac.event_log_y2023m11 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2023m11_table_name_action_timestamp_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2023m11_table_name_action_timestamp_idx ON test_cac.event_log_y2023m11 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2023m11_timestamp_action_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2023m11_timestamp_action_table_name_idx ON test_cac.event_log_y2023m11 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2023m12_action_timestamp_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2023m12_action_timestamp_table_name_idx ON test_cac.event_log_y2023m12 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2023m12_table_name_action_timestamp_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2023m12_table_name_action_timestamp_idx ON test_cac.event_log_y2023m12 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2023m12_timestamp_action_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2023m12_timestamp_action_table_name_idx ON test_cac.event_log_y2023m12 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2024m01_action_timestamp_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m01_action_timestamp_table_name_idx ON test_cac.event_log_y2024m01 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2024m01_table_name_action_timestamp_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m01_table_name_action_timestamp_idx ON test_cac.event_log_y2024m01 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2024m01_timestamp_action_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m01_timestamp_action_table_name_idx ON test_cac.event_log_y2024m01 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2024m02_action_timestamp_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m02_action_timestamp_table_name_idx ON test_cac.event_log_y2024m02 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2024m02_table_name_action_timestamp_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m02_table_name_action_timestamp_idx ON test_cac.event_log_y2024m02 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2024m02_timestamp_action_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m02_timestamp_action_table_name_idx ON test_cac.event_log_y2024m02 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2024m03_action_timestamp_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m03_action_timestamp_table_name_idx ON test_cac.event_log_y2024m03 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2024m03_table_name_action_timestamp_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m03_table_name_action_timestamp_idx ON test_cac.event_log_y2024m03 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2024m03_timestamp_action_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m03_timestamp_action_table_name_idx ON test_cac.event_log_y2024m03 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2024m04_action_timestamp_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m04_action_timestamp_table_name_idx ON test_cac.event_log_y2024m04 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2024m04_table_name_action_timestamp_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m04_table_name_action_timestamp_idx ON test_cac.event_log_y2024m04 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2024m04_timestamp_action_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m04_timestamp_action_table_name_idx ON test_cac.event_log_y2024m04 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2024m05_action_timestamp_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m05_action_timestamp_table_name_idx ON test_cac.event_log_y2024m05 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2024m05_table_name_action_timestamp_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m05_table_name_action_timestamp_idx ON test_cac.event_log_y2024m05 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2024m05_timestamp_action_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m05_timestamp_action_table_name_idx ON test_cac.event_log_y2024m05 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2024m06_action_timestamp_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m06_action_timestamp_table_name_idx ON test_cac.event_log_y2024m06 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2024m06_table_name_action_timestamp_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m06_table_name_action_timestamp_idx ON test_cac.event_log_y2024m06 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2024m06_timestamp_action_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m06_timestamp_action_table_name_idx ON test_cac.event_log_y2024m06 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2024m07_action_timestamp_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m07_action_timestamp_table_name_idx ON test_cac.event_log_y2024m07 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2024m07_table_name_action_timestamp_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m07_table_name_action_timestamp_idx ON test_cac.event_log_y2024m07 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2024m07_timestamp_action_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m07_timestamp_action_table_name_idx ON test_cac.event_log_y2024m07 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2024m08_action_timestamp_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m08_action_timestamp_table_name_idx ON test_cac.event_log_y2024m08 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2024m08_table_name_action_timestamp_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m08_table_name_action_timestamp_idx ON test_cac.event_log_y2024m08 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2024m08_timestamp_action_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m08_timestamp_action_table_name_idx ON test_cac.event_log_y2024m08 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2024m09_action_timestamp_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m09_action_timestamp_table_name_idx ON test_cac.event_log_y2024m09 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2024m09_table_name_action_timestamp_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m09_table_name_action_timestamp_idx ON test_cac.event_log_y2024m09 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2024m09_timestamp_action_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m09_timestamp_action_table_name_idx ON test_cac.event_log_y2024m09 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2024m10_action_timestamp_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m10_action_timestamp_table_name_idx ON test_cac.event_log_y2024m10 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2024m10_table_name_action_timestamp_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m10_table_name_action_timestamp_idx ON test_cac.event_log_y2024m10 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2024m10_timestamp_action_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m10_timestamp_action_table_name_idx ON test_cac.event_log_y2024m10 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2024m11_action_timestamp_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m11_action_timestamp_table_name_idx ON test_cac.event_log_y2024m11 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2024m11_table_name_action_timestamp_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m11_table_name_action_timestamp_idx ON test_cac.event_log_y2024m11 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2024m11_timestamp_action_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m11_timestamp_action_table_name_idx ON test_cac.event_log_y2024m11 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2024m12_action_timestamp_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m12_action_timestamp_table_name_idx ON test_cac.event_log_y2024m12 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2024m12_table_name_action_timestamp_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m12_table_name_action_timestamp_idx ON test_cac.event_log_y2024m12 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2024m12_timestamp_action_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2024m12_timestamp_action_table_name_idx ON test_cac.event_log_y2024m12 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2025m01_action_timestamp_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m01_action_timestamp_table_name_idx ON test_cac.event_log_y2025m01 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2025m01_table_name_action_timestamp_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m01_table_name_action_timestamp_idx ON test_cac.event_log_y2025m01 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2025m01_timestamp_action_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m01_timestamp_action_table_name_idx ON test_cac.event_log_y2025m01 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2025m02_action_timestamp_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m02_action_timestamp_table_name_idx ON test_cac.event_log_y2025m02 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2025m02_table_name_action_timestamp_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m02_table_name_action_timestamp_idx ON test_cac.event_log_y2025m02 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2025m02_timestamp_action_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m02_timestamp_action_table_name_idx ON test_cac.event_log_y2025m02 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2025m03_action_timestamp_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m03_action_timestamp_table_name_idx ON test_cac.event_log_y2025m03 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2025m03_table_name_action_timestamp_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m03_table_name_action_timestamp_idx ON test_cac.event_log_y2025m03 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2025m03_timestamp_action_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m03_timestamp_action_table_name_idx ON test_cac.event_log_y2025m03 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2025m04_action_timestamp_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m04_action_timestamp_table_name_idx ON test_cac.event_log_y2025m04 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2025m04_table_name_action_timestamp_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m04_table_name_action_timestamp_idx ON test_cac.event_log_y2025m04 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2025m04_timestamp_action_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m04_timestamp_action_table_name_idx ON test_cac.event_log_y2025m04 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2025m05_action_timestamp_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m05_action_timestamp_table_name_idx ON test_cac.event_log_y2025m05 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2025m05_table_name_action_timestamp_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m05_table_name_action_timestamp_idx ON test_cac.event_log_y2025m05 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2025m05_timestamp_action_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m05_timestamp_action_table_name_idx ON test_cac.event_log_y2025m05 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2025m06_action_timestamp_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m06_action_timestamp_table_name_idx ON test_cac.event_log_y2025m06 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2025m06_table_name_action_timestamp_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m06_table_name_action_timestamp_idx ON test_cac.event_log_y2025m06 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2025m06_timestamp_action_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m06_timestamp_action_table_name_idx ON test_cac.event_log_y2025m06 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2025m07_action_timestamp_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m07_action_timestamp_table_name_idx ON test_cac.event_log_y2025m07 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2025m07_table_name_action_timestamp_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m07_table_name_action_timestamp_idx ON test_cac.event_log_y2025m07 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2025m07_timestamp_action_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m07_timestamp_action_table_name_idx ON test_cac.event_log_y2025m07 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2025m08_action_timestamp_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m08_action_timestamp_table_name_idx ON test_cac.event_log_y2025m08 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2025m08_table_name_action_timestamp_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m08_table_name_action_timestamp_idx ON test_cac.event_log_y2025m08 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2025m08_timestamp_action_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m08_timestamp_action_table_name_idx ON test_cac.event_log_y2025m08 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2025m09_action_timestamp_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m09_action_timestamp_table_name_idx ON test_cac.event_log_y2025m09 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2025m09_table_name_action_timestamp_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m09_table_name_action_timestamp_idx ON test_cac.event_log_y2025m09 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2025m09_timestamp_action_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m09_timestamp_action_table_name_idx ON test_cac.event_log_y2025m09 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2025m10_action_timestamp_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m10_action_timestamp_table_name_idx ON test_cac.event_log_y2025m10 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2025m10_table_name_action_timestamp_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m10_table_name_action_timestamp_idx ON test_cac.event_log_y2025m10 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2025m10_timestamp_action_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m10_timestamp_action_table_name_idx ON test_cac.event_log_y2025m10 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2025m11_action_timestamp_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m11_action_timestamp_table_name_idx ON test_cac.event_log_y2025m11 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2025m11_table_name_action_timestamp_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m11_table_name_action_timestamp_idx ON test_cac.event_log_y2025m11 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2025m11_timestamp_action_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m11_timestamp_action_table_name_idx ON test_cac.event_log_y2025m11 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2025m12_action_timestamp_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m12_action_timestamp_table_name_idx ON test_cac.event_log_y2025m12 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2025m12_table_name_action_timestamp_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m12_table_name_action_timestamp_idx ON test_cac.event_log_y2025m12 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2025m12_timestamp_action_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2025m12_timestamp_action_table_name_idx ON test_cac.event_log_y2025m12 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2026m01_action_timestamp_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m01_action_timestamp_table_name_idx ON test_cac.event_log_y2026m01 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2026m01_table_name_action_timestamp_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m01_table_name_action_timestamp_idx ON test_cac.event_log_y2026m01 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2026m01_timestamp_action_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m01_timestamp_action_table_name_idx ON test_cac.event_log_y2026m01 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2026m02_action_timestamp_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m02_action_timestamp_table_name_idx ON test_cac.event_log_y2026m02 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2026m02_table_name_action_timestamp_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m02_table_name_action_timestamp_idx ON test_cac.event_log_y2026m02 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2026m02_timestamp_action_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m02_timestamp_action_table_name_idx ON test_cac.event_log_y2026m02 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2026m03_action_timestamp_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m03_action_timestamp_table_name_idx ON test_cac.event_log_y2026m03 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2026m03_table_name_action_timestamp_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m03_table_name_action_timestamp_idx ON test_cac.event_log_y2026m03 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2026m03_timestamp_action_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m03_timestamp_action_table_name_idx ON test_cac.event_log_y2026m03 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2026m04_action_timestamp_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m04_action_timestamp_table_name_idx ON test_cac.event_log_y2026m04 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2026m04_table_name_action_timestamp_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m04_table_name_action_timestamp_idx ON test_cac.event_log_y2026m04 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2026m04_timestamp_action_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m04_timestamp_action_table_name_idx ON test_cac.event_log_y2026m04 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2026m05_action_timestamp_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m05_action_timestamp_table_name_idx ON test_cac.event_log_y2026m05 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2026m05_table_name_action_timestamp_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m05_table_name_action_timestamp_idx ON test_cac.event_log_y2026m05 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2026m05_timestamp_action_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m05_timestamp_action_table_name_idx ON test_cac.event_log_y2026m05 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2026m06_action_timestamp_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m06_action_timestamp_table_name_idx ON test_cac.event_log_y2026m06 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2026m06_table_name_action_timestamp_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m06_table_name_action_timestamp_idx ON test_cac.event_log_y2026m06 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2026m06_timestamp_action_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m06_timestamp_action_table_name_idx ON test_cac.event_log_y2026m06 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2026m07_action_timestamp_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m07_action_timestamp_table_name_idx ON test_cac.event_log_y2026m07 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2026m07_table_name_action_timestamp_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m07_table_name_action_timestamp_idx ON test_cac.event_log_y2026m07 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2026m07_timestamp_action_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m07_timestamp_action_table_name_idx ON test_cac.event_log_y2026m07 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2026m08_action_timestamp_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m08_action_timestamp_table_name_idx ON test_cac.event_log_y2026m08 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2026m08_table_name_action_timestamp_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m08_table_name_action_timestamp_idx ON test_cac.event_log_y2026m08 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2026m08_timestamp_action_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m08_timestamp_action_table_name_idx ON test_cac.event_log_y2026m08 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2026m09_action_timestamp_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m09_action_timestamp_table_name_idx ON test_cac.event_log_y2026m09 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2026m09_table_name_action_timestamp_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m09_table_name_action_timestamp_idx ON test_cac.event_log_y2026m09 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2026m09_timestamp_action_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m09_timestamp_action_table_name_idx ON test_cac.event_log_y2026m09 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2026m10_action_timestamp_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m10_action_timestamp_table_name_idx ON test_cac.event_log_y2026m10 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2026m10_table_name_action_timestamp_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m10_table_name_action_timestamp_idx ON test_cac.event_log_y2026m10 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2026m10_timestamp_action_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m10_timestamp_action_table_name_idx ON test_cac.event_log_y2026m10 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2026m11_action_timestamp_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m11_action_timestamp_table_name_idx ON test_cac.event_log_y2026m11 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2026m11_table_name_action_timestamp_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m11_table_name_action_timestamp_idx ON test_cac.event_log_y2026m11 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2026m11_timestamp_action_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m11_timestamp_action_table_name_idx ON test_cac.event_log_y2026m11 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2026m12_action_timestamp_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m12_action_timestamp_table_name_idx ON test_cac.event_log_y2026m12 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2026m12_table_name_action_timestamp_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m12_table_name_action_timestamp_idx ON test_cac.event_log_y2026m12 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2026m12_timestamp_action_table_name_idx; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX event_log_y2026m12_timestamp_action_table_name_idx ON test_cac.event_log_y2026m12 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_action_index; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_action_index ON ONLY test_experimentation.event_log USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_table_name_index; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_table_name_index ON ONLY test_experimentation.event_log USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_timestamp_index; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_timestamp_index ON ONLY test_experimentation.event_log USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2023m08_action_timestamp_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2023m08_action_timestamp_table_name_idx ON test_experimentation.event_log_y2023m08 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2023m08_table_name_action_timestamp_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2023m08_table_name_action_timestamp_idx ON test_experimentation.event_log_y2023m08 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2023m08_timestamp_action_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2023m08_timestamp_action_table_name_idx ON test_experimentation.event_log_y2023m08 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2023m09_action_timestamp_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2023m09_action_timestamp_table_name_idx ON test_experimentation.event_log_y2023m09 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2023m09_table_name_action_timestamp_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2023m09_table_name_action_timestamp_idx ON test_experimentation.event_log_y2023m09 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2023m09_timestamp_action_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2023m09_timestamp_action_table_name_idx ON test_experimentation.event_log_y2023m09 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2023m10_action_timestamp_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2023m10_action_timestamp_table_name_idx ON test_experimentation.event_log_y2023m10 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2023m10_table_name_action_timestamp_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2023m10_table_name_action_timestamp_idx ON test_experimentation.event_log_y2023m10 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2023m10_timestamp_action_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2023m10_timestamp_action_table_name_idx ON test_experimentation.event_log_y2023m10 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2023m11_action_timestamp_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2023m11_action_timestamp_table_name_idx ON test_experimentation.event_log_y2023m11 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2023m11_table_name_action_timestamp_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2023m11_table_name_action_timestamp_idx ON test_experimentation.event_log_y2023m11 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2023m11_timestamp_action_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2023m11_timestamp_action_table_name_idx ON test_experimentation.event_log_y2023m11 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2023m12_action_timestamp_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2023m12_action_timestamp_table_name_idx ON test_experimentation.event_log_y2023m12 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2023m12_table_name_action_timestamp_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2023m12_table_name_action_timestamp_idx ON test_experimentation.event_log_y2023m12 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2023m12_timestamp_action_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2023m12_timestamp_action_table_name_idx ON test_experimentation.event_log_y2023m12 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2024m01_action_timestamp_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m01_action_timestamp_table_name_idx ON test_experimentation.event_log_y2024m01 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2024m01_table_name_action_timestamp_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m01_table_name_action_timestamp_idx ON test_experimentation.event_log_y2024m01 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2024m01_timestamp_action_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m01_timestamp_action_table_name_idx ON test_experimentation.event_log_y2024m01 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2024m02_action_timestamp_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m02_action_timestamp_table_name_idx ON test_experimentation.event_log_y2024m02 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2024m02_table_name_action_timestamp_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m02_table_name_action_timestamp_idx ON test_experimentation.event_log_y2024m02 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2024m02_timestamp_action_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m02_timestamp_action_table_name_idx ON test_experimentation.event_log_y2024m02 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2024m03_action_timestamp_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m03_action_timestamp_table_name_idx ON test_experimentation.event_log_y2024m03 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2024m03_table_name_action_timestamp_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m03_table_name_action_timestamp_idx ON test_experimentation.event_log_y2024m03 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2024m03_timestamp_action_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m03_timestamp_action_table_name_idx ON test_experimentation.event_log_y2024m03 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2024m04_action_timestamp_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m04_action_timestamp_table_name_idx ON test_experimentation.event_log_y2024m04 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2024m04_table_name_action_timestamp_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m04_table_name_action_timestamp_idx ON test_experimentation.event_log_y2024m04 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2024m04_timestamp_action_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m04_timestamp_action_table_name_idx ON test_experimentation.event_log_y2024m04 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2024m05_action_timestamp_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m05_action_timestamp_table_name_idx ON test_experimentation.event_log_y2024m05 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2024m05_table_name_action_timestamp_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m05_table_name_action_timestamp_idx ON test_experimentation.event_log_y2024m05 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2024m05_timestamp_action_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m05_timestamp_action_table_name_idx ON test_experimentation.event_log_y2024m05 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2024m06_action_timestamp_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m06_action_timestamp_table_name_idx ON test_experimentation.event_log_y2024m06 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2024m06_table_name_action_timestamp_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m06_table_name_action_timestamp_idx ON test_experimentation.event_log_y2024m06 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2024m06_timestamp_action_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m06_timestamp_action_table_name_idx ON test_experimentation.event_log_y2024m06 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2024m07_action_timestamp_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m07_action_timestamp_table_name_idx ON test_experimentation.event_log_y2024m07 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2024m07_table_name_action_timestamp_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m07_table_name_action_timestamp_idx ON test_experimentation.event_log_y2024m07 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2024m07_timestamp_action_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m07_timestamp_action_table_name_idx ON test_experimentation.event_log_y2024m07 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2024m08_action_timestamp_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m08_action_timestamp_table_name_idx ON test_experimentation.event_log_y2024m08 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2024m08_table_name_action_timestamp_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m08_table_name_action_timestamp_idx ON test_experimentation.event_log_y2024m08 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2024m08_timestamp_action_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m08_timestamp_action_table_name_idx ON test_experimentation.event_log_y2024m08 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2024m09_action_timestamp_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m09_action_timestamp_table_name_idx ON test_experimentation.event_log_y2024m09 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2024m09_table_name_action_timestamp_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m09_table_name_action_timestamp_idx ON test_experimentation.event_log_y2024m09 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2024m09_timestamp_action_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m09_timestamp_action_table_name_idx ON test_experimentation.event_log_y2024m09 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2024m10_action_timestamp_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m10_action_timestamp_table_name_idx ON test_experimentation.event_log_y2024m10 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2024m10_table_name_action_timestamp_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m10_table_name_action_timestamp_idx ON test_experimentation.event_log_y2024m10 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2024m10_timestamp_action_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m10_timestamp_action_table_name_idx ON test_experimentation.event_log_y2024m10 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2024m11_action_timestamp_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m11_action_timestamp_table_name_idx ON test_experimentation.event_log_y2024m11 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2024m11_table_name_action_timestamp_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m11_table_name_action_timestamp_idx ON test_experimentation.event_log_y2024m11 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2024m11_timestamp_action_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m11_timestamp_action_table_name_idx ON test_experimentation.event_log_y2024m11 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2024m12_action_timestamp_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m12_action_timestamp_table_name_idx ON test_experimentation.event_log_y2024m12 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2024m12_table_name_action_timestamp_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m12_table_name_action_timestamp_idx ON test_experimentation.event_log_y2024m12 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2024m12_timestamp_action_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2024m12_timestamp_action_table_name_idx ON test_experimentation.event_log_y2024m12 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2025m01_action_timestamp_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m01_action_timestamp_table_name_idx ON test_experimentation.event_log_y2025m01 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2025m01_table_name_action_timestamp_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m01_table_name_action_timestamp_idx ON test_experimentation.event_log_y2025m01 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2025m01_timestamp_action_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m01_timestamp_action_table_name_idx ON test_experimentation.event_log_y2025m01 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2025m02_action_timestamp_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m02_action_timestamp_table_name_idx ON test_experimentation.event_log_y2025m02 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2025m02_table_name_action_timestamp_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m02_table_name_action_timestamp_idx ON test_experimentation.event_log_y2025m02 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2025m02_timestamp_action_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m02_timestamp_action_table_name_idx ON test_experimentation.event_log_y2025m02 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2025m03_action_timestamp_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m03_action_timestamp_table_name_idx ON test_experimentation.event_log_y2025m03 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2025m03_table_name_action_timestamp_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m03_table_name_action_timestamp_idx ON test_experimentation.event_log_y2025m03 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2025m03_timestamp_action_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m03_timestamp_action_table_name_idx ON test_experimentation.event_log_y2025m03 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2025m04_action_timestamp_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m04_action_timestamp_table_name_idx ON test_experimentation.event_log_y2025m04 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2025m04_table_name_action_timestamp_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m04_table_name_action_timestamp_idx ON test_experimentation.event_log_y2025m04 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2025m04_timestamp_action_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m04_timestamp_action_table_name_idx ON test_experimentation.event_log_y2025m04 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2025m05_action_timestamp_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m05_action_timestamp_table_name_idx ON test_experimentation.event_log_y2025m05 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2025m05_table_name_action_timestamp_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m05_table_name_action_timestamp_idx ON test_experimentation.event_log_y2025m05 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2025m05_timestamp_action_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m05_timestamp_action_table_name_idx ON test_experimentation.event_log_y2025m05 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2025m06_action_timestamp_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m06_action_timestamp_table_name_idx ON test_experimentation.event_log_y2025m06 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2025m06_table_name_action_timestamp_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m06_table_name_action_timestamp_idx ON test_experimentation.event_log_y2025m06 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2025m06_timestamp_action_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m06_timestamp_action_table_name_idx ON test_experimentation.event_log_y2025m06 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2025m07_action_timestamp_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m07_action_timestamp_table_name_idx ON test_experimentation.event_log_y2025m07 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2025m07_table_name_action_timestamp_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m07_table_name_action_timestamp_idx ON test_experimentation.event_log_y2025m07 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2025m07_timestamp_action_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m07_timestamp_action_table_name_idx ON test_experimentation.event_log_y2025m07 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2025m08_action_timestamp_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m08_action_timestamp_table_name_idx ON test_experimentation.event_log_y2025m08 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2025m08_table_name_action_timestamp_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m08_table_name_action_timestamp_idx ON test_experimentation.event_log_y2025m08 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2025m08_timestamp_action_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m08_timestamp_action_table_name_idx ON test_experimentation.event_log_y2025m08 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2025m09_action_timestamp_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m09_action_timestamp_table_name_idx ON test_experimentation.event_log_y2025m09 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2025m09_table_name_action_timestamp_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m09_table_name_action_timestamp_idx ON test_experimentation.event_log_y2025m09 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2025m09_timestamp_action_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m09_timestamp_action_table_name_idx ON test_experimentation.event_log_y2025m09 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2025m10_action_timestamp_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m10_action_timestamp_table_name_idx ON test_experimentation.event_log_y2025m10 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2025m10_table_name_action_timestamp_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m10_table_name_action_timestamp_idx ON test_experimentation.event_log_y2025m10 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2025m10_timestamp_action_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m10_timestamp_action_table_name_idx ON test_experimentation.event_log_y2025m10 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2025m11_action_timestamp_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m11_action_timestamp_table_name_idx ON test_experimentation.event_log_y2025m11 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2025m11_table_name_action_timestamp_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m11_table_name_action_timestamp_idx ON test_experimentation.event_log_y2025m11 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2025m11_timestamp_action_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m11_timestamp_action_table_name_idx ON test_experimentation.event_log_y2025m11 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2025m12_action_timestamp_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m12_action_timestamp_table_name_idx ON test_experimentation.event_log_y2025m12 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2025m12_table_name_action_timestamp_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m12_table_name_action_timestamp_idx ON test_experimentation.event_log_y2025m12 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2025m12_timestamp_action_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2025m12_timestamp_action_table_name_idx ON test_experimentation.event_log_y2025m12 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2026m01_action_timestamp_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m01_action_timestamp_table_name_idx ON test_experimentation.event_log_y2026m01 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2026m01_table_name_action_timestamp_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m01_table_name_action_timestamp_idx ON test_experimentation.event_log_y2026m01 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2026m01_timestamp_action_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m01_timestamp_action_table_name_idx ON test_experimentation.event_log_y2026m01 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2026m02_action_timestamp_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m02_action_timestamp_table_name_idx ON test_experimentation.event_log_y2026m02 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2026m02_table_name_action_timestamp_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m02_table_name_action_timestamp_idx ON test_experimentation.event_log_y2026m02 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2026m02_timestamp_action_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m02_timestamp_action_table_name_idx ON test_experimentation.event_log_y2026m02 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2026m03_action_timestamp_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m03_action_timestamp_table_name_idx ON test_experimentation.event_log_y2026m03 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2026m03_table_name_action_timestamp_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m03_table_name_action_timestamp_idx ON test_experimentation.event_log_y2026m03 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2026m03_timestamp_action_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m03_timestamp_action_table_name_idx ON test_experimentation.event_log_y2026m03 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2026m04_action_timestamp_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m04_action_timestamp_table_name_idx ON test_experimentation.event_log_y2026m04 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2026m04_table_name_action_timestamp_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m04_table_name_action_timestamp_idx ON test_experimentation.event_log_y2026m04 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2026m04_timestamp_action_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m04_timestamp_action_table_name_idx ON test_experimentation.event_log_y2026m04 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2026m05_action_timestamp_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m05_action_timestamp_table_name_idx ON test_experimentation.event_log_y2026m05 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2026m05_table_name_action_timestamp_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m05_table_name_action_timestamp_idx ON test_experimentation.event_log_y2026m05 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2026m05_timestamp_action_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m05_timestamp_action_table_name_idx ON test_experimentation.event_log_y2026m05 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2026m06_action_timestamp_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m06_action_timestamp_table_name_idx ON test_experimentation.event_log_y2026m06 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2026m06_table_name_action_timestamp_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m06_table_name_action_timestamp_idx ON test_experimentation.event_log_y2026m06 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2026m06_timestamp_action_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m06_timestamp_action_table_name_idx ON test_experimentation.event_log_y2026m06 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2026m07_action_timestamp_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m07_action_timestamp_table_name_idx ON test_experimentation.event_log_y2026m07 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2026m07_table_name_action_timestamp_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m07_table_name_action_timestamp_idx ON test_experimentation.event_log_y2026m07 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2026m07_timestamp_action_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m07_timestamp_action_table_name_idx ON test_experimentation.event_log_y2026m07 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2026m08_action_timestamp_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m08_action_timestamp_table_name_idx ON test_experimentation.event_log_y2026m08 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2026m08_table_name_action_timestamp_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m08_table_name_action_timestamp_idx ON test_experimentation.event_log_y2026m08 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2026m08_timestamp_action_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m08_timestamp_action_table_name_idx ON test_experimentation.event_log_y2026m08 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2026m09_action_timestamp_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m09_action_timestamp_table_name_idx ON test_experimentation.event_log_y2026m09 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2026m09_table_name_action_timestamp_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m09_table_name_action_timestamp_idx ON test_experimentation.event_log_y2026m09 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2026m09_timestamp_action_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m09_timestamp_action_table_name_idx ON test_experimentation.event_log_y2026m09 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2026m10_action_timestamp_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m10_action_timestamp_table_name_idx ON test_experimentation.event_log_y2026m10 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2026m10_table_name_action_timestamp_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m10_table_name_action_timestamp_idx ON test_experimentation.event_log_y2026m10 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2026m10_timestamp_action_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m10_timestamp_action_table_name_idx ON test_experimentation.event_log_y2026m10 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2026m11_action_timestamp_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m11_action_timestamp_table_name_idx ON test_experimentation.event_log_y2026m11 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2026m11_table_name_action_timestamp_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m11_table_name_action_timestamp_idx ON test_experimentation.event_log_y2026m11 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2026m11_timestamp_action_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m11_timestamp_action_table_name_idx ON test_experimentation.event_log_y2026m11 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2026m12_action_timestamp_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m12_action_timestamp_table_name_idx ON test_experimentation.event_log_y2026m12 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2026m12_table_name_action_timestamp_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m12_table_name_action_timestamp_idx ON test_experimentation.event_log_y2026m12 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2026m12_timestamp_action_table_name_idx; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX event_log_y2026m12_timestamp_action_table_name_idx ON test_experimentation.event_log_y2026m12 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: experiment_created_date_index; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX experiment_created_date_index ON test_experimentation.experiments USING btree (created_at) INCLUDE (id);


--
-- Name: experiment_last_modified_index; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX experiment_last_modified_index ON test_experimentation.experiments USING btree (last_modified) INCLUDE (id, created_at);


--
-- Name: experiment_status_index; Type: INDEX; Schema: test_experimentation; Owner: postgres
--

CREATE INDEX experiment_status_index ON test_experimentation.experiments USING btree (status) INCLUDE (created_at, last_modified);


--
-- Name: event_log_y2023m08_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_action_index ATTACH PARTITION dev_cac.event_log_y2023m08_action_timestamp_table_name_idx;


--
-- Name: event_log_y2023m08_pkey; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_pkey ATTACH PARTITION dev_cac.event_log_y2023m08_pkey;


--
-- Name: event_log_y2023m08_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_table_name_index ATTACH PARTITION dev_cac.event_log_y2023m08_table_name_action_timestamp_idx;


--
-- Name: event_log_y2023m08_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_timestamp_index ATTACH PARTITION dev_cac.event_log_y2023m08_timestamp_action_table_name_idx;


--
-- Name: event_log_y2023m09_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_action_index ATTACH PARTITION dev_cac.event_log_y2023m09_action_timestamp_table_name_idx;


--
-- Name: event_log_y2023m09_pkey; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_pkey ATTACH PARTITION dev_cac.event_log_y2023m09_pkey;


--
-- Name: event_log_y2023m09_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_table_name_index ATTACH PARTITION dev_cac.event_log_y2023m09_table_name_action_timestamp_idx;


--
-- Name: event_log_y2023m09_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_timestamp_index ATTACH PARTITION dev_cac.event_log_y2023m09_timestamp_action_table_name_idx;


--
-- Name: event_log_y2023m10_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_action_index ATTACH PARTITION dev_cac.event_log_y2023m10_action_timestamp_table_name_idx;


--
-- Name: event_log_y2023m10_pkey; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_pkey ATTACH PARTITION dev_cac.event_log_y2023m10_pkey;


--
-- Name: event_log_y2023m10_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_table_name_index ATTACH PARTITION dev_cac.event_log_y2023m10_table_name_action_timestamp_idx;


--
-- Name: event_log_y2023m10_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_timestamp_index ATTACH PARTITION dev_cac.event_log_y2023m10_timestamp_action_table_name_idx;


--
-- Name: event_log_y2023m11_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_action_index ATTACH PARTITION dev_cac.event_log_y2023m11_action_timestamp_table_name_idx;


--
-- Name: event_log_y2023m11_pkey; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_pkey ATTACH PARTITION dev_cac.event_log_y2023m11_pkey;


--
-- Name: event_log_y2023m11_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_table_name_index ATTACH PARTITION dev_cac.event_log_y2023m11_table_name_action_timestamp_idx;


--
-- Name: event_log_y2023m11_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_timestamp_index ATTACH PARTITION dev_cac.event_log_y2023m11_timestamp_action_table_name_idx;


--
-- Name: event_log_y2023m12_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_action_index ATTACH PARTITION dev_cac.event_log_y2023m12_action_timestamp_table_name_idx;


--
-- Name: event_log_y2023m12_pkey; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_pkey ATTACH PARTITION dev_cac.event_log_y2023m12_pkey;


--
-- Name: event_log_y2023m12_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_table_name_index ATTACH PARTITION dev_cac.event_log_y2023m12_table_name_action_timestamp_idx;


--
-- Name: event_log_y2023m12_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_timestamp_index ATTACH PARTITION dev_cac.event_log_y2023m12_timestamp_action_table_name_idx;


--
-- Name: event_log_y2024m01_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_action_index ATTACH PARTITION dev_cac.event_log_y2024m01_action_timestamp_table_name_idx;


--
-- Name: event_log_y2024m01_pkey; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_pkey ATTACH PARTITION dev_cac.event_log_y2024m01_pkey;


--
-- Name: event_log_y2024m01_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_table_name_index ATTACH PARTITION dev_cac.event_log_y2024m01_table_name_action_timestamp_idx;


--
-- Name: event_log_y2024m01_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_timestamp_index ATTACH PARTITION dev_cac.event_log_y2024m01_timestamp_action_table_name_idx;


--
-- Name: event_log_y2024m02_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_action_index ATTACH PARTITION dev_cac.event_log_y2024m02_action_timestamp_table_name_idx;


--
-- Name: event_log_y2024m02_pkey; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_pkey ATTACH PARTITION dev_cac.event_log_y2024m02_pkey;


--
-- Name: event_log_y2024m02_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_table_name_index ATTACH PARTITION dev_cac.event_log_y2024m02_table_name_action_timestamp_idx;


--
-- Name: event_log_y2024m02_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_timestamp_index ATTACH PARTITION dev_cac.event_log_y2024m02_timestamp_action_table_name_idx;


--
-- Name: event_log_y2024m03_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_action_index ATTACH PARTITION dev_cac.event_log_y2024m03_action_timestamp_table_name_idx;


--
-- Name: event_log_y2024m03_pkey; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_pkey ATTACH PARTITION dev_cac.event_log_y2024m03_pkey;


--
-- Name: event_log_y2024m03_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_table_name_index ATTACH PARTITION dev_cac.event_log_y2024m03_table_name_action_timestamp_idx;


--
-- Name: event_log_y2024m03_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_timestamp_index ATTACH PARTITION dev_cac.event_log_y2024m03_timestamp_action_table_name_idx;


--
-- Name: event_log_y2024m04_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_action_index ATTACH PARTITION dev_cac.event_log_y2024m04_action_timestamp_table_name_idx;


--
-- Name: event_log_y2024m04_pkey; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_pkey ATTACH PARTITION dev_cac.event_log_y2024m04_pkey;


--
-- Name: event_log_y2024m04_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_table_name_index ATTACH PARTITION dev_cac.event_log_y2024m04_table_name_action_timestamp_idx;


--
-- Name: event_log_y2024m04_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_timestamp_index ATTACH PARTITION dev_cac.event_log_y2024m04_timestamp_action_table_name_idx;


--
-- Name: event_log_y2024m05_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_action_index ATTACH PARTITION dev_cac.event_log_y2024m05_action_timestamp_table_name_idx;


--
-- Name: event_log_y2024m05_pkey; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_pkey ATTACH PARTITION dev_cac.event_log_y2024m05_pkey;


--
-- Name: event_log_y2024m05_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_table_name_index ATTACH PARTITION dev_cac.event_log_y2024m05_table_name_action_timestamp_idx;


--
-- Name: event_log_y2024m05_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_timestamp_index ATTACH PARTITION dev_cac.event_log_y2024m05_timestamp_action_table_name_idx;


--
-- Name: event_log_y2024m06_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_action_index ATTACH PARTITION dev_cac.event_log_y2024m06_action_timestamp_table_name_idx;


--
-- Name: event_log_y2024m06_pkey; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_pkey ATTACH PARTITION dev_cac.event_log_y2024m06_pkey;


--
-- Name: event_log_y2024m06_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_table_name_index ATTACH PARTITION dev_cac.event_log_y2024m06_table_name_action_timestamp_idx;


--
-- Name: event_log_y2024m06_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_timestamp_index ATTACH PARTITION dev_cac.event_log_y2024m06_timestamp_action_table_name_idx;


--
-- Name: event_log_y2024m07_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_action_index ATTACH PARTITION dev_cac.event_log_y2024m07_action_timestamp_table_name_idx;


--
-- Name: event_log_y2024m07_pkey; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_pkey ATTACH PARTITION dev_cac.event_log_y2024m07_pkey;


--
-- Name: event_log_y2024m07_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_table_name_index ATTACH PARTITION dev_cac.event_log_y2024m07_table_name_action_timestamp_idx;


--
-- Name: event_log_y2024m07_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_timestamp_index ATTACH PARTITION dev_cac.event_log_y2024m07_timestamp_action_table_name_idx;


--
-- Name: event_log_y2024m08_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_action_index ATTACH PARTITION dev_cac.event_log_y2024m08_action_timestamp_table_name_idx;


--
-- Name: event_log_y2024m08_pkey; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_pkey ATTACH PARTITION dev_cac.event_log_y2024m08_pkey;


--
-- Name: event_log_y2024m08_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_table_name_index ATTACH PARTITION dev_cac.event_log_y2024m08_table_name_action_timestamp_idx;


--
-- Name: event_log_y2024m08_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_timestamp_index ATTACH PARTITION dev_cac.event_log_y2024m08_timestamp_action_table_name_idx;


--
-- Name: event_log_y2024m09_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_action_index ATTACH PARTITION dev_cac.event_log_y2024m09_action_timestamp_table_name_idx;


--
-- Name: event_log_y2024m09_pkey; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_pkey ATTACH PARTITION dev_cac.event_log_y2024m09_pkey;


--
-- Name: event_log_y2024m09_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_table_name_index ATTACH PARTITION dev_cac.event_log_y2024m09_table_name_action_timestamp_idx;


--
-- Name: event_log_y2024m09_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_timestamp_index ATTACH PARTITION dev_cac.event_log_y2024m09_timestamp_action_table_name_idx;


--
-- Name: event_log_y2024m10_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_action_index ATTACH PARTITION dev_cac.event_log_y2024m10_action_timestamp_table_name_idx;


--
-- Name: event_log_y2024m10_pkey; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_pkey ATTACH PARTITION dev_cac.event_log_y2024m10_pkey;


--
-- Name: event_log_y2024m10_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_table_name_index ATTACH PARTITION dev_cac.event_log_y2024m10_table_name_action_timestamp_idx;


--
-- Name: event_log_y2024m10_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_timestamp_index ATTACH PARTITION dev_cac.event_log_y2024m10_timestamp_action_table_name_idx;


--
-- Name: event_log_y2024m11_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_action_index ATTACH PARTITION dev_cac.event_log_y2024m11_action_timestamp_table_name_idx;


--
-- Name: event_log_y2024m11_pkey; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_pkey ATTACH PARTITION dev_cac.event_log_y2024m11_pkey;


--
-- Name: event_log_y2024m11_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_table_name_index ATTACH PARTITION dev_cac.event_log_y2024m11_table_name_action_timestamp_idx;


--
-- Name: event_log_y2024m11_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_timestamp_index ATTACH PARTITION dev_cac.event_log_y2024m11_timestamp_action_table_name_idx;


--
-- Name: event_log_y2024m12_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_action_index ATTACH PARTITION dev_cac.event_log_y2024m12_action_timestamp_table_name_idx;


--
-- Name: event_log_y2024m12_pkey; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_pkey ATTACH PARTITION dev_cac.event_log_y2024m12_pkey;


--
-- Name: event_log_y2024m12_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_table_name_index ATTACH PARTITION dev_cac.event_log_y2024m12_table_name_action_timestamp_idx;


--
-- Name: event_log_y2024m12_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_timestamp_index ATTACH PARTITION dev_cac.event_log_y2024m12_timestamp_action_table_name_idx;


--
-- Name: event_log_y2025m01_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_action_index ATTACH PARTITION dev_cac.event_log_y2025m01_action_timestamp_table_name_idx;


--
-- Name: event_log_y2025m01_pkey; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_pkey ATTACH PARTITION dev_cac.event_log_y2025m01_pkey;


--
-- Name: event_log_y2025m01_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_table_name_index ATTACH PARTITION dev_cac.event_log_y2025m01_table_name_action_timestamp_idx;


--
-- Name: event_log_y2025m01_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_timestamp_index ATTACH PARTITION dev_cac.event_log_y2025m01_timestamp_action_table_name_idx;


--
-- Name: event_log_y2025m02_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_action_index ATTACH PARTITION dev_cac.event_log_y2025m02_action_timestamp_table_name_idx;


--
-- Name: event_log_y2025m02_pkey; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_pkey ATTACH PARTITION dev_cac.event_log_y2025m02_pkey;


--
-- Name: event_log_y2025m02_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_table_name_index ATTACH PARTITION dev_cac.event_log_y2025m02_table_name_action_timestamp_idx;


--
-- Name: event_log_y2025m02_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_timestamp_index ATTACH PARTITION dev_cac.event_log_y2025m02_timestamp_action_table_name_idx;


--
-- Name: event_log_y2025m03_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_action_index ATTACH PARTITION dev_cac.event_log_y2025m03_action_timestamp_table_name_idx;


--
-- Name: event_log_y2025m03_pkey; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_pkey ATTACH PARTITION dev_cac.event_log_y2025m03_pkey;


--
-- Name: event_log_y2025m03_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_table_name_index ATTACH PARTITION dev_cac.event_log_y2025m03_table_name_action_timestamp_idx;


--
-- Name: event_log_y2025m03_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_timestamp_index ATTACH PARTITION dev_cac.event_log_y2025m03_timestamp_action_table_name_idx;


--
-- Name: event_log_y2025m04_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_action_index ATTACH PARTITION dev_cac.event_log_y2025m04_action_timestamp_table_name_idx;


--
-- Name: event_log_y2025m04_pkey; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_pkey ATTACH PARTITION dev_cac.event_log_y2025m04_pkey;


--
-- Name: event_log_y2025m04_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_table_name_index ATTACH PARTITION dev_cac.event_log_y2025m04_table_name_action_timestamp_idx;


--
-- Name: event_log_y2025m04_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_timestamp_index ATTACH PARTITION dev_cac.event_log_y2025m04_timestamp_action_table_name_idx;


--
-- Name: event_log_y2025m05_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_action_index ATTACH PARTITION dev_cac.event_log_y2025m05_action_timestamp_table_name_idx;


--
-- Name: event_log_y2025m05_pkey; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_pkey ATTACH PARTITION dev_cac.event_log_y2025m05_pkey;


--
-- Name: event_log_y2025m05_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_table_name_index ATTACH PARTITION dev_cac.event_log_y2025m05_table_name_action_timestamp_idx;


--
-- Name: event_log_y2025m05_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_timestamp_index ATTACH PARTITION dev_cac.event_log_y2025m05_timestamp_action_table_name_idx;


--
-- Name: event_log_y2025m06_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_action_index ATTACH PARTITION dev_cac.event_log_y2025m06_action_timestamp_table_name_idx;


--
-- Name: event_log_y2025m06_pkey; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_pkey ATTACH PARTITION dev_cac.event_log_y2025m06_pkey;


--
-- Name: event_log_y2025m06_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_table_name_index ATTACH PARTITION dev_cac.event_log_y2025m06_table_name_action_timestamp_idx;


--
-- Name: event_log_y2025m06_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_timestamp_index ATTACH PARTITION dev_cac.event_log_y2025m06_timestamp_action_table_name_idx;


--
-- Name: event_log_y2025m07_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_action_index ATTACH PARTITION dev_cac.event_log_y2025m07_action_timestamp_table_name_idx;


--
-- Name: event_log_y2025m07_pkey; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_pkey ATTACH PARTITION dev_cac.event_log_y2025m07_pkey;


--
-- Name: event_log_y2025m07_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_table_name_index ATTACH PARTITION dev_cac.event_log_y2025m07_table_name_action_timestamp_idx;


--
-- Name: event_log_y2025m07_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_timestamp_index ATTACH PARTITION dev_cac.event_log_y2025m07_timestamp_action_table_name_idx;


--
-- Name: event_log_y2025m08_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_action_index ATTACH PARTITION dev_cac.event_log_y2025m08_action_timestamp_table_name_idx;


--
-- Name: event_log_y2025m08_pkey; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_pkey ATTACH PARTITION dev_cac.event_log_y2025m08_pkey;


--
-- Name: event_log_y2025m08_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_table_name_index ATTACH PARTITION dev_cac.event_log_y2025m08_table_name_action_timestamp_idx;


--
-- Name: event_log_y2025m08_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_timestamp_index ATTACH PARTITION dev_cac.event_log_y2025m08_timestamp_action_table_name_idx;


--
-- Name: event_log_y2025m09_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_action_index ATTACH PARTITION dev_cac.event_log_y2025m09_action_timestamp_table_name_idx;


--
-- Name: event_log_y2025m09_pkey; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_pkey ATTACH PARTITION dev_cac.event_log_y2025m09_pkey;


--
-- Name: event_log_y2025m09_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_table_name_index ATTACH PARTITION dev_cac.event_log_y2025m09_table_name_action_timestamp_idx;


--
-- Name: event_log_y2025m09_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_timestamp_index ATTACH PARTITION dev_cac.event_log_y2025m09_timestamp_action_table_name_idx;


--
-- Name: event_log_y2025m10_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_action_index ATTACH PARTITION dev_cac.event_log_y2025m10_action_timestamp_table_name_idx;


--
-- Name: event_log_y2025m10_pkey; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_pkey ATTACH PARTITION dev_cac.event_log_y2025m10_pkey;


--
-- Name: event_log_y2025m10_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_table_name_index ATTACH PARTITION dev_cac.event_log_y2025m10_table_name_action_timestamp_idx;


--
-- Name: event_log_y2025m10_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_timestamp_index ATTACH PARTITION dev_cac.event_log_y2025m10_timestamp_action_table_name_idx;


--
-- Name: event_log_y2025m11_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_action_index ATTACH PARTITION dev_cac.event_log_y2025m11_action_timestamp_table_name_idx;


--
-- Name: event_log_y2025m11_pkey; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_pkey ATTACH PARTITION dev_cac.event_log_y2025m11_pkey;


--
-- Name: event_log_y2025m11_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_table_name_index ATTACH PARTITION dev_cac.event_log_y2025m11_table_name_action_timestamp_idx;


--
-- Name: event_log_y2025m11_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_timestamp_index ATTACH PARTITION dev_cac.event_log_y2025m11_timestamp_action_table_name_idx;


--
-- Name: event_log_y2025m12_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_action_index ATTACH PARTITION dev_cac.event_log_y2025m12_action_timestamp_table_name_idx;


--
-- Name: event_log_y2025m12_pkey; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_pkey ATTACH PARTITION dev_cac.event_log_y2025m12_pkey;


--
-- Name: event_log_y2025m12_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_table_name_index ATTACH PARTITION dev_cac.event_log_y2025m12_table_name_action_timestamp_idx;


--
-- Name: event_log_y2025m12_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_timestamp_index ATTACH PARTITION dev_cac.event_log_y2025m12_timestamp_action_table_name_idx;


--
-- Name: event_log_y2026m01_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_action_index ATTACH PARTITION dev_cac.event_log_y2026m01_action_timestamp_table_name_idx;


--
-- Name: event_log_y2026m01_pkey; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_pkey ATTACH PARTITION dev_cac.event_log_y2026m01_pkey;


--
-- Name: event_log_y2026m01_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_table_name_index ATTACH PARTITION dev_cac.event_log_y2026m01_table_name_action_timestamp_idx;


--
-- Name: event_log_y2026m01_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_timestamp_index ATTACH PARTITION dev_cac.event_log_y2026m01_timestamp_action_table_name_idx;


--
-- Name: event_log_y2026m02_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_action_index ATTACH PARTITION dev_cac.event_log_y2026m02_action_timestamp_table_name_idx;


--
-- Name: event_log_y2026m02_pkey; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_pkey ATTACH PARTITION dev_cac.event_log_y2026m02_pkey;


--
-- Name: event_log_y2026m02_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_table_name_index ATTACH PARTITION dev_cac.event_log_y2026m02_table_name_action_timestamp_idx;


--
-- Name: event_log_y2026m02_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_timestamp_index ATTACH PARTITION dev_cac.event_log_y2026m02_timestamp_action_table_name_idx;


--
-- Name: event_log_y2026m03_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_action_index ATTACH PARTITION dev_cac.event_log_y2026m03_action_timestamp_table_name_idx;


--
-- Name: event_log_y2026m03_pkey; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_pkey ATTACH PARTITION dev_cac.event_log_y2026m03_pkey;


--
-- Name: event_log_y2026m03_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_table_name_index ATTACH PARTITION dev_cac.event_log_y2026m03_table_name_action_timestamp_idx;


--
-- Name: event_log_y2026m03_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_timestamp_index ATTACH PARTITION dev_cac.event_log_y2026m03_timestamp_action_table_name_idx;


--
-- Name: event_log_y2026m04_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_action_index ATTACH PARTITION dev_cac.event_log_y2026m04_action_timestamp_table_name_idx;


--
-- Name: event_log_y2026m04_pkey; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_pkey ATTACH PARTITION dev_cac.event_log_y2026m04_pkey;


--
-- Name: event_log_y2026m04_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_table_name_index ATTACH PARTITION dev_cac.event_log_y2026m04_table_name_action_timestamp_idx;


--
-- Name: event_log_y2026m04_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_timestamp_index ATTACH PARTITION dev_cac.event_log_y2026m04_timestamp_action_table_name_idx;


--
-- Name: event_log_y2026m05_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_action_index ATTACH PARTITION dev_cac.event_log_y2026m05_action_timestamp_table_name_idx;


--
-- Name: event_log_y2026m05_pkey; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_pkey ATTACH PARTITION dev_cac.event_log_y2026m05_pkey;


--
-- Name: event_log_y2026m05_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_table_name_index ATTACH PARTITION dev_cac.event_log_y2026m05_table_name_action_timestamp_idx;


--
-- Name: event_log_y2026m05_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_timestamp_index ATTACH PARTITION dev_cac.event_log_y2026m05_timestamp_action_table_name_idx;


--
-- Name: event_log_y2026m06_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_action_index ATTACH PARTITION dev_cac.event_log_y2026m06_action_timestamp_table_name_idx;


--
-- Name: event_log_y2026m06_pkey; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_pkey ATTACH PARTITION dev_cac.event_log_y2026m06_pkey;


--
-- Name: event_log_y2026m06_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_table_name_index ATTACH PARTITION dev_cac.event_log_y2026m06_table_name_action_timestamp_idx;


--
-- Name: event_log_y2026m06_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_timestamp_index ATTACH PARTITION dev_cac.event_log_y2026m06_timestamp_action_table_name_idx;


--
-- Name: event_log_y2026m07_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_action_index ATTACH PARTITION dev_cac.event_log_y2026m07_action_timestamp_table_name_idx;


--
-- Name: event_log_y2026m07_pkey; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_pkey ATTACH PARTITION dev_cac.event_log_y2026m07_pkey;


--
-- Name: event_log_y2026m07_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_table_name_index ATTACH PARTITION dev_cac.event_log_y2026m07_table_name_action_timestamp_idx;


--
-- Name: event_log_y2026m07_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_timestamp_index ATTACH PARTITION dev_cac.event_log_y2026m07_timestamp_action_table_name_idx;


--
-- Name: event_log_y2026m08_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_action_index ATTACH PARTITION dev_cac.event_log_y2026m08_action_timestamp_table_name_idx;


--
-- Name: event_log_y2026m08_pkey; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_pkey ATTACH PARTITION dev_cac.event_log_y2026m08_pkey;


--
-- Name: event_log_y2026m08_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_table_name_index ATTACH PARTITION dev_cac.event_log_y2026m08_table_name_action_timestamp_idx;


--
-- Name: event_log_y2026m08_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_timestamp_index ATTACH PARTITION dev_cac.event_log_y2026m08_timestamp_action_table_name_idx;


--
-- Name: event_log_y2026m09_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_action_index ATTACH PARTITION dev_cac.event_log_y2026m09_action_timestamp_table_name_idx;


--
-- Name: event_log_y2026m09_pkey; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_pkey ATTACH PARTITION dev_cac.event_log_y2026m09_pkey;


--
-- Name: event_log_y2026m09_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_table_name_index ATTACH PARTITION dev_cac.event_log_y2026m09_table_name_action_timestamp_idx;


--
-- Name: event_log_y2026m09_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_timestamp_index ATTACH PARTITION dev_cac.event_log_y2026m09_timestamp_action_table_name_idx;


--
-- Name: event_log_y2026m10_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_action_index ATTACH PARTITION dev_cac.event_log_y2026m10_action_timestamp_table_name_idx;


--
-- Name: event_log_y2026m10_pkey; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_pkey ATTACH PARTITION dev_cac.event_log_y2026m10_pkey;


--
-- Name: event_log_y2026m10_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_table_name_index ATTACH PARTITION dev_cac.event_log_y2026m10_table_name_action_timestamp_idx;


--
-- Name: event_log_y2026m10_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_timestamp_index ATTACH PARTITION dev_cac.event_log_y2026m10_timestamp_action_table_name_idx;


--
-- Name: event_log_y2026m11_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_action_index ATTACH PARTITION dev_cac.event_log_y2026m11_action_timestamp_table_name_idx;


--
-- Name: event_log_y2026m11_pkey; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_pkey ATTACH PARTITION dev_cac.event_log_y2026m11_pkey;


--
-- Name: event_log_y2026m11_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_table_name_index ATTACH PARTITION dev_cac.event_log_y2026m11_table_name_action_timestamp_idx;


--
-- Name: event_log_y2026m11_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_timestamp_index ATTACH PARTITION dev_cac.event_log_y2026m11_timestamp_action_table_name_idx;


--
-- Name: event_log_y2026m12_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_action_index ATTACH PARTITION dev_cac.event_log_y2026m12_action_timestamp_table_name_idx;


--
-- Name: event_log_y2026m12_pkey; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_pkey ATTACH PARTITION dev_cac.event_log_y2026m12_pkey;


--
-- Name: event_log_y2026m12_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_table_name_index ATTACH PARTITION dev_cac.event_log_y2026m12_table_name_action_timestamp_idx;


--
-- Name: event_log_y2026m12_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_cac; Owner: postgres
--

ALTER INDEX dev_cac.event_log_timestamp_index ATTACH PARTITION dev_cac.event_log_y2026m12_timestamp_action_table_name_idx;


--
-- Name: event_log_y2023m08_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_action_index ATTACH PARTITION dev_experimentation.event_log_y2023m08_action_timestamp_table_name_idx;


--
-- Name: event_log_y2023m08_pkey; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_pkey ATTACH PARTITION dev_experimentation.event_log_y2023m08_pkey;


--
-- Name: event_log_y2023m08_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_table_name_index ATTACH PARTITION dev_experimentation.event_log_y2023m08_table_name_action_timestamp_idx;


--
-- Name: event_log_y2023m08_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_timestamp_index ATTACH PARTITION dev_experimentation.event_log_y2023m08_timestamp_action_table_name_idx;


--
-- Name: event_log_y2023m09_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_action_index ATTACH PARTITION dev_experimentation.event_log_y2023m09_action_timestamp_table_name_idx;


--
-- Name: event_log_y2023m09_pkey; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_pkey ATTACH PARTITION dev_experimentation.event_log_y2023m09_pkey;


--
-- Name: event_log_y2023m09_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_table_name_index ATTACH PARTITION dev_experimentation.event_log_y2023m09_table_name_action_timestamp_idx;


--
-- Name: event_log_y2023m09_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_timestamp_index ATTACH PARTITION dev_experimentation.event_log_y2023m09_timestamp_action_table_name_idx;


--
-- Name: event_log_y2023m10_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_action_index ATTACH PARTITION dev_experimentation.event_log_y2023m10_action_timestamp_table_name_idx;


--
-- Name: event_log_y2023m10_pkey; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_pkey ATTACH PARTITION dev_experimentation.event_log_y2023m10_pkey;


--
-- Name: event_log_y2023m10_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_table_name_index ATTACH PARTITION dev_experimentation.event_log_y2023m10_table_name_action_timestamp_idx;


--
-- Name: event_log_y2023m10_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_timestamp_index ATTACH PARTITION dev_experimentation.event_log_y2023m10_timestamp_action_table_name_idx;


--
-- Name: event_log_y2023m11_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_action_index ATTACH PARTITION dev_experimentation.event_log_y2023m11_action_timestamp_table_name_idx;


--
-- Name: event_log_y2023m11_pkey; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_pkey ATTACH PARTITION dev_experimentation.event_log_y2023m11_pkey;


--
-- Name: event_log_y2023m11_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_table_name_index ATTACH PARTITION dev_experimentation.event_log_y2023m11_table_name_action_timestamp_idx;


--
-- Name: event_log_y2023m11_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_timestamp_index ATTACH PARTITION dev_experimentation.event_log_y2023m11_timestamp_action_table_name_idx;


--
-- Name: event_log_y2023m12_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_action_index ATTACH PARTITION dev_experimentation.event_log_y2023m12_action_timestamp_table_name_idx;


--
-- Name: event_log_y2023m12_pkey; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_pkey ATTACH PARTITION dev_experimentation.event_log_y2023m12_pkey;


--
-- Name: event_log_y2023m12_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_table_name_index ATTACH PARTITION dev_experimentation.event_log_y2023m12_table_name_action_timestamp_idx;


--
-- Name: event_log_y2023m12_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_timestamp_index ATTACH PARTITION dev_experimentation.event_log_y2023m12_timestamp_action_table_name_idx;


--
-- Name: event_log_y2024m01_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_action_index ATTACH PARTITION dev_experimentation.event_log_y2024m01_action_timestamp_table_name_idx;


--
-- Name: event_log_y2024m01_pkey; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_pkey ATTACH PARTITION dev_experimentation.event_log_y2024m01_pkey;


--
-- Name: event_log_y2024m01_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_table_name_index ATTACH PARTITION dev_experimentation.event_log_y2024m01_table_name_action_timestamp_idx;


--
-- Name: event_log_y2024m01_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_timestamp_index ATTACH PARTITION dev_experimentation.event_log_y2024m01_timestamp_action_table_name_idx;


--
-- Name: event_log_y2024m02_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_action_index ATTACH PARTITION dev_experimentation.event_log_y2024m02_action_timestamp_table_name_idx;


--
-- Name: event_log_y2024m02_pkey; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_pkey ATTACH PARTITION dev_experimentation.event_log_y2024m02_pkey;


--
-- Name: event_log_y2024m02_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_table_name_index ATTACH PARTITION dev_experimentation.event_log_y2024m02_table_name_action_timestamp_idx;


--
-- Name: event_log_y2024m02_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_timestamp_index ATTACH PARTITION dev_experimentation.event_log_y2024m02_timestamp_action_table_name_idx;


--
-- Name: event_log_y2024m03_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_action_index ATTACH PARTITION dev_experimentation.event_log_y2024m03_action_timestamp_table_name_idx;


--
-- Name: event_log_y2024m03_pkey; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_pkey ATTACH PARTITION dev_experimentation.event_log_y2024m03_pkey;


--
-- Name: event_log_y2024m03_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_table_name_index ATTACH PARTITION dev_experimentation.event_log_y2024m03_table_name_action_timestamp_idx;


--
-- Name: event_log_y2024m03_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_timestamp_index ATTACH PARTITION dev_experimentation.event_log_y2024m03_timestamp_action_table_name_idx;


--
-- Name: event_log_y2024m04_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_action_index ATTACH PARTITION dev_experimentation.event_log_y2024m04_action_timestamp_table_name_idx;


--
-- Name: event_log_y2024m04_pkey; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_pkey ATTACH PARTITION dev_experimentation.event_log_y2024m04_pkey;


--
-- Name: event_log_y2024m04_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_table_name_index ATTACH PARTITION dev_experimentation.event_log_y2024m04_table_name_action_timestamp_idx;


--
-- Name: event_log_y2024m04_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_timestamp_index ATTACH PARTITION dev_experimentation.event_log_y2024m04_timestamp_action_table_name_idx;


--
-- Name: event_log_y2024m05_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_action_index ATTACH PARTITION dev_experimentation.event_log_y2024m05_action_timestamp_table_name_idx;


--
-- Name: event_log_y2024m05_pkey; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_pkey ATTACH PARTITION dev_experimentation.event_log_y2024m05_pkey;


--
-- Name: event_log_y2024m05_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_table_name_index ATTACH PARTITION dev_experimentation.event_log_y2024m05_table_name_action_timestamp_idx;


--
-- Name: event_log_y2024m05_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_timestamp_index ATTACH PARTITION dev_experimentation.event_log_y2024m05_timestamp_action_table_name_idx;


--
-- Name: event_log_y2024m06_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_action_index ATTACH PARTITION dev_experimentation.event_log_y2024m06_action_timestamp_table_name_idx;


--
-- Name: event_log_y2024m06_pkey; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_pkey ATTACH PARTITION dev_experimentation.event_log_y2024m06_pkey;


--
-- Name: event_log_y2024m06_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_table_name_index ATTACH PARTITION dev_experimentation.event_log_y2024m06_table_name_action_timestamp_idx;


--
-- Name: event_log_y2024m06_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_timestamp_index ATTACH PARTITION dev_experimentation.event_log_y2024m06_timestamp_action_table_name_idx;


--
-- Name: event_log_y2024m07_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_action_index ATTACH PARTITION dev_experimentation.event_log_y2024m07_action_timestamp_table_name_idx;


--
-- Name: event_log_y2024m07_pkey; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_pkey ATTACH PARTITION dev_experimentation.event_log_y2024m07_pkey;


--
-- Name: event_log_y2024m07_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_table_name_index ATTACH PARTITION dev_experimentation.event_log_y2024m07_table_name_action_timestamp_idx;


--
-- Name: event_log_y2024m07_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_timestamp_index ATTACH PARTITION dev_experimentation.event_log_y2024m07_timestamp_action_table_name_idx;


--
-- Name: event_log_y2024m08_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_action_index ATTACH PARTITION dev_experimentation.event_log_y2024m08_action_timestamp_table_name_idx;


--
-- Name: event_log_y2024m08_pkey; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_pkey ATTACH PARTITION dev_experimentation.event_log_y2024m08_pkey;


--
-- Name: event_log_y2024m08_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_table_name_index ATTACH PARTITION dev_experimentation.event_log_y2024m08_table_name_action_timestamp_idx;


--
-- Name: event_log_y2024m08_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_timestamp_index ATTACH PARTITION dev_experimentation.event_log_y2024m08_timestamp_action_table_name_idx;


--
-- Name: event_log_y2024m09_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_action_index ATTACH PARTITION dev_experimentation.event_log_y2024m09_action_timestamp_table_name_idx;


--
-- Name: event_log_y2024m09_pkey; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_pkey ATTACH PARTITION dev_experimentation.event_log_y2024m09_pkey;


--
-- Name: event_log_y2024m09_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_table_name_index ATTACH PARTITION dev_experimentation.event_log_y2024m09_table_name_action_timestamp_idx;


--
-- Name: event_log_y2024m09_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_timestamp_index ATTACH PARTITION dev_experimentation.event_log_y2024m09_timestamp_action_table_name_idx;


--
-- Name: event_log_y2024m10_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_action_index ATTACH PARTITION dev_experimentation.event_log_y2024m10_action_timestamp_table_name_idx;


--
-- Name: event_log_y2024m10_pkey; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_pkey ATTACH PARTITION dev_experimentation.event_log_y2024m10_pkey;


--
-- Name: event_log_y2024m10_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_table_name_index ATTACH PARTITION dev_experimentation.event_log_y2024m10_table_name_action_timestamp_idx;


--
-- Name: event_log_y2024m10_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_timestamp_index ATTACH PARTITION dev_experimentation.event_log_y2024m10_timestamp_action_table_name_idx;


--
-- Name: event_log_y2024m11_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_action_index ATTACH PARTITION dev_experimentation.event_log_y2024m11_action_timestamp_table_name_idx;


--
-- Name: event_log_y2024m11_pkey; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_pkey ATTACH PARTITION dev_experimentation.event_log_y2024m11_pkey;


--
-- Name: event_log_y2024m11_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_table_name_index ATTACH PARTITION dev_experimentation.event_log_y2024m11_table_name_action_timestamp_idx;


--
-- Name: event_log_y2024m11_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_timestamp_index ATTACH PARTITION dev_experimentation.event_log_y2024m11_timestamp_action_table_name_idx;


--
-- Name: event_log_y2024m12_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_action_index ATTACH PARTITION dev_experimentation.event_log_y2024m12_action_timestamp_table_name_idx;


--
-- Name: event_log_y2024m12_pkey; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_pkey ATTACH PARTITION dev_experimentation.event_log_y2024m12_pkey;


--
-- Name: event_log_y2024m12_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_table_name_index ATTACH PARTITION dev_experimentation.event_log_y2024m12_table_name_action_timestamp_idx;


--
-- Name: event_log_y2024m12_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_timestamp_index ATTACH PARTITION dev_experimentation.event_log_y2024m12_timestamp_action_table_name_idx;


--
-- Name: event_log_y2025m01_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_action_index ATTACH PARTITION dev_experimentation.event_log_y2025m01_action_timestamp_table_name_idx;


--
-- Name: event_log_y2025m01_pkey; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_pkey ATTACH PARTITION dev_experimentation.event_log_y2025m01_pkey;


--
-- Name: event_log_y2025m01_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_table_name_index ATTACH PARTITION dev_experimentation.event_log_y2025m01_table_name_action_timestamp_idx;


--
-- Name: event_log_y2025m01_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_timestamp_index ATTACH PARTITION dev_experimentation.event_log_y2025m01_timestamp_action_table_name_idx;


--
-- Name: event_log_y2025m02_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_action_index ATTACH PARTITION dev_experimentation.event_log_y2025m02_action_timestamp_table_name_idx;


--
-- Name: event_log_y2025m02_pkey; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_pkey ATTACH PARTITION dev_experimentation.event_log_y2025m02_pkey;


--
-- Name: event_log_y2025m02_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_table_name_index ATTACH PARTITION dev_experimentation.event_log_y2025m02_table_name_action_timestamp_idx;


--
-- Name: event_log_y2025m02_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_timestamp_index ATTACH PARTITION dev_experimentation.event_log_y2025m02_timestamp_action_table_name_idx;


--
-- Name: event_log_y2025m03_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_action_index ATTACH PARTITION dev_experimentation.event_log_y2025m03_action_timestamp_table_name_idx;


--
-- Name: event_log_y2025m03_pkey; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_pkey ATTACH PARTITION dev_experimentation.event_log_y2025m03_pkey;


--
-- Name: event_log_y2025m03_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_table_name_index ATTACH PARTITION dev_experimentation.event_log_y2025m03_table_name_action_timestamp_idx;


--
-- Name: event_log_y2025m03_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_timestamp_index ATTACH PARTITION dev_experimentation.event_log_y2025m03_timestamp_action_table_name_idx;


--
-- Name: event_log_y2025m04_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_action_index ATTACH PARTITION dev_experimentation.event_log_y2025m04_action_timestamp_table_name_idx;


--
-- Name: event_log_y2025m04_pkey; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_pkey ATTACH PARTITION dev_experimentation.event_log_y2025m04_pkey;


--
-- Name: event_log_y2025m04_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_table_name_index ATTACH PARTITION dev_experimentation.event_log_y2025m04_table_name_action_timestamp_idx;


--
-- Name: event_log_y2025m04_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_timestamp_index ATTACH PARTITION dev_experimentation.event_log_y2025m04_timestamp_action_table_name_idx;


--
-- Name: event_log_y2025m05_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_action_index ATTACH PARTITION dev_experimentation.event_log_y2025m05_action_timestamp_table_name_idx;


--
-- Name: event_log_y2025m05_pkey; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_pkey ATTACH PARTITION dev_experimentation.event_log_y2025m05_pkey;


--
-- Name: event_log_y2025m05_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_table_name_index ATTACH PARTITION dev_experimentation.event_log_y2025m05_table_name_action_timestamp_idx;


--
-- Name: event_log_y2025m05_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_timestamp_index ATTACH PARTITION dev_experimentation.event_log_y2025m05_timestamp_action_table_name_idx;


--
-- Name: event_log_y2025m06_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_action_index ATTACH PARTITION dev_experimentation.event_log_y2025m06_action_timestamp_table_name_idx;


--
-- Name: event_log_y2025m06_pkey; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_pkey ATTACH PARTITION dev_experimentation.event_log_y2025m06_pkey;


--
-- Name: event_log_y2025m06_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_table_name_index ATTACH PARTITION dev_experimentation.event_log_y2025m06_table_name_action_timestamp_idx;


--
-- Name: event_log_y2025m06_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_timestamp_index ATTACH PARTITION dev_experimentation.event_log_y2025m06_timestamp_action_table_name_idx;


--
-- Name: event_log_y2025m07_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_action_index ATTACH PARTITION dev_experimentation.event_log_y2025m07_action_timestamp_table_name_idx;


--
-- Name: event_log_y2025m07_pkey; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_pkey ATTACH PARTITION dev_experimentation.event_log_y2025m07_pkey;


--
-- Name: event_log_y2025m07_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_table_name_index ATTACH PARTITION dev_experimentation.event_log_y2025m07_table_name_action_timestamp_idx;


--
-- Name: event_log_y2025m07_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_timestamp_index ATTACH PARTITION dev_experimentation.event_log_y2025m07_timestamp_action_table_name_idx;


--
-- Name: event_log_y2025m08_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_action_index ATTACH PARTITION dev_experimentation.event_log_y2025m08_action_timestamp_table_name_idx;


--
-- Name: event_log_y2025m08_pkey; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_pkey ATTACH PARTITION dev_experimentation.event_log_y2025m08_pkey;


--
-- Name: event_log_y2025m08_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_table_name_index ATTACH PARTITION dev_experimentation.event_log_y2025m08_table_name_action_timestamp_idx;


--
-- Name: event_log_y2025m08_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_timestamp_index ATTACH PARTITION dev_experimentation.event_log_y2025m08_timestamp_action_table_name_idx;


--
-- Name: event_log_y2025m09_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_action_index ATTACH PARTITION dev_experimentation.event_log_y2025m09_action_timestamp_table_name_idx;


--
-- Name: event_log_y2025m09_pkey; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_pkey ATTACH PARTITION dev_experimentation.event_log_y2025m09_pkey;


--
-- Name: event_log_y2025m09_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_table_name_index ATTACH PARTITION dev_experimentation.event_log_y2025m09_table_name_action_timestamp_idx;


--
-- Name: event_log_y2025m09_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_timestamp_index ATTACH PARTITION dev_experimentation.event_log_y2025m09_timestamp_action_table_name_idx;


--
-- Name: event_log_y2025m10_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_action_index ATTACH PARTITION dev_experimentation.event_log_y2025m10_action_timestamp_table_name_idx;


--
-- Name: event_log_y2025m10_pkey; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_pkey ATTACH PARTITION dev_experimentation.event_log_y2025m10_pkey;


--
-- Name: event_log_y2025m10_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_table_name_index ATTACH PARTITION dev_experimentation.event_log_y2025m10_table_name_action_timestamp_idx;


--
-- Name: event_log_y2025m10_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_timestamp_index ATTACH PARTITION dev_experimentation.event_log_y2025m10_timestamp_action_table_name_idx;


--
-- Name: event_log_y2025m11_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_action_index ATTACH PARTITION dev_experimentation.event_log_y2025m11_action_timestamp_table_name_idx;


--
-- Name: event_log_y2025m11_pkey; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_pkey ATTACH PARTITION dev_experimentation.event_log_y2025m11_pkey;


--
-- Name: event_log_y2025m11_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_table_name_index ATTACH PARTITION dev_experimentation.event_log_y2025m11_table_name_action_timestamp_idx;


--
-- Name: event_log_y2025m11_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_timestamp_index ATTACH PARTITION dev_experimentation.event_log_y2025m11_timestamp_action_table_name_idx;


--
-- Name: event_log_y2025m12_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_action_index ATTACH PARTITION dev_experimentation.event_log_y2025m12_action_timestamp_table_name_idx;


--
-- Name: event_log_y2025m12_pkey; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_pkey ATTACH PARTITION dev_experimentation.event_log_y2025m12_pkey;


--
-- Name: event_log_y2025m12_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_table_name_index ATTACH PARTITION dev_experimentation.event_log_y2025m12_table_name_action_timestamp_idx;


--
-- Name: event_log_y2025m12_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_timestamp_index ATTACH PARTITION dev_experimentation.event_log_y2025m12_timestamp_action_table_name_idx;


--
-- Name: event_log_y2026m01_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_action_index ATTACH PARTITION dev_experimentation.event_log_y2026m01_action_timestamp_table_name_idx;


--
-- Name: event_log_y2026m01_pkey; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_pkey ATTACH PARTITION dev_experimentation.event_log_y2026m01_pkey;


--
-- Name: event_log_y2026m01_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_table_name_index ATTACH PARTITION dev_experimentation.event_log_y2026m01_table_name_action_timestamp_idx;


--
-- Name: event_log_y2026m01_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_timestamp_index ATTACH PARTITION dev_experimentation.event_log_y2026m01_timestamp_action_table_name_idx;


--
-- Name: event_log_y2026m02_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_action_index ATTACH PARTITION dev_experimentation.event_log_y2026m02_action_timestamp_table_name_idx;


--
-- Name: event_log_y2026m02_pkey; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_pkey ATTACH PARTITION dev_experimentation.event_log_y2026m02_pkey;


--
-- Name: event_log_y2026m02_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_table_name_index ATTACH PARTITION dev_experimentation.event_log_y2026m02_table_name_action_timestamp_idx;


--
-- Name: event_log_y2026m02_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_timestamp_index ATTACH PARTITION dev_experimentation.event_log_y2026m02_timestamp_action_table_name_idx;


--
-- Name: event_log_y2026m03_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_action_index ATTACH PARTITION dev_experimentation.event_log_y2026m03_action_timestamp_table_name_idx;


--
-- Name: event_log_y2026m03_pkey; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_pkey ATTACH PARTITION dev_experimentation.event_log_y2026m03_pkey;


--
-- Name: event_log_y2026m03_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_table_name_index ATTACH PARTITION dev_experimentation.event_log_y2026m03_table_name_action_timestamp_idx;


--
-- Name: event_log_y2026m03_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_timestamp_index ATTACH PARTITION dev_experimentation.event_log_y2026m03_timestamp_action_table_name_idx;


--
-- Name: event_log_y2026m04_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_action_index ATTACH PARTITION dev_experimentation.event_log_y2026m04_action_timestamp_table_name_idx;


--
-- Name: event_log_y2026m04_pkey; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_pkey ATTACH PARTITION dev_experimentation.event_log_y2026m04_pkey;


--
-- Name: event_log_y2026m04_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_table_name_index ATTACH PARTITION dev_experimentation.event_log_y2026m04_table_name_action_timestamp_idx;


--
-- Name: event_log_y2026m04_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_timestamp_index ATTACH PARTITION dev_experimentation.event_log_y2026m04_timestamp_action_table_name_idx;


--
-- Name: event_log_y2026m05_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_action_index ATTACH PARTITION dev_experimentation.event_log_y2026m05_action_timestamp_table_name_idx;


--
-- Name: event_log_y2026m05_pkey; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_pkey ATTACH PARTITION dev_experimentation.event_log_y2026m05_pkey;


--
-- Name: event_log_y2026m05_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_table_name_index ATTACH PARTITION dev_experimentation.event_log_y2026m05_table_name_action_timestamp_idx;


--
-- Name: event_log_y2026m05_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_timestamp_index ATTACH PARTITION dev_experimentation.event_log_y2026m05_timestamp_action_table_name_idx;


--
-- Name: event_log_y2026m06_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_action_index ATTACH PARTITION dev_experimentation.event_log_y2026m06_action_timestamp_table_name_idx;


--
-- Name: event_log_y2026m06_pkey; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_pkey ATTACH PARTITION dev_experimentation.event_log_y2026m06_pkey;


--
-- Name: event_log_y2026m06_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_table_name_index ATTACH PARTITION dev_experimentation.event_log_y2026m06_table_name_action_timestamp_idx;


--
-- Name: event_log_y2026m06_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_timestamp_index ATTACH PARTITION dev_experimentation.event_log_y2026m06_timestamp_action_table_name_idx;


--
-- Name: event_log_y2026m07_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_action_index ATTACH PARTITION dev_experimentation.event_log_y2026m07_action_timestamp_table_name_idx;


--
-- Name: event_log_y2026m07_pkey; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_pkey ATTACH PARTITION dev_experimentation.event_log_y2026m07_pkey;


--
-- Name: event_log_y2026m07_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_table_name_index ATTACH PARTITION dev_experimentation.event_log_y2026m07_table_name_action_timestamp_idx;


--
-- Name: event_log_y2026m07_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_timestamp_index ATTACH PARTITION dev_experimentation.event_log_y2026m07_timestamp_action_table_name_idx;


--
-- Name: event_log_y2026m08_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_action_index ATTACH PARTITION dev_experimentation.event_log_y2026m08_action_timestamp_table_name_idx;


--
-- Name: event_log_y2026m08_pkey; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_pkey ATTACH PARTITION dev_experimentation.event_log_y2026m08_pkey;


--
-- Name: event_log_y2026m08_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_table_name_index ATTACH PARTITION dev_experimentation.event_log_y2026m08_table_name_action_timestamp_idx;


--
-- Name: event_log_y2026m08_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_timestamp_index ATTACH PARTITION dev_experimentation.event_log_y2026m08_timestamp_action_table_name_idx;


--
-- Name: event_log_y2026m09_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_action_index ATTACH PARTITION dev_experimentation.event_log_y2026m09_action_timestamp_table_name_idx;


--
-- Name: event_log_y2026m09_pkey; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_pkey ATTACH PARTITION dev_experimentation.event_log_y2026m09_pkey;


--
-- Name: event_log_y2026m09_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_table_name_index ATTACH PARTITION dev_experimentation.event_log_y2026m09_table_name_action_timestamp_idx;


--
-- Name: event_log_y2026m09_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_timestamp_index ATTACH PARTITION dev_experimentation.event_log_y2026m09_timestamp_action_table_name_idx;


--
-- Name: event_log_y2026m10_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_action_index ATTACH PARTITION dev_experimentation.event_log_y2026m10_action_timestamp_table_name_idx;


--
-- Name: event_log_y2026m10_pkey; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_pkey ATTACH PARTITION dev_experimentation.event_log_y2026m10_pkey;


--
-- Name: event_log_y2026m10_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_table_name_index ATTACH PARTITION dev_experimentation.event_log_y2026m10_table_name_action_timestamp_idx;


--
-- Name: event_log_y2026m10_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_timestamp_index ATTACH PARTITION dev_experimentation.event_log_y2026m10_timestamp_action_table_name_idx;


--
-- Name: event_log_y2026m11_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_action_index ATTACH PARTITION dev_experimentation.event_log_y2026m11_action_timestamp_table_name_idx;


--
-- Name: event_log_y2026m11_pkey; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_pkey ATTACH PARTITION dev_experimentation.event_log_y2026m11_pkey;


--
-- Name: event_log_y2026m11_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_table_name_index ATTACH PARTITION dev_experimentation.event_log_y2026m11_table_name_action_timestamp_idx;


--
-- Name: event_log_y2026m11_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_timestamp_index ATTACH PARTITION dev_experimentation.event_log_y2026m11_timestamp_action_table_name_idx;


--
-- Name: event_log_y2026m12_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_action_index ATTACH PARTITION dev_experimentation.event_log_y2026m12_action_timestamp_table_name_idx;


--
-- Name: event_log_y2026m12_pkey; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_pkey ATTACH PARTITION dev_experimentation.event_log_y2026m12_pkey;


--
-- Name: event_log_y2026m12_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_table_name_index ATTACH PARTITION dev_experimentation.event_log_y2026m12_table_name_action_timestamp_idx;


--
-- Name: event_log_y2026m12_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: dev_experimentation; Owner: postgres
--

ALTER INDEX dev_experimentation.event_log_timestamp_index ATTACH PARTITION dev_experimentation.event_log_y2026m12_timestamp_action_table_name_idx;


--
-- Name: event_log_y2023m08_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_action_index ATTACH PARTITION test_cac.event_log_y2023m08_action_timestamp_table_name_idx;


--
-- Name: event_log_y2023m08_pkey; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_pkey ATTACH PARTITION test_cac.event_log_y2023m08_pkey;


--
-- Name: event_log_y2023m08_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_table_name_index ATTACH PARTITION test_cac.event_log_y2023m08_table_name_action_timestamp_idx;


--
-- Name: event_log_y2023m08_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_timestamp_index ATTACH PARTITION test_cac.event_log_y2023m08_timestamp_action_table_name_idx;


--
-- Name: event_log_y2023m09_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_action_index ATTACH PARTITION test_cac.event_log_y2023m09_action_timestamp_table_name_idx;


--
-- Name: event_log_y2023m09_pkey; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_pkey ATTACH PARTITION test_cac.event_log_y2023m09_pkey;


--
-- Name: event_log_y2023m09_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_table_name_index ATTACH PARTITION test_cac.event_log_y2023m09_table_name_action_timestamp_idx;


--
-- Name: event_log_y2023m09_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_timestamp_index ATTACH PARTITION test_cac.event_log_y2023m09_timestamp_action_table_name_idx;


--
-- Name: event_log_y2023m10_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_action_index ATTACH PARTITION test_cac.event_log_y2023m10_action_timestamp_table_name_idx;


--
-- Name: event_log_y2023m10_pkey; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_pkey ATTACH PARTITION test_cac.event_log_y2023m10_pkey;


--
-- Name: event_log_y2023m10_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_table_name_index ATTACH PARTITION test_cac.event_log_y2023m10_table_name_action_timestamp_idx;


--
-- Name: event_log_y2023m10_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_timestamp_index ATTACH PARTITION test_cac.event_log_y2023m10_timestamp_action_table_name_idx;


--
-- Name: event_log_y2023m11_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_action_index ATTACH PARTITION test_cac.event_log_y2023m11_action_timestamp_table_name_idx;


--
-- Name: event_log_y2023m11_pkey; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_pkey ATTACH PARTITION test_cac.event_log_y2023m11_pkey;


--
-- Name: event_log_y2023m11_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_table_name_index ATTACH PARTITION test_cac.event_log_y2023m11_table_name_action_timestamp_idx;


--
-- Name: event_log_y2023m11_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_timestamp_index ATTACH PARTITION test_cac.event_log_y2023m11_timestamp_action_table_name_idx;


--
-- Name: event_log_y2023m12_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_action_index ATTACH PARTITION test_cac.event_log_y2023m12_action_timestamp_table_name_idx;


--
-- Name: event_log_y2023m12_pkey; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_pkey ATTACH PARTITION test_cac.event_log_y2023m12_pkey;


--
-- Name: event_log_y2023m12_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_table_name_index ATTACH PARTITION test_cac.event_log_y2023m12_table_name_action_timestamp_idx;


--
-- Name: event_log_y2023m12_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_timestamp_index ATTACH PARTITION test_cac.event_log_y2023m12_timestamp_action_table_name_idx;


--
-- Name: event_log_y2024m01_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_action_index ATTACH PARTITION test_cac.event_log_y2024m01_action_timestamp_table_name_idx;


--
-- Name: event_log_y2024m01_pkey; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_pkey ATTACH PARTITION test_cac.event_log_y2024m01_pkey;


--
-- Name: event_log_y2024m01_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_table_name_index ATTACH PARTITION test_cac.event_log_y2024m01_table_name_action_timestamp_idx;


--
-- Name: event_log_y2024m01_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_timestamp_index ATTACH PARTITION test_cac.event_log_y2024m01_timestamp_action_table_name_idx;


--
-- Name: event_log_y2024m02_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_action_index ATTACH PARTITION test_cac.event_log_y2024m02_action_timestamp_table_name_idx;


--
-- Name: event_log_y2024m02_pkey; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_pkey ATTACH PARTITION test_cac.event_log_y2024m02_pkey;


--
-- Name: event_log_y2024m02_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_table_name_index ATTACH PARTITION test_cac.event_log_y2024m02_table_name_action_timestamp_idx;


--
-- Name: event_log_y2024m02_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_timestamp_index ATTACH PARTITION test_cac.event_log_y2024m02_timestamp_action_table_name_idx;


--
-- Name: event_log_y2024m03_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_action_index ATTACH PARTITION test_cac.event_log_y2024m03_action_timestamp_table_name_idx;


--
-- Name: event_log_y2024m03_pkey; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_pkey ATTACH PARTITION test_cac.event_log_y2024m03_pkey;


--
-- Name: event_log_y2024m03_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_table_name_index ATTACH PARTITION test_cac.event_log_y2024m03_table_name_action_timestamp_idx;


--
-- Name: event_log_y2024m03_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_timestamp_index ATTACH PARTITION test_cac.event_log_y2024m03_timestamp_action_table_name_idx;


--
-- Name: event_log_y2024m04_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_action_index ATTACH PARTITION test_cac.event_log_y2024m04_action_timestamp_table_name_idx;


--
-- Name: event_log_y2024m04_pkey; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_pkey ATTACH PARTITION test_cac.event_log_y2024m04_pkey;


--
-- Name: event_log_y2024m04_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_table_name_index ATTACH PARTITION test_cac.event_log_y2024m04_table_name_action_timestamp_idx;


--
-- Name: event_log_y2024m04_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_timestamp_index ATTACH PARTITION test_cac.event_log_y2024m04_timestamp_action_table_name_idx;


--
-- Name: event_log_y2024m05_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_action_index ATTACH PARTITION test_cac.event_log_y2024m05_action_timestamp_table_name_idx;


--
-- Name: event_log_y2024m05_pkey; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_pkey ATTACH PARTITION test_cac.event_log_y2024m05_pkey;


--
-- Name: event_log_y2024m05_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_table_name_index ATTACH PARTITION test_cac.event_log_y2024m05_table_name_action_timestamp_idx;


--
-- Name: event_log_y2024m05_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_timestamp_index ATTACH PARTITION test_cac.event_log_y2024m05_timestamp_action_table_name_idx;


--
-- Name: event_log_y2024m06_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_action_index ATTACH PARTITION test_cac.event_log_y2024m06_action_timestamp_table_name_idx;


--
-- Name: event_log_y2024m06_pkey; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_pkey ATTACH PARTITION test_cac.event_log_y2024m06_pkey;


--
-- Name: event_log_y2024m06_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_table_name_index ATTACH PARTITION test_cac.event_log_y2024m06_table_name_action_timestamp_idx;


--
-- Name: event_log_y2024m06_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_timestamp_index ATTACH PARTITION test_cac.event_log_y2024m06_timestamp_action_table_name_idx;


--
-- Name: event_log_y2024m07_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_action_index ATTACH PARTITION test_cac.event_log_y2024m07_action_timestamp_table_name_idx;


--
-- Name: event_log_y2024m07_pkey; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_pkey ATTACH PARTITION test_cac.event_log_y2024m07_pkey;


--
-- Name: event_log_y2024m07_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_table_name_index ATTACH PARTITION test_cac.event_log_y2024m07_table_name_action_timestamp_idx;


--
-- Name: event_log_y2024m07_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_timestamp_index ATTACH PARTITION test_cac.event_log_y2024m07_timestamp_action_table_name_idx;


--
-- Name: event_log_y2024m08_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_action_index ATTACH PARTITION test_cac.event_log_y2024m08_action_timestamp_table_name_idx;


--
-- Name: event_log_y2024m08_pkey; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_pkey ATTACH PARTITION test_cac.event_log_y2024m08_pkey;


--
-- Name: event_log_y2024m08_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_table_name_index ATTACH PARTITION test_cac.event_log_y2024m08_table_name_action_timestamp_idx;


--
-- Name: event_log_y2024m08_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_timestamp_index ATTACH PARTITION test_cac.event_log_y2024m08_timestamp_action_table_name_idx;


--
-- Name: event_log_y2024m09_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_action_index ATTACH PARTITION test_cac.event_log_y2024m09_action_timestamp_table_name_idx;


--
-- Name: event_log_y2024m09_pkey; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_pkey ATTACH PARTITION test_cac.event_log_y2024m09_pkey;


--
-- Name: event_log_y2024m09_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_table_name_index ATTACH PARTITION test_cac.event_log_y2024m09_table_name_action_timestamp_idx;


--
-- Name: event_log_y2024m09_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_timestamp_index ATTACH PARTITION test_cac.event_log_y2024m09_timestamp_action_table_name_idx;


--
-- Name: event_log_y2024m10_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_action_index ATTACH PARTITION test_cac.event_log_y2024m10_action_timestamp_table_name_idx;


--
-- Name: event_log_y2024m10_pkey; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_pkey ATTACH PARTITION test_cac.event_log_y2024m10_pkey;


--
-- Name: event_log_y2024m10_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_table_name_index ATTACH PARTITION test_cac.event_log_y2024m10_table_name_action_timestamp_idx;


--
-- Name: event_log_y2024m10_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_timestamp_index ATTACH PARTITION test_cac.event_log_y2024m10_timestamp_action_table_name_idx;


--
-- Name: event_log_y2024m11_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_action_index ATTACH PARTITION test_cac.event_log_y2024m11_action_timestamp_table_name_idx;


--
-- Name: event_log_y2024m11_pkey; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_pkey ATTACH PARTITION test_cac.event_log_y2024m11_pkey;


--
-- Name: event_log_y2024m11_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_table_name_index ATTACH PARTITION test_cac.event_log_y2024m11_table_name_action_timestamp_idx;


--
-- Name: event_log_y2024m11_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_timestamp_index ATTACH PARTITION test_cac.event_log_y2024m11_timestamp_action_table_name_idx;


--
-- Name: event_log_y2024m12_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_action_index ATTACH PARTITION test_cac.event_log_y2024m12_action_timestamp_table_name_idx;


--
-- Name: event_log_y2024m12_pkey; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_pkey ATTACH PARTITION test_cac.event_log_y2024m12_pkey;


--
-- Name: event_log_y2024m12_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_table_name_index ATTACH PARTITION test_cac.event_log_y2024m12_table_name_action_timestamp_idx;


--
-- Name: event_log_y2024m12_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_timestamp_index ATTACH PARTITION test_cac.event_log_y2024m12_timestamp_action_table_name_idx;


--
-- Name: event_log_y2025m01_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_action_index ATTACH PARTITION test_cac.event_log_y2025m01_action_timestamp_table_name_idx;


--
-- Name: event_log_y2025m01_pkey; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_pkey ATTACH PARTITION test_cac.event_log_y2025m01_pkey;


--
-- Name: event_log_y2025m01_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_table_name_index ATTACH PARTITION test_cac.event_log_y2025m01_table_name_action_timestamp_idx;


--
-- Name: event_log_y2025m01_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_timestamp_index ATTACH PARTITION test_cac.event_log_y2025m01_timestamp_action_table_name_idx;


--
-- Name: event_log_y2025m02_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_action_index ATTACH PARTITION test_cac.event_log_y2025m02_action_timestamp_table_name_idx;


--
-- Name: event_log_y2025m02_pkey; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_pkey ATTACH PARTITION test_cac.event_log_y2025m02_pkey;


--
-- Name: event_log_y2025m02_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_table_name_index ATTACH PARTITION test_cac.event_log_y2025m02_table_name_action_timestamp_idx;


--
-- Name: event_log_y2025m02_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_timestamp_index ATTACH PARTITION test_cac.event_log_y2025m02_timestamp_action_table_name_idx;


--
-- Name: event_log_y2025m03_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_action_index ATTACH PARTITION test_cac.event_log_y2025m03_action_timestamp_table_name_idx;


--
-- Name: event_log_y2025m03_pkey; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_pkey ATTACH PARTITION test_cac.event_log_y2025m03_pkey;


--
-- Name: event_log_y2025m03_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_table_name_index ATTACH PARTITION test_cac.event_log_y2025m03_table_name_action_timestamp_idx;


--
-- Name: event_log_y2025m03_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_timestamp_index ATTACH PARTITION test_cac.event_log_y2025m03_timestamp_action_table_name_idx;


--
-- Name: event_log_y2025m04_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_action_index ATTACH PARTITION test_cac.event_log_y2025m04_action_timestamp_table_name_idx;


--
-- Name: event_log_y2025m04_pkey; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_pkey ATTACH PARTITION test_cac.event_log_y2025m04_pkey;


--
-- Name: event_log_y2025m04_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_table_name_index ATTACH PARTITION test_cac.event_log_y2025m04_table_name_action_timestamp_idx;


--
-- Name: event_log_y2025m04_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_timestamp_index ATTACH PARTITION test_cac.event_log_y2025m04_timestamp_action_table_name_idx;


--
-- Name: event_log_y2025m05_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_action_index ATTACH PARTITION test_cac.event_log_y2025m05_action_timestamp_table_name_idx;


--
-- Name: event_log_y2025m05_pkey; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_pkey ATTACH PARTITION test_cac.event_log_y2025m05_pkey;


--
-- Name: event_log_y2025m05_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_table_name_index ATTACH PARTITION test_cac.event_log_y2025m05_table_name_action_timestamp_idx;


--
-- Name: event_log_y2025m05_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_timestamp_index ATTACH PARTITION test_cac.event_log_y2025m05_timestamp_action_table_name_idx;


--
-- Name: event_log_y2025m06_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_action_index ATTACH PARTITION test_cac.event_log_y2025m06_action_timestamp_table_name_idx;


--
-- Name: event_log_y2025m06_pkey; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_pkey ATTACH PARTITION test_cac.event_log_y2025m06_pkey;


--
-- Name: event_log_y2025m06_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_table_name_index ATTACH PARTITION test_cac.event_log_y2025m06_table_name_action_timestamp_idx;


--
-- Name: event_log_y2025m06_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_timestamp_index ATTACH PARTITION test_cac.event_log_y2025m06_timestamp_action_table_name_idx;


--
-- Name: event_log_y2025m07_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_action_index ATTACH PARTITION test_cac.event_log_y2025m07_action_timestamp_table_name_idx;


--
-- Name: event_log_y2025m07_pkey; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_pkey ATTACH PARTITION test_cac.event_log_y2025m07_pkey;


--
-- Name: event_log_y2025m07_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_table_name_index ATTACH PARTITION test_cac.event_log_y2025m07_table_name_action_timestamp_idx;


--
-- Name: event_log_y2025m07_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_timestamp_index ATTACH PARTITION test_cac.event_log_y2025m07_timestamp_action_table_name_idx;


--
-- Name: event_log_y2025m08_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_action_index ATTACH PARTITION test_cac.event_log_y2025m08_action_timestamp_table_name_idx;


--
-- Name: event_log_y2025m08_pkey; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_pkey ATTACH PARTITION test_cac.event_log_y2025m08_pkey;


--
-- Name: event_log_y2025m08_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_table_name_index ATTACH PARTITION test_cac.event_log_y2025m08_table_name_action_timestamp_idx;


--
-- Name: event_log_y2025m08_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_timestamp_index ATTACH PARTITION test_cac.event_log_y2025m08_timestamp_action_table_name_idx;


--
-- Name: event_log_y2025m09_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_action_index ATTACH PARTITION test_cac.event_log_y2025m09_action_timestamp_table_name_idx;


--
-- Name: event_log_y2025m09_pkey; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_pkey ATTACH PARTITION test_cac.event_log_y2025m09_pkey;


--
-- Name: event_log_y2025m09_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_table_name_index ATTACH PARTITION test_cac.event_log_y2025m09_table_name_action_timestamp_idx;


--
-- Name: event_log_y2025m09_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_timestamp_index ATTACH PARTITION test_cac.event_log_y2025m09_timestamp_action_table_name_idx;


--
-- Name: event_log_y2025m10_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_action_index ATTACH PARTITION test_cac.event_log_y2025m10_action_timestamp_table_name_idx;


--
-- Name: event_log_y2025m10_pkey; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_pkey ATTACH PARTITION test_cac.event_log_y2025m10_pkey;


--
-- Name: event_log_y2025m10_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_table_name_index ATTACH PARTITION test_cac.event_log_y2025m10_table_name_action_timestamp_idx;


--
-- Name: event_log_y2025m10_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_timestamp_index ATTACH PARTITION test_cac.event_log_y2025m10_timestamp_action_table_name_idx;


--
-- Name: event_log_y2025m11_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_action_index ATTACH PARTITION test_cac.event_log_y2025m11_action_timestamp_table_name_idx;


--
-- Name: event_log_y2025m11_pkey; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_pkey ATTACH PARTITION test_cac.event_log_y2025m11_pkey;


--
-- Name: event_log_y2025m11_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_table_name_index ATTACH PARTITION test_cac.event_log_y2025m11_table_name_action_timestamp_idx;


--
-- Name: event_log_y2025m11_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_timestamp_index ATTACH PARTITION test_cac.event_log_y2025m11_timestamp_action_table_name_idx;


--
-- Name: event_log_y2025m12_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_action_index ATTACH PARTITION test_cac.event_log_y2025m12_action_timestamp_table_name_idx;


--
-- Name: event_log_y2025m12_pkey; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_pkey ATTACH PARTITION test_cac.event_log_y2025m12_pkey;


--
-- Name: event_log_y2025m12_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_table_name_index ATTACH PARTITION test_cac.event_log_y2025m12_table_name_action_timestamp_idx;


--
-- Name: event_log_y2025m12_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_timestamp_index ATTACH PARTITION test_cac.event_log_y2025m12_timestamp_action_table_name_idx;


--
-- Name: event_log_y2026m01_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_action_index ATTACH PARTITION test_cac.event_log_y2026m01_action_timestamp_table_name_idx;


--
-- Name: event_log_y2026m01_pkey; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_pkey ATTACH PARTITION test_cac.event_log_y2026m01_pkey;


--
-- Name: event_log_y2026m01_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_table_name_index ATTACH PARTITION test_cac.event_log_y2026m01_table_name_action_timestamp_idx;


--
-- Name: event_log_y2026m01_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_timestamp_index ATTACH PARTITION test_cac.event_log_y2026m01_timestamp_action_table_name_idx;


--
-- Name: event_log_y2026m02_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_action_index ATTACH PARTITION test_cac.event_log_y2026m02_action_timestamp_table_name_idx;


--
-- Name: event_log_y2026m02_pkey; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_pkey ATTACH PARTITION test_cac.event_log_y2026m02_pkey;


--
-- Name: event_log_y2026m02_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_table_name_index ATTACH PARTITION test_cac.event_log_y2026m02_table_name_action_timestamp_idx;


--
-- Name: event_log_y2026m02_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_timestamp_index ATTACH PARTITION test_cac.event_log_y2026m02_timestamp_action_table_name_idx;


--
-- Name: event_log_y2026m03_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_action_index ATTACH PARTITION test_cac.event_log_y2026m03_action_timestamp_table_name_idx;


--
-- Name: event_log_y2026m03_pkey; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_pkey ATTACH PARTITION test_cac.event_log_y2026m03_pkey;


--
-- Name: event_log_y2026m03_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_table_name_index ATTACH PARTITION test_cac.event_log_y2026m03_table_name_action_timestamp_idx;


--
-- Name: event_log_y2026m03_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_timestamp_index ATTACH PARTITION test_cac.event_log_y2026m03_timestamp_action_table_name_idx;


--
-- Name: event_log_y2026m04_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_action_index ATTACH PARTITION test_cac.event_log_y2026m04_action_timestamp_table_name_idx;


--
-- Name: event_log_y2026m04_pkey; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_pkey ATTACH PARTITION test_cac.event_log_y2026m04_pkey;


--
-- Name: event_log_y2026m04_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_table_name_index ATTACH PARTITION test_cac.event_log_y2026m04_table_name_action_timestamp_idx;


--
-- Name: event_log_y2026m04_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_timestamp_index ATTACH PARTITION test_cac.event_log_y2026m04_timestamp_action_table_name_idx;


--
-- Name: event_log_y2026m05_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_action_index ATTACH PARTITION test_cac.event_log_y2026m05_action_timestamp_table_name_idx;


--
-- Name: event_log_y2026m05_pkey; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_pkey ATTACH PARTITION test_cac.event_log_y2026m05_pkey;


--
-- Name: event_log_y2026m05_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_table_name_index ATTACH PARTITION test_cac.event_log_y2026m05_table_name_action_timestamp_idx;


--
-- Name: event_log_y2026m05_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_timestamp_index ATTACH PARTITION test_cac.event_log_y2026m05_timestamp_action_table_name_idx;


--
-- Name: event_log_y2026m06_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_action_index ATTACH PARTITION test_cac.event_log_y2026m06_action_timestamp_table_name_idx;


--
-- Name: event_log_y2026m06_pkey; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_pkey ATTACH PARTITION test_cac.event_log_y2026m06_pkey;


--
-- Name: event_log_y2026m06_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_table_name_index ATTACH PARTITION test_cac.event_log_y2026m06_table_name_action_timestamp_idx;


--
-- Name: event_log_y2026m06_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_timestamp_index ATTACH PARTITION test_cac.event_log_y2026m06_timestamp_action_table_name_idx;


--
-- Name: event_log_y2026m07_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_action_index ATTACH PARTITION test_cac.event_log_y2026m07_action_timestamp_table_name_idx;


--
-- Name: event_log_y2026m07_pkey; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_pkey ATTACH PARTITION test_cac.event_log_y2026m07_pkey;


--
-- Name: event_log_y2026m07_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_table_name_index ATTACH PARTITION test_cac.event_log_y2026m07_table_name_action_timestamp_idx;


--
-- Name: event_log_y2026m07_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_timestamp_index ATTACH PARTITION test_cac.event_log_y2026m07_timestamp_action_table_name_idx;


--
-- Name: event_log_y2026m08_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_action_index ATTACH PARTITION test_cac.event_log_y2026m08_action_timestamp_table_name_idx;


--
-- Name: event_log_y2026m08_pkey; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_pkey ATTACH PARTITION test_cac.event_log_y2026m08_pkey;


--
-- Name: event_log_y2026m08_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_table_name_index ATTACH PARTITION test_cac.event_log_y2026m08_table_name_action_timestamp_idx;


--
-- Name: event_log_y2026m08_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_timestamp_index ATTACH PARTITION test_cac.event_log_y2026m08_timestamp_action_table_name_idx;


--
-- Name: event_log_y2026m09_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_action_index ATTACH PARTITION test_cac.event_log_y2026m09_action_timestamp_table_name_idx;


--
-- Name: event_log_y2026m09_pkey; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_pkey ATTACH PARTITION test_cac.event_log_y2026m09_pkey;


--
-- Name: event_log_y2026m09_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_table_name_index ATTACH PARTITION test_cac.event_log_y2026m09_table_name_action_timestamp_idx;


--
-- Name: event_log_y2026m09_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_timestamp_index ATTACH PARTITION test_cac.event_log_y2026m09_timestamp_action_table_name_idx;


--
-- Name: event_log_y2026m10_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_action_index ATTACH PARTITION test_cac.event_log_y2026m10_action_timestamp_table_name_idx;


--
-- Name: event_log_y2026m10_pkey; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_pkey ATTACH PARTITION test_cac.event_log_y2026m10_pkey;


--
-- Name: event_log_y2026m10_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_table_name_index ATTACH PARTITION test_cac.event_log_y2026m10_table_name_action_timestamp_idx;


--
-- Name: event_log_y2026m10_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_timestamp_index ATTACH PARTITION test_cac.event_log_y2026m10_timestamp_action_table_name_idx;


--
-- Name: event_log_y2026m11_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_action_index ATTACH PARTITION test_cac.event_log_y2026m11_action_timestamp_table_name_idx;


--
-- Name: event_log_y2026m11_pkey; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_pkey ATTACH PARTITION test_cac.event_log_y2026m11_pkey;


--
-- Name: event_log_y2026m11_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_table_name_index ATTACH PARTITION test_cac.event_log_y2026m11_table_name_action_timestamp_idx;


--
-- Name: event_log_y2026m11_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_timestamp_index ATTACH PARTITION test_cac.event_log_y2026m11_timestamp_action_table_name_idx;


--
-- Name: event_log_y2026m12_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_action_index ATTACH PARTITION test_cac.event_log_y2026m12_action_timestamp_table_name_idx;


--
-- Name: event_log_y2026m12_pkey; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_pkey ATTACH PARTITION test_cac.event_log_y2026m12_pkey;


--
-- Name: event_log_y2026m12_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_table_name_index ATTACH PARTITION test_cac.event_log_y2026m12_table_name_action_timestamp_idx;


--
-- Name: event_log_y2026m12_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_cac; Owner: postgres
--

ALTER INDEX test_cac.event_log_timestamp_index ATTACH PARTITION test_cac.event_log_y2026m12_timestamp_action_table_name_idx;


--
-- Name: event_log_y2023m08_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_action_index ATTACH PARTITION test_experimentation.event_log_y2023m08_action_timestamp_table_name_idx;


--
-- Name: event_log_y2023m08_pkey; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_pkey ATTACH PARTITION test_experimentation.event_log_y2023m08_pkey;


--
-- Name: event_log_y2023m08_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_table_name_index ATTACH PARTITION test_experimentation.event_log_y2023m08_table_name_action_timestamp_idx;


--
-- Name: event_log_y2023m08_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_timestamp_index ATTACH PARTITION test_experimentation.event_log_y2023m08_timestamp_action_table_name_idx;


--
-- Name: event_log_y2023m09_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_action_index ATTACH PARTITION test_experimentation.event_log_y2023m09_action_timestamp_table_name_idx;


--
-- Name: event_log_y2023m09_pkey; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_pkey ATTACH PARTITION test_experimentation.event_log_y2023m09_pkey;


--
-- Name: event_log_y2023m09_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_table_name_index ATTACH PARTITION test_experimentation.event_log_y2023m09_table_name_action_timestamp_idx;


--
-- Name: event_log_y2023m09_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_timestamp_index ATTACH PARTITION test_experimentation.event_log_y2023m09_timestamp_action_table_name_idx;


--
-- Name: event_log_y2023m10_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_action_index ATTACH PARTITION test_experimentation.event_log_y2023m10_action_timestamp_table_name_idx;


--
-- Name: event_log_y2023m10_pkey; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_pkey ATTACH PARTITION test_experimentation.event_log_y2023m10_pkey;


--
-- Name: event_log_y2023m10_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_table_name_index ATTACH PARTITION test_experimentation.event_log_y2023m10_table_name_action_timestamp_idx;


--
-- Name: event_log_y2023m10_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_timestamp_index ATTACH PARTITION test_experimentation.event_log_y2023m10_timestamp_action_table_name_idx;


--
-- Name: event_log_y2023m11_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_action_index ATTACH PARTITION test_experimentation.event_log_y2023m11_action_timestamp_table_name_idx;


--
-- Name: event_log_y2023m11_pkey; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_pkey ATTACH PARTITION test_experimentation.event_log_y2023m11_pkey;


--
-- Name: event_log_y2023m11_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_table_name_index ATTACH PARTITION test_experimentation.event_log_y2023m11_table_name_action_timestamp_idx;


--
-- Name: event_log_y2023m11_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_timestamp_index ATTACH PARTITION test_experimentation.event_log_y2023m11_timestamp_action_table_name_idx;


--
-- Name: event_log_y2023m12_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_action_index ATTACH PARTITION test_experimentation.event_log_y2023m12_action_timestamp_table_name_idx;


--
-- Name: event_log_y2023m12_pkey; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_pkey ATTACH PARTITION test_experimentation.event_log_y2023m12_pkey;


--
-- Name: event_log_y2023m12_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_table_name_index ATTACH PARTITION test_experimentation.event_log_y2023m12_table_name_action_timestamp_idx;


--
-- Name: event_log_y2023m12_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_timestamp_index ATTACH PARTITION test_experimentation.event_log_y2023m12_timestamp_action_table_name_idx;


--
-- Name: event_log_y2024m01_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_action_index ATTACH PARTITION test_experimentation.event_log_y2024m01_action_timestamp_table_name_idx;


--
-- Name: event_log_y2024m01_pkey; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_pkey ATTACH PARTITION test_experimentation.event_log_y2024m01_pkey;


--
-- Name: event_log_y2024m01_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_table_name_index ATTACH PARTITION test_experimentation.event_log_y2024m01_table_name_action_timestamp_idx;


--
-- Name: event_log_y2024m01_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_timestamp_index ATTACH PARTITION test_experimentation.event_log_y2024m01_timestamp_action_table_name_idx;


--
-- Name: event_log_y2024m02_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_action_index ATTACH PARTITION test_experimentation.event_log_y2024m02_action_timestamp_table_name_idx;


--
-- Name: event_log_y2024m02_pkey; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_pkey ATTACH PARTITION test_experimentation.event_log_y2024m02_pkey;


--
-- Name: event_log_y2024m02_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_table_name_index ATTACH PARTITION test_experimentation.event_log_y2024m02_table_name_action_timestamp_idx;


--
-- Name: event_log_y2024m02_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_timestamp_index ATTACH PARTITION test_experimentation.event_log_y2024m02_timestamp_action_table_name_idx;


--
-- Name: event_log_y2024m03_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_action_index ATTACH PARTITION test_experimentation.event_log_y2024m03_action_timestamp_table_name_idx;


--
-- Name: event_log_y2024m03_pkey; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_pkey ATTACH PARTITION test_experimentation.event_log_y2024m03_pkey;


--
-- Name: event_log_y2024m03_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_table_name_index ATTACH PARTITION test_experimentation.event_log_y2024m03_table_name_action_timestamp_idx;


--
-- Name: event_log_y2024m03_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_timestamp_index ATTACH PARTITION test_experimentation.event_log_y2024m03_timestamp_action_table_name_idx;


--
-- Name: event_log_y2024m04_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_action_index ATTACH PARTITION test_experimentation.event_log_y2024m04_action_timestamp_table_name_idx;


--
-- Name: event_log_y2024m04_pkey; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_pkey ATTACH PARTITION test_experimentation.event_log_y2024m04_pkey;


--
-- Name: event_log_y2024m04_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_table_name_index ATTACH PARTITION test_experimentation.event_log_y2024m04_table_name_action_timestamp_idx;


--
-- Name: event_log_y2024m04_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_timestamp_index ATTACH PARTITION test_experimentation.event_log_y2024m04_timestamp_action_table_name_idx;


--
-- Name: event_log_y2024m05_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_action_index ATTACH PARTITION test_experimentation.event_log_y2024m05_action_timestamp_table_name_idx;


--
-- Name: event_log_y2024m05_pkey; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_pkey ATTACH PARTITION test_experimentation.event_log_y2024m05_pkey;


--
-- Name: event_log_y2024m05_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_table_name_index ATTACH PARTITION test_experimentation.event_log_y2024m05_table_name_action_timestamp_idx;


--
-- Name: event_log_y2024m05_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_timestamp_index ATTACH PARTITION test_experimentation.event_log_y2024m05_timestamp_action_table_name_idx;


--
-- Name: event_log_y2024m06_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_action_index ATTACH PARTITION test_experimentation.event_log_y2024m06_action_timestamp_table_name_idx;


--
-- Name: event_log_y2024m06_pkey; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_pkey ATTACH PARTITION test_experimentation.event_log_y2024m06_pkey;


--
-- Name: event_log_y2024m06_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_table_name_index ATTACH PARTITION test_experimentation.event_log_y2024m06_table_name_action_timestamp_idx;


--
-- Name: event_log_y2024m06_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_timestamp_index ATTACH PARTITION test_experimentation.event_log_y2024m06_timestamp_action_table_name_idx;


--
-- Name: event_log_y2024m07_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_action_index ATTACH PARTITION test_experimentation.event_log_y2024m07_action_timestamp_table_name_idx;


--
-- Name: event_log_y2024m07_pkey; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_pkey ATTACH PARTITION test_experimentation.event_log_y2024m07_pkey;


--
-- Name: event_log_y2024m07_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_table_name_index ATTACH PARTITION test_experimentation.event_log_y2024m07_table_name_action_timestamp_idx;


--
-- Name: event_log_y2024m07_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_timestamp_index ATTACH PARTITION test_experimentation.event_log_y2024m07_timestamp_action_table_name_idx;


--
-- Name: event_log_y2024m08_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_action_index ATTACH PARTITION test_experimentation.event_log_y2024m08_action_timestamp_table_name_idx;


--
-- Name: event_log_y2024m08_pkey; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_pkey ATTACH PARTITION test_experimentation.event_log_y2024m08_pkey;


--
-- Name: event_log_y2024m08_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_table_name_index ATTACH PARTITION test_experimentation.event_log_y2024m08_table_name_action_timestamp_idx;


--
-- Name: event_log_y2024m08_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_timestamp_index ATTACH PARTITION test_experimentation.event_log_y2024m08_timestamp_action_table_name_idx;


--
-- Name: event_log_y2024m09_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_action_index ATTACH PARTITION test_experimentation.event_log_y2024m09_action_timestamp_table_name_idx;


--
-- Name: event_log_y2024m09_pkey; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_pkey ATTACH PARTITION test_experimentation.event_log_y2024m09_pkey;


--
-- Name: event_log_y2024m09_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_table_name_index ATTACH PARTITION test_experimentation.event_log_y2024m09_table_name_action_timestamp_idx;


--
-- Name: event_log_y2024m09_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_timestamp_index ATTACH PARTITION test_experimentation.event_log_y2024m09_timestamp_action_table_name_idx;


--
-- Name: event_log_y2024m10_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_action_index ATTACH PARTITION test_experimentation.event_log_y2024m10_action_timestamp_table_name_idx;


--
-- Name: event_log_y2024m10_pkey; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_pkey ATTACH PARTITION test_experimentation.event_log_y2024m10_pkey;


--
-- Name: event_log_y2024m10_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_table_name_index ATTACH PARTITION test_experimentation.event_log_y2024m10_table_name_action_timestamp_idx;


--
-- Name: event_log_y2024m10_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_timestamp_index ATTACH PARTITION test_experimentation.event_log_y2024m10_timestamp_action_table_name_idx;


--
-- Name: event_log_y2024m11_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_action_index ATTACH PARTITION test_experimentation.event_log_y2024m11_action_timestamp_table_name_idx;


--
-- Name: event_log_y2024m11_pkey; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_pkey ATTACH PARTITION test_experimentation.event_log_y2024m11_pkey;


--
-- Name: event_log_y2024m11_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_table_name_index ATTACH PARTITION test_experimentation.event_log_y2024m11_table_name_action_timestamp_idx;


--
-- Name: event_log_y2024m11_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_timestamp_index ATTACH PARTITION test_experimentation.event_log_y2024m11_timestamp_action_table_name_idx;


--
-- Name: event_log_y2024m12_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_action_index ATTACH PARTITION test_experimentation.event_log_y2024m12_action_timestamp_table_name_idx;


--
-- Name: event_log_y2024m12_pkey; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_pkey ATTACH PARTITION test_experimentation.event_log_y2024m12_pkey;


--
-- Name: event_log_y2024m12_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_table_name_index ATTACH PARTITION test_experimentation.event_log_y2024m12_table_name_action_timestamp_idx;


--
-- Name: event_log_y2024m12_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_timestamp_index ATTACH PARTITION test_experimentation.event_log_y2024m12_timestamp_action_table_name_idx;


--
-- Name: event_log_y2025m01_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_action_index ATTACH PARTITION test_experimentation.event_log_y2025m01_action_timestamp_table_name_idx;


--
-- Name: event_log_y2025m01_pkey; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_pkey ATTACH PARTITION test_experimentation.event_log_y2025m01_pkey;


--
-- Name: event_log_y2025m01_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_table_name_index ATTACH PARTITION test_experimentation.event_log_y2025m01_table_name_action_timestamp_idx;


--
-- Name: event_log_y2025m01_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_timestamp_index ATTACH PARTITION test_experimentation.event_log_y2025m01_timestamp_action_table_name_idx;


--
-- Name: event_log_y2025m02_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_action_index ATTACH PARTITION test_experimentation.event_log_y2025m02_action_timestamp_table_name_idx;


--
-- Name: event_log_y2025m02_pkey; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_pkey ATTACH PARTITION test_experimentation.event_log_y2025m02_pkey;


--
-- Name: event_log_y2025m02_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_table_name_index ATTACH PARTITION test_experimentation.event_log_y2025m02_table_name_action_timestamp_idx;


--
-- Name: event_log_y2025m02_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_timestamp_index ATTACH PARTITION test_experimentation.event_log_y2025m02_timestamp_action_table_name_idx;


--
-- Name: event_log_y2025m03_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_action_index ATTACH PARTITION test_experimentation.event_log_y2025m03_action_timestamp_table_name_idx;


--
-- Name: event_log_y2025m03_pkey; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_pkey ATTACH PARTITION test_experimentation.event_log_y2025m03_pkey;


--
-- Name: event_log_y2025m03_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_table_name_index ATTACH PARTITION test_experimentation.event_log_y2025m03_table_name_action_timestamp_idx;


--
-- Name: event_log_y2025m03_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_timestamp_index ATTACH PARTITION test_experimentation.event_log_y2025m03_timestamp_action_table_name_idx;


--
-- Name: event_log_y2025m04_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_action_index ATTACH PARTITION test_experimentation.event_log_y2025m04_action_timestamp_table_name_idx;


--
-- Name: event_log_y2025m04_pkey; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_pkey ATTACH PARTITION test_experimentation.event_log_y2025m04_pkey;


--
-- Name: event_log_y2025m04_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_table_name_index ATTACH PARTITION test_experimentation.event_log_y2025m04_table_name_action_timestamp_idx;


--
-- Name: event_log_y2025m04_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_timestamp_index ATTACH PARTITION test_experimentation.event_log_y2025m04_timestamp_action_table_name_idx;


--
-- Name: event_log_y2025m05_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_action_index ATTACH PARTITION test_experimentation.event_log_y2025m05_action_timestamp_table_name_idx;


--
-- Name: event_log_y2025m05_pkey; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_pkey ATTACH PARTITION test_experimentation.event_log_y2025m05_pkey;


--
-- Name: event_log_y2025m05_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_table_name_index ATTACH PARTITION test_experimentation.event_log_y2025m05_table_name_action_timestamp_idx;


--
-- Name: event_log_y2025m05_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_timestamp_index ATTACH PARTITION test_experimentation.event_log_y2025m05_timestamp_action_table_name_idx;


--
-- Name: event_log_y2025m06_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_action_index ATTACH PARTITION test_experimentation.event_log_y2025m06_action_timestamp_table_name_idx;


--
-- Name: event_log_y2025m06_pkey; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_pkey ATTACH PARTITION test_experimentation.event_log_y2025m06_pkey;


--
-- Name: event_log_y2025m06_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_table_name_index ATTACH PARTITION test_experimentation.event_log_y2025m06_table_name_action_timestamp_idx;


--
-- Name: event_log_y2025m06_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_timestamp_index ATTACH PARTITION test_experimentation.event_log_y2025m06_timestamp_action_table_name_idx;


--
-- Name: event_log_y2025m07_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_action_index ATTACH PARTITION test_experimentation.event_log_y2025m07_action_timestamp_table_name_idx;


--
-- Name: event_log_y2025m07_pkey; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_pkey ATTACH PARTITION test_experimentation.event_log_y2025m07_pkey;


--
-- Name: event_log_y2025m07_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_table_name_index ATTACH PARTITION test_experimentation.event_log_y2025m07_table_name_action_timestamp_idx;


--
-- Name: event_log_y2025m07_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_timestamp_index ATTACH PARTITION test_experimentation.event_log_y2025m07_timestamp_action_table_name_idx;


--
-- Name: event_log_y2025m08_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_action_index ATTACH PARTITION test_experimentation.event_log_y2025m08_action_timestamp_table_name_idx;


--
-- Name: event_log_y2025m08_pkey; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_pkey ATTACH PARTITION test_experimentation.event_log_y2025m08_pkey;


--
-- Name: event_log_y2025m08_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_table_name_index ATTACH PARTITION test_experimentation.event_log_y2025m08_table_name_action_timestamp_idx;


--
-- Name: event_log_y2025m08_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_timestamp_index ATTACH PARTITION test_experimentation.event_log_y2025m08_timestamp_action_table_name_idx;


--
-- Name: event_log_y2025m09_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_action_index ATTACH PARTITION test_experimentation.event_log_y2025m09_action_timestamp_table_name_idx;


--
-- Name: event_log_y2025m09_pkey; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_pkey ATTACH PARTITION test_experimentation.event_log_y2025m09_pkey;


--
-- Name: event_log_y2025m09_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_table_name_index ATTACH PARTITION test_experimentation.event_log_y2025m09_table_name_action_timestamp_idx;


--
-- Name: event_log_y2025m09_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_timestamp_index ATTACH PARTITION test_experimentation.event_log_y2025m09_timestamp_action_table_name_idx;


--
-- Name: event_log_y2025m10_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_action_index ATTACH PARTITION test_experimentation.event_log_y2025m10_action_timestamp_table_name_idx;


--
-- Name: event_log_y2025m10_pkey; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_pkey ATTACH PARTITION test_experimentation.event_log_y2025m10_pkey;


--
-- Name: event_log_y2025m10_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_table_name_index ATTACH PARTITION test_experimentation.event_log_y2025m10_table_name_action_timestamp_idx;


--
-- Name: event_log_y2025m10_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_timestamp_index ATTACH PARTITION test_experimentation.event_log_y2025m10_timestamp_action_table_name_idx;


--
-- Name: event_log_y2025m11_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_action_index ATTACH PARTITION test_experimentation.event_log_y2025m11_action_timestamp_table_name_idx;


--
-- Name: event_log_y2025m11_pkey; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_pkey ATTACH PARTITION test_experimentation.event_log_y2025m11_pkey;


--
-- Name: event_log_y2025m11_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_table_name_index ATTACH PARTITION test_experimentation.event_log_y2025m11_table_name_action_timestamp_idx;


--
-- Name: event_log_y2025m11_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_timestamp_index ATTACH PARTITION test_experimentation.event_log_y2025m11_timestamp_action_table_name_idx;


--
-- Name: event_log_y2025m12_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_action_index ATTACH PARTITION test_experimentation.event_log_y2025m12_action_timestamp_table_name_idx;


--
-- Name: event_log_y2025m12_pkey; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_pkey ATTACH PARTITION test_experimentation.event_log_y2025m12_pkey;


--
-- Name: event_log_y2025m12_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_table_name_index ATTACH PARTITION test_experimentation.event_log_y2025m12_table_name_action_timestamp_idx;


--
-- Name: event_log_y2025m12_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_timestamp_index ATTACH PARTITION test_experimentation.event_log_y2025m12_timestamp_action_table_name_idx;


--
-- Name: event_log_y2026m01_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_action_index ATTACH PARTITION test_experimentation.event_log_y2026m01_action_timestamp_table_name_idx;


--
-- Name: event_log_y2026m01_pkey; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_pkey ATTACH PARTITION test_experimentation.event_log_y2026m01_pkey;


--
-- Name: event_log_y2026m01_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_table_name_index ATTACH PARTITION test_experimentation.event_log_y2026m01_table_name_action_timestamp_idx;


--
-- Name: event_log_y2026m01_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_timestamp_index ATTACH PARTITION test_experimentation.event_log_y2026m01_timestamp_action_table_name_idx;


--
-- Name: event_log_y2026m02_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_action_index ATTACH PARTITION test_experimentation.event_log_y2026m02_action_timestamp_table_name_idx;


--
-- Name: event_log_y2026m02_pkey; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_pkey ATTACH PARTITION test_experimentation.event_log_y2026m02_pkey;


--
-- Name: event_log_y2026m02_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_table_name_index ATTACH PARTITION test_experimentation.event_log_y2026m02_table_name_action_timestamp_idx;


--
-- Name: event_log_y2026m02_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_timestamp_index ATTACH PARTITION test_experimentation.event_log_y2026m02_timestamp_action_table_name_idx;


--
-- Name: event_log_y2026m03_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_action_index ATTACH PARTITION test_experimentation.event_log_y2026m03_action_timestamp_table_name_idx;


--
-- Name: event_log_y2026m03_pkey; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_pkey ATTACH PARTITION test_experimentation.event_log_y2026m03_pkey;


--
-- Name: event_log_y2026m03_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_table_name_index ATTACH PARTITION test_experimentation.event_log_y2026m03_table_name_action_timestamp_idx;


--
-- Name: event_log_y2026m03_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_timestamp_index ATTACH PARTITION test_experimentation.event_log_y2026m03_timestamp_action_table_name_idx;


--
-- Name: event_log_y2026m04_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_action_index ATTACH PARTITION test_experimentation.event_log_y2026m04_action_timestamp_table_name_idx;


--
-- Name: event_log_y2026m04_pkey; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_pkey ATTACH PARTITION test_experimentation.event_log_y2026m04_pkey;


--
-- Name: event_log_y2026m04_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_table_name_index ATTACH PARTITION test_experimentation.event_log_y2026m04_table_name_action_timestamp_idx;


--
-- Name: event_log_y2026m04_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_timestamp_index ATTACH PARTITION test_experimentation.event_log_y2026m04_timestamp_action_table_name_idx;


--
-- Name: event_log_y2026m05_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_action_index ATTACH PARTITION test_experimentation.event_log_y2026m05_action_timestamp_table_name_idx;


--
-- Name: event_log_y2026m05_pkey; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_pkey ATTACH PARTITION test_experimentation.event_log_y2026m05_pkey;


--
-- Name: event_log_y2026m05_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_table_name_index ATTACH PARTITION test_experimentation.event_log_y2026m05_table_name_action_timestamp_idx;


--
-- Name: event_log_y2026m05_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_timestamp_index ATTACH PARTITION test_experimentation.event_log_y2026m05_timestamp_action_table_name_idx;


--
-- Name: event_log_y2026m06_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_action_index ATTACH PARTITION test_experimentation.event_log_y2026m06_action_timestamp_table_name_idx;


--
-- Name: event_log_y2026m06_pkey; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_pkey ATTACH PARTITION test_experimentation.event_log_y2026m06_pkey;


--
-- Name: event_log_y2026m06_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_table_name_index ATTACH PARTITION test_experimentation.event_log_y2026m06_table_name_action_timestamp_idx;


--
-- Name: event_log_y2026m06_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_timestamp_index ATTACH PARTITION test_experimentation.event_log_y2026m06_timestamp_action_table_name_idx;


--
-- Name: event_log_y2026m07_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_action_index ATTACH PARTITION test_experimentation.event_log_y2026m07_action_timestamp_table_name_idx;


--
-- Name: event_log_y2026m07_pkey; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_pkey ATTACH PARTITION test_experimentation.event_log_y2026m07_pkey;


--
-- Name: event_log_y2026m07_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_table_name_index ATTACH PARTITION test_experimentation.event_log_y2026m07_table_name_action_timestamp_idx;


--
-- Name: event_log_y2026m07_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_timestamp_index ATTACH PARTITION test_experimentation.event_log_y2026m07_timestamp_action_table_name_idx;


--
-- Name: event_log_y2026m08_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_action_index ATTACH PARTITION test_experimentation.event_log_y2026m08_action_timestamp_table_name_idx;


--
-- Name: event_log_y2026m08_pkey; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_pkey ATTACH PARTITION test_experimentation.event_log_y2026m08_pkey;


--
-- Name: event_log_y2026m08_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_table_name_index ATTACH PARTITION test_experimentation.event_log_y2026m08_table_name_action_timestamp_idx;


--
-- Name: event_log_y2026m08_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_timestamp_index ATTACH PARTITION test_experimentation.event_log_y2026m08_timestamp_action_table_name_idx;


--
-- Name: event_log_y2026m09_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_action_index ATTACH PARTITION test_experimentation.event_log_y2026m09_action_timestamp_table_name_idx;


--
-- Name: event_log_y2026m09_pkey; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_pkey ATTACH PARTITION test_experimentation.event_log_y2026m09_pkey;


--
-- Name: event_log_y2026m09_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_table_name_index ATTACH PARTITION test_experimentation.event_log_y2026m09_table_name_action_timestamp_idx;


--
-- Name: event_log_y2026m09_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_timestamp_index ATTACH PARTITION test_experimentation.event_log_y2026m09_timestamp_action_table_name_idx;


--
-- Name: event_log_y2026m10_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_action_index ATTACH PARTITION test_experimentation.event_log_y2026m10_action_timestamp_table_name_idx;


--
-- Name: event_log_y2026m10_pkey; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_pkey ATTACH PARTITION test_experimentation.event_log_y2026m10_pkey;


--
-- Name: event_log_y2026m10_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_table_name_index ATTACH PARTITION test_experimentation.event_log_y2026m10_table_name_action_timestamp_idx;


--
-- Name: event_log_y2026m10_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_timestamp_index ATTACH PARTITION test_experimentation.event_log_y2026m10_timestamp_action_table_name_idx;


--
-- Name: event_log_y2026m11_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_action_index ATTACH PARTITION test_experimentation.event_log_y2026m11_action_timestamp_table_name_idx;


--
-- Name: event_log_y2026m11_pkey; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_pkey ATTACH PARTITION test_experimentation.event_log_y2026m11_pkey;


--
-- Name: event_log_y2026m11_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_table_name_index ATTACH PARTITION test_experimentation.event_log_y2026m11_table_name_action_timestamp_idx;


--
-- Name: event_log_y2026m11_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_timestamp_index ATTACH PARTITION test_experimentation.event_log_y2026m11_timestamp_action_table_name_idx;


--
-- Name: event_log_y2026m12_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_action_index ATTACH PARTITION test_experimentation.event_log_y2026m12_action_timestamp_table_name_idx;


--
-- Name: event_log_y2026m12_pkey; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_pkey ATTACH PARTITION test_experimentation.event_log_y2026m12_pkey;


--
-- Name: event_log_y2026m12_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_table_name_index ATTACH PARTITION test_experimentation.event_log_y2026m12_table_name_action_timestamp_idx;


--
-- Name: event_log_y2026m12_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: test_experimentation; Owner: postgres
--

ALTER INDEX test_experimentation.event_log_timestamp_index ATTACH PARTITION test_experimentation.event_log_y2026m12_timestamp_action_table_name_idx;


--
-- Name: contexts contexts_audit; Type: TRIGGER; Schema: dev_cac; Owner: postgres
--

CREATE TRIGGER contexts_audit AFTER INSERT OR DELETE OR UPDATE ON dev_cac.contexts FOR EACH ROW EXECUTE FUNCTION dev_cac.event_logger();


--
-- Name: default_configs default_configs_audit; Type: TRIGGER; Schema: dev_cac; Owner: postgres
--

CREATE TRIGGER default_configs_audit AFTER INSERT OR DELETE OR UPDATE ON dev_cac.default_configs FOR EACH ROW EXECUTE FUNCTION dev_cac.event_logger();


--
-- Name: dimensions dimensions_audit; Type: TRIGGER; Schema: dev_cac; Owner: postgres
--

CREATE TRIGGER dimensions_audit AFTER INSERT OR DELETE OR UPDATE ON dev_cac.dimensions FOR EACH ROW EXECUTE FUNCTION dev_cac.event_logger();


--
-- Name: functions functions_audit; Type: TRIGGER; Schema: dev_cac; Owner: postgres
--

CREATE TRIGGER functions_audit AFTER INSERT OR DELETE OR UPDATE ON dev_cac.functions FOR EACH ROW EXECUTE FUNCTION dev_cac.event_logger();


--
-- Name: experiments experiments_audit; Type: TRIGGER; Schema: dev_experimentation; Owner: postgres
--

CREATE TRIGGER experiments_audit AFTER INSERT OR DELETE OR UPDATE ON dev_experimentation.experiments FOR EACH ROW EXECUTE FUNCTION dev_experimentation.event_logger();


--
-- Name: contexts contexts_audit; Type: TRIGGER; Schema: test_cac; Owner: postgres
--

CREATE TRIGGER contexts_audit AFTER INSERT OR DELETE OR UPDATE ON test_cac.contexts FOR EACH ROW EXECUTE FUNCTION test_cac.event_logger();


--
-- Name: default_configs default_configs_audit; Type: TRIGGER; Schema: test_cac; Owner: postgres
--

CREATE TRIGGER default_configs_audit AFTER INSERT OR DELETE OR UPDATE ON test_cac.default_configs FOR EACH ROW EXECUTE FUNCTION test_cac.event_logger();


--
-- Name: dimensions dimensions_audit; Type: TRIGGER; Schema: test_cac; Owner: postgres
--

CREATE TRIGGER dimensions_audit AFTER INSERT OR DELETE OR UPDATE ON test_cac.dimensions FOR EACH ROW EXECUTE FUNCTION test_cac.event_logger();


--
-- Name: functions functions_audit; Type: TRIGGER; Schema: test_cac; Owner: postgres
--

CREATE TRIGGER functions_audit AFTER INSERT OR DELETE OR UPDATE ON test_cac.functions FOR EACH ROW EXECUTE FUNCTION test_cac.event_logger();


--
-- Name: experiments experiments_audit; Type: TRIGGER; Schema: test_experimentation; Owner: postgres
--

CREATE TRIGGER experiments_audit AFTER INSERT OR DELETE OR UPDATE ON test_experimentation.experiments FOR EACH ROW EXECUTE FUNCTION test_experimentation.event_logger();


--
-- Name: default_configs default_configs_function_name_fkey; Type: FK CONSTRAINT; Schema: dev_cac; Owner: postgres
--

ALTER TABLE ONLY dev_cac.default_configs
    ADD CONSTRAINT default_configs_function_name_fkey FOREIGN KEY (function_name) REFERENCES dev_cac.functions(function_name);


--
-- Name: dimensions dimensions_function_name_fkey; Type: FK CONSTRAINT; Schema: dev_cac; Owner: postgres
--

ALTER TABLE ONLY dev_cac.dimensions
    ADD CONSTRAINT dimensions_function_name_fkey FOREIGN KEY (function_name) REFERENCES dev_cac.functions(function_name);


--
-- Name: default_configs default_configs_function_name_fkey; Type: FK CONSTRAINT; Schema: test_cac; Owner: postgres
--

ALTER TABLE ONLY test_cac.default_configs
    ADD CONSTRAINT default_configs_function_name_fkey FOREIGN KEY (function_name) REFERENCES test_cac.functions(function_name);


--
-- Name: dimensions dimensions_function_name_fkey; Type: FK CONSTRAINT; Schema: test_cac; Owner: postgres
--

ALTER TABLE ONLY test_cac.dimensions
    ADD CONSTRAINT dimensions_function_name_fkey FOREIGN KEY (function_name) REFERENCES test_cac.functions(function_name);


--
-- PostgreSQL database dump complete
--

