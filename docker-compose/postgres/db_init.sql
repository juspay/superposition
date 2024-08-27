--
-- PostgreSQL database dump
--

-- Dumped from database version 12.19
-- Dumped by pg_dump version 12.19

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
-- Name: dimension_type; Type: TYPE; Schema: public; Owner: postgres
--

CREATE TYPE public.dimension_type AS ENUM (
    'NULL',
    'BOOL',
    'NUMBER',
    'STRING',
    'ARRAY',
    'OBJECT'
);


ALTER TYPE public.dimension_type OWNER TO postgres;

--
-- Name: experiment_status_type; Type: TYPE; Schema: public; Owner: postgres
--

CREATE TYPE public.experiment_status_type AS ENUM (
    'CREATED',
    'CONCLUDED',
    'INPROGRESS'
);


ALTER TYPE public.experiment_status_type OWNER TO postgres;

--
-- Name: not_null_text; Type: DOMAIN; Schema: public; Owner: postgres
--

CREATE DOMAIN public.not_null_text AS text NOT NULL;


ALTER DOMAIN public.not_null_text OWNER TO postgres;

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
-- Name: diesel_manage_updated_at(regclass); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.diesel_manage_updated_at(_tbl regclass) RETURNS void
    LANGUAGE plpgsql
    AS $$
BEGIN
    EXECUTE format('CREATE TRIGGER set_updated_at BEFORE UPDATE ON %s
                    FOR EACH ROW EXECUTE PROCEDURE diesel_set_updated_at()', _tbl);
END;
$$;


ALTER FUNCTION public.diesel_manage_updated_at(_tbl regclass) OWNER TO postgres;

--
-- Name: diesel_set_updated_at(); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.diesel_set_updated_at() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
    IF (
        NEW IS DISTINCT FROM OLD AND
        NEW.updated_at IS NOT DISTINCT FROM OLD.updated_at
    ) THEN
        NEW.updated_at := current_timestamp;
    END IF;
    RETURN NEW;
END;
$$;


ALTER FUNCTION public.diesel_set_updated_at() OWNER TO postgres;

--
-- Name: event_logger(); Type: FUNCTION; Schema: public; Owner: postgres
--

CREATE FUNCTION public.event_logger() RETURNS trigger
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


ALTER FUNCTION public.event_logger() OWNER TO postgres;

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
-- Name: config_versions; Type: TABLE; Schema: dev_cac; Owner: postgres
--

CREATE TABLE dev_cac.config_versions (
    id bigint NOT NULL,
    config json NOT NULL,
    config_hash text NOT NULL,
    tags character varying(100)[],
    created_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT config_versions_tags_check CHECK ((array_position(tags, NULL::character varying) IS NULL))
);


ALTER TABLE dev_cac.config_versions OWNER TO postgres;

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
    override json DEFAULT '{}'::json NOT NULL,
    last_modified_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    last_modified_by character varying(200) DEFAULT 'null'::character varying NOT NULL
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
    function_name text,
    last_modified_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    last_modified_by character varying(200) DEFAULT 'null'::character varying NOT NULL
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
    function_name text,
    last_modified_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    last_modified_by character varying(200) DEFAULT 'null'::character varying NOT NULL
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
    draft_edited_by text NOT NULL,
    last_modified_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    last_modified_by character varying(200) DEFAULT 'null'::character varying NOT NULL
);


ALTER TABLE dev_cac.functions OWNER TO postgres;

--
-- Name: type_templates; Type: TABLE; Schema: dev_cac; Owner: postgres
--

CREATE TABLE dev_cac.type_templates (
    type_name text NOT NULL,
    type_schema json NOT NULL,
    created_by text NOT NULL,
    created_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    last_modified_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    last_modified_by character varying(200) DEFAULT 'null'::character varying NOT NULL
);


ALTER TABLE dev_cac.type_templates OWNER TO postgres;

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
-- Name: __diesel_schema_migrations; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.__diesel_schema_migrations (
    version character varying(50) NOT NULL,
    run_on timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


ALTER TABLE public.__diesel_schema_migrations OWNER TO postgres;

--
-- Name: config_versions; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.config_versions (
    id bigint NOT NULL,
    config json NOT NULL,
    config_hash text NOT NULL,
    tags character varying(100)[],
    created_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT config_versions_tags_check CHECK ((array_position(tags, NULL::character varying) IS NULL))
);


ALTER TABLE public.config_versions OWNER TO postgres;

--
-- Name: contexts; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.contexts (
    id character varying NOT NULL,
    value json NOT NULL,
    override_id character varying NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_by character varying NOT NULL,
    priority integer DEFAULT 1 NOT NULL,
    override json DEFAULT '{}'::json NOT NULL,
    last_modified_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    last_modified_by character varying(200) DEFAULT 'null'::character varying NOT NULL
);


ALTER TABLE public.contexts OWNER TO postgres;

--
-- Name: default_configs; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.default_configs (
    key character varying NOT NULL,
    value json NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_by character varying NOT NULL,
    schema json DEFAULT '{}'::json NOT NULL,
    function_name text,
    last_modified_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    last_modified_by character varying(200) DEFAULT 'null'::character varying NOT NULL
);


ALTER TABLE public.default_configs OWNER TO postgres;

--
-- Name: dimensions; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.dimensions (
    dimension character varying NOT NULL,
    priority integer NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_by character varying NOT NULL,
    schema json DEFAULT '{}'::json NOT NULL,
    function_name text,
    last_modified_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    last_modified_by character varying(200) DEFAULT 'null'::character varying NOT NULL
);


ALTER TABLE public.dimensions OWNER TO postgres;

--
-- Name: event_log; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.event_log (
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


ALTER TABLE public.event_log OWNER TO postgres;

--
-- Name: event_log_y2023m08; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.event_log_y2023m08 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY public.event_log ATTACH PARTITION public.event_log_y2023m08 FOR VALUES FROM ('2023-08-01 00:00:00') TO ('2023-09-01 00:00:00');


ALTER TABLE public.event_log_y2023m08 OWNER TO postgres;

--
-- Name: event_log_y2023m09; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.event_log_y2023m09 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY public.event_log ATTACH PARTITION public.event_log_y2023m09 FOR VALUES FROM ('2023-09-01 00:00:00') TO ('2023-10-01 00:00:00');


ALTER TABLE public.event_log_y2023m09 OWNER TO postgres;

--
-- Name: event_log_y2023m10; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.event_log_y2023m10 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY public.event_log ATTACH PARTITION public.event_log_y2023m10 FOR VALUES FROM ('2023-10-01 00:00:00') TO ('2023-11-01 00:00:00');


ALTER TABLE public.event_log_y2023m10 OWNER TO postgres;

--
-- Name: event_log_y2023m11; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.event_log_y2023m11 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY public.event_log ATTACH PARTITION public.event_log_y2023m11 FOR VALUES FROM ('2023-11-01 00:00:00') TO ('2023-12-01 00:00:00');


ALTER TABLE public.event_log_y2023m11 OWNER TO postgres;

--
-- Name: event_log_y2023m12; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.event_log_y2023m12 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY public.event_log ATTACH PARTITION public.event_log_y2023m12 FOR VALUES FROM ('2023-12-01 00:00:00') TO ('2024-01-01 00:00:00');


ALTER TABLE public.event_log_y2023m12 OWNER TO postgres;

--
-- Name: event_log_y2024m01; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.event_log_y2024m01 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY public.event_log ATTACH PARTITION public.event_log_y2024m01 FOR VALUES FROM ('2024-01-01 00:00:00') TO ('2024-02-01 00:00:00');


ALTER TABLE public.event_log_y2024m01 OWNER TO postgres;

--
-- Name: event_log_y2024m02; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.event_log_y2024m02 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY public.event_log ATTACH PARTITION public.event_log_y2024m02 FOR VALUES FROM ('2024-02-01 00:00:00') TO ('2024-03-01 00:00:00');


ALTER TABLE public.event_log_y2024m02 OWNER TO postgres;

--
-- Name: event_log_y2024m03; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.event_log_y2024m03 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY public.event_log ATTACH PARTITION public.event_log_y2024m03 FOR VALUES FROM ('2024-03-01 00:00:00') TO ('2024-04-01 00:00:00');


ALTER TABLE public.event_log_y2024m03 OWNER TO postgres;

--
-- Name: event_log_y2024m04; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.event_log_y2024m04 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY public.event_log ATTACH PARTITION public.event_log_y2024m04 FOR VALUES FROM ('2024-04-01 00:00:00') TO ('2024-05-01 00:00:00');


ALTER TABLE public.event_log_y2024m04 OWNER TO postgres;

--
-- Name: event_log_y2024m05; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.event_log_y2024m05 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY public.event_log ATTACH PARTITION public.event_log_y2024m05 FOR VALUES FROM ('2024-05-01 00:00:00') TO ('2024-06-01 00:00:00');


ALTER TABLE public.event_log_y2024m05 OWNER TO postgres;

--
-- Name: event_log_y2024m06; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.event_log_y2024m06 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY public.event_log ATTACH PARTITION public.event_log_y2024m06 FOR VALUES FROM ('2024-06-01 00:00:00') TO ('2024-07-01 00:00:00');


ALTER TABLE public.event_log_y2024m06 OWNER TO postgres;

--
-- Name: event_log_y2024m07; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.event_log_y2024m07 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY public.event_log ATTACH PARTITION public.event_log_y2024m07 FOR VALUES FROM ('2024-07-01 00:00:00') TO ('2024-08-01 00:00:00');


ALTER TABLE public.event_log_y2024m07 OWNER TO postgres;

--
-- Name: event_log_y2024m08; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.event_log_y2024m08 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY public.event_log ATTACH PARTITION public.event_log_y2024m08 FOR VALUES FROM ('2024-08-01 00:00:00') TO ('2024-09-01 00:00:00');


ALTER TABLE public.event_log_y2024m08 OWNER TO postgres;

--
-- Name: event_log_y2024m09; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.event_log_y2024m09 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY public.event_log ATTACH PARTITION public.event_log_y2024m09 FOR VALUES FROM ('2024-09-01 00:00:00') TO ('2024-10-01 00:00:00');


ALTER TABLE public.event_log_y2024m09 OWNER TO postgres;

--
-- Name: event_log_y2024m10; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.event_log_y2024m10 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY public.event_log ATTACH PARTITION public.event_log_y2024m10 FOR VALUES FROM ('2024-10-01 00:00:00') TO ('2024-11-01 00:00:00');


ALTER TABLE public.event_log_y2024m10 OWNER TO postgres;

--
-- Name: event_log_y2024m11; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.event_log_y2024m11 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY public.event_log ATTACH PARTITION public.event_log_y2024m11 FOR VALUES FROM ('2024-11-01 00:00:00') TO ('2024-12-01 00:00:00');


ALTER TABLE public.event_log_y2024m11 OWNER TO postgres;

--
-- Name: event_log_y2024m12; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.event_log_y2024m12 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY public.event_log ATTACH PARTITION public.event_log_y2024m12 FOR VALUES FROM ('2024-12-01 00:00:00') TO ('2025-01-01 00:00:00');


ALTER TABLE public.event_log_y2024m12 OWNER TO postgres;

--
-- Name: event_log_y2025m01; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.event_log_y2025m01 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY public.event_log ATTACH PARTITION public.event_log_y2025m01 FOR VALUES FROM ('2025-01-01 00:00:00') TO ('2025-02-01 00:00:00');


ALTER TABLE public.event_log_y2025m01 OWNER TO postgres;

--
-- Name: event_log_y2025m02; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.event_log_y2025m02 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY public.event_log ATTACH PARTITION public.event_log_y2025m02 FOR VALUES FROM ('2025-02-01 00:00:00') TO ('2025-03-01 00:00:00');


ALTER TABLE public.event_log_y2025m02 OWNER TO postgres;

--
-- Name: event_log_y2025m03; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.event_log_y2025m03 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY public.event_log ATTACH PARTITION public.event_log_y2025m03 FOR VALUES FROM ('2025-03-01 00:00:00') TO ('2025-04-01 00:00:00');


ALTER TABLE public.event_log_y2025m03 OWNER TO postgres;

--
-- Name: event_log_y2025m04; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.event_log_y2025m04 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY public.event_log ATTACH PARTITION public.event_log_y2025m04 FOR VALUES FROM ('2025-04-01 00:00:00') TO ('2025-05-01 00:00:00');


ALTER TABLE public.event_log_y2025m04 OWNER TO postgres;

--
-- Name: event_log_y2025m05; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.event_log_y2025m05 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY public.event_log ATTACH PARTITION public.event_log_y2025m05 FOR VALUES FROM ('2025-05-01 00:00:00') TO ('2025-06-01 00:00:00');


ALTER TABLE public.event_log_y2025m05 OWNER TO postgres;

--
-- Name: event_log_y2025m06; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.event_log_y2025m06 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY public.event_log ATTACH PARTITION public.event_log_y2025m06 FOR VALUES FROM ('2025-06-01 00:00:00') TO ('2025-07-01 00:00:00');


ALTER TABLE public.event_log_y2025m06 OWNER TO postgres;

--
-- Name: event_log_y2025m07; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.event_log_y2025m07 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY public.event_log ATTACH PARTITION public.event_log_y2025m07 FOR VALUES FROM ('2025-07-01 00:00:00') TO ('2025-08-01 00:00:00');


ALTER TABLE public.event_log_y2025m07 OWNER TO postgres;

--
-- Name: event_log_y2025m08; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.event_log_y2025m08 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY public.event_log ATTACH PARTITION public.event_log_y2025m08 FOR VALUES FROM ('2025-08-01 00:00:00') TO ('2025-09-01 00:00:00');


ALTER TABLE public.event_log_y2025m08 OWNER TO postgres;

--
-- Name: event_log_y2025m09; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.event_log_y2025m09 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY public.event_log ATTACH PARTITION public.event_log_y2025m09 FOR VALUES FROM ('2025-09-01 00:00:00') TO ('2025-10-01 00:00:00');


ALTER TABLE public.event_log_y2025m09 OWNER TO postgres;

--
-- Name: event_log_y2025m10; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.event_log_y2025m10 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY public.event_log ATTACH PARTITION public.event_log_y2025m10 FOR VALUES FROM ('2025-10-01 00:00:00') TO ('2025-11-01 00:00:00');


ALTER TABLE public.event_log_y2025m10 OWNER TO postgres;

--
-- Name: event_log_y2025m11; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.event_log_y2025m11 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY public.event_log ATTACH PARTITION public.event_log_y2025m11 FOR VALUES FROM ('2025-11-01 00:00:00') TO ('2025-12-01 00:00:00');


ALTER TABLE public.event_log_y2025m11 OWNER TO postgres;

--
-- Name: event_log_y2025m12; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.event_log_y2025m12 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY public.event_log ATTACH PARTITION public.event_log_y2025m12 FOR VALUES FROM ('2025-12-01 00:00:00') TO ('2026-01-01 00:00:00');


ALTER TABLE public.event_log_y2025m12 OWNER TO postgres;

--
-- Name: event_log_y2026m01; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.event_log_y2026m01 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY public.event_log ATTACH PARTITION public.event_log_y2026m01 FOR VALUES FROM ('2026-01-01 00:00:00') TO ('2026-02-01 00:00:00');


ALTER TABLE public.event_log_y2026m01 OWNER TO postgres;

--
-- Name: event_log_y2026m02; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.event_log_y2026m02 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY public.event_log ATTACH PARTITION public.event_log_y2026m02 FOR VALUES FROM ('2026-02-01 00:00:00') TO ('2026-03-01 00:00:00');


ALTER TABLE public.event_log_y2026m02 OWNER TO postgres;

--
-- Name: event_log_y2026m03; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.event_log_y2026m03 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY public.event_log ATTACH PARTITION public.event_log_y2026m03 FOR VALUES FROM ('2026-03-01 00:00:00') TO ('2026-04-01 00:00:00');


ALTER TABLE public.event_log_y2026m03 OWNER TO postgres;

--
-- Name: event_log_y2026m04; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.event_log_y2026m04 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY public.event_log ATTACH PARTITION public.event_log_y2026m04 FOR VALUES FROM ('2026-04-01 00:00:00') TO ('2026-05-01 00:00:00');


ALTER TABLE public.event_log_y2026m04 OWNER TO postgres;

--
-- Name: event_log_y2026m05; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.event_log_y2026m05 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY public.event_log ATTACH PARTITION public.event_log_y2026m05 FOR VALUES FROM ('2026-05-01 00:00:00') TO ('2026-06-01 00:00:00');


ALTER TABLE public.event_log_y2026m05 OWNER TO postgres;

--
-- Name: event_log_y2026m06; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.event_log_y2026m06 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY public.event_log ATTACH PARTITION public.event_log_y2026m06 FOR VALUES FROM ('2026-06-01 00:00:00') TO ('2026-07-01 00:00:00');


ALTER TABLE public.event_log_y2026m06 OWNER TO postgres;

--
-- Name: event_log_y2026m07; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.event_log_y2026m07 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY public.event_log ATTACH PARTITION public.event_log_y2026m07 FOR VALUES FROM ('2026-07-01 00:00:00') TO ('2026-08-01 00:00:00');


ALTER TABLE public.event_log_y2026m07 OWNER TO postgres;

--
-- Name: event_log_y2026m08; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.event_log_y2026m08 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY public.event_log ATTACH PARTITION public.event_log_y2026m08 FOR VALUES FROM ('2026-08-01 00:00:00') TO ('2026-09-01 00:00:00');


ALTER TABLE public.event_log_y2026m08 OWNER TO postgres;

--
-- Name: event_log_y2026m09; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.event_log_y2026m09 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY public.event_log ATTACH PARTITION public.event_log_y2026m09 FOR VALUES FROM ('2026-09-01 00:00:00') TO ('2026-10-01 00:00:00');


ALTER TABLE public.event_log_y2026m09 OWNER TO postgres;

--
-- Name: event_log_y2026m10; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.event_log_y2026m10 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY public.event_log ATTACH PARTITION public.event_log_y2026m10 FOR VALUES FROM ('2026-10-01 00:00:00') TO ('2026-11-01 00:00:00');


ALTER TABLE public.event_log_y2026m10 OWNER TO postgres;

--
-- Name: event_log_y2026m11; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.event_log_y2026m11 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY public.event_log ATTACH PARTITION public.event_log_y2026m11 FOR VALUES FROM ('2026-11-01 00:00:00') TO ('2026-12-01 00:00:00');


ALTER TABLE public.event_log_y2026m11 OWNER TO postgres;

--
-- Name: event_log_y2026m12; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.event_log_y2026m12 (
    id uuid DEFAULT public.uuid_generate_v4() NOT NULL,
    table_name text NOT NULL,
    user_name text NOT NULL,
    "timestamp" timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    action text NOT NULL,
    original_data json,
    new_data json,
    query text NOT NULL
);
ALTER TABLE ONLY public.event_log ATTACH PARTITION public.event_log_y2026m12 FOR VALUES FROM ('2026-12-01 00:00:00') TO ('2027-01-01 00:00:00');


ALTER TABLE public.event_log_y2026m12 OWNER TO postgres;

--
-- Name: experiments; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.experiments (
    id bigint NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_by text NOT NULL,
    last_modified timestamp with time zone DEFAULT now() NOT NULL,
    name text NOT NULL,
    override_keys public.not_null_text[] NOT NULL,
    status public.experiment_status_type NOT NULL,
    traffic_percentage integer NOT NULL,
    context json NOT NULL,
    variants json NOT NULL,
    last_modified_by text DEFAULT 'Null'::text NOT NULL,
    chosen_variant text,
    CONSTRAINT experiments_traffic_percentage_check CHECK ((traffic_percentage >= 0))
);


ALTER TABLE public.experiments OWNER TO postgres;

--
-- Name: functions; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.functions (
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


ALTER TABLE public.functions OWNER TO postgres;

--
-- Name: type_templates; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.type_templates (
    type_name text NOT NULL,
    type_schema json NOT NULL,
    created_by text NOT NULL,
    created_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    last_modified_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    last_modified_by character varying(200) DEFAULT 'null'::character varying NOT NULL
);


ALTER TABLE public.type_templates OWNER TO postgres;

--
-- Name: config_versions; Type: TABLE; Schema: test_cac; Owner: postgres
--

CREATE TABLE test_cac.config_versions (
    id bigint NOT NULL,
    config json NOT NULL,
    config_hash text NOT NULL,
    tags character varying(100)[],
    created_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    CONSTRAINT config_versions_tags_check CHECK ((array_position(tags, NULL::character varying) IS NULL))
);


ALTER TABLE test_cac.config_versions OWNER TO postgres;

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
    override json DEFAULT '{}'::json NOT NULL,
    last_modified_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    last_modified_by character varying(200) DEFAULT 'null'::character varying NOT NULL
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
    function_name text,
    last_modified_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    last_modified_by character varying(200) DEFAULT 'null'::character varying NOT NULL
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
    function_name text,
    last_modified_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    last_modified_by character varying(200) DEFAULT 'null'::character varying NOT NULL
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
    draft_edited_by text NOT NULL,
    last_modified_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    last_modified_by character varying(200) DEFAULT 'null'::character varying NOT NULL
);


ALTER TABLE test_cac.functions OWNER TO postgres;

--
-- Name: type_templates; Type: TABLE; Schema: test_cac; Owner: postgres
--

CREATE TABLE test_cac.type_templates (
    type_name text NOT NULL,
    type_schema json NOT NULL,
    created_by text NOT NULL,
    created_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    last_modified_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    last_modified_by character varying(200) DEFAULT 'null'::character varying NOT NULL
);


ALTER TABLE test_cac.type_templates OWNER TO postgres;

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
-- Data for Name: config_versions; Type: TABLE DATA; Schema: dev_cac; Owner: postgres
--

COPY dev_cac.config_versions (id, config, config_hash, tags, created_at) FROM stdin;
7232358713959518208	{"contexts":[],"default_configs":{"base_rate":10.0},"overrides":{}}	a3a50ef5b9a9367bdd48fa4003b5f07686bcf67833085403c5fff5f93b82eba1	\N	2024-08-22 12:11:28.140378
7232358930557571072	{"contexts":[],"default_configs":{"base_rate":10.0,"currency":"INR"},"overrides":{}}	dc10e297ed72652a199e8ca90d460baa61889f171d8e8d9d738d336dafa312f8	\N	2024-08-22 12:12:19.784469
7232359004368932864	{"contexts":[],"default_configs":{"base_rate":10.0,"currency":"INR","per_distance_unit_rate":15.0},"overrides":{}}	ac9e3d933a912bb368f807dce0f9b2eda1d0d04de45e2f65eaada0b7f73f3618	\N	2024-08-22 12:12:37.376048
7232359073344262144	{"contexts":[],"default_configs":{"base_rate":10.0,"currency":"INR","distance_unit":"Km","per_distance_unit_rate":15.0},"overrides":{}}	53f07c85de4c1f0ca36dea05f777b371e5bd781c4f8f91f5413a73537a0f74e9	\N	2024-08-22 12:12:53.82093
7232359201773850624	{"contexts":[],"default_configs":{"base_rate":10.0,"currency":"INR","distance_unit":"Km","foo.foo":1,"per_distance_unit_rate":15.0},"overrides":{}}	b3eccaacd7e0e41e1dd1b5c98e3e13bc355e157da9c3ba0c2b5df6b2274d8413	\N	2024-08-22 12:13:24.441938
7232359257818140672	{"contexts":[],"default_configs":{"base_rate":10.0,"currency":"INR","distance_unit":"Km","foo.bar":2,"foo.foo":1,"per_distance_unit_rate":15.0},"overrides":{}}	5e78b646a4cf075a1077bb29428d89f5c2766a82574c667344f562bee6f7d35e	\N	2024-08-22 12:13:37.802393
7232359361463586816	{"contexts":[],"default_configs":{"base_rate":10.0,"currency":"INR","distance_unit":"Km","foo.bar":2,"foo.foo":1,"hello_message":"Hello World !!!","per_distance_unit_rate":15.0},"overrides":{}}	93b148788b8cdf5a53ff0838a2e4cc57e4639506d72cd3145012773bcc3160f6	\N	2024-08-22 12:14:02.515588
7232359465125810176	{"contexts":[],"default_configs":{"base_rate":10.0,"currency":"INR","distance_unit":"Km","foo.bar":2,"foo.foo":1,"hello_message":"Hello World !!!","hello_message_color":"black","per_distance_unit_rate":15.0},"overrides":{}}	c2868ac9f66cd4ff612abb0dcb9179fc21474935c1c7ee0782d52a344750c083	\N	2024-08-22 12:14:27.229587
7232359642309988352	{"contexts":[],"default_configs":{"base_rate":10.0,"currency":"INR","distance_unit":"Km","foo.bar":2,"foo.foo":1,"hello_message":"Hello World !!!","hello_message_color":"black","logo":"https://cdn1.vectorstock.com/i/1000x1000/03/20/big-city-logo-vector-20480320.jpg","per_distance_unit_rate":15.0},"overrides":{}}	ba7a77fa7075d31a5570b619819e1d7e8ba1501b37ca09c75219961383a66c4d	\N	2024-08-22 12:15:09.473512
7232360003494088704	{"contexts":[{"condition":{"==":[{"var":"city"},"Bangalore"]},"id":"11c6bd932b8888a9e50be8f822b76116fbaa20fd58b059f98b9e7179543bb9a4","override_with_keys":["be6a0ddbb08dae3007756cdc2be0676b94328b339f23cdfed5401b501c2c92fa"],"priority":4}],"default_configs":{"base_rate":10.0,"currency":"INR","distance_unit":"Km","foo.bar":2,"foo.foo":1,"hello_message":"Hello World !!!","hello_message_color":"black","logo":"https://cdn1.vectorstock.com/i/1000x1000/03/20/big-city-logo-vector-20480320.jpg","per_distance_unit_rate":15.0},"overrides":{"be6a0ddbb08dae3007756cdc2be0676b94328b339f23cdfed5401b501c2c92fa":{"base_rate":20,"distance_unit":"Km","hello_message":"ನಮಸ್ಕಾರ ಬೆಂಗಳೂರು","hello_message_color":"red","logo":"\\thttps://www.shutterstock.com/image-vector/bangalore-logo-vidhana-soudha-600nw-1506258893.jpg","per_distance_unit_rate":15}}}	cd1fb4ac34f5aebb0d5d868b6e0ed9344b8778606dc3295fa9e9e62b95d582cd	\N	2024-08-22 12:16:35.58566
7232360038369726464	{"contexts":[{"condition":{"==":[{"var":"city"},"Bangalore"]},"id":"11c6bd932b8888a9e50be8f822b76116fbaa20fd58b059f98b9e7179543bb9a4","override_with_keys":["2a2eb08ced59872063128fc531b17a04e0e3be31d565b57273508008b207636a"],"priority":4}],"default_configs":{"base_rate":10.0,"currency":"INR","distance_unit":"Km","foo.bar":2,"foo.foo":1,"hello_message":"Hello World !!!","hello_message_color":"black","logo":"https://cdn1.vectorstock.com/i/1000x1000/03/20/big-city-logo-vector-20480320.jpg","per_distance_unit_rate":15.0},"overrides":{"2a2eb08ced59872063128fc531b17a04e0e3be31d565b57273508008b207636a":{"base_rate":20,"distance_unit":"Km","hello_message":"ನಮಸ್ಕಾರ ಬೆಂಗಳೂರು","hello_message_color":"red","logo":"https://www.shutterstock.com/image-vector/bangalore-logo-vidhana-soudha-600nw-1506258893.jpg","per_distance_unit_rate":15}}}	5caf05c91b6a65c061786dfb0c864a57a1e93d764c4411a9f3977fc9a72192c7	\N	2024-08-22 12:16:43.900492
7232364985974919168	{"contexts":[{"condition":{"==":[{"var":"city"},"Bangalore"]},"id":"11c6bd932b8888a9e50be8f822b76116fbaa20fd58b059f98b9e7179543bb9a4","override_with_keys":["2a2eb08ced59872063128fc531b17a04e0e3be31d565b57273508008b207636a"],"priority":4},{"condition":{"==":[{"var":"city"},"Chennai"]},"id":"e47edbe36bf3333f42ca2ded0a4604997adc4f6e0bf6176079272597c01353d8","override_with_keys":["645b12a924823be87eb423791f3c42393ab19103afcd8ce923c4a1112408b32b"],"priority":4}],"default_configs":{"base_rate":10.0,"currency":"INR","distance_unit":"Km","foo.bar":2,"foo.foo":1,"hello_message":"Hello World !!!","hello_message_color":"black","logo":"https://cdn1.vectorstock.com/i/1000x1000/03/20/big-city-logo-vector-20480320.jpg","per_distance_unit_rate":15.0},"overrides":{"2a2eb08ced59872063128fc531b17a04e0e3be31d565b57273508008b207636a":{"base_rate":20,"distance_unit":"Km","hello_message":"ನಮಸ್ಕಾರ ಬೆಂಗಳೂರು","hello_message_color":"red","logo":"https://www.shutterstock.com/image-vector/bangalore-logo-vidhana-soudha-600nw-1506258893.jpg","per_distance_unit_rate":15},"645b12a924823be87eb423791f3c42393ab19103afcd8ce923c4a1112408b32b":{"hello_message":"வணக்கம் சென்னை","hello_message_color":"green","logo":"https://www.shutterstock.com/image-vector/chennai-skyline-color-landmarks-blue-260nw-515862346.jpg"}}}	7cf9194bcaa8a633264af875b43b1a44c36e67210c2c48652400db633da85be0	\N	2024-08-22 12:36:23.503558
7232365274614337536	{"contexts":[{"condition":{"==":[{"var":"city"},"Bangalore"]},"id":"11c6bd932b8888a9e50be8f822b76116fbaa20fd58b059f98b9e7179543bb9a4","override_with_keys":["2a2eb08ced59872063128fc531b17a04e0e3be31d565b57273508008b207636a"],"priority":4},{"condition":{"==":[{"var":"city"},"Chennai"]},"id":"e47edbe36bf3333f42ca2ded0a4604997adc4f6e0bf6176079272597c01353d8","override_with_keys":["645b12a924823be87eb423791f3c42393ab19103afcd8ce923c4a1112408b32b"],"priority":4},{"condition":{"==":[{"var":"city"},"Seattle"]},"id":"3f0e5fe95e2e758151d4cef712185767a137c926a9e7156b6face3a98b167d35","override_with_keys":["f73412831793a34df7df95a79bc65754b1eebf31534e4ccfa9c79681fcf8ac69"],"priority":4}],"default_configs":{"base_rate":10.0,"currency":"INR","distance_unit":"Km","foo.bar":2,"foo.foo":1,"hello_message":"Hello World !!!","hello_message_color":"black","logo":"https://cdn1.vectorstock.com/i/1000x1000/03/20/big-city-logo-vector-20480320.jpg","per_distance_unit_rate":15.0},"overrides":{"2a2eb08ced59872063128fc531b17a04e0e3be31d565b57273508008b207636a":{"base_rate":20,"distance_unit":"Km","hello_message":"ನಮಸ್ಕಾರ ಬೆಂಗಳೂರು","hello_message_color":"red","logo":"https://www.shutterstock.com/image-vector/bangalore-logo-vidhana-soudha-600nw-1506258893.jpg","per_distance_unit_rate":15},"645b12a924823be87eb423791f3c42393ab19103afcd8ce923c4a1112408b32b":{"hello_message":"வணக்கம் சென்னை","hello_message_color":"green","logo":"https://www.shutterstock.com/image-vector/chennai-skyline-color-landmarks-blue-260nw-515862346.jpg"},"f73412831793a34df7df95a79bc65754b1eebf31534e4ccfa9c79681fcf8ac69":{"base_rate":5,"currency":"USD","distance_unit":"Miles","hello_message":"Hello Seattle","hello_message_color":"blue","logo":"https://t4.ftcdn.net/jpg/04/24/15/07/360_F_424150716_kDZIgUONDIKpIhHqsxlDcVIiglyjIOQs.jpg","per_distance_unit_rate":2.5}}}	73c39f8ed9b6a7d7bda339f9f393576e83fef4a45600c135b1fddddb6bfcecc6	\N	2024-08-22 12:37:32.321073
7232365362266902528	{"contexts":[{"condition":{"==":[{"var":"city"},"Bangalore"]},"id":"11c6bd932b8888a9e50be8f822b76116fbaa20fd58b059f98b9e7179543bb9a4","override_with_keys":["2a2eb08ced59872063128fc531b17a04e0e3be31d565b57273508008b207636a"],"priority":4},{"condition":{"==":[{"var":"city"},"Chennai"]},"id":"e47edbe36bf3333f42ca2ded0a4604997adc4f6e0bf6176079272597c01353d8","override_with_keys":["645b12a924823be87eb423791f3c42393ab19103afcd8ce923c4a1112408b32b"],"priority":4},{"condition":{"==":[{"var":"city"},"Seattle"]},"id":"3f0e5fe95e2e758151d4cef712185767a137c926a9e7156b6face3a98b167d35","override_with_keys":["f73412831793a34df7df95a79bc65754b1eebf31534e4ccfa9c79681fcf8ac69"],"priority":4},{"condition":{"and":[{"==":[{"var":"city"},"Bangalore"]},{"==":[{"var":"vehicle_type"},"cab"]}]},"id":"9fb61ecfb662467e9036ec79bfd8c02ac244c1836f113242aec5fe6d9265d8dc","override_with_keys":["e2411e811b927824f910f9edd75b984a3ad226c46a1216ffa735b7bda60c1ede"],"priority":6}],"default_configs":{"base_rate":10.0,"currency":"INR","distance_unit":"Km","foo.bar":2,"foo.foo":1,"hello_message":"Hello World !!!","hello_message_color":"black","logo":"https://cdn1.vectorstock.com/i/1000x1000/03/20/big-city-logo-vector-20480320.jpg","per_distance_unit_rate":15.0},"overrides":{"2a2eb08ced59872063128fc531b17a04e0e3be31d565b57273508008b207636a":{"base_rate":20,"distance_unit":"Km","hello_message":"ನಮಸ್ಕಾರ ಬೆಂಗಳೂರು","hello_message_color":"red","logo":"https://www.shutterstock.com/image-vector/bangalore-logo-vidhana-soudha-600nw-1506258893.jpg","per_distance_unit_rate":15},"645b12a924823be87eb423791f3c42393ab19103afcd8ce923c4a1112408b32b":{"hello_message":"வணக்கம் சென்னை","hello_message_color":"green","logo":"https://www.shutterstock.com/image-vector/chennai-skyline-color-landmarks-blue-260nw-515862346.jpg"},"e2411e811b927824f910f9edd75b984a3ad226c46a1216ffa735b7bda60c1ede":{"base_rate":12},"f73412831793a34df7df95a79bc65754b1eebf31534e4ccfa9c79681fcf8ac69":{"base_rate":5,"currency":"USD","distance_unit":"Miles","hello_message":"Hello Seattle","hello_message_color":"blue","logo":"https://t4.ftcdn.net/jpg/04/24/15/07/360_F_424150716_kDZIgUONDIKpIhHqsxlDcVIiglyjIOQs.jpg","per_distance_unit_rate":2.5}}}	f31a2b17519f38b5d859faf645da70e323566c3bca437a496453da0aa6f9e4bb	\N	2024-08-22 12:37:53.219121
\.


--
-- Data for Name: contexts; Type: TABLE DATA; Schema: dev_cac; Owner: postgres
--

COPY dev_cac.contexts (id, value, override_id, created_at, created_by, priority, override, last_modified_at, last_modified_by) FROM stdin;
11c6bd932b8888a9e50be8f822b76116fbaa20fd58b059f98b9e7179543bb9a4	{"==":[{"var":"city"},"Bangalore"]}	2a2eb08ced59872063128fc531b17a04e0e3be31d565b57273508008b207636a	2024-08-22 12:16:35.579131+00	user@superposition.io	4	{"base_rate":20,"distance_unit":"Km","hello_message":"ನಮಸ್ಕಾರ ಬೆಂಗಳೂರು","hello_message_color":"red","logo":"https://www.shutterstock.com/image-vector/bangalore-logo-vidhana-soudha-600nw-1506258893.jpg","per_distance_unit_rate":15}	2024-08-22 12:16:43.897928	user@superposition.io
e47edbe36bf3333f42ca2ded0a4604997adc4f6e0bf6176079272597c01353d8	{"==":[{"var":"city"},"Chennai"]}	645b12a924823be87eb423791f3c42393ab19103afcd8ce923c4a1112408b32b	2024-08-22 12:36:23.489214+00	user@superposition.io	4	{"hello_message":"வணக்கம் சென்னை","hello_message_color":"green","logo":"https://www.shutterstock.com/image-vector/chennai-skyline-color-landmarks-blue-260nw-515862346.jpg"}	2024-08-22 12:36:23.489216	user@superposition.io
3f0e5fe95e2e758151d4cef712185767a137c926a9e7156b6face3a98b167d35	{"==":[{"var":"city"},"Seattle"]}	f73412831793a34df7df95a79bc65754b1eebf31534e4ccfa9c79681fcf8ac69	2024-08-22 12:37:32.306949+00	user@superposition.io	4	{"base_rate":5,"currency":"USD","distance_unit":"Miles","hello_message":"Hello Seattle","hello_message_color":"blue","logo":"https://t4.ftcdn.net/jpg/04/24/15/07/360_F_424150716_kDZIgUONDIKpIhHqsxlDcVIiglyjIOQs.jpg","per_distance_unit_rate":2.5}	2024-08-22 12:37:32.306953	user@superposition.io
9fb61ecfb662467e9036ec79bfd8c02ac244c1836f113242aec5fe6d9265d8dc	{"and":[{"==":[{"var":"city"},"Bangalore"]},{"==":[{"var":"vehicle_type"},"cab"]}]}	e2411e811b927824f910f9edd75b984a3ad226c46a1216ffa735b7bda60c1ede	2024-08-22 12:37:53.206065+00	user@superposition.io	6	{"base_rate":12}	2024-08-22 12:37:53.206068	user@superposition.io
\.


--
-- Data for Name: default_configs; Type: TABLE DATA; Schema: dev_cac; Owner: postgres
--

COPY dev_cac.default_configs (key, value, created_at, created_by, schema, function_name, last_modified_at, last_modified_by) FROM stdin;
base_rate	10.0	2024-08-22 12:11:28.122453+00	user@superposition.io	{"type":"number"}	\N	2024-08-22 12:11:28.122802	user@superposition.io
currency	"INR"	2024-08-22 12:12:19.765579+00	user@superposition.io	{"enum":["INR","USD","EUR"],"type":"string"}	\N	2024-08-22 12:12:19.765591	user@superposition.io
per_distance_unit_rate	15.0	2024-08-22 12:12:37.370436+00	user@superposition.io	{"type":"number"}	\N	2024-08-22 12:12:37.370446	user@superposition.io
distance_unit	"Km"	2024-08-22 12:12:53.814292+00	user@superposition.io	{"enum":["Km","Miles"],"type":"string"}	\N	2024-08-22 12:12:53.814303	user@superposition.io
foo.foo	1	2024-08-22 12:13:24.43122+00	user@superposition.io	{"type":"integer"}	\N	2024-08-22 12:13:24.431231	user@superposition.io
foo.bar	2	2024-08-22 12:13:37.788173+00	user@superposition.io	{"type":"number"}	\N	2024-08-22 12:13:37.788184	user@superposition.io
hello_message	"Hello World !!!"	2024-08-22 12:14:02.505322+00	user@superposition.io	{"pattern":".*","type":"string"}	\N	2024-08-22 12:14:02.505334	user@superposition.io
hello_message_color	"black"	2024-08-22 12:14:27.222544+00	user@superposition.io	{"enum":["black","red","blue","green","pink"],"type":"string"}	\N	2024-08-22 12:14:27.222551	user@superposition.io
logo	"https://cdn1.vectorstock.com/i/1000x1000/03/20/big-city-logo-vector-20480320.jpg"	2024-08-22 12:15:08.701643+00	user@superposition.io	{"pattern":"https://.*","type":"string"}	axios_validator_example	2024-08-22 12:15:08.701648	user@superposition.io
\.


--
-- Data for Name: dimensions; Type: TABLE DATA; Schema: dev_cac; Owner: postgres
--

COPY dev_cac.dimensions (dimension, priority, created_at, created_by, schema, function_name, last_modified_at, last_modified_by) FROM stdin;
variantIds	1	2024-08-22 12:05:35.298665+00	user@example.com	{"type": "string","pattern": ".*"}	\N	2024-08-22 12:05:35.298665	null
vehicle_type	2	2024-08-22 12:08:29.152994+00	user@superposition.io	{"enum":["cab","auto"],"type":"string"}	\N	2024-08-22 12:08:29.153249	user@superposition.io
city	4	2024-08-22 12:08:56.435259+00	user@superposition.io	{"pattern":"[a-zA-Z ]+","type":"string"}	\N	2024-08-22 12:08:56.435264	user@superposition.io
hour_of_day	32	2024-08-22 12:09:12.112655+00	user@superposition.io	{"type":"number"}	\N	2024-08-22 12:09:12.112665	user@superposition.io
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
793b78ad-e6b9-4110-ad2c-0327ec39a298	dimensions	postgres	2024-08-22 12:05:35.298665	INSERT	\N	{"dimension":"variantIds","priority":1,"created_at":"2024-08-22T12:05:35.298665+00:00","created_by":"user@example.com","schema":{"type": "string","pattern": ".*"},"function_name":null,"last_modified_at":"2024-08-22T12:05:35.298665","last_modified_by":"null"}	INSERT INTO dev_cac.dimensions (dimension, priority, created_at, created_by, schema, function_name) VALUES ('variantIds', 1, CURRENT_TIMESTAMP, 'user@example.com', '{"type": "string","pattern": ".*"}'::json, null);
2fba7469-1df3-4f76-aa73-c0debf40f05c	dimensions	postgres	2024-08-22 12:08:29.15758	INSERT	\N	{"dimension":"vehicle_type","priority":2,"created_at":"2024-08-22T12:08:29.152994+00:00","created_by":"user@superposition.io","schema":{"enum":["cab","auto"],"type":"string"},"function_name":null,"last_modified_at":"2024-08-22T12:08:29.153249","last_modified_by":"user@superposition.io"}	INSERT INTO "dimensions" ("dimension", "priority", "created_at", "created_by", "schema", "function_name", "last_modified_at", "last_modified_by") VALUES ($1, $2, $3, $4, $5, DEFAULT, $6, $7) ON CONFLICT ("dimension") DO UPDATE SET "priority" = $8, "created_at" = $9, "created_by" = $10, "schema" = $11, "function_name" = $12, "last_modified_at" = $13, "last_modified_by" = $14 RETURNING "dimensions"."dimension", "dimensions"."priority", "dimensions"."created_at", "dimensions"."created_by", "dimensions"."schema", "dimensions"."function_name", "dimensions"."last_modified_at", "dimensions"."last_modified_by"
f495f584-366b-4564-9deb-8b1ad1f064c1	dimensions	postgres	2024-08-22 12:08:56.442895	INSERT	\N	{"dimension":"city","priority":4,"created_at":"2024-08-22T12:08:56.435259+00:00","created_by":"user@superposition.io","schema":{"pattern":"[a-zA-Z ]+","type":"string"},"function_name":null,"last_modified_at":"2024-08-22T12:08:56.435264","last_modified_by":"user@superposition.io"}	INSERT INTO "dimensions" ("dimension", "priority", "created_at", "created_by", "schema", "function_name", "last_modified_at", "last_modified_by") VALUES ($1, $2, $3, $4, $5, DEFAULT, $6, $7) ON CONFLICT ("dimension") DO UPDATE SET "priority" = $8, "created_at" = $9, "created_by" = $10, "schema" = $11, "function_name" = $12, "last_modified_at" = $13, "last_modified_by" = $14 RETURNING "dimensions"."dimension", "dimensions"."priority", "dimensions"."created_at", "dimensions"."created_by", "dimensions"."schema", "dimensions"."function_name", "dimensions"."last_modified_at", "dimensions"."last_modified_by"
7b9adcc7-7117-4f89-8e69-ffebf66b1c28	dimensions	postgres	2024-08-22 12:09:12.116893	INSERT	\N	{"dimension":"hour_of_day","priority":32,"created_at":"2024-08-22T12:09:12.112655+00:00","created_by":"user@superposition.io","schema":{"type":"number"},"function_name":null,"last_modified_at":"2024-08-22T12:09:12.112665","last_modified_by":"user@superposition.io"}	INSERT INTO "dimensions" ("dimension", "priority", "created_at", "created_by", "schema", "function_name", "last_modified_at", "last_modified_by") VALUES ($1, $2, $3, $4, $5, DEFAULT, $6, $7) ON CONFLICT ("dimension") DO UPDATE SET "priority" = $8, "created_at" = $9, "created_by" = $10, "schema" = $11, "function_name" = $12, "last_modified_at" = $13, "last_modified_by" = $14 RETURNING "dimensions"."dimension", "dimensions"."priority", "dimensions"."created_at", "dimensions"."created_by", "dimensions"."schema", "dimensions"."function_name", "dimensions"."last_modified_at", "dimensions"."last_modified_by"
80468043-5662-4330-b562-14e1410bb1ba	functions	postgres	2024-08-22 12:10:44.420382	INSERT	\N	{"function_name":"axios_validator_example","published_code":null,"draft_code":"YXN5bmMgZnVuY3Rpb24gdmFsaWRhdGUoa2V5LCB2YWx1ZSkgewogICAgcmV0dXJuIGF4aW9zLmdldCh2YWx1ZSkudGhlbihyZXNwb25zZSA9PiByZXNwb25zZS5zdGF0dXMgPT0gJzIwMCcpCiAgICAuY2F0Y2goZXJyID0+IHsKICAgICAgICBjb25zb2xlLmxvZyhlcnIpOwogICAgICAgIHJldHVybiBmYWxzZTsKICAgIH0pOwp9Cg==","function_description":"An example function that shows off validator functions","published_runtime_version":null,"draft_runtime_version":"1.0.0","published_at":null,"draft_edited_at":"2024-08-22T12:10:44.416185","published_by":null,"draft_edited_by":"user@superposition.io","last_modified_at":"2024-08-22T12:10:44.416187","last_modified_by":"user@superposition.io"}	INSERT INTO "functions" ("function_name", "published_code", "draft_code", "function_description", "published_runtime_version", "draft_runtime_version", "published_at", "draft_edited_at", "published_by", "draft_edited_by", "last_modified_at", "last_modified_by") VALUES ($1, DEFAULT, $2, $3, DEFAULT, $4, DEFAULT, $5, DEFAULT, $6, $7, $8) RETURNING "functions"."function_name", "functions"."published_code", "functions"."draft_code", "functions"."function_description", "functions"."published_runtime_version", "functions"."draft_runtime_version", "functions"."published_at", "functions"."draft_edited_at", "functions"."published_by", "functions"."draft_edited_by", "functions"."last_modified_at", "functions"."last_modified_by"
76df90b7-bb2b-45f3-aceb-8c980b3f5801	functions	postgres	2024-08-22 12:10:48.788942	UPDATE	{"function_name":"axios_validator_example","published_code":null,"draft_code":"YXN5bmMgZnVuY3Rpb24gdmFsaWRhdGUoa2V5LCB2YWx1ZSkgewogICAgcmV0dXJuIGF4aW9zLmdldCh2YWx1ZSkudGhlbihyZXNwb25zZSA9PiByZXNwb25zZS5zdGF0dXMgPT0gJzIwMCcpCiAgICAuY2F0Y2goZXJyID0+IHsKICAgICAgICBjb25zb2xlLmxvZyhlcnIpOwogICAgICAgIHJldHVybiBmYWxzZTsKICAgIH0pOwp9Cg==","function_description":"An example function that shows off validator functions","published_runtime_version":null,"draft_runtime_version":"1.0.0","published_at":null,"draft_edited_at":"2024-08-22T12:10:44.416185","published_by":null,"draft_edited_by":"user@superposition.io","last_modified_at":"2024-08-22T12:10:44.416187","last_modified_by":"user@superposition.io"}	{"function_name":"axios_validator_example","published_code":"YXN5bmMgZnVuY3Rpb24gdmFsaWRhdGUoa2V5LCB2YWx1ZSkgewogICAgcmV0dXJuIGF4aW9zLmdldCh2YWx1ZSkudGhlbihyZXNwb25zZSA9PiByZXNwb25zZS5zdGF0dXMgPT0gJzIwMCcpCiAgICAuY2F0Y2goZXJyID0+IHsKICAgICAgICBjb25zb2xlLmxvZyhlcnIpOwogICAgICAgIHJldHVybiBmYWxzZTsKICAgIH0pOwp9Cg==","draft_code":"YXN5bmMgZnVuY3Rpb24gdmFsaWRhdGUoa2V5LCB2YWx1ZSkgewogICAgcmV0dXJuIGF4aW9zLmdldCh2YWx1ZSkudGhlbihyZXNwb25zZSA9PiByZXNwb25zZS5zdGF0dXMgPT0gJzIwMCcpCiAgICAuY2F0Y2goZXJyID0+IHsKICAgICAgICBjb25zb2xlLmxvZyhlcnIpOwogICAgICAgIHJldHVybiBmYWxzZTsKICAgIH0pOwp9Cg==","function_description":"An example function that shows off validator functions","published_runtime_version":"1.0.0","draft_runtime_version":"1.0.0","published_at":"2024-08-22T12:10:48.787527","draft_edited_at":"2024-08-22T12:10:44.416185","published_by":"user@superposition.io","draft_edited_by":"user@superposition.io","last_modified_at":"2024-08-22T12:10:44.416187","last_modified_by":"user@superposition.io"}	UPDATE "functions" SET "published_code" = $1, "published_runtime_version" = $2, "published_by" = $3, "published_at" = $4 WHERE ("functions"."function_name" = $5) RETURNING "functions"."function_name", "functions"."published_code", "functions"."draft_code", "functions"."function_description", "functions"."published_runtime_version", "functions"."draft_runtime_version", "functions"."published_at", "functions"."draft_edited_at", "functions"."published_by", "functions"."draft_edited_by", "functions"."last_modified_at", "functions"."last_modified_by"
0331b8d6-f34d-436f-9991-05bae112ae5e	default_configs	postgres	2024-08-22 12:11:28.124147	INSERT	\N	{"key":"base_rate","value":10.0,"created_at":"2024-08-22T12:11:28.122453+00:00","created_by":"user@superposition.io","schema":{"type":"number"},"function_name":null,"last_modified_at":"2024-08-22T12:11:28.122802","last_modified_by":"user@superposition.io"}	INSERT INTO "default_configs" ("key", "value", "created_at", "created_by", "schema", "function_name", "last_modified_at", "last_modified_by") VALUES ($1, $2, $3, $4, $5, DEFAULT, $6, $7) ON CONFLICT ("key") DO UPDATE SET "value" = $8, "created_at" = $9, "created_by" = $10, "schema" = $11, "function_name" = $12, "last_modified_at" = $13, "last_modified_by" = $14
520aee64-d0c6-4728-990e-85b22fa3d8a1	default_configs	postgres	2024-08-22 12:12:19.767051	INSERT	\N	{"key":"currency","value":"INR","created_at":"2024-08-22T12:12:19.765579+00:00","created_by":"user@superposition.io","schema":{"enum":["INR","USD","EUR"],"type":"string"},"function_name":null,"last_modified_at":"2024-08-22T12:12:19.765591","last_modified_by":"user@superposition.io"}	INSERT INTO "default_configs" ("key", "value", "created_at", "created_by", "schema", "function_name", "last_modified_at", "last_modified_by") VALUES ($1, $2, $3, $4, $5, DEFAULT, $6, $7) ON CONFLICT ("key") DO UPDATE SET "value" = $8, "created_at" = $9, "created_by" = $10, "schema" = $11, "function_name" = $12, "last_modified_at" = $13, "last_modified_by" = $14
658acd0e-290c-4905-9aa1-626624ed49b2	default_configs	postgres	2024-08-22 12:12:53.815329	INSERT	\N	{"key":"distance_unit","value":"Km","created_at":"2024-08-22T12:12:53.814292+00:00","created_by":"user@superposition.io","schema":{"enum":["Km","Miles"],"type":"string"},"function_name":null,"last_modified_at":"2024-08-22T12:12:53.814303","last_modified_by":"user@superposition.io"}	INSERT INTO "default_configs" ("key", "value", "created_at", "created_by", "schema", "function_name", "last_modified_at", "last_modified_by") VALUES ($1, $2, $3, $4, $5, DEFAULT, $6, $7) ON CONFLICT ("key") DO UPDATE SET "value" = $8, "created_at" = $9, "created_by" = $10, "schema" = $11, "function_name" = $12, "last_modified_at" = $13, "last_modified_by" = $14
4e267194-f1ec-4e5b-88b2-5f75388400b6	default_configs	postgres	2024-08-22 12:14:27.222883	INSERT	\N	{"key":"hello_message_color","value":"black","created_at":"2024-08-22T12:14:27.222544+00:00","created_by":"user@superposition.io","schema":{"enum":["black","red","blue","green","pink"],"type":"string"},"function_name":null,"last_modified_at":"2024-08-22T12:14:27.222551","last_modified_by":"user@superposition.io"}	INSERT INTO "default_configs" ("key", "value", "created_at", "created_by", "schema", "function_name", "last_modified_at", "last_modified_by") VALUES ($1, $2, $3, $4, $5, DEFAULT, $6, $7) ON CONFLICT ("key") DO UPDATE SET "value" = $8, "created_at" = $9, "created_by" = $10, "schema" = $11, "function_name" = $12, "last_modified_at" = $13, "last_modified_by" = $14
f8290731-67b3-4a94-9985-5335aa309056	default_configs	postgres	2024-08-22 12:12:37.371593	INSERT	\N	{"key":"per_distance_unit_rate","value":15.0,"created_at":"2024-08-22T12:12:37.370436+00:00","created_by":"user@superposition.io","schema":{"type":"number"},"function_name":null,"last_modified_at":"2024-08-22T12:12:37.370446","last_modified_by":"user@superposition.io"}	INSERT INTO "default_configs" ("key", "value", "created_at", "created_by", "schema", "function_name", "last_modified_at", "last_modified_by") VALUES ($1, $2, $3, $4, $5, DEFAULT, $6, $7) ON CONFLICT ("key") DO UPDATE SET "value" = $8, "created_at" = $9, "created_by" = $10, "schema" = $11, "function_name" = $12, "last_modified_at" = $13, "last_modified_by" = $14
aafbb4dc-5aea-469c-8d63-ecdbe08bc111	default_configs	postgres	2024-08-22 12:13:24.430688	INSERT	\N	{"key":"foo.foo","value":1,"created_at":"2024-08-22T12:13:24.43122+00:00","created_by":"user@superposition.io","schema":{"type":"integer"},"function_name":null,"last_modified_at":"2024-08-22T12:13:24.431231","last_modified_by":"user@superposition.io"}	INSERT INTO "default_configs" ("key", "value", "created_at", "created_by", "schema", "function_name", "last_modified_at", "last_modified_by") VALUES ($1, $2, $3, $4, $5, DEFAULT, $6, $7) ON CONFLICT ("key") DO UPDATE SET "value" = $8, "created_at" = $9, "created_by" = $10, "schema" = $11, "function_name" = $12, "last_modified_at" = $13, "last_modified_by" = $14
b5cb1349-5048-4eb9-8178-4c82d0fa3895	default_configs	postgres	2024-08-22 12:13:37.788889	INSERT	\N	{"key":"foo.bar","value":2,"created_at":"2024-08-22T12:13:37.788173+00:00","created_by":"user@superposition.io","schema":{"type":"number"},"function_name":null,"last_modified_at":"2024-08-22T12:13:37.788184","last_modified_by":"user@superposition.io"}	INSERT INTO "default_configs" ("key", "value", "created_at", "created_by", "schema", "function_name", "last_modified_at", "last_modified_by") VALUES ($1, $2, $3, $4, $5, DEFAULT, $6, $7) ON CONFLICT ("key") DO UPDATE SET "value" = $8, "created_at" = $9, "created_by" = $10, "schema" = $11, "function_name" = $12, "last_modified_at" = $13, "last_modified_by" = $14
a0e4d158-38b2-44f3-a9cd-650190510100	default_configs	postgres	2024-08-22 12:14:02.508721	INSERT	\N	{"key":"hello_message","value":"Hello World !!!","created_at":"2024-08-22T12:14:02.505322+00:00","created_by":"user@superposition.io","schema":{"pattern":".*","type":"string"},"function_name":null,"last_modified_at":"2024-08-22T12:14:02.505334","last_modified_by":"user@superposition.io"}	INSERT INTO "default_configs" ("key", "value", "created_at", "created_by", "schema", "function_name", "last_modified_at", "last_modified_by") VALUES ($1, $2, $3, $4, $5, DEFAULT, $6, $7) ON CONFLICT ("key") DO UPDATE SET "value" = $8, "created_at" = $9, "created_by" = $10, "schema" = $11, "function_name" = $12, "last_modified_at" = $13, "last_modified_by" = $14
4e248746-a41d-4968-84e0-5d64dc89f1dc	default_configs	postgres	2024-08-22 12:15:09.467582	INSERT	\N	{"key":"logo","value":"https://cdn1.vectorstock.com/i/1000x1000/03/20/big-city-logo-vector-20480320.jpg","created_at":"2024-08-22T12:15:08.701643+00:00","created_by":"user@superposition.io","schema":{"pattern":"https://.*","type":"string"},"function_name":"axios_validator_example","last_modified_at":"2024-08-22T12:15:08.701648","last_modified_by":"user@superposition.io"}	INSERT INTO "default_configs" ("key", "value", "created_at", "created_by", "schema", "function_name", "last_modified_at", "last_modified_by") VALUES ($1, $2, $3, $4, $5, $6, $7, $8) ON CONFLICT ("key") DO UPDATE SET "value" = $9, "created_at" = $10, "created_by" = $11, "schema" = $12, "function_name" = $13, "last_modified_at" = $14, "last_modified_by" = $15
eca10992-d451-49ae-9090-fad34a7c937d	contexts	postgres	2024-08-22 12:16:35.103289	INSERT	\N	{"id":"11c6bd932b8888a9e50be8f822b76116fbaa20fd58b059f98b9e7179543bb9a4","value":{"==":[{"var":"city"},"Bangalore"]},"override_id":"be6a0ddbb08dae3007756cdc2be0676b94328b339f23cdfed5401b501c2c92fa","created_at":"2024-08-22T12:16:35.579131+00:00","created_by":"user@superposition.io","priority":4,"override":{"base_rate":20,"distance_unit":"Km","hello_message":"ನಮಸ್ಕಾರ ಬೆಂಗಳೂರು","hello_message_color":"red","logo":"\\thttps://www.shutterstock.com/image-vector/bangalore-logo-vidhana-soudha-600nw-1506258893.jpg","per_distance_unit_rate":15},"last_modified_at":"2024-08-22T12:16:35.579132","last_modified_by":"user@superposition.io"}	INSERT INTO "contexts" ("id", "value", "override_id", "created_at", "created_by", "priority", "override", "last_modified_at", "last_modified_by") VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9)
4d35ebb2-8d1f-47d7-b692-ce9dc2f3cbf9	contexts	postgres	2024-08-22 12:16:43.5698	UPDATE	{"id":"11c6bd932b8888a9e50be8f822b76116fbaa20fd58b059f98b9e7179543bb9a4","value":{"==":[{"var":"city"},"Bangalore"]},"override_id":"be6a0ddbb08dae3007756cdc2be0676b94328b339f23cdfed5401b501c2c92fa","created_at":"2024-08-22T12:16:35.579131+00:00","created_by":"user@superposition.io","priority":4,"override":{"base_rate":20,"distance_unit":"Km","hello_message":"ನಮಸ್ಕಾರ ಬೆಂಗಳೂರು","hello_message_color":"red","logo":"\\thttps://www.shutterstock.com/image-vector/bangalore-logo-vidhana-soudha-600nw-1506258893.jpg","per_distance_unit_rate":15},"last_modified_at":"2024-08-22T12:16:35.579132","last_modified_by":"user@superposition.io"}	{"id":"11c6bd932b8888a9e50be8f822b76116fbaa20fd58b059f98b9e7179543bb9a4","value":{"==":[{"var":"city"},"Bangalore"]},"override_id":"2a2eb08ced59872063128fc531b17a04e0e3be31d565b57273508008b207636a","created_at":"2024-08-22T12:16:35.579131+00:00","created_by":"user@superposition.io","priority":4,"override":{"base_rate":20,"distance_unit":"Km","hello_message":"ನಮಸ್ಕಾರ ಬೆಂಗಳೂರು","hello_message_color":"red","logo":"https://www.shutterstock.com/image-vector/bangalore-logo-vidhana-soudha-600nw-1506258893.jpg","per_distance_unit_rate":15},"last_modified_at":"2024-08-22T12:16:43.897928","last_modified_by":"user@superposition.io"}	UPDATE "contexts" SET "override" = $1, "override_id" = $2, "last_modified_at" = $3, "last_modified_by" = $4 WHERE ("contexts"."id" = $5) RETURNING "contexts"."id", "contexts"."value", "contexts"."override_id", "contexts"."created_at", "contexts"."created_by", "contexts"."priority", "contexts"."override", "contexts"."last_modified_at", "contexts"."last_modified_by"
c344f6ed-b996-4b01-8c4a-d0605799a53b	contexts	postgres	2024-08-22 12:36:23.004557	INSERT	\N	{"id":"e47edbe36bf3333f42ca2ded0a4604997adc4f6e0bf6176079272597c01353d8","value":{"==":[{"var":"city"},"Chennai"]},"override_id":"645b12a924823be87eb423791f3c42393ab19103afcd8ce923c4a1112408b32b","created_at":"2024-08-22T12:36:23.489214+00:00","created_by":"user@superposition.io","priority":4,"override":{"hello_message":"வணக்கம் சென்னை","hello_message_color":"green","logo":"https://www.shutterstock.com/image-vector/chennai-skyline-color-landmarks-blue-260nw-515862346.jpg"},"last_modified_at":"2024-08-22T12:36:23.489216","last_modified_by":"user@superposition.io"}	INSERT INTO "contexts" ("id", "value", "override_id", "created_at", "created_by", "priority", "override", "last_modified_at", "last_modified_by") VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9)
fab51b58-20f8-4a2e-8a87-ea3d07fcab06	contexts	postgres	2024-08-22 12:37:31.773998	INSERT	\N	{"id":"3f0e5fe95e2e758151d4cef712185767a137c926a9e7156b6face3a98b167d35","value":{"==":[{"var":"city"},"Seattle"]},"override_id":"f73412831793a34df7df95a79bc65754b1eebf31534e4ccfa9c79681fcf8ac69","created_at":"2024-08-22T12:37:32.306949+00:00","created_by":"user@superposition.io","priority":4,"override":{"base_rate":5,"currency":"USD","distance_unit":"Miles","hello_message":"Hello Seattle","hello_message_color":"blue","logo":"https://t4.ftcdn.net/jpg/04/24/15/07/360_F_424150716_kDZIgUONDIKpIhHqsxlDcVIiglyjIOQs.jpg","per_distance_unit_rate":2.5},"last_modified_at":"2024-08-22T12:37:32.306953","last_modified_by":"user@superposition.io"}	INSERT INTO "contexts" ("id", "value", "override_id", "created_at", "created_by", "priority", "override", "last_modified_at", "last_modified_by") VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9)
11321d35-13c7-458b-b7fc-d463164780ac	contexts	postgres	2024-08-22 12:37:53.174162	INSERT	\N	{"id":"9fb61ecfb662467e9036ec79bfd8c02ac244c1836f113242aec5fe6d9265d8dc","value":{"and":[{"==":[{"var":"city"},"Bangalore"]},{"==":[{"var":"vehicle_type"},"cab"]}]},"override_id":"e2411e811b927824f910f9edd75b984a3ad226c46a1216ffa735b7bda60c1ede","created_at":"2024-08-22T12:37:53.206065+00:00","created_by":"user@superposition.io","priority":6,"override":{"base_rate":12},"last_modified_at":"2024-08-22T12:37:53.206068","last_modified_by":"user@superposition.io"}	INSERT INTO "contexts" ("id", "value", "override_id", "created_at", "created_by", "priority", "override", "last_modified_at", "last_modified_by") VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9)
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

COPY dev_cac.functions (function_name, published_code, draft_code, function_description, published_runtime_version, draft_runtime_version, published_at, draft_edited_at, published_by, draft_edited_by, last_modified_at, last_modified_by) FROM stdin;
axios_validator_example	YXN5bmMgZnVuY3Rpb24gdmFsaWRhdGUoa2V5LCB2YWx1ZSkgewogICAgcmV0dXJuIGF4aW9zLmdldCh2YWx1ZSkudGhlbihyZXNwb25zZSA9PiByZXNwb25zZS5zdGF0dXMgPT0gJzIwMCcpCiAgICAuY2F0Y2goZXJyID0+IHsKICAgICAgICBjb25zb2xlLmxvZyhlcnIpOwogICAgICAgIHJldHVybiBmYWxzZTsKICAgIH0pOwp9Cg==	YXN5bmMgZnVuY3Rpb24gdmFsaWRhdGUoa2V5LCB2YWx1ZSkgewogICAgcmV0dXJuIGF4aW9zLmdldCh2YWx1ZSkudGhlbihyZXNwb25zZSA9PiByZXNwb25zZS5zdGF0dXMgPT0gJzIwMCcpCiAgICAuY2F0Y2goZXJyID0+IHsKICAgICAgICBjb25zb2xlLmxvZyhlcnIpOwogICAgICAgIHJldHVybiBmYWxzZTsKICAgIH0pOwp9Cg==	An example function that shows off validator functions	1.0.0	1.0.0	2024-08-22 12:10:48.787527	2024-08-22 12:10:44.416185	user@superposition.io	user@superposition.io	2024-08-22 12:10:44.416187	user@superposition.io
\.


--
-- Data for Name: type_templates; Type: TABLE DATA; Schema: dev_cac; Owner: postgres
--

COPY dev_cac.type_templates (type_name, type_schema, created_by, created_at, last_modified_at, last_modified_by) FROM stdin;
Number	{"type": "integer"}	user@superposition.io	2024-08-22 12:05:34.969937	2024-08-22 12:05:34.969937	null
Decimal	{"type": "number"}	user@superposition.io	2024-08-22 12:05:34.969937	2024-08-22 12:05:34.969937	null
Boolean	{"type": "boolean"}	user@superposition.io	2024-08-22 12:05:34.969937	2024-08-22 12:05:34.969937	null
Enum	{"type": "string", "enum": ["android", "ios"]}	user@superposition.io	2024-08-22 12:05:34.969937	2024-08-22 12:05:34.969937	null
Pattern	{"type": "string", "pattern": ".*"}	user@superposition.io	2024-08-22 12:05:34.969937	2024-08-22 12:05:34.969937	null
URL	{"pattern":"https://.*","type":"string"}	user@superposition.io	2024-08-22 12:14:46.657825	2024-08-22 12:14:46.657825	user@superposition.io
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
\.


--
-- Data for Name: __diesel_schema_migrations; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.__diesel_schema_migrations (version, run_on) FROM stdin;
00000000000000	2024-08-22 10:49:57.89682
20231016133815	2024-08-22 10:49:57.901301
20240123123559	2024-08-22 10:49:57.991129
20240219125126	2024-08-22 10:49:58.16889
20240305122806	2024-08-22 10:49:58.173486
20240422122806	2024-08-22 10:49:58.177093
20240506133756	2024-08-22 10:49:58.182094
20240731065515	2024-08-22 10:49:58.189142
20231016134612	2024-08-22 10:49:58.324097
20240118063937	2024-08-22 10:49:58.334082
\.


--
-- Data for Name: config_versions; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.config_versions (id, config, config_hash, tags, created_at) FROM stdin;
7232338676053839872	{"contexts":[],"default_configs":{"base_rate":10.0},"overrides":{}}	a3a50ef5b9a9367bdd48fa4003b5f07686bcf67833085403c5fff5f93b82eba1	\N	2024-08-22 10:51:50.731042
7232338782970843136	{"contexts":[],"default_configs":{"base_rate":10.0,"currency":"INR"},"overrides":{}}	dc10e297ed72652a199e8ca90d460baa61889f171d8e8d9d738d336dafa312f8	\N	2024-08-22 10:52:16.222832
7232338902726610944	{"contexts":[],"default_configs":{"base_rate":10.0,"currency":"INR","per_distance_unit_rate":15.0},"overrides":{}}	ac9e3d933a912bb368f807dce0f9b2eda1d0d04de45e2f65eaada0b7f73f3618	\N	2024-08-22 10:52:44.771626
7232338993331965952	{"contexts":[],"default_configs":{"base_rate":10.0,"currency":"INR","distance_unit":"Km","per_distance_unit_rate":15.0},"overrides":{}}	53f07c85de4c1f0ca36dea05f777b371e5bd781c4f8f91f5413a73537a0f74e9	\N	2024-08-22 10:53:06.372277
7232339178661482496	{"contexts":[],"default_configs":{"base_rate":10.0,"currency":"INR","distance_unit":"Km","hello_message":"Hello World !!!","per_distance_unit_rate":15.0},"overrides":{}}	00ab9929de0a52f9c63af36086397b0314df3e044b97540570d92aa0cefec95d	\N	2024-08-22 10:53:50.559477
7232339269442998272	{"contexts":[],"default_configs":{"base_rate":10.0,"currency":"INR","distance_unit":"Km","hello_message":"Hello World !!!","hello_message_color":"black","per_distance_unit_rate":15.0},"overrides":{}}	61031b938a053d3a6a9834cd97a5a3d90e3e1bdcf581d940585bb6d555d15d0f	\N	2024-08-22 10:54:12.202808
7232339361629605888	{"contexts":[],"default_configs":{"base_rate":10.0,"currency":"INR","distance_unit":"Km","hello_message":"Hello World !!!","hello_message_color":"black","logo":"https://cdn1.vectorstock.com/i/1000x1000/03/20/big-city-logo-vector-20480320.jpg","per_distance_unit_rate":15.0},"overrides":{}}	0179e598de97d0861be8eab2cbbe8b1db68991a431e80f0f8bb27571ba8f6cd8	\N	2024-08-22 10:54:34.181901
7232342504790691840	{"contexts":[],"default_configs":{"base_rate":10.0,"currency":"INR","distance_unit":"Km","hello_message":"Hello World !!!","hello_message_color":"black","logo":"https://cdn1.vectorstock.com/i/1000x1000/03/20/big-city-logo-vector-20480320.jpg","per_distance_unit_rate":15.0},"overrides":{}}	0179e598de97d0861be8eab2cbbe8b1db68991a431e80f0f8bb27571ba8f6cd8	\N	2024-08-22 11:07:03.571585
7232343389096775680	{"contexts":[{"condition":{"==":[{"var":"city"},"Bangalore"]},"id":"11c6bd932b8888a9e50be8f822b76116fbaa20fd58b059f98b9e7179543bb9a4","override_with_keys":["932a2b3502038c2e975d9ace00c90f016860ecb14564cf13085b0d40d1959153"],"priority":4}],"default_configs":{"base_rate":10.0,"currency":"INR","distance_unit":"Km","hello_message":"Hello World !!!","hello_message_color":"black","logo":"https://cdn1.vectorstock.com/i/1000x1000/03/20/big-city-logo-vector-20480320.jpg","per_distance_unit_rate":15.0},"overrides":{"932a2b3502038c2e975d9ace00c90f016860ecb14564cf13085b0d40d1959153":{"base_rate":20,"currency":"INR","distance_unit":"Km","hello_message":"ನಮಸ್ಕಾರ ಬೆಂಗಳೂರು","hello_message_color":"red","logo":"https://www.shutterstock.com/image-vector/bangalore-logo-vidhana-soudha-600nw-1506258893.jpg","per_distance_unit_rate":15}}}	ef1a4c8bfe83cbd9c1a32336985d99c601640990db677261627cb5f54fd05245	\N	2024-08-22 11:10:34.4086
7232344922727256064	{"contexts":[{"condition":{"==":[{"var":"city"},"Bangalore"]},"id":"11c6bd932b8888a9e50be8f822b76116fbaa20fd58b059f98b9e7179543bb9a4","override_with_keys":["932a2b3502038c2e975d9ace00c90f016860ecb14564cf13085b0d40d1959153"],"priority":4},{"condition":{"==":[{"var":"city"},"Chennai"]},"id":"e47edbe36bf3333f42ca2ded0a4604997adc4f6e0bf6176079272597c01353d8","override_with_keys":["645b12a924823be87eb423791f3c42393ab19103afcd8ce923c4a1112408b32b"],"priority":4}],"default_configs":{"base_rate":10.0,"currency":"INR","distance_unit":"Km","hello_message":"Hello World !!!","hello_message_color":"black","logo":"https://cdn1.vectorstock.com/i/1000x1000/03/20/big-city-logo-vector-20480320.jpg","per_distance_unit_rate":15.0},"overrides":{"645b12a924823be87eb423791f3c42393ab19103afcd8ce923c4a1112408b32b":{"hello_message":"வணக்கம் சென்னை","hello_message_color":"green","logo":"https://www.shutterstock.com/image-vector/chennai-skyline-color-landmarks-blue-260nw-515862346.jpg"},"932a2b3502038c2e975d9ace00c90f016860ecb14564cf13085b0d40d1959153":{"base_rate":20,"currency":"INR","distance_unit":"Km","hello_message":"ನಮಸ್ಕಾರ ಬೆಂಗಳೂರು","hello_message_color":"red","logo":"https://www.shutterstock.com/image-vector/bangalore-logo-vidhana-soudha-600nw-1506258893.jpg","per_distance_unit_rate":15}}}	2d93211fd3e1721dfb63c0faf9decd730e87f427f6540b80ce32e0d7e69beb4e	\N	2024-08-22 11:16:40.056076
7232345263912914944	{"contexts":[{"condition":{"==":[{"var":"city"},"Bangalore"]},"id":"11c6bd932b8888a9e50be8f822b76116fbaa20fd58b059f98b9e7179543bb9a4","override_with_keys":["932a2b3502038c2e975d9ace00c90f016860ecb14564cf13085b0d40d1959153"],"priority":4},{"condition":{"==":[{"var":"city"},"Chennai"]},"id":"e47edbe36bf3333f42ca2ded0a4604997adc4f6e0bf6176079272597c01353d8","override_with_keys":["645b12a924823be87eb423791f3c42393ab19103afcd8ce923c4a1112408b32b"],"priority":4},{"condition":{"==":[{"var":"city"},"Seattle"]},"id":"3f0e5fe95e2e758151d4cef712185767a137c926a9e7156b6face3a98b167d35","override_with_keys":["f73412831793a34df7df95a79bc65754b1eebf31534e4ccfa9c79681fcf8ac69"],"priority":4}],"default_configs":{"base_rate":10.0,"currency":"INR","distance_unit":"Km","hello_message":"Hello World !!!","hello_message_color":"black","logo":"https://cdn1.vectorstock.com/i/1000x1000/03/20/big-city-logo-vector-20480320.jpg","per_distance_unit_rate":15.0},"overrides":{"645b12a924823be87eb423791f3c42393ab19103afcd8ce923c4a1112408b32b":{"hello_message":"வணக்கம் சென்னை","hello_message_color":"green","logo":"https://www.shutterstock.com/image-vector/chennai-skyline-color-landmarks-blue-260nw-515862346.jpg"},"932a2b3502038c2e975d9ace00c90f016860ecb14564cf13085b0d40d1959153":{"base_rate":20,"currency":"INR","distance_unit":"Km","hello_message":"ನಮಸ್ಕಾರ ಬೆಂಗಳೂರು","hello_message_color":"red","logo":"https://www.shutterstock.com/image-vector/bangalore-logo-vidhana-soudha-600nw-1506258893.jpg","per_distance_unit_rate":15},"f73412831793a34df7df95a79bc65754b1eebf31534e4ccfa9c79681fcf8ac69":{"base_rate":5,"currency":"USD","distance_unit":"Miles","hello_message":"Hello Seattle","hello_message_color":"blue","logo":"https://t4.ftcdn.net/jpg/04/24/15/07/360_F_424150716_kDZIgUONDIKpIhHqsxlDcVIiglyjIOQs.jpg","per_distance_unit_rate":2.5}}}	3648f3a2e4c0cb6aaf549a7aefb769d42e630688f70bd84412779ffc1552b889	\N	2024-08-22 11:18:01.39626
7232345353880735744	{"contexts":[{"condition":{"==":[{"var":"city"},"Bangalore"]},"id":"11c6bd932b8888a9e50be8f822b76116fbaa20fd58b059f98b9e7179543bb9a4","override_with_keys":["932a2b3502038c2e975d9ace00c90f016860ecb14564cf13085b0d40d1959153"],"priority":4},{"condition":{"==":[{"var":"city"},"Chennai"]},"id":"e47edbe36bf3333f42ca2ded0a4604997adc4f6e0bf6176079272597c01353d8","override_with_keys":["645b12a924823be87eb423791f3c42393ab19103afcd8ce923c4a1112408b32b"],"priority":4},{"condition":{"==":[{"var":"city"},"Seattle"]},"id":"3f0e5fe95e2e758151d4cef712185767a137c926a9e7156b6face3a98b167d35","override_with_keys":["f73412831793a34df7df95a79bc65754b1eebf31534e4ccfa9c79681fcf8ac69"],"priority":4},{"condition":{"and":[{"==":[{"var":"vehicle_type"},"cab"]},{"==":[{"var":"city"},"Bangalore"]}]},"id":"9fb61ecfb662467e9036ec79bfd8c02ac244c1836f113242aec5fe6d9265d8dc","override_with_keys":["e2411e811b927824f910f9edd75b984a3ad226c46a1216ffa735b7bda60c1ede"],"priority":6}],"default_configs":{"base_rate":10.0,"currency":"INR","distance_unit":"Km","hello_message":"Hello World !!!","hello_message_color":"black","logo":"https://cdn1.vectorstock.com/i/1000x1000/03/20/big-city-logo-vector-20480320.jpg","per_distance_unit_rate":15.0},"overrides":{"645b12a924823be87eb423791f3c42393ab19103afcd8ce923c4a1112408b32b":{"hello_message":"வணக்கம் சென்னை","hello_message_color":"green","logo":"https://www.shutterstock.com/image-vector/chennai-skyline-color-landmarks-blue-260nw-515862346.jpg"},"932a2b3502038c2e975d9ace00c90f016860ecb14564cf13085b0d40d1959153":{"base_rate":20,"currency":"INR","distance_unit":"Km","hello_message":"ನಮಸ್ಕಾರ ಬೆಂಗಳೂರು","hello_message_color":"red","logo":"https://www.shutterstock.com/image-vector/bangalore-logo-vidhana-soudha-600nw-1506258893.jpg","per_distance_unit_rate":15},"e2411e811b927824f910f9edd75b984a3ad226c46a1216ffa735b7bda60c1ede":{"base_rate":12},"f73412831793a34df7df95a79bc65754b1eebf31534e4ccfa9c79681fcf8ac69":{"base_rate":5,"currency":"USD","distance_unit":"Miles","hello_message":"Hello Seattle","hello_message_color":"blue","logo":"https://t4.ftcdn.net/jpg/04/24/15/07/360_F_424150716_kDZIgUONDIKpIhHqsxlDcVIiglyjIOQs.jpg","per_distance_unit_rate":2.5}}}	6c523b7b2ccc302052f58f27d5981ad8159aa68b4b64b6c02eb192e7a6ae73bc	\N	2024-08-22 11:18:22.84735
7232345556251709440	{"contexts":[{"condition":{"==":[{"var":"city"},"Bangalore"]},"id":"11c6bd932b8888a9e50be8f822b76116fbaa20fd58b059f98b9e7179543bb9a4","override_with_keys":["932a2b3502038c2e975d9ace00c90f016860ecb14564cf13085b0d40d1959153"],"priority":4},{"condition":{"==":[{"var":"city"},"Chennai"]},"id":"e47edbe36bf3333f42ca2ded0a4604997adc4f6e0bf6176079272597c01353d8","override_with_keys":["645b12a924823be87eb423791f3c42393ab19103afcd8ce923c4a1112408b32b"],"priority":4},{"condition":{"==":[{"var":"city"},"Seattle"]},"id":"3f0e5fe95e2e758151d4cef712185767a137c926a9e7156b6face3a98b167d35","override_with_keys":["f73412831793a34df7df95a79bc65754b1eebf31534e4ccfa9c79681fcf8ac69"],"priority":4},{"condition":{"and":[{"==":[{"var":"vehicle_type"},"cab"]},{"==":[{"var":"city"},"Bangalore"]}]},"id":"9fb61ecfb662467e9036ec79bfd8c02ac244c1836f113242aec5fe6d9265d8dc","override_with_keys":["e2411e811b927824f910f9edd75b984a3ad226c46a1216ffa735b7bda60c1ede"],"priority":6},{"condition":{"and":[{"==":[{"var":"vehicle_type"},"cab"]},{"==":[{"var":"city"},"Bangalore"]},{"==":[{"var":"hour_of_day"},9]}]},"id":"d8256cd0c9fbbfae460057ed14f0630f4dd0b050cf271832b0c9bfb5e31ca43b","override_with_keys":["204dc317acb302d38289cbfbc3227461d45bdde8dc5bfc0f6f961a8bb476e910"],"priority":38}],"default_configs":{"base_rate":10.0,"currency":"INR","distance_unit":"Km","hello_message":"Hello World !!!","hello_message_color":"black","logo":"https://cdn1.vectorstock.com/i/1000x1000/03/20/big-city-logo-vector-20480320.jpg","per_distance_unit_rate":15.0},"overrides":{"204dc317acb302d38289cbfbc3227461d45bdde8dc5bfc0f6f961a8bb476e910":{"base_rate":100,"per_distance_unit_rate":50},"645b12a924823be87eb423791f3c42393ab19103afcd8ce923c4a1112408b32b":{"hello_message":"வணக்கம் சென்னை","hello_message_color":"green","logo":"https://www.shutterstock.com/image-vector/chennai-skyline-color-landmarks-blue-260nw-515862346.jpg"},"932a2b3502038c2e975d9ace00c90f016860ecb14564cf13085b0d40d1959153":{"base_rate":20,"currency":"INR","distance_unit":"Km","hello_message":"ನಮಸ್ಕಾರ ಬೆಂಗಳೂರು","hello_message_color":"red","logo":"https://www.shutterstock.com/image-vector/bangalore-logo-vidhana-soudha-600nw-1506258893.jpg","per_distance_unit_rate":15},"e2411e811b927824f910f9edd75b984a3ad226c46a1216ffa735b7bda60c1ede":{"base_rate":12},"f73412831793a34df7df95a79bc65754b1eebf31534e4ccfa9c79681fcf8ac69":{"base_rate":5,"currency":"USD","distance_unit":"Miles","hello_message":"Hello Seattle","hello_message_color":"blue","logo":"https://t4.ftcdn.net/jpg/04/24/15/07/360_F_424150716_kDZIgUONDIKpIhHqsxlDcVIiglyjIOQs.jpg","per_distance_unit_rate":2.5}}}	3f6a59b118bad407826069253484d3f5cf09fbbe17b5a2122620b1d6a3401a35	\N	2024-08-22 11:19:11.095851
\.


--
-- Data for Name: contexts; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.contexts (id, value, override_id, created_at, created_by, priority, override, last_modified_at, last_modified_by) FROM stdin;
11c6bd932b8888a9e50be8f822b76116fbaa20fd58b059f98b9e7179543bb9a4	{"==":[{"var":"city"},"Bangalore"]}	932a2b3502038c2e975d9ace00c90f016860ecb14564cf13085b0d40d1959153	2024-08-22 11:10:34.385255+00	user@superposition.io	4	{"base_rate":20,"currency":"INR","distance_unit":"Km","hello_message":"ನಮಸ್ಕಾರ ಬೆಂಗಳೂರು","hello_message_color":"red","logo":"https://www.shutterstock.com/image-vector/bangalore-logo-vidhana-soudha-600nw-1506258893.jpg","per_distance_unit_rate":15}	2024-08-22 11:10:34.385262	user@superposition.io
e47edbe36bf3333f42ca2ded0a4604997adc4f6e0bf6176079272597c01353d8	{"==":[{"var":"city"},"Chennai"]}	645b12a924823be87eb423791f3c42393ab19103afcd8ce923c4a1112408b32b	2024-08-22 11:16:40.025706+00	user@superposition.io	4	{"hello_message":"வணக்கம் சென்னை","hello_message_color":"green","logo":"https://www.shutterstock.com/image-vector/chennai-skyline-color-landmarks-blue-260nw-515862346.jpg"}	2024-08-22 11:16:40.025709	user@superposition.io
3f0e5fe95e2e758151d4cef712185767a137c926a9e7156b6face3a98b167d35	{"==":[{"var":"city"},"Seattle"]}	f73412831793a34df7df95a79bc65754b1eebf31534e4ccfa9c79681fcf8ac69	2024-08-22 11:18:01.390332+00	user@superposition.io	4	{"base_rate":5,"currency":"USD","distance_unit":"Miles","hello_message":"Hello Seattle","hello_message_color":"blue","logo":"https://t4.ftcdn.net/jpg/04/24/15/07/360_F_424150716_kDZIgUONDIKpIhHqsxlDcVIiglyjIOQs.jpg","per_distance_unit_rate":2.5}	2024-08-22 11:18:01.390335	user@superposition.io
9fb61ecfb662467e9036ec79bfd8c02ac244c1836f113242aec5fe6d9265d8dc	{"and":[{"==":[{"var":"vehicle_type"},"cab"]},{"==":[{"var":"city"},"Bangalore"]}]}	e2411e811b927824f910f9edd75b984a3ad226c46a1216ffa735b7bda60c1ede	2024-08-22 11:18:22.838349+00	user@superposition.io	6	{"base_rate":12}	2024-08-22 11:18:22.838376	user@superposition.io
d8256cd0c9fbbfae460057ed14f0630f4dd0b050cf271832b0c9bfb5e31ca43b	{"and":[{"==":[{"var":"vehicle_type"},"cab"]},{"==":[{"var":"city"},"Bangalore"]},{"==":[{"var":"hour_of_day"},9]}]}	204dc317acb302d38289cbfbc3227461d45bdde8dc5bfc0f6f961a8bb476e910	2024-08-22 11:19:11.08507+00	user@superposition.io	38	{"base_rate":100,"per_distance_unit_rate":50}	2024-08-22 11:19:11.085073	user@superposition.io
\.


--
-- Data for Name: default_configs; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.default_configs (key, value, created_at, created_by, schema, function_name, last_modified_at, last_modified_by) FROM stdin;
base_rate	10.0	2024-08-22 10:51:50.704636+00	user@superposition.io	{"type":"number"}	\N	2024-08-22 10:51:50.704963	user@superposition.io
currency	"INR"	2024-08-22 10:52:16.209834+00	user@superposition.io	{"enum":["INR","USD","EUR"],"type":"string"}	\N	2024-08-22 10:52:16.209844	user@superposition.io
per_distance_unit_rate	15.0	2024-08-22 10:52:44.764395+00	user@superposition.io	{"type":"number"}	\N	2024-08-22 10:52:44.76441	user@superposition.io
distance_unit	"Km"	2024-08-22 10:53:06.368909+00	user@superposition.io	{"enum":["Km","Miles"],"type":"string"}	\N	2024-08-22 10:53:06.368914	user@superposition.io
hello_message	"Hello World !!!"	2024-08-22 10:53:50.551716+00	user@superposition.io	{"pattern":".*","type":"string"}	\N	2024-08-22 10:53:50.55172	user@superposition.io
hello_message_color	"black"	2024-08-22 10:54:12.197449+00	user@superposition.io	{"enum":["black","red","blue","green","pink"],"type":"string"}	\N	2024-08-22 10:54:12.197459	user@superposition.io
logo	"https://cdn1.vectorstock.com/i/1000x1000/03/20/big-city-logo-vector-20480320.jpg"	2024-08-22 10:54:34.169556+00	user@superposition.io	{"pattern":"https://.*","type":"string"}	axios_validator_example	2024-08-22 11:07:03.007146	user@superposition.io
\.


--
-- Data for Name: dimensions; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.dimensions (dimension, priority, created_at, created_by, schema, function_name, last_modified_at, last_modified_by) FROM stdin;
variantIds	1	2024-08-22 10:55:19.609194+00	user@superposition.io	{"pattern":".*","type":"string"}	\N	2024-08-22 10:55:19.609202	user@superposition.io
vehicle_type	2	2024-08-22 10:55:54.394804+00	user@superposition.io	{"enum":["cab","auto"],"type":"string"}	\N	2024-08-22 10:55:54.394812	user@superposition.io
city	4	2024-08-22 10:56:13.771152+00	user@superposition.io	{"pattern":"[a-zA-Z ]+","type":"string"}	\N	2024-08-22 10:56:13.771157	user@superposition.io
hour_of_day	32	2024-08-22 10:56:31.492196+00	user@superposition.io	{"type":"integer"}	\N	2024-08-22 10:56:31.492201	user@superposition.io
\.


--
-- Data for Name: event_log_y2023m08; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.event_log_y2023m08 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2023m09; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.event_log_y2023m09 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2023m10; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.event_log_y2023m10 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2023m11; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.event_log_y2023m11 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2023m12; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.event_log_y2023m12 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2024m01; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.event_log_y2024m01 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2024m02; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.event_log_y2024m02 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2024m03; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.event_log_y2024m03 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2024m04; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.event_log_y2024m04 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2024m05; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.event_log_y2024m05 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2024m06; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.event_log_y2024m06 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2024m07; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.event_log_y2024m07 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2024m08; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.event_log_y2024m08 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
61a05923-ee36-42b5-8bba-97f5ad514c6e	default_configs	postgres	2024-08-22 10:51:50.698077	INSERT	\N	{"key":"base_rate","value":10.0,"created_at":"2024-08-22T10:51:50.704636+00:00","created_by":"user@superposition.io","schema":{"type":"number"},"function_name":null,"last_modified_at":"2024-08-22T10:51:50.704963","last_modified_by":"user@superposition.io"}	INSERT INTO "default_configs" ("key", "value", "created_at", "created_by", "schema", "function_name", "last_modified_at", "last_modified_by") VALUES ($1, $2, $3, $4, $5, DEFAULT, $6, $7) ON CONFLICT ("key") DO UPDATE SET "value" = $8, "created_at" = $9, "created_by" = $10, "schema" = $11, "function_name" = $12, "last_modified_at" = $13, "last_modified_by" = $14
8c004912-e875-4a18-8179-1309f63f804a	default_configs	postgres	2024-08-22 10:52:16.209675	INSERT	\N	{"key":"currency","value":"INR","created_at":"2024-08-22T10:52:16.209834+00:00","created_by":"user@superposition.io","schema":{"enum":["INR","USD","EUR"],"type":"string"},"function_name":null,"last_modified_at":"2024-08-22T10:52:16.209844","last_modified_by":"user@superposition.io"}	INSERT INTO "default_configs" ("key", "value", "created_at", "created_by", "schema", "function_name", "last_modified_at", "last_modified_by") VALUES ($1, $2, $3, $4, $5, DEFAULT, $6, $7) ON CONFLICT ("key") DO UPDATE SET "value" = $8, "created_at" = $9, "created_by" = $10, "schema" = $11, "function_name" = $12, "last_modified_at" = $13, "last_modified_by" = $14
ad9b4fb1-2d62-49ec-8422-9cec504b15e7	default_configs	postgres	2024-08-22 10:52:44.764932	INSERT	\N	{"key":"per_distance_unit_rate","value":15.0,"created_at":"2024-08-22T10:52:44.764395+00:00","created_by":"user@superposition.io","schema":{"type":"number"},"function_name":null,"last_modified_at":"2024-08-22T10:52:44.76441","last_modified_by":"user@superposition.io"}	INSERT INTO "default_configs" ("key", "value", "created_at", "created_by", "schema", "function_name", "last_modified_at", "last_modified_by") VALUES ($1, $2, $3, $4, $5, DEFAULT, $6, $7) ON CONFLICT ("key") DO UPDATE SET "value" = $8, "created_at" = $9, "created_by" = $10, "schema" = $11, "function_name" = $12, "last_modified_at" = $13, "last_modified_by" = $14
2773fe75-3992-49f4-b332-7d2298916c05	default_configs	postgres	2024-08-22 10:53:06.367024	INSERT	\N	{"key":"distance_unit","value":"Km","created_at":"2024-08-22T10:53:06.368909+00:00","created_by":"user@superposition.io","schema":{"enum":["Km","Miles"],"type":"string"},"function_name":null,"last_modified_at":"2024-08-22T10:53:06.368914","last_modified_by":"user@superposition.io"}	INSERT INTO "default_configs" ("key", "value", "created_at", "created_by", "schema", "function_name", "last_modified_at", "last_modified_by") VALUES ($1, $2, $3, $4, $5, DEFAULT, $6, $7) ON CONFLICT ("key") DO UPDATE SET "value" = $8, "created_at" = $9, "created_by" = $10, "schema" = $11, "function_name" = $12, "last_modified_at" = $13, "last_modified_by" = $14
c2a2b240-26ab-4aaa-b78f-018e53a8f6b4	default_configs	postgres	2024-08-22 10:53:50.555561	INSERT	\N	{"key":"hello_message","value":"Hello World !!!","created_at":"2024-08-22T10:53:50.551716+00:00","created_by":"user@superposition.io","schema":{"pattern":".*","type":"string"},"function_name":null,"last_modified_at":"2024-08-22T10:53:50.55172","last_modified_by":"user@superposition.io"}	INSERT INTO "default_configs" ("key", "value", "created_at", "created_by", "schema", "function_name", "last_modified_at", "last_modified_by") VALUES ($1, $2, $3, $4, $5, DEFAULT, $6, $7) ON CONFLICT ("key") DO UPDATE SET "value" = $8, "created_at" = $9, "created_by" = $10, "schema" = $11, "function_name" = $12, "last_modified_at" = $13, "last_modified_by" = $14
30823c39-5c2b-4e35-9cfd-970c7b758007	default_configs	postgres	2024-08-22 10:54:12.198189	INSERT	\N	{"key":"hello_message_color","value":"black","created_at":"2024-08-22T10:54:12.197449+00:00","created_by":"user@superposition.io","schema":{"enum":["black","red","blue","green","pink"],"type":"string"},"function_name":null,"last_modified_at":"2024-08-22T10:54:12.197459","last_modified_by":"user@superposition.io"}	INSERT INTO "default_configs" ("key", "value", "created_at", "created_by", "schema", "function_name", "last_modified_at", "last_modified_by") VALUES ($1, $2, $3, $4, $5, DEFAULT, $6, $7) ON CONFLICT ("key") DO UPDATE SET "value" = $8, "created_at" = $9, "created_by" = $10, "schema" = $11, "function_name" = $12, "last_modified_at" = $13, "last_modified_by" = $14
d0551c40-d8b5-4863-bb0f-0ef20cc984d5	default_configs	postgres	2024-08-22 10:54:34.173895	INSERT	\N	{"key":"logo","value":"https://cdn1.vectorstock.com/i/1000x1000/03/20/big-city-logo-vector-20480320.jpg","created_at":"2024-08-22T10:54:34.169556+00:00","created_by":"user@superposition.io","schema":{"pattern":"https://.*","type":"string"},"function_name":null,"last_modified_at":"2024-08-22T10:54:34.169566","last_modified_by":"user@superposition.io"}	INSERT INTO "default_configs" ("key", "value", "created_at", "created_by", "schema", "function_name", "last_modified_at", "last_modified_by") VALUES ($1, $2, $3, $4, $5, DEFAULT, $6, $7) ON CONFLICT ("key") DO UPDATE SET "value" = $8, "created_at" = $9, "created_by" = $10, "schema" = $11, "function_name" = $12, "last_modified_at" = $13, "last_modified_by" = $14
68042015-8fb5-43ac-a7d3-f813cdd6adf6	dimensions	postgres	2024-08-22 10:55:19.612784	INSERT	\N	{"dimension":"variantIds","priority":1,"created_at":"2024-08-22T10:55:19.609194+00:00","created_by":"user@superposition.io","schema":{"pattern":".*","type":"string"},"function_name":null,"last_modified_at":"2024-08-22T10:55:19.609202","last_modified_by":"user@superposition.io"}	INSERT INTO "dimensions" ("dimension", "priority", "created_at", "created_by", "schema", "function_name", "last_modified_at", "last_modified_by") VALUES ($1, $2, $3, $4, $5, DEFAULT, $6, $7) ON CONFLICT ("dimension") DO UPDATE SET "priority" = $8, "created_at" = $9, "created_by" = $10, "schema" = $11, "function_name" = $12, "last_modified_at" = $13, "last_modified_by" = $14 RETURNING "dimensions"."dimension", "dimensions"."priority", "dimensions"."created_at", "dimensions"."created_by", "dimensions"."schema", "dimensions"."function_name", "dimensions"."last_modified_at", "dimensions"."last_modified_by"
c3135370-3f05-4538-9b03-04fec8ac6b07	dimensions	postgres	2024-08-22 10:55:54.399114	INSERT	\N	{"dimension":"vehicle_type","priority":2,"created_at":"2024-08-22T10:55:54.394804+00:00","created_by":"user@superposition.io","schema":{"enum":["cab","auto"],"type":"string"},"function_name":null,"last_modified_at":"2024-08-22T10:55:54.394812","last_modified_by":"user@superposition.io"}	INSERT INTO "dimensions" ("dimension", "priority", "created_at", "created_by", "schema", "function_name", "last_modified_at", "last_modified_by") VALUES ($1, $2, $3, $4, $5, DEFAULT, $6, $7) ON CONFLICT ("dimension") DO UPDATE SET "priority" = $8, "created_at" = $9, "created_by" = $10, "schema" = $11, "function_name" = $12, "last_modified_at" = $13, "last_modified_by" = $14 RETURNING "dimensions"."dimension", "dimensions"."priority", "dimensions"."created_at", "dimensions"."created_by", "dimensions"."schema", "dimensions"."function_name", "dimensions"."last_modified_at", "dimensions"."last_modified_by"
885a7d17-b271-4b27-b2e3-28db0c9edc81	dimensions	postgres	2024-08-22 10:56:13.774001	INSERT	\N	{"dimension":"city","priority":4,"created_at":"2024-08-22T10:56:13.771152+00:00","created_by":"user@superposition.io","schema":{"pattern":"[a-zA-Z ]+","type":"string"},"function_name":null,"last_modified_at":"2024-08-22T10:56:13.771157","last_modified_by":"user@superposition.io"}	INSERT INTO "dimensions" ("dimension", "priority", "created_at", "created_by", "schema", "function_name", "last_modified_at", "last_modified_by") VALUES ($1, $2, $3, $4, $5, DEFAULT, $6, $7) ON CONFLICT ("dimension") DO UPDATE SET "priority" = $8, "created_at" = $9, "created_by" = $10, "schema" = $11, "function_name" = $12, "last_modified_at" = $13, "last_modified_by" = $14 RETURNING "dimensions"."dimension", "dimensions"."priority", "dimensions"."created_at", "dimensions"."created_by", "dimensions"."schema", "dimensions"."function_name", "dimensions"."last_modified_at", "dimensions"."last_modified_by"
9afcf5f8-3b73-4e05-a95a-7826a5fcfc7b	dimensions	postgres	2024-08-22 10:56:31.495544	INSERT	\N	{"dimension":"hour_of_day","priority":32,"created_at":"2024-08-22T10:56:31.492196+00:00","created_by":"user@superposition.io","schema":{"type":"integer"},"function_name":null,"last_modified_at":"2024-08-22T10:56:31.492201","last_modified_by":"user@superposition.io"}	INSERT INTO "dimensions" ("dimension", "priority", "created_at", "created_by", "schema", "function_name", "last_modified_at", "last_modified_by") VALUES ($1, $2, $3, $4, $5, DEFAULT, $6, $7) ON CONFLICT ("dimension") DO UPDATE SET "priority" = $8, "created_at" = $9, "created_by" = $10, "schema" = $11, "function_name" = $12, "last_modified_at" = $13, "last_modified_by" = $14 RETURNING "dimensions"."dimension", "dimensions"."priority", "dimensions"."created_at", "dimensions"."created_by", "dimensions"."schema", "dimensions"."function_name", "dimensions"."last_modified_at", "dimensions"."last_modified_by"
f3ac701d-20ec-4e10-83e9-3766935f9ec9	functions	postgres	2024-08-22 11:00:45.891024	INSERT	\N	{"function_name":"axios_validator_example","published_code":null,"draft_code":"YXN5bmMgZnVuY3Rpb24gdmFsaWRhdGUoa2V5LCB2YWx1ZSkgewogICAgcmV0dXJuIGF4aW9zLmdldCh2YWx1ZSkudGhlbihyZXNwb25zZSA9PiByZXNwb25zZS5zdGF0dXMgPT0gJzIwMCcpCiAgICAuY2F0Y2goZXJyID0+IHsKICAgICAgICBjb25zb2xlLmxvZyhlcnIpOwogICAgICAgIHJldHVybiBmYWxzZTsKICAgIH0pOwp9Cg==","function_description":"A function that validates URLs using axios","published_runtime_version":null,"draft_runtime_version":"1.0.0","published_at":null,"draft_edited_at":"2024-08-22T11:00:45.887906","published_by":null,"draft_edited_by":"user@superposition.io","last_modified_at":"2024-08-22T11:00:45.887909","last_modified_by":"user@superposition.io"}	INSERT INTO "functions" ("function_name", "published_code", "draft_code", "function_description", "published_runtime_version", "draft_runtime_version", "published_at", "draft_edited_at", "published_by", "draft_edited_by", "last_modified_at", "last_modified_by") VALUES ($1, DEFAULT, $2, $3, DEFAULT, $4, DEFAULT, $5, DEFAULT, $6, $7, $8) RETURNING "functions"."function_name", "functions"."published_code", "functions"."draft_code", "functions"."function_description", "functions"."published_runtime_version", "functions"."draft_runtime_version", "functions"."published_at", "functions"."draft_edited_at", "functions"."published_by", "functions"."draft_edited_by", "functions"."last_modified_at", "functions"."last_modified_by"
6c6d9102-8154-4aff-9187-00569443bdd1	functions	postgres	2024-08-22 11:00:54.686902	UPDATE	{"function_name":"axios_validator_example","published_code":null,"draft_code":"YXN5bmMgZnVuY3Rpb24gdmFsaWRhdGUoa2V5LCB2YWx1ZSkgewogICAgcmV0dXJuIGF4aW9zLmdldCh2YWx1ZSkudGhlbihyZXNwb25zZSA9PiByZXNwb25zZS5zdGF0dXMgPT0gJzIwMCcpCiAgICAuY2F0Y2goZXJyID0+IHsKICAgICAgICBjb25zb2xlLmxvZyhlcnIpOwogICAgICAgIHJldHVybiBmYWxzZTsKICAgIH0pOwp9Cg==","function_description":"A function that validates URLs using axios","published_runtime_version":null,"draft_runtime_version":"1.0.0","published_at":null,"draft_edited_at":"2024-08-22T11:00:45.887906","published_by":null,"draft_edited_by":"user@superposition.io","last_modified_at":"2024-08-22T11:00:45.887909","last_modified_by":"user@superposition.io"}	{"function_name":"axios_validator_example","published_code":"YXN5bmMgZnVuY3Rpb24gdmFsaWRhdGUoa2V5LCB2YWx1ZSkgewogICAgcmV0dXJuIGF4aW9zLmdldCh2YWx1ZSkudGhlbihyZXNwb25zZSA9PiByZXNwb25zZS5zdGF0dXMgPT0gJzIwMCcpCiAgICAuY2F0Y2goZXJyID0+IHsKICAgICAgICBjb25zb2xlLmxvZyhlcnIpOwogICAgICAgIHJldHVybiBmYWxzZTsKICAgIH0pOwp9Cg==","draft_code":"YXN5bmMgZnVuY3Rpb24gdmFsaWRhdGUoa2V5LCB2YWx1ZSkgewogICAgcmV0dXJuIGF4aW9zLmdldCh2YWx1ZSkudGhlbihyZXNwb25zZSA9PiByZXNwb25zZS5zdGF0dXMgPT0gJzIwMCcpCiAgICAuY2F0Y2goZXJyID0+IHsKICAgICAgICBjb25zb2xlLmxvZyhlcnIpOwogICAgICAgIHJldHVybiBmYWxzZTsKICAgIH0pOwp9Cg==","function_description":"A function that validates URLs using axios","published_runtime_version":"1.0.0","draft_runtime_version":"1.0.0","published_at":"2024-08-22T11:00:54.684656","draft_edited_at":"2024-08-22T11:00:45.887906","published_by":"user@superposition.io","draft_edited_by":"user@superposition.io","last_modified_at":"2024-08-22T11:00:45.887909","last_modified_by":"user@superposition.io"}	UPDATE "functions" SET "published_code" = $1, "published_runtime_version" = $2, "published_by" = $3, "published_at" = $4 WHERE ("functions"."function_name" = $5) RETURNING "functions"."function_name", "functions"."published_code", "functions"."draft_code", "functions"."function_description", "functions"."published_runtime_version", "functions"."draft_runtime_version", "functions"."published_at", "functions"."draft_edited_at", "functions"."published_by", "functions"."draft_edited_by", "functions"."last_modified_at", "functions"."last_modified_by"
794319fd-2975-4c11-9e17-985312faaed8	functions	postgres	2024-08-22 11:02:07.804337	UPDATE	{"function_name":"axios_validator_example","published_code":"YXN5bmMgZnVuY3Rpb24gdmFsaWRhdGUoa2V5LCB2YWx1ZSkgewogICAgcmV0dXJuIGF4aW9zLmdldCh2YWx1ZSkudGhlbihyZXNwb25zZSA9PiByZXNwb25zZS5zdGF0dXMgPT0gJzIwMCcpCiAgICAuY2F0Y2goZXJyID0+IHsKICAgICAgICBjb25zb2xlLmxvZyhlcnIpOwogICAgICAgIHJldHVybiBmYWxzZTsKICAgIH0pOwp9Cg==","draft_code":"YXN5bmMgZnVuY3Rpb24gdmFsaWRhdGUoa2V5LCB2YWx1ZSkgewogICAgcmV0dXJuIGF4aW9zLmdldCh2YWx1ZSkudGhlbihyZXNwb25zZSA9PiByZXNwb25zZS5zdGF0dXMgPT0gJzIwMCcpCiAgICAuY2F0Y2goZXJyID0+IHsKICAgICAgICBjb25zb2xlLmxvZyhlcnIpOwogICAgICAgIHJldHVybiBmYWxzZTsKICAgIH0pOwp9Cg==","function_description":"A function that validates URLs using axios","published_runtime_version":"1.0.0","draft_runtime_version":"1.0.0","published_at":"2024-08-22T11:00:54.684656","draft_edited_at":"2024-08-22T11:00:45.887906","published_by":"user@superposition.io","draft_edited_by":"user@superposition.io","last_modified_at":"2024-08-22T11:00:45.887909","last_modified_by":"user@superposition.io"}	{"function_name":"axios_validator_example","published_code":"YXN5bmMgZnVuY3Rpb24gdmFsaWRhdGUoa2V5LCB2YWx1ZSkgewogICAgcmV0dXJuIGF4aW9zLmdldCh2YWx1ZSkudGhlbihyZXNwb25zZSA9PiByZXNwb25zZS5zdGF0dXMgPT0gJzIwMCcpCiAgICAuY2F0Y2goZXJyID0+IHsKICAgICAgICBjb25zb2xlLmxvZyhlcnIpOwogICAgICAgIHJldHVybiBmYWxzZTsKICAgIH0pOwp9Cg==","draft_code":"Ly8gVGhpcyBmdW5jdGlvbiBzaG91bGQgcmV0dXJuIGEgYm9vbGVhbiwgdHJ1ZSBmb3Igc3VjY2Vzc2Z1bCB2YWxpZGF0aW9uIGFuZCBmYWxzZSBpZiBub3QuIElmIHRoZSB2YWxpZGF0ZSBmdW5jdGlvbiByZXR1cm5zIGZhbHNlLAovLyBTdXBlcnBvc2l0aW9uIHdpbGwgbm90IGFsbG93IHRoZSBpdGVtIHRvIGJlIHNhdmVkCgphc3luYyBmdW5jdGlvbiB2YWxpZGF0ZShrZXksIHZhbHVlKSB7CiAgICByZXR1cm4gYXhpb3MuZ2V0KHZhbHVlKS50aGVuKHJlc3BvbnNlID0+IHJlc3BvbnNlLnN0YXR1cyA9PSAnMjAwJykKICAgIC5jYXRjaChlcnIgPT4gewogICAgICAgIGNvbnNvbGUubG9nKGVycik7CiAgICAgICAgcmV0dXJuIGZhbHNlOwogICAgfSk7Cn0K","function_description":"A function that validates URLs using axios","published_runtime_version":"1.0.0","draft_runtime_version":"1.0.0","published_at":"2024-08-22T11:00:54.684656","draft_edited_at":"2024-08-22T11:02:07.801713","published_by":"user@superposition.io","draft_edited_by":"user@superposition.io","last_modified_at":"2024-08-22T11:02:07.801715","last_modified_by":"user@superposition.io"}	UPDATE "functions" SET "function_name" = $1, "published_code" = $2, "draft_code" = $3, "function_description" = $4, "published_runtime_version" = $5, "draft_runtime_version" = $6, "published_at" = $7, "draft_edited_at" = $8, "published_by" = $9, "draft_edited_by" = $10, "last_modified_at" = $11, "last_modified_by" = $12 WHERE ("functions"."function_name" = $13) RETURNING "functions"."function_name", "functions"."published_code", "functions"."draft_code", "functions"."function_description", "functions"."published_runtime_version", "functions"."draft_runtime_version", "functions"."published_at", "functions"."draft_edited_at", "functions"."published_by", "functions"."draft_edited_by", "functions"."last_modified_at", "functions"."last_modified_by"
e8ca2138-fb5d-4846-8071-68cffd38c6aa	functions	postgres	2024-08-22 11:02:09.422562	UPDATE	{"function_name":"axios_validator_example","published_code":"YXN5bmMgZnVuY3Rpb24gdmFsaWRhdGUoa2V5LCB2YWx1ZSkgewogICAgcmV0dXJuIGF4aW9zLmdldCh2YWx1ZSkudGhlbihyZXNwb25zZSA9PiByZXNwb25zZS5zdGF0dXMgPT0gJzIwMCcpCiAgICAuY2F0Y2goZXJyID0+IHsKICAgICAgICBjb25zb2xlLmxvZyhlcnIpOwogICAgICAgIHJldHVybiBmYWxzZTsKICAgIH0pOwp9Cg==","draft_code":"Ly8gVGhpcyBmdW5jdGlvbiBzaG91bGQgcmV0dXJuIGEgYm9vbGVhbiwgdHJ1ZSBmb3Igc3VjY2Vzc2Z1bCB2YWxpZGF0aW9uIGFuZCBmYWxzZSBpZiBub3QuIElmIHRoZSB2YWxpZGF0ZSBmdW5jdGlvbiByZXR1cm5zIGZhbHNlLAovLyBTdXBlcnBvc2l0aW9uIHdpbGwgbm90IGFsbG93IHRoZSBpdGVtIHRvIGJlIHNhdmVkCgphc3luYyBmdW5jdGlvbiB2YWxpZGF0ZShrZXksIHZhbHVlKSB7CiAgICByZXR1cm4gYXhpb3MuZ2V0KHZhbHVlKS50aGVuKHJlc3BvbnNlID0+IHJlc3BvbnNlLnN0YXR1cyA9PSAnMjAwJykKICAgIC5jYXRjaChlcnIgPT4gewogICAgICAgIGNvbnNvbGUubG9nKGVycik7CiAgICAgICAgcmV0dXJuIGZhbHNlOwogICAgfSk7Cn0K","function_description":"A function that validates URLs using axios","published_runtime_version":"1.0.0","draft_runtime_version":"1.0.0","published_at":"2024-08-22T11:00:54.684656","draft_edited_at":"2024-08-22T11:02:07.801713","published_by":"user@superposition.io","draft_edited_by":"user@superposition.io","last_modified_at":"2024-08-22T11:02:07.801715","last_modified_by":"user@superposition.io"}	{"function_name":"axios_validator_example","published_code":"Ly8gVGhpcyBmdW5jdGlvbiBzaG91bGQgcmV0dXJuIGEgYm9vbGVhbiwgdHJ1ZSBmb3Igc3VjY2Vzc2Z1bCB2YWxpZGF0aW9uIGFuZCBmYWxzZSBpZiBub3QuIElmIHRoZSB2YWxpZGF0ZSBmdW5jdGlvbiByZXR1cm5zIGZhbHNlLAovLyBTdXBlcnBvc2l0aW9uIHdpbGwgbm90IGFsbG93IHRoZSBpdGVtIHRvIGJlIHNhdmVkCgphc3luYyBmdW5jdGlvbiB2YWxpZGF0ZShrZXksIHZhbHVlKSB7CiAgICByZXR1cm4gYXhpb3MuZ2V0KHZhbHVlKS50aGVuKHJlc3BvbnNlID0+IHJlc3BvbnNlLnN0YXR1cyA9PSAnMjAwJykKICAgIC5jYXRjaChlcnIgPT4gewogICAgICAgIGNvbnNvbGUubG9nKGVycik7CiAgICAgICAgcmV0dXJuIGZhbHNlOwogICAgfSk7Cn0K","draft_code":"Ly8gVGhpcyBmdW5jdGlvbiBzaG91bGQgcmV0dXJuIGEgYm9vbGVhbiwgdHJ1ZSBmb3Igc3VjY2Vzc2Z1bCB2YWxpZGF0aW9uIGFuZCBmYWxzZSBpZiBub3QuIElmIHRoZSB2YWxpZGF0ZSBmdW5jdGlvbiByZXR1cm5zIGZhbHNlLAovLyBTdXBlcnBvc2l0aW9uIHdpbGwgbm90IGFsbG93IHRoZSBpdGVtIHRvIGJlIHNhdmVkCgphc3luYyBmdW5jdGlvbiB2YWxpZGF0ZShrZXksIHZhbHVlKSB7CiAgICByZXR1cm4gYXhpb3MuZ2V0KHZhbHVlKS50aGVuKHJlc3BvbnNlID0+IHJlc3BvbnNlLnN0YXR1cyA9PSAnMjAwJykKICAgIC5jYXRjaChlcnIgPT4gewogICAgICAgIGNvbnNvbGUubG9nKGVycik7CiAgICAgICAgcmV0dXJuIGZhbHNlOwogICAgfSk7Cn0K","function_description":"A function that validates URLs using axios","published_runtime_version":"1.0.0","draft_runtime_version":"1.0.0","published_at":"2024-08-22T11:02:09.420582","draft_edited_at":"2024-08-22T11:02:07.801713","published_by":"user@superposition.io","draft_edited_by":"user@superposition.io","last_modified_at":"2024-08-22T11:02:07.801715","last_modified_by":"user@superposition.io"}	UPDATE "functions" SET "published_code" = $1, "published_runtime_version" = $2, "published_by" = $3, "published_at" = $4 WHERE ("functions"."function_name" = $5) RETURNING "functions"."function_name", "functions"."published_code", "functions"."draft_code", "functions"."function_description", "functions"."published_runtime_version", "functions"."draft_runtime_version", "functions"."published_at", "functions"."draft_edited_at", "functions"."published_by", "functions"."draft_edited_by", "functions"."last_modified_at", "functions"."last_modified_by"
a2b79f81-1508-49e1-abbc-c4d0bb1c86b9	default_configs	postgres	2024-08-22 11:07:03.550305	UPDATE	{"key":"logo","value":"https://cdn1.vectorstock.com/i/1000x1000/03/20/big-city-logo-vector-20480320.jpg","created_at":"2024-08-22T10:54:34.169556+00:00","created_by":"user@superposition.io","schema":{"pattern":"https://.*","type":"string"},"function_name":null,"last_modified_at":"2024-08-22T10:54:34.169566","last_modified_by":"user@superposition.io"}	{"key":"logo","value":"https://cdn1.vectorstock.com/i/1000x1000/03/20/big-city-logo-vector-20480320.jpg","created_at":"2024-08-22T10:54:34.169556+00:00","created_by":"user@superposition.io","schema":{"pattern":"https://.*","type":"string"},"function_name":"axios_validator_example","last_modified_at":"2024-08-22T11:07:03.007146","last_modified_by":"user@superposition.io"}	INSERT INTO "default_configs" ("key", "value", "created_at", "created_by", "schema", "function_name", "last_modified_at", "last_modified_by") VALUES ($1, $2, $3, $4, $5, $6, $7, $8) ON CONFLICT ("key") DO UPDATE SET "value" = $9, "created_at" = $10, "created_by" = $11, "schema" = $12, "function_name" = $13, "last_modified_at" = $14, "last_modified_by" = $15
c3fc7b7e-7eb2-4d21-a986-b0b6f78d0978	contexts	postgres	2024-08-22 11:10:33.64684	INSERT	\N	{"id":"11c6bd932b8888a9e50be8f822b76116fbaa20fd58b059f98b9e7179543bb9a4","value":{"==":[{"var":"city"},"Bangalore"]},"override_id":"932a2b3502038c2e975d9ace00c90f016860ecb14564cf13085b0d40d1959153","created_at":"2024-08-22T11:10:34.385255+00:00","created_by":"user@superposition.io","priority":4,"override":{"base_rate":20,"currency":"INR","distance_unit":"Km","hello_message":"ನಮಸ್ಕಾರ ಬೆಂಗಳೂರು","hello_message_color":"red","logo":"https://www.shutterstock.com/image-vector/bangalore-logo-vidhana-soudha-600nw-1506258893.jpg","per_distance_unit_rate":15},"last_modified_at":"2024-08-22T11:10:34.385262","last_modified_by":"user@superposition.io"}	INSERT INTO "contexts" ("id", "value", "override_id", "created_at", "created_by", "priority", "override", "last_modified_at", "last_modified_by") VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9)
75a8dd6b-27c3-4369-be40-845a46ed3312	contexts	postgres	2024-08-22 11:16:39.214364	INSERT	\N	{"id":"e47edbe36bf3333f42ca2ded0a4604997adc4f6e0bf6176079272597c01353d8","value":{"==":[{"var":"city"},"Chennai"]},"override_id":"645b12a924823be87eb423791f3c42393ab19103afcd8ce923c4a1112408b32b","created_at":"2024-08-22T11:16:40.025706+00:00","created_by":"user@superposition.io","priority":4,"override":{"hello_message":"வணக்கம் சென்னை","hello_message_color":"green","logo":"https://www.shutterstock.com/image-vector/chennai-skyline-color-landmarks-blue-260nw-515862346.jpg"},"last_modified_at":"2024-08-22T11:16:40.025709","last_modified_by":"user@superposition.io"}	INSERT INTO "contexts" ("id", "value", "override_id", "created_at", "created_by", "priority", "override", "last_modified_at", "last_modified_by") VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9)
a411e43b-8503-46a3-89b7-17ad007f5fd5	contexts	postgres	2024-08-22 11:18:00.655318	INSERT	\N	{"id":"3f0e5fe95e2e758151d4cef712185767a137c926a9e7156b6face3a98b167d35","value":{"==":[{"var":"city"},"Seattle"]},"override_id":"f73412831793a34df7df95a79bc65754b1eebf31534e4ccfa9c79681fcf8ac69","created_at":"2024-08-22T11:18:01.390332+00:00","created_by":"user@superposition.io","priority":4,"override":{"base_rate":5,"currency":"USD","distance_unit":"Miles","hello_message":"Hello Seattle","hello_message_color":"blue","logo":"https://t4.ftcdn.net/jpg/04/24/15/07/360_F_424150716_kDZIgUONDIKpIhHqsxlDcVIiglyjIOQs.jpg","per_distance_unit_rate":2.5},"last_modified_at":"2024-08-22T11:18:01.390335","last_modified_by":"user@superposition.io"}	INSERT INTO "contexts" ("id", "value", "override_id", "created_at", "created_by", "priority", "override", "last_modified_at", "last_modified_by") VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9)
05c675b3-434d-4222-815a-8fcaa5376d32	contexts	postgres	2024-08-22 11:18:22.825149	INSERT	\N	{"id":"9fb61ecfb662467e9036ec79bfd8c02ac244c1836f113242aec5fe6d9265d8dc","value":{"and":[{"==":[{"var":"vehicle_type"},"cab"]},{"==":[{"var":"city"},"Bangalore"]}]},"override_id":"e2411e811b927824f910f9edd75b984a3ad226c46a1216ffa735b7bda60c1ede","created_at":"2024-08-22T11:18:22.838349+00:00","created_by":"user@superposition.io","priority":6,"override":{"base_rate":12},"last_modified_at":"2024-08-22T11:18:22.838376","last_modified_by":"user@superposition.io"}	INSERT INTO "contexts" ("id", "value", "override_id", "created_at", "created_by", "priority", "override", "last_modified_at", "last_modified_by") VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9)
378960f7-0f5b-4de9-b8ee-e9412640edb3	contexts	postgres	2024-08-22 11:19:11.070187	INSERT	\N	{"id":"d8256cd0c9fbbfae460057ed14f0630f4dd0b050cf271832b0c9bfb5e31ca43b","value":{"and":[{"==":[{"var":"vehicle_type"},"cab"]},{"==":[{"var":"city"},"Bangalore"]},{"==":[{"var":"hour_of_day"},9]}]},"override_id":"204dc317acb302d38289cbfbc3227461d45bdde8dc5bfc0f6f961a8bb476e910","created_at":"2024-08-22T11:19:11.08507+00:00","created_by":"user@superposition.io","priority":38,"override":{"base_rate":100,"per_distance_unit_rate":50},"last_modified_at":"2024-08-22T11:19:11.085073","last_modified_by":"user@superposition.io"}	INSERT INTO "contexts" ("id", "value", "override_id", "created_at", "created_by", "priority", "override", "last_modified_at", "last_modified_by") VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9)
\.


--
-- Data for Name: event_log_y2024m09; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.event_log_y2024m09 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2024m10; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.event_log_y2024m10 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2024m11; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.event_log_y2024m11 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2024m12; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.event_log_y2024m12 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2025m01; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.event_log_y2025m01 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2025m02; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.event_log_y2025m02 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2025m03; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.event_log_y2025m03 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2025m04; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.event_log_y2025m04 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2025m05; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.event_log_y2025m05 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2025m06; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.event_log_y2025m06 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2025m07; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.event_log_y2025m07 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2025m08; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.event_log_y2025m08 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2025m09; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.event_log_y2025m09 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2025m10; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.event_log_y2025m10 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2025m11; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.event_log_y2025m11 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2025m12; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.event_log_y2025m12 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2026m01; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.event_log_y2026m01 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2026m02; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.event_log_y2026m02 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2026m03; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.event_log_y2026m03 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2026m04; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.event_log_y2026m04 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2026m05; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.event_log_y2026m05 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2026m06; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.event_log_y2026m06 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2026m07; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.event_log_y2026m07 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2026m08; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.event_log_y2026m08 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2026m09; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.event_log_y2026m09 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2026m10; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.event_log_y2026m10 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2026m11; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.event_log_y2026m11 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: event_log_y2026m12; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.event_log_y2026m12 (id, table_name, user_name, "timestamp", action, original_data, new_data, query) FROM stdin;
\.


--
-- Data for Name: experiments; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.experiments (id, created_at, created_by, last_modified, name, override_keys, status, traffic_percentage, context, variants, last_modified_by, chosen_variant) FROM stdin;
\.


--
-- Data for Name: functions; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.functions (function_name, published_code, draft_code, function_description, published_runtime_version, draft_runtime_version, published_at, draft_edited_at, published_by, draft_edited_by, last_modified_at, last_modified_by) FROM stdin;
axios_validator_example	Ly8gVGhpcyBmdW5jdGlvbiBzaG91bGQgcmV0dXJuIGEgYm9vbGVhbiwgdHJ1ZSBmb3Igc3VjY2Vzc2Z1bCB2YWxpZGF0aW9uIGFuZCBmYWxzZSBpZiBub3QuIElmIHRoZSB2YWxpZGF0ZSBmdW5jdGlvbiByZXR1cm5zIGZhbHNlLAovLyBTdXBlcnBvc2l0aW9uIHdpbGwgbm90IGFsbG93IHRoZSBpdGVtIHRvIGJlIHNhdmVkCgphc3luYyBmdW5jdGlvbiB2YWxpZGF0ZShrZXksIHZhbHVlKSB7CiAgICByZXR1cm4gYXhpb3MuZ2V0KHZhbHVlKS50aGVuKHJlc3BvbnNlID0+IHJlc3BvbnNlLnN0YXR1cyA9PSAnMjAwJykKICAgIC5jYXRjaChlcnIgPT4gewogICAgICAgIGNvbnNvbGUubG9nKGVycik7CiAgICAgICAgcmV0dXJuIGZhbHNlOwogICAgfSk7Cn0K	Ly8gVGhpcyBmdW5jdGlvbiBzaG91bGQgcmV0dXJuIGEgYm9vbGVhbiwgdHJ1ZSBmb3Igc3VjY2Vzc2Z1bCB2YWxpZGF0aW9uIGFuZCBmYWxzZSBpZiBub3QuIElmIHRoZSB2YWxpZGF0ZSBmdW5jdGlvbiByZXR1cm5zIGZhbHNlLAovLyBTdXBlcnBvc2l0aW9uIHdpbGwgbm90IGFsbG93IHRoZSBpdGVtIHRvIGJlIHNhdmVkCgphc3luYyBmdW5jdGlvbiB2YWxpZGF0ZShrZXksIHZhbHVlKSB7CiAgICByZXR1cm4gYXhpb3MuZ2V0KHZhbHVlKS50aGVuKHJlc3BvbnNlID0+IHJlc3BvbnNlLnN0YXR1cyA9PSAnMjAwJykKICAgIC5jYXRjaChlcnIgPT4gewogICAgICAgIGNvbnNvbGUubG9nKGVycik7CiAgICAgICAgcmV0dXJuIGZhbHNlOwogICAgfSk7Cn0K	A function that validates URLs using axios	1.0.0	1.0.0	2024-08-22 11:02:09.420582	2024-08-22 11:02:07.801713	user@superposition.io	user@superposition.io	2024-08-22 11:02:07.801715	user@superposition.io
\.


--
-- Data for Name: type_templates; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.type_templates (type_name, type_schema, created_by, created_at, last_modified_at, last_modified_by) FROM stdin;
Number	{"type": "integer"}	user@superposition.io	2024-08-22 10:49:58.182094	2024-08-22 10:49:58.182094	null
Decimal	{"type": "number"}	user@superposition.io	2024-08-22 10:49:58.182094	2024-08-22 10:49:58.182094	null
Boolean	{"type": "boolean"}	user@superposition.io	2024-08-22 10:49:58.182094	2024-08-22 10:49:58.182094	null
Enum	{"type": "string", "enum": ["android", "ios"]}	user@superposition.io	2024-08-22 10:49:58.182094	2024-08-22 10:49:58.182094	null
Pattern	{"type": "string", "pattern": ".*"}	user@superposition.io	2024-08-22 10:49:58.182094	2024-08-22 10:49:58.182094	null
URL	{"pattern":"https://.*","type":"string"}	user@superposition.io	2024-08-22 10:51:20.428375	2024-08-22 10:51:20.428375	user@superposition.io
\.


--
-- Data for Name: config_versions; Type: TABLE DATA; Schema: test_cac; Owner: postgres
--

COPY test_cac.config_versions (id, config, config_hash, tags, created_at) FROM stdin;
\.


--
-- Data for Name: contexts; Type: TABLE DATA; Schema: test_cac; Owner: postgres
--

COPY test_cac.contexts (id, value, override_id, created_at, created_by, priority, override, last_modified_at, last_modified_by) FROM stdin;
\.


--
-- Data for Name: default_configs; Type: TABLE DATA; Schema: test_cac; Owner: postgres
--

COPY test_cac.default_configs (key, value, created_at, created_by, schema, function_name, last_modified_at, last_modified_by) FROM stdin;
\.


--
-- Data for Name: dimensions; Type: TABLE DATA; Schema: test_cac; Owner: postgres
--

COPY test_cac.dimensions (dimension, priority, created_at, created_by, schema, function_name, last_modified_at, last_modified_by) FROM stdin;
variantIds	1	2024-08-22 12:05:39.024602+00	user@example.com	{"type": "string","pattern": ".*"}	\N	2024-08-22 12:05:39.024602	null
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
4bc2e2ba-e52f-444c-ac89-40d5f5461901	dimensions	postgres	2024-08-22 12:05:39.024602	INSERT	\N	{"dimension":"variantIds","priority":1,"created_at":"2024-08-22T12:05:39.024602+00:00","created_by":"user@example.com","schema":{"type": "string","pattern": ".*"},"function_name":null,"last_modified_at":"2024-08-22T12:05:39.024602","last_modified_by":"null"}	INSERT INTO test_cac.dimensions (dimension, priority, created_at, created_by, schema, function_name) VALUES ('variantIds', 1, CURRENT_TIMESTAMP, 'user@example.com', '{"type": "string","pattern": ".*"}'::json, null);
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

COPY test_cac.functions (function_name, published_code, draft_code, function_description, published_runtime_version, draft_runtime_version, published_at, draft_edited_at, published_by, draft_edited_by, last_modified_at, last_modified_by) FROM stdin;
\.


--
-- Data for Name: type_templates; Type: TABLE DATA; Schema: test_cac; Owner: postgres
--

COPY test_cac.type_templates (type_name, type_schema, created_by, created_at, last_modified_at, last_modified_by) FROM stdin;
Number	{"type": "integer"}	user@superposition.io	2024-08-22 12:05:38.712154	2024-08-22 12:05:38.712154	null
Decimal	{"type": "number"}	user@superposition.io	2024-08-22 12:05:38.712154	2024-08-22 12:05:38.712154	null
Boolean	{"type": "boolean"}	user@superposition.io	2024-08-22 12:05:38.712154	2024-08-22 12:05:38.712154	null
Enum	{"type": "string", "enum": ["android", "ios"]}	user@superposition.io	2024-08-22 12:05:38.712154	2024-08-22 12:05:38.712154	null
Pattern	{"type": "string", "pattern": ".*"}	user@superposition.io	2024-08-22 12:05:38.712154	2024-08-22 12:05:38.712154	null
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
-- Name: config_versions config_versions_pkey; Type: CONSTRAINT; Schema: dev_cac; Owner: postgres
--

ALTER TABLE ONLY dev_cac.config_versions
    ADD CONSTRAINT config_versions_pkey PRIMARY KEY (id);


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
-- Name: type_templates type_templates_pkey; Type: CONSTRAINT; Schema: dev_cac; Owner: postgres
--

ALTER TABLE ONLY dev_cac.type_templates
    ADD CONSTRAINT type_templates_pkey PRIMARY KEY (type_name);


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
-- Name: __diesel_schema_migrations __diesel_schema_migrations_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.__diesel_schema_migrations
    ADD CONSTRAINT __diesel_schema_migrations_pkey PRIMARY KEY (version);


--
-- Name: config_versions config_versions_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.config_versions
    ADD CONSTRAINT config_versions_pkey PRIMARY KEY (id);


--
-- Name: contexts contexts_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.contexts
    ADD CONSTRAINT contexts_pkey PRIMARY KEY (id);


--
-- Name: default_configs default_configs_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.default_configs
    ADD CONSTRAINT default_configs_pkey PRIMARY KEY (key);


--
-- Name: dimensions dimensions_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.dimensions
    ADD CONSTRAINT dimensions_pkey PRIMARY KEY (dimension);


--
-- Name: event_log event_log_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.event_log
    ADD CONSTRAINT event_log_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2023m08 event_log_y2023m08_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.event_log_y2023m08
    ADD CONSTRAINT event_log_y2023m08_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2023m09 event_log_y2023m09_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.event_log_y2023m09
    ADD CONSTRAINT event_log_y2023m09_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2023m10 event_log_y2023m10_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.event_log_y2023m10
    ADD CONSTRAINT event_log_y2023m10_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2023m11 event_log_y2023m11_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.event_log_y2023m11
    ADD CONSTRAINT event_log_y2023m11_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2023m12 event_log_y2023m12_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.event_log_y2023m12
    ADD CONSTRAINT event_log_y2023m12_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2024m01 event_log_y2024m01_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.event_log_y2024m01
    ADD CONSTRAINT event_log_y2024m01_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2024m02 event_log_y2024m02_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.event_log_y2024m02
    ADD CONSTRAINT event_log_y2024m02_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2024m03 event_log_y2024m03_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.event_log_y2024m03
    ADD CONSTRAINT event_log_y2024m03_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2024m04 event_log_y2024m04_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.event_log_y2024m04
    ADD CONSTRAINT event_log_y2024m04_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2024m05 event_log_y2024m05_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.event_log_y2024m05
    ADD CONSTRAINT event_log_y2024m05_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2024m06 event_log_y2024m06_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.event_log_y2024m06
    ADD CONSTRAINT event_log_y2024m06_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2024m07 event_log_y2024m07_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.event_log_y2024m07
    ADD CONSTRAINT event_log_y2024m07_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2024m08 event_log_y2024m08_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.event_log_y2024m08
    ADD CONSTRAINT event_log_y2024m08_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2024m09 event_log_y2024m09_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.event_log_y2024m09
    ADD CONSTRAINT event_log_y2024m09_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2024m10 event_log_y2024m10_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.event_log_y2024m10
    ADD CONSTRAINT event_log_y2024m10_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2024m11 event_log_y2024m11_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.event_log_y2024m11
    ADD CONSTRAINT event_log_y2024m11_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2024m12 event_log_y2024m12_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.event_log_y2024m12
    ADD CONSTRAINT event_log_y2024m12_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2025m01 event_log_y2025m01_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.event_log_y2025m01
    ADD CONSTRAINT event_log_y2025m01_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2025m02 event_log_y2025m02_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.event_log_y2025m02
    ADD CONSTRAINT event_log_y2025m02_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2025m03 event_log_y2025m03_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.event_log_y2025m03
    ADD CONSTRAINT event_log_y2025m03_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2025m04 event_log_y2025m04_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.event_log_y2025m04
    ADD CONSTRAINT event_log_y2025m04_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2025m05 event_log_y2025m05_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.event_log_y2025m05
    ADD CONSTRAINT event_log_y2025m05_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2025m06 event_log_y2025m06_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.event_log_y2025m06
    ADD CONSTRAINT event_log_y2025m06_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2025m07 event_log_y2025m07_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.event_log_y2025m07
    ADD CONSTRAINT event_log_y2025m07_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2025m08 event_log_y2025m08_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.event_log_y2025m08
    ADD CONSTRAINT event_log_y2025m08_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2025m09 event_log_y2025m09_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.event_log_y2025m09
    ADD CONSTRAINT event_log_y2025m09_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2025m10 event_log_y2025m10_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.event_log_y2025m10
    ADD CONSTRAINT event_log_y2025m10_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2025m11 event_log_y2025m11_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.event_log_y2025m11
    ADD CONSTRAINT event_log_y2025m11_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2025m12 event_log_y2025m12_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.event_log_y2025m12
    ADD CONSTRAINT event_log_y2025m12_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2026m01 event_log_y2026m01_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.event_log_y2026m01
    ADD CONSTRAINT event_log_y2026m01_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2026m02 event_log_y2026m02_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.event_log_y2026m02
    ADD CONSTRAINT event_log_y2026m02_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2026m03 event_log_y2026m03_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.event_log_y2026m03
    ADD CONSTRAINT event_log_y2026m03_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2026m04 event_log_y2026m04_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.event_log_y2026m04
    ADD CONSTRAINT event_log_y2026m04_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2026m05 event_log_y2026m05_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.event_log_y2026m05
    ADD CONSTRAINT event_log_y2026m05_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2026m06 event_log_y2026m06_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.event_log_y2026m06
    ADD CONSTRAINT event_log_y2026m06_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2026m07 event_log_y2026m07_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.event_log_y2026m07
    ADD CONSTRAINT event_log_y2026m07_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2026m08 event_log_y2026m08_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.event_log_y2026m08
    ADD CONSTRAINT event_log_y2026m08_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2026m09 event_log_y2026m09_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.event_log_y2026m09
    ADD CONSTRAINT event_log_y2026m09_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2026m10 event_log_y2026m10_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.event_log_y2026m10
    ADD CONSTRAINT event_log_y2026m10_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2026m11 event_log_y2026m11_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.event_log_y2026m11
    ADD CONSTRAINT event_log_y2026m11_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: event_log_y2026m12 event_log_y2026m12_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.event_log_y2026m12
    ADD CONSTRAINT event_log_y2026m12_pkey PRIMARY KEY (id, "timestamp");


--
-- Name: experiments experiments_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.experiments
    ADD CONSTRAINT experiments_pkey PRIMARY KEY (id);


--
-- Name: functions functions_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.functions
    ADD CONSTRAINT functions_pkey PRIMARY KEY (function_name);


--
-- Name: type_templates type_templates_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.type_templates
    ADD CONSTRAINT type_templates_pkey PRIMARY KEY (type_name);


--
-- Name: config_versions config_versions_pkey; Type: CONSTRAINT; Schema: test_cac; Owner: postgres
--

ALTER TABLE ONLY test_cac.config_versions
    ADD CONSTRAINT config_versions_pkey PRIMARY KEY (id);


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
-- Name: type_templates type_templates_pkey; Type: CONSTRAINT; Schema: test_cac; Owner: postgres
--

ALTER TABLE ONLY test_cac.type_templates
    ADD CONSTRAINT type_templates_pkey PRIMARY KEY (type_name);


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
-- Name: config_verions_tags_index; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX config_verions_tags_index ON dev_cac.config_versions USING gin (tags);


--
-- Name: config_versions_id_index; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX config_versions_id_index ON dev_cac.config_versions USING btree (id);


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
-- Name: type_templates_created_at_index; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX type_templates_created_at_index ON dev_cac.type_templates USING btree (created_at);


--
-- Name: type_templates_index; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX type_templates_index ON dev_cac.type_templates USING btree (type_name);


--
-- Name: type_templates_last_modifed_index; Type: INDEX; Schema: dev_cac; Owner: postgres
--

CREATE INDEX type_templates_last_modifed_index ON dev_cac.type_templates USING btree (last_modified_at);


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
-- Name: config_verions_tags_index; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX config_verions_tags_index ON public.config_versions USING gin (tags);


--
-- Name: config_versions_id_index; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX config_versions_id_index ON public.config_versions USING btree (id);


--
-- Name: event_log_action_index; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_action_index ON ONLY public.event_log USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_table_name_index; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_table_name_index ON ONLY public.event_log USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_timestamp_index; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_timestamp_index ON ONLY public.event_log USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2023m08_action_timestamp_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2023m08_action_timestamp_table_name_idx ON public.event_log_y2023m08 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2023m08_table_name_action_timestamp_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2023m08_table_name_action_timestamp_idx ON public.event_log_y2023m08 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2023m08_timestamp_action_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2023m08_timestamp_action_table_name_idx ON public.event_log_y2023m08 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2023m09_action_timestamp_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2023m09_action_timestamp_table_name_idx ON public.event_log_y2023m09 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2023m09_table_name_action_timestamp_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2023m09_table_name_action_timestamp_idx ON public.event_log_y2023m09 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2023m09_timestamp_action_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2023m09_timestamp_action_table_name_idx ON public.event_log_y2023m09 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2023m10_action_timestamp_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2023m10_action_timestamp_table_name_idx ON public.event_log_y2023m10 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2023m10_table_name_action_timestamp_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2023m10_table_name_action_timestamp_idx ON public.event_log_y2023m10 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2023m10_timestamp_action_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2023m10_timestamp_action_table_name_idx ON public.event_log_y2023m10 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2023m11_action_timestamp_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2023m11_action_timestamp_table_name_idx ON public.event_log_y2023m11 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2023m11_table_name_action_timestamp_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2023m11_table_name_action_timestamp_idx ON public.event_log_y2023m11 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2023m11_timestamp_action_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2023m11_timestamp_action_table_name_idx ON public.event_log_y2023m11 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2023m12_action_timestamp_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2023m12_action_timestamp_table_name_idx ON public.event_log_y2023m12 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2023m12_table_name_action_timestamp_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2023m12_table_name_action_timestamp_idx ON public.event_log_y2023m12 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2023m12_timestamp_action_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2023m12_timestamp_action_table_name_idx ON public.event_log_y2023m12 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2024m01_action_timestamp_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2024m01_action_timestamp_table_name_idx ON public.event_log_y2024m01 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2024m01_table_name_action_timestamp_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2024m01_table_name_action_timestamp_idx ON public.event_log_y2024m01 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2024m01_timestamp_action_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2024m01_timestamp_action_table_name_idx ON public.event_log_y2024m01 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2024m02_action_timestamp_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2024m02_action_timestamp_table_name_idx ON public.event_log_y2024m02 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2024m02_table_name_action_timestamp_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2024m02_table_name_action_timestamp_idx ON public.event_log_y2024m02 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2024m02_timestamp_action_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2024m02_timestamp_action_table_name_idx ON public.event_log_y2024m02 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2024m03_action_timestamp_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2024m03_action_timestamp_table_name_idx ON public.event_log_y2024m03 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2024m03_table_name_action_timestamp_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2024m03_table_name_action_timestamp_idx ON public.event_log_y2024m03 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2024m03_timestamp_action_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2024m03_timestamp_action_table_name_idx ON public.event_log_y2024m03 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2024m04_action_timestamp_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2024m04_action_timestamp_table_name_idx ON public.event_log_y2024m04 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2024m04_table_name_action_timestamp_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2024m04_table_name_action_timestamp_idx ON public.event_log_y2024m04 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2024m04_timestamp_action_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2024m04_timestamp_action_table_name_idx ON public.event_log_y2024m04 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2024m05_action_timestamp_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2024m05_action_timestamp_table_name_idx ON public.event_log_y2024m05 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2024m05_table_name_action_timestamp_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2024m05_table_name_action_timestamp_idx ON public.event_log_y2024m05 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2024m05_timestamp_action_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2024m05_timestamp_action_table_name_idx ON public.event_log_y2024m05 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2024m06_action_timestamp_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2024m06_action_timestamp_table_name_idx ON public.event_log_y2024m06 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2024m06_table_name_action_timestamp_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2024m06_table_name_action_timestamp_idx ON public.event_log_y2024m06 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2024m06_timestamp_action_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2024m06_timestamp_action_table_name_idx ON public.event_log_y2024m06 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2024m07_action_timestamp_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2024m07_action_timestamp_table_name_idx ON public.event_log_y2024m07 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2024m07_table_name_action_timestamp_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2024m07_table_name_action_timestamp_idx ON public.event_log_y2024m07 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2024m07_timestamp_action_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2024m07_timestamp_action_table_name_idx ON public.event_log_y2024m07 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2024m08_action_timestamp_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2024m08_action_timestamp_table_name_idx ON public.event_log_y2024m08 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2024m08_table_name_action_timestamp_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2024m08_table_name_action_timestamp_idx ON public.event_log_y2024m08 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2024m08_timestamp_action_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2024m08_timestamp_action_table_name_idx ON public.event_log_y2024m08 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2024m09_action_timestamp_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2024m09_action_timestamp_table_name_idx ON public.event_log_y2024m09 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2024m09_table_name_action_timestamp_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2024m09_table_name_action_timestamp_idx ON public.event_log_y2024m09 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2024m09_timestamp_action_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2024m09_timestamp_action_table_name_idx ON public.event_log_y2024m09 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2024m10_action_timestamp_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2024m10_action_timestamp_table_name_idx ON public.event_log_y2024m10 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2024m10_table_name_action_timestamp_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2024m10_table_name_action_timestamp_idx ON public.event_log_y2024m10 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2024m10_timestamp_action_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2024m10_timestamp_action_table_name_idx ON public.event_log_y2024m10 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2024m11_action_timestamp_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2024m11_action_timestamp_table_name_idx ON public.event_log_y2024m11 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2024m11_table_name_action_timestamp_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2024m11_table_name_action_timestamp_idx ON public.event_log_y2024m11 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2024m11_timestamp_action_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2024m11_timestamp_action_table_name_idx ON public.event_log_y2024m11 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2024m12_action_timestamp_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2024m12_action_timestamp_table_name_idx ON public.event_log_y2024m12 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2024m12_table_name_action_timestamp_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2024m12_table_name_action_timestamp_idx ON public.event_log_y2024m12 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2024m12_timestamp_action_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2024m12_timestamp_action_table_name_idx ON public.event_log_y2024m12 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2025m01_action_timestamp_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2025m01_action_timestamp_table_name_idx ON public.event_log_y2025m01 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2025m01_table_name_action_timestamp_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2025m01_table_name_action_timestamp_idx ON public.event_log_y2025m01 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2025m01_timestamp_action_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2025m01_timestamp_action_table_name_idx ON public.event_log_y2025m01 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2025m02_action_timestamp_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2025m02_action_timestamp_table_name_idx ON public.event_log_y2025m02 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2025m02_table_name_action_timestamp_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2025m02_table_name_action_timestamp_idx ON public.event_log_y2025m02 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2025m02_timestamp_action_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2025m02_timestamp_action_table_name_idx ON public.event_log_y2025m02 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2025m03_action_timestamp_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2025m03_action_timestamp_table_name_idx ON public.event_log_y2025m03 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2025m03_table_name_action_timestamp_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2025m03_table_name_action_timestamp_idx ON public.event_log_y2025m03 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2025m03_timestamp_action_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2025m03_timestamp_action_table_name_idx ON public.event_log_y2025m03 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2025m04_action_timestamp_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2025m04_action_timestamp_table_name_idx ON public.event_log_y2025m04 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2025m04_table_name_action_timestamp_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2025m04_table_name_action_timestamp_idx ON public.event_log_y2025m04 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2025m04_timestamp_action_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2025m04_timestamp_action_table_name_idx ON public.event_log_y2025m04 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2025m05_action_timestamp_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2025m05_action_timestamp_table_name_idx ON public.event_log_y2025m05 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2025m05_table_name_action_timestamp_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2025m05_table_name_action_timestamp_idx ON public.event_log_y2025m05 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2025m05_timestamp_action_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2025m05_timestamp_action_table_name_idx ON public.event_log_y2025m05 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2025m06_action_timestamp_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2025m06_action_timestamp_table_name_idx ON public.event_log_y2025m06 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2025m06_table_name_action_timestamp_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2025m06_table_name_action_timestamp_idx ON public.event_log_y2025m06 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2025m06_timestamp_action_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2025m06_timestamp_action_table_name_idx ON public.event_log_y2025m06 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2025m07_action_timestamp_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2025m07_action_timestamp_table_name_idx ON public.event_log_y2025m07 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2025m07_table_name_action_timestamp_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2025m07_table_name_action_timestamp_idx ON public.event_log_y2025m07 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2025m07_timestamp_action_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2025m07_timestamp_action_table_name_idx ON public.event_log_y2025m07 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2025m08_action_timestamp_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2025m08_action_timestamp_table_name_idx ON public.event_log_y2025m08 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2025m08_table_name_action_timestamp_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2025m08_table_name_action_timestamp_idx ON public.event_log_y2025m08 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2025m08_timestamp_action_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2025m08_timestamp_action_table_name_idx ON public.event_log_y2025m08 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2025m09_action_timestamp_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2025m09_action_timestamp_table_name_idx ON public.event_log_y2025m09 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2025m09_table_name_action_timestamp_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2025m09_table_name_action_timestamp_idx ON public.event_log_y2025m09 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2025m09_timestamp_action_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2025m09_timestamp_action_table_name_idx ON public.event_log_y2025m09 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2025m10_action_timestamp_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2025m10_action_timestamp_table_name_idx ON public.event_log_y2025m10 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2025m10_table_name_action_timestamp_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2025m10_table_name_action_timestamp_idx ON public.event_log_y2025m10 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2025m10_timestamp_action_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2025m10_timestamp_action_table_name_idx ON public.event_log_y2025m10 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2025m11_action_timestamp_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2025m11_action_timestamp_table_name_idx ON public.event_log_y2025m11 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2025m11_table_name_action_timestamp_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2025m11_table_name_action_timestamp_idx ON public.event_log_y2025m11 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2025m11_timestamp_action_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2025m11_timestamp_action_table_name_idx ON public.event_log_y2025m11 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2025m12_action_timestamp_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2025m12_action_timestamp_table_name_idx ON public.event_log_y2025m12 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2025m12_table_name_action_timestamp_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2025m12_table_name_action_timestamp_idx ON public.event_log_y2025m12 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2025m12_timestamp_action_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2025m12_timestamp_action_table_name_idx ON public.event_log_y2025m12 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2026m01_action_timestamp_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2026m01_action_timestamp_table_name_idx ON public.event_log_y2026m01 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2026m01_table_name_action_timestamp_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2026m01_table_name_action_timestamp_idx ON public.event_log_y2026m01 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2026m01_timestamp_action_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2026m01_timestamp_action_table_name_idx ON public.event_log_y2026m01 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2026m02_action_timestamp_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2026m02_action_timestamp_table_name_idx ON public.event_log_y2026m02 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2026m02_table_name_action_timestamp_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2026m02_table_name_action_timestamp_idx ON public.event_log_y2026m02 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2026m02_timestamp_action_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2026m02_timestamp_action_table_name_idx ON public.event_log_y2026m02 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2026m03_action_timestamp_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2026m03_action_timestamp_table_name_idx ON public.event_log_y2026m03 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2026m03_table_name_action_timestamp_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2026m03_table_name_action_timestamp_idx ON public.event_log_y2026m03 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2026m03_timestamp_action_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2026m03_timestamp_action_table_name_idx ON public.event_log_y2026m03 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2026m04_action_timestamp_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2026m04_action_timestamp_table_name_idx ON public.event_log_y2026m04 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2026m04_table_name_action_timestamp_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2026m04_table_name_action_timestamp_idx ON public.event_log_y2026m04 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2026m04_timestamp_action_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2026m04_timestamp_action_table_name_idx ON public.event_log_y2026m04 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2026m05_action_timestamp_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2026m05_action_timestamp_table_name_idx ON public.event_log_y2026m05 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2026m05_table_name_action_timestamp_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2026m05_table_name_action_timestamp_idx ON public.event_log_y2026m05 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2026m05_timestamp_action_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2026m05_timestamp_action_table_name_idx ON public.event_log_y2026m05 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2026m06_action_timestamp_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2026m06_action_timestamp_table_name_idx ON public.event_log_y2026m06 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2026m06_table_name_action_timestamp_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2026m06_table_name_action_timestamp_idx ON public.event_log_y2026m06 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2026m06_timestamp_action_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2026m06_timestamp_action_table_name_idx ON public.event_log_y2026m06 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2026m07_action_timestamp_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2026m07_action_timestamp_table_name_idx ON public.event_log_y2026m07 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2026m07_table_name_action_timestamp_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2026m07_table_name_action_timestamp_idx ON public.event_log_y2026m07 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2026m07_timestamp_action_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2026m07_timestamp_action_table_name_idx ON public.event_log_y2026m07 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2026m08_action_timestamp_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2026m08_action_timestamp_table_name_idx ON public.event_log_y2026m08 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2026m08_table_name_action_timestamp_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2026m08_table_name_action_timestamp_idx ON public.event_log_y2026m08 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2026m08_timestamp_action_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2026m08_timestamp_action_table_name_idx ON public.event_log_y2026m08 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2026m09_action_timestamp_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2026m09_action_timestamp_table_name_idx ON public.event_log_y2026m09 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2026m09_table_name_action_timestamp_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2026m09_table_name_action_timestamp_idx ON public.event_log_y2026m09 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2026m09_timestamp_action_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2026m09_timestamp_action_table_name_idx ON public.event_log_y2026m09 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2026m10_action_timestamp_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2026m10_action_timestamp_table_name_idx ON public.event_log_y2026m10 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2026m10_table_name_action_timestamp_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2026m10_table_name_action_timestamp_idx ON public.event_log_y2026m10 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2026m10_timestamp_action_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2026m10_timestamp_action_table_name_idx ON public.event_log_y2026m10 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2026m11_action_timestamp_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2026m11_action_timestamp_table_name_idx ON public.event_log_y2026m11 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2026m11_table_name_action_timestamp_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2026m11_table_name_action_timestamp_idx ON public.event_log_y2026m11 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2026m11_timestamp_action_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2026m11_timestamp_action_table_name_idx ON public.event_log_y2026m11 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: event_log_y2026m12_action_timestamp_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2026m12_action_timestamp_table_name_idx ON public.event_log_y2026m12 USING btree (action) INCLUDE ("timestamp", table_name);


--
-- Name: event_log_y2026m12_table_name_action_timestamp_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2026m12_table_name_action_timestamp_idx ON public.event_log_y2026m12 USING btree (table_name) INCLUDE (action, "timestamp");


--
-- Name: event_log_y2026m12_timestamp_action_table_name_idx; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX event_log_y2026m12_timestamp_action_table_name_idx ON public.event_log_y2026m12 USING btree ("timestamp") INCLUDE (action, table_name);


--
-- Name: experiment_created_date_index; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX experiment_created_date_index ON public.experiments USING btree (created_at) INCLUDE (id);


--
-- Name: experiment_last_modified_index; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX experiment_last_modified_index ON public.experiments USING btree (last_modified) INCLUDE (id, created_at);


--
-- Name: experiment_status_index; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX experiment_status_index ON public.experiments USING btree (status) INCLUDE (created_at, last_modified);


--
-- Name: type_templates_created_at_index; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX type_templates_created_at_index ON public.type_templates USING btree (created_at);


--
-- Name: type_templates_index; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX type_templates_index ON public.type_templates USING btree (type_name);


--
-- Name: type_templates_last_modifed_index; Type: INDEX; Schema: public; Owner: postgres
--

CREATE INDEX type_templates_last_modifed_index ON public.type_templates USING btree (last_modified_at);


--
-- Name: config_verions_tags_index; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX config_verions_tags_index ON test_cac.config_versions USING gin (tags);


--
-- Name: config_versions_id_index; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX config_versions_id_index ON test_cac.config_versions USING btree (id);


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
-- Name: type_templates_created_at_index; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX type_templates_created_at_index ON test_cac.type_templates USING btree (created_at);


--
-- Name: type_templates_index; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX type_templates_index ON test_cac.type_templates USING btree (type_name);


--
-- Name: type_templates_last_modifed_index; Type: INDEX; Schema: test_cac; Owner: postgres
--

CREATE INDEX type_templates_last_modifed_index ON test_cac.type_templates USING btree (last_modified_at);


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
-- Name: event_log_y2023m08_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_action_index ATTACH PARTITION public.event_log_y2023m08_action_timestamp_table_name_idx;


--
-- Name: event_log_y2023m08_pkey; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_pkey ATTACH PARTITION public.event_log_y2023m08_pkey;


--
-- Name: event_log_y2023m08_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_table_name_index ATTACH PARTITION public.event_log_y2023m08_table_name_action_timestamp_idx;


--
-- Name: event_log_y2023m08_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_timestamp_index ATTACH PARTITION public.event_log_y2023m08_timestamp_action_table_name_idx;


--
-- Name: event_log_y2023m09_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_action_index ATTACH PARTITION public.event_log_y2023m09_action_timestamp_table_name_idx;


--
-- Name: event_log_y2023m09_pkey; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_pkey ATTACH PARTITION public.event_log_y2023m09_pkey;


--
-- Name: event_log_y2023m09_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_table_name_index ATTACH PARTITION public.event_log_y2023m09_table_name_action_timestamp_idx;


--
-- Name: event_log_y2023m09_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_timestamp_index ATTACH PARTITION public.event_log_y2023m09_timestamp_action_table_name_idx;


--
-- Name: event_log_y2023m10_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_action_index ATTACH PARTITION public.event_log_y2023m10_action_timestamp_table_name_idx;


--
-- Name: event_log_y2023m10_pkey; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_pkey ATTACH PARTITION public.event_log_y2023m10_pkey;


--
-- Name: event_log_y2023m10_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_table_name_index ATTACH PARTITION public.event_log_y2023m10_table_name_action_timestamp_idx;


--
-- Name: event_log_y2023m10_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_timestamp_index ATTACH PARTITION public.event_log_y2023m10_timestamp_action_table_name_idx;


--
-- Name: event_log_y2023m11_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_action_index ATTACH PARTITION public.event_log_y2023m11_action_timestamp_table_name_idx;


--
-- Name: event_log_y2023m11_pkey; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_pkey ATTACH PARTITION public.event_log_y2023m11_pkey;


--
-- Name: event_log_y2023m11_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_table_name_index ATTACH PARTITION public.event_log_y2023m11_table_name_action_timestamp_idx;


--
-- Name: event_log_y2023m11_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_timestamp_index ATTACH PARTITION public.event_log_y2023m11_timestamp_action_table_name_idx;


--
-- Name: event_log_y2023m12_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_action_index ATTACH PARTITION public.event_log_y2023m12_action_timestamp_table_name_idx;


--
-- Name: event_log_y2023m12_pkey; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_pkey ATTACH PARTITION public.event_log_y2023m12_pkey;


--
-- Name: event_log_y2023m12_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_table_name_index ATTACH PARTITION public.event_log_y2023m12_table_name_action_timestamp_idx;


--
-- Name: event_log_y2023m12_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_timestamp_index ATTACH PARTITION public.event_log_y2023m12_timestamp_action_table_name_idx;


--
-- Name: event_log_y2024m01_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_action_index ATTACH PARTITION public.event_log_y2024m01_action_timestamp_table_name_idx;


--
-- Name: event_log_y2024m01_pkey; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_pkey ATTACH PARTITION public.event_log_y2024m01_pkey;


--
-- Name: event_log_y2024m01_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_table_name_index ATTACH PARTITION public.event_log_y2024m01_table_name_action_timestamp_idx;


--
-- Name: event_log_y2024m01_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_timestamp_index ATTACH PARTITION public.event_log_y2024m01_timestamp_action_table_name_idx;


--
-- Name: event_log_y2024m02_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_action_index ATTACH PARTITION public.event_log_y2024m02_action_timestamp_table_name_idx;


--
-- Name: event_log_y2024m02_pkey; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_pkey ATTACH PARTITION public.event_log_y2024m02_pkey;


--
-- Name: event_log_y2024m02_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_table_name_index ATTACH PARTITION public.event_log_y2024m02_table_name_action_timestamp_idx;


--
-- Name: event_log_y2024m02_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_timestamp_index ATTACH PARTITION public.event_log_y2024m02_timestamp_action_table_name_idx;


--
-- Name: event_log_y2024m03_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_action_index ATTACH PARTITION public.event_log_y2024m03_action_timestamp_table_name_idx;


--
-- Name: event_log_y2024m03_pkey; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_pkey ATTACH PARTITION public.event_log_y2024m03_pkey;


--
-- Name: event_log_y2024m03_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_table_name_index ATTACH PARTITION public.event_log_y2024m03_table_name_action_timestamp_idx;


--
-- Name: event_log_y2024m03_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_timestamp_index ATTACH PARTITION public.event_log_y2024m03_timestamp_action_table_name_idx;


--
-- Name: event_log_y2024m04_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_action_index ATTACH PARTITION public.event_log_y2024m04_action_timestamp_table_name_idx;


--
-- Name: event_log_y2024m04_pkey; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_pkey ATTACH PARTITION public.event_log_y2024m04_pkey;


--
-- Name: event_log_y2024m04_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_table_name_index ATTACH PARTITION public.event_log_y2024m04_table_name_action_timestamp_idx;


--
-- Name: event_log_y2024m04_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_timestamp_index ATTACH PARTITION public.event_log_y2024m04_timestamp_action_table_name_idx;


--
-- Name: event_log_y2024m05_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_action_index ATTACH PARTITION public.event_log_y2024m05_action_timestamp_table_name_idx;


--
-- Name: event_log_y2024m05_pkey; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_pkey ATTACH PARTITION public.event_log_y2024m05_pkey;


--
-- Name: event_log_y2024m05_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_table_name_index ATTACH PARTITION public.event_log_y2024m05_table_name_action_timestamp_idx;


--
-- Name: event_log_y2024m05_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_timestamp_index ATTACH PARTITION public.event_log_y2024m05_timestamp_action_table_name_idx;


--
-- Name: event_log_y2024m06_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_action_index ATTACH PARTITION public.event_log_y2024m06_action_timestamp_table_name_idx;


--
-- Name: event_log_y2024m06_pkey; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_pkey ATTACH PARTITION public.event_log_y2024m06_pkey;


--
-- Name: event_log_y2024m06_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_table_name_index ATTACH PARTITION public.event_log_y2024m06_table_name_action_timestamp_idx;


--
-- Name: event_log_y2024m06_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_timestamp_index ATTACH PARTITION public.event_log_y2024m06_timestamp_action_table_name_idx;


--
-- Name: event_log_y2024m07_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_action_index ATTACH PARTITION public.event_log_y2024m07_action_timestamp_table_name_idx;


--
-- Name: event_log_y2024m07_pkey; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_pkey ATTACH PARTITION public.event_log_y2024m07_pkey;


--
-- Name: event_log_y2024m07_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_table_name_index ATTACH PARTITION public.event_log_y2024m07_table_name_action_timestamp_idx;


--
-- Name: event_log_y2024m07_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_timestamp_index ATTACH PARTITION public.event_log_y2024m07_timestamp_action_table_name_idx;


--
-- Name: event_log_y2024m08_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_action_index ATTACH PARTITION public.event_log_y2024m08_action_timestamp_table_name_idx;


--
-- Name: event_log_y2024m08_pkey; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_pkey ATTACH PARTITION public.event_log_y2024m08_pkey;


--
-- Name: event_log_y2024m08_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_table_name_index ATTACH PARTITION public.event_log_y2024m08_table_name_action_timestamp_idx;


--
-- Name: event_log_y2024m08_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_timestamp_index ATTACH PARTITION public.event_log_y2024m08_timestamp_action_table_name_idx;


--
-- Name: event_log_y2024m09_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_action_index ATTACH PARTITION public.event_log_y2024m09_action_timestamp_table_name_idx;


--
-- Name: event_log_y2024m09_pkey; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_pkey ATTACH PARTITION public.event_log_y2024m09_pkey;


--
-- Name: event_log_y2024m09_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_table_name_index ATTACH PARTITION public.event_log_y2024m09_table_name_action_timestamp_idx;


--
-- Name: event_log_y2024m09_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_timestamp_index ATTACH PARTITION public.event_log_y2024m09_timestamp_action_table_name_idx;


--
-- Name: event_log_y2024m10_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_action_index ATTACH PARTITION public.event_log_y2024m10_action_timestamp_table_name_idx;


--
-- Name: event_log_y2024m10_pkey; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_pkey ATTACH PARTITION public.event_log_y2024m10_pkey;


--
-- Name: event_log_y2024m10_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_table_name_index ATTACH PARTITION public.event_log_y2024m10_table_name_action_timestamp_idx;


--
-- Name: event_log_y2024m10_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_timestamp_index ATTACH PARTITION public.event_log_y2024m10_timestamp_action_table_name_idx;


--
-- Name: event_log_y2024m11_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_action_index ATTACH PARTITION public.event_log_y2024m11_action_timestamp_table_name_idx;


--
-- Name: event_log_y2024m11_pkey; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_pkey ATTACH PARTITION public.event_log_y2024m11_pkey;


--
-- Name: event_log_y2024m11_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_table_name_index ATTACH PARTITION public.event_log_y2024m11_table_name_action_timestamp_idx;


--
-- Name: event_log_y2024m11_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_timestamp_index ATTACH PARTITION public.event_log_y2024m11_timestamp_action_table_name_idx;


--
-- Name: event_log_y2024m12_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_action_index ATTACH PARTITION public.event_log_y2024m12_action_timestamp_table_name_idx;


--
-- Name: event_log_y2024m12_pkey; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_pkey ATTACH PARTITION public.event_log_y2024m12_pkey;


--
-- Name: event_log_y2024m12_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_table_name_index ATTACH PARTITION public.event_log_y2024m12_table_name_action_timestamp_idx;


--
-- Name: event_log_y2024m12_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_timestamp_index ATTACH PARTITION public.event_log_y2024m12_timestamp_action_table_name_idx;


--
-- Name: event_log_y2025m01_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_action_index ATTACH PARTITION public.event_log_y2025m01_action_timestamp_table_name_idx;


--
-- Name: event_log_y2025m01_pkey; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_pkey ATTACH PARTITION public.event_log_y2025m01_pkey;


--
-- Name: event_log_y2025m01_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_table_name_index ATTACH PARTITION public.event_log_y2025m01_table_name_action_timestamp_idx;


--
-- Name: event_log_y2025m01_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_timestamp_index ATTACH PARTITION public.event_log_y2025m01_timestamp_action_table_name_idx;


--
-- Name: event_log_y2025m02_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_action_index ATTACH PARTITION public.event_log_y2025m02_action_timestamp_table_name_idx;


--
-- Name: event_log_y2025m02_pkey; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_pkey ATTACH PARTITION public.event_log_y2025m02_pkey;


--
-- Name: event_log_y2025m02_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_table_name_index ATTACH PARTITION public.event_log_y2025m02_table_name_action_timestamp_idx;


--
-- Name: event_log_y2025m02_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_timestamp_index ATTACH PARTITION public.event_log_y2025m02_timestamp_action_table_name_idx;


--
-- Name: event_log_y2025m03_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_action_index ATTACH PARTITION public.event_log_y2025m03_action_timestamp_table_name_idx;


--
-- Name: event_log_y2025m03_pkey; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_pkey ATTACH PARTITION public.event_log_y2025m03_pkey;


--
-- Name: event_log_y2025m03_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_table_name_index ATTACH PARTITION public.event_log_y2025m03_table_name_action_timestamp_idx;


--
-- Name: event_log_y2025m03_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_timestamp_index ATTACH PARTITION public.event_log_y2025m03_timestamp_action_table_name_idx;


--
-- Name: event_log_y2025m04_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_action_index ATTACH PARTITION public.event_log_y2025m04_action_timestamp_table_name_idx;


--
-- Name: event_log_y2025m04_pkey; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_pkey ATTACH PARTITION public.event_log_y2025m04_pkey;


--
-- Name: event_log_y2025m04_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_table_name_index ATTACH PARTITION public.event_log_y2025m04_table_name_action_timestamp_idx;


--
-- Name: event_log_y2025m04_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_timestamp_index ATTACH PARTITION public.event_log_y2025m04_timestamp_action_table_name_idx;


--
-- Name: event_log_y2025m05_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_action_index ATTACH PARTITION public.event_log_y2025m05_action_timestamp_table_name_idx;


--
-- Name: event_log_y2025m05_pkey; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_pkey ATTACH PARTITION public.event_log_y2025m05_pkey;


--
-- Name: event_log_y2025m05_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_table_name_index ATTACH PARTITION public.event_log_y2025m05_table_name_action_timestamp_idx;


--
-- Name: event_log_y2025m05_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_timestamp_index ATTACH PARTITION public.event_log_y2025m05_timestamp_action_table_name_idx;


--
-- Name: event_log_y2025m06_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_action_index ATTACH PARTITION public.event_log_y2025m06_action_timestamp_table_name_idx;


--
-- Name: event_log_y2025m06_pkey; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_pkey ATTACH PARTITION public.event_log_y2025m06_pkey;


--
-- Name: event_log_y2025m06_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_table_name_index ATTACH PARTITION public.event_log_y2025m06_table_name_action_timestamp_idx;


--
-- Name: event_log_y2025m06_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_timestamp_index ATTACH PARTITION public.event_log_y2025m06_timestamp_action_table_name_idx;


--
-- Name: event_log_y2025m07_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_action_index ATTACH PARTITION public.event_log_y2025m07_action_timestamp_table_name_idx;


--
-- Name: event_log_y2025m07_pkey; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_pkey ATTACH PARTITION public.event_log_y2025m07_pkey;


--
-- Name: event_log_y2025m07_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_table_name_index ATTACH PARTITION public.event_log_y2025m07_table_name_action_timestamp_idx;


--
-- Name: event_log_y2025m07_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_timestamp_index ATTACH PARTITION public.event_log_y2025m07_timestamp_action_table_name_idx;


--
-- Name: event_log_y2025m08_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_action_index ATTACH PARTITION public.event_log_y2025m08_action_timestamp_table_name_idx;


--
-- Name: event_log_y2025m08_pkey; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_pkey ATTACH PARTITION public.event_log_y2025m08_pkey;


--
-- Name: event_log_y2025m08_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_table_name_index ATTACH PARTITION public.event_log_y2025m08_table_name_action_timestamp_idx;


--
-- Name: event_log_y2025m08_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_timestamp_index ATTACH PARTITION public.event_log_y2025m08_timestamp_action_table_name_idx;


--
-- Name: event_log_y2025m09_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_action_index ATTACH PARTITION public.event_log_y2025m09_action_timestamp_table_name_idx;


--
-- Name: event_log_y2025m09_pkey; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_pkey ATTACH PARTITION public.event_log_y2025m09_pkey;


--
-- Name: event_log_y2025m09_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_table_name_index ATTACH PARTITION public.event_log_y2025m09_table_name_action_timestamp_idx;


--
-- Name: event_log_y2025m09_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_timestamp_index ATTACH PARTITION public.event_log_y2025m09_timestamp_action_table_name_idx;


--
-- Name: event_log_y2025m10_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_action_index ATTACH PARTITION public.event_log_y2025m10_action_timestamp_table_name_idx;


--
-- Name: event_log_y2025m10_pkey; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_pkey ATTACH PARTITION public.event_log_y2025m10_pkey;


--
-- Name: event_log_y2025m10_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_table_name_index ATTACH PARTITION public.event_log_y2025m10_table_name_action_timestamp_idx;


--
-- Name: event_log_y2025m10_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_timestamp_index ATTACH PARTITION public.event_log_y2025m10_timestamp_action_table_name_idx;


--
-- Name: event_log_y2025m11_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_action_index ATTACH PARTITION public.event_log_y2025m11_action_timestamp_table_name_idx;


--
-- Name: event_log_y2025m11_pkey; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_pkey ATTACH PARTITION public.event_log_y2025m11_pkey;


--
-- Name: event_log_y2025m11_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_table_name_index ATTACH PARTITION public.event_log_y2025m11_table_name_action_timestamp_idx;


--
-- Name: event_log_y2025m11_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_timestamp_index ATTACH PARTITION public.event_log_y2025m11_timestamp_action_table_name_idx;


--
-- Name: event_log_y2025m12_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_action_index ATTACH PARTITION public.event_log_y2025m12_action_timestamp_table_name_idx;


--
-- Name: event_log_y2025m12_pkey; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_pkey ATTACH PARTITION public.event_log_y2025m12_pkey;


--
-- Name: event_log_y2025m12_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_table_name_index ATTACH PARTITION public.event_log_y2025m12_table_name_action_timestamp_idx;


--
-- Name: event_log_y2025m12_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_timestamp_index ATTACH PARTITION public.event_log_y2025m12_timestamp_action_table_name_idx;


--
-- Name: event_log_y2026m01_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_action_index ATTACH PARTITION public.event_log_y2026m01_action_timestamp_table_name_idx;


--
-- Name: event_log_y2026m01_pkey; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_pkey ATTACH PARTITION public.event_log_y2026m01_pkey;


--
-- Name: event_log_y2026m01_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_table_name_index ATTACH PARTITION public.event_log_y2026m01_table_name_action_timestamp_idx;


--
-- Name: event_log_y2026m01_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_timestamp_index ATTACH PARTITION public.event_log_y2026m01_timestamp_action_table_name_idx;


--
-- Name: event_log_y2026m02_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_action_index ATTACH PARTITION public.event_log_y2026m02_action_timestamp_table_name_idx;


--
-- Name: event_log_y2026m02_pkey; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_pkey ATTACH PARTITION public.event_log_y2026m02_pkey;


--
-- Name: event_log_y2026m02_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_table_name_index ATTACH PARTITION public.event_log_y2026m02_table_name_action_timestamp_idx;


--
-- Name: event_log_y2026m02_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_timestamp_index ATTACH PARTITION public.event_log_y2026m02_timestamp_action_table_name_idx;


--
-- Name: event_log_y2026m03_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_action_index ATTACH PARTITION public.event_log_y2026m03_action_timestamp_table_name_idx;


--
-- Name: event_log_y2026m03_pkey; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_pkey ATTACH PARTITION public.event_log_y2026m03_pkey;


--
-- Name: event_log_y2026m03_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_table_name_index ATTACH PARTITION public.event_log_y2026m03_table_name_action_timestamp_idx;


--
-- Name: event_log_y2026m03_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_timestamp_index ATTACH PARTITION public.event_log_y2026m03_timestamp_action_table_name_idx;


--
-- Name: event_log_y2026m04_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_action_index ATTACH PARTITION public.event_log_y2026m04_action_timestamp_table_name_idx;


--
-- Name: event_log_y2026m04_pkey; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_pkey ATTACH PARTITION public.event_log_y2026m04_pkey;


--
-- Name: event_log_y2026m04_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_table_name_index ATTACH PARTITION public.event_log_y2026m04_table_name_action_timestamp_idx;


--
-- Name: event_log_y2026m04_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_timestamp_index ATTACH PARTITION public.event_log_y2026m04_timestamp_action_table_name_idx;


--
-- Name: event_log_y2026m05_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_action_index ATTACH PARTITION public.event_log_y2026m05_action_timestamp_table_name_idx;


--
-- Name: event_log_y2026m05_pkey; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_pkey ATTACH PARTITION public.event_log_y2026m05_pkey;


--
-- Name: event_log_y2026m05_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_table_name_index ATTACH PARTITION public.event_log_y2026m05_table_name_action_timestamp_idx;


--
-- Name: event_log_y2026m05_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_timestamp_index ATTACH PARTITION public.event_log_y2026m05_timestamp_action_table_name_idx;


--
-- Name: event_log_y2026m06_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_action_index ATTACH PARTITION public.event_log_y2026m06_action_timestamp_table_name_idx;


--
-- Name: event_log_y2026m06_pkey; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_pkey ATTACH PARTITION public.event_log_y2026m06_pkey;


--
-- Name: event_log_y2026m06_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_table_name_index ATTACH PARTITION public.event_log_y2026m06_table_name_action_timestamp_idx;


--
-- Name: event_log_y2026m06_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_timestamp_index ATTACH PARTITION public.event_log_y2026m06_timestamp_action_table_name_idx;


--
-- Name: event_log_y2026m07_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_action_index ATTACH PARTITION public.event_log_y2026m07_action_timestamp_table_name_idx;


--
-- Name: event_log_y2026m07_pkey; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_pkey ATTACH PARTITION public.event_log_y2026m07_pkey;


--
-- Name: event_log_y2026m07_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_table_name_index ATTACH PARTITION public.event_log_y2026m07_table_name_action_timestamp_idx;


--
-- Name: event_log_y2026m07_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_timestamp_index ATTACH PARTITION public.event_log_y2026m07_timestamp_action_table_name_idx;


--
-- Name: event_log_y2026m08_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_action_index ATTACH PARTITION public.event_log_y2026m08_action_timestamp_table_name_idx;


--
-- Name: event_log_y2026m08_pkey; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_pkey ATTACH PARTITION public.event_log_y2026m08_pkey;


--
-- Name: event_log_y2026m08_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_table_name_index ATTACH PARTITION public.event_log_y2026m08_table_name_action_timestamp_idx;


--
-- Name: event_log_y2026m08_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_timestamp_index ATTACH PARTITION public.event_log_y2026m08_timestamp_action_table_name_idx;


--
-- Name: event_log_y2026m09_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_action_index ATTACH PARTITION public.event_log_y2026m09_action_timestamp_table_name_idx;


--
-- Name: event_log_y2026m09_pkey; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_pkey ATTACH PARTITION public.event_log_y2026m09_pkey;


--
-- Name: event_log_y2026m09_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_table_name_index ATTACH PARTITION public.event_log_y2026m09_table_name_action_timestamp_idx;


--
-- Name: event_log_y2026m09_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_timestamp_index ATTACH PARTITION public.event_log_y2026m09_timestamp_action_table_name_idx;


--
-- Name: event_log_y2026m10_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_action_index ATTACH PARTITION public.event_log_y2026m10_action_timestamp_table_name_idx;


--
-- Name: event_log_y2026m10_pkey; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_pkey ATTACH PARTITION public.event_log_y2026m10_pkey;


--
-- Name: event_log_y2026m10_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_table_name_index ATTACH PARTITION public.event_log_y2026m10_table_name_action_timestamp_idx;


--
-- Name: event_log_y2026m10_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_timestamp_index ATTACH PARTITION public.event_log_y2026m10_timestamp_action_table_name_idx;


--
-- Name: event_log_y2026m11_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_action_index ATTACH PARTITION public.event_log_y2026m11_action_timestamp_table_name_idx;


--
-- Name: event_log_y2026m11_pkey; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_pkey ATTACH PARTITION public.event_log_y2026m11_pkey;


--
-- Name: event_log_y2026m11_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_table_name_index ATTACH PARTITION public.event_log_y2026m11_table_name_action_timestamp_idx;


--
-- Name: event_log_y2026m11_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_timestamp_index ATTACH PARTITION public.event_log_y2026m11_timestamp_action_table_name_idx;


--
-- Name: event_log_y2026m12_action_timestamp_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_action_index ATTACH PARTITION public.event_log_y2026m12_action_timestamp_table_name_idx;


--
-- Name: event_log_y2026m12_pkey; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_pkey ATTACH PARTITION public.event_log_y2026m12_pkey;


--
-- Name: event_log_y2026m12_table_name_action_timestamp_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_table_name_index ATTACH PARTITION public.event_log_y2026m12_table_name_action_timestamp_idx;


--
-- Name: event_log_y2026m12_timestamp_action_table_name_idx; Type: INDEX ATTACH; Schema: public; Owner: postgres
--

ALTER INDEX public.event_log_timestamp_index ATTACH PARTITION public.event_log_y2026m12_timestamp_action_table_name_idx;


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
-- Name: contexts contexts_audit; Type: TRIGGER; Schema: public; Owner: postgres
--

CREATE TRIGGER contexts_audit AFTER INSERT OR DELETE OR UPDATE ON public.contexts FOR EACH ROW EXECUTE FUNCTION public.event_logger();


--
-- Name: default_configs default_configs_audit; Type: TRIGGER; Schema: public; Owner: postgres
--

CREATE TRIGGER default_configs_audit AFTER INSERT OR DELETE OR UPDATE ON public.default_configs FOR EACH ROW EXECUTE FUNCTION public.event_logger();


--
-- Name: dimensions dimensions_audit; Type: TRIGGER; Schema: public; Owner: postgres
--

CREATE TRIGGER dimensions_audit AFTER INSERT OR DELETE OR UPDATE ON public.dimensions FOR EACH ROW EXECUTE FUNCTION public.event_logger();


--
-- Name: experiments experiments_audit; Type: TRIGGER; Schema: public; Owner: postgres
--

CREATE TRIGGER experiments_audit AFTER INSERT OR DELETE OR UPDATE ON public.experiments FOR EACH ROW EXECUTE FUNCTION public.event_logger();


--
-- Name: functions functions_audit; Type: TRIGGER; Schema: public; Owner: postgres
--

CREATE TRIGGER functions_audit AFTER INSERT OR DELETE OR UPDATE ON public.functions FOR EACH ROW EXECUTE FUNCTION public.event_logger();


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
-- Name: default_configs default_configs_function_name_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.default_configs
    ADD CONSTRAINT default_configs_function_name_fkey FOREIGN KEY (function_name) REFERENCES public.functions(function_name);


--
-- Name: dimensions dimensions_function_name_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY public.dimensions
    ADD CONSTRAINT dimensions_function_name_fkey FOREIGN KEY (function_name) REFERENCES public.functions(function_name);


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

CREATE TABLE dev_cac.config_versions (
    id bigint PRIMARY KEY,
    config json NOT NULL,
    config_hash TEXT NOT NULL,
    tags varchar(100)[] check (array_position(tags, null) is null),
    created_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);

CREATE INDEX IF NOT EXISTS config_verions_tags_index ON dev_cac.config_versions USING gin(tags);
CREATE INDEX IF NOT EXISTS config_versions_id_index ON dev_cac.config_versions(id);
--
-- Your SQL goes here
CREATE TABLE dev_cac.type_templates (
    type_name TEXT PRIMARY KEY,
    type_schema JSON NOT NULL,
    created_by TEXT NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    last_modified TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL
);
CREATE INDEX IF NOT EXISTS type_templates_index ON dev_cac.type_templates(type_name);
CREATE INDEX IF NOT EXISTS type_templates_created_at_index ON dev_cac.type_templates(created_at);
CREATE INDEX IF NOT EXISTS type_templates_last_modifed_index ON dev_cac.type_templates(last_modified);
INSERT INTO dev_cac.type_templates(type_name, type_schema, created_by, created_at)
VALUES (
        'Number',
        '{"type": "integer"}',
        'user@superposition.io',
        NOW()
    ),
    (
        'Decimal',
        '{"type": "number"}',
        'user@superposition.io',
        NOW()
    ),
    (
        'Boolean',
        '{"type": "boolean"}',
        'user@superposition.io',
        NOW()
    ),
    (
        'Enum',
        '{"type": "string", "enum": ["android", "ios"]}',
        'user@superposition.io',
        NOW()
    ),
    (
        'Pattern',
        '{"type": "string", "pattern": ".*"}',
        'user@superposition.io',
        NOW()
    );

CREATE TABLE test_cac.config_versions (
    id bigint PRIMARY KEY,
    config json NOT NULL,
    config_hash TEXT NOT NULL,
    tags varchar(100)[] check (array_position(tags, null) is null),
    created_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);

CREATE INDEX IF NOT EXISTS config_verions_tags_index ON test_cac.config_versions USING gin(tags);
CREATE INDEX IF NOT EXISTS config_versions_id_index ON test_cac.config_versions(id);
--
-- Your SQL goes here
CREATE TABLE test_cac.type_templates (
    type_name TEXT PRIMARY KEY,
    type_schema JSON NOT NULL,
    created_by TEXT NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    last_modified TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL
);
CREATE INDEX IF NOT EXISTS type_templates_index ON test_cac.type_templates(type_name);
CREATE INDEX IF NOT EXISTS type_templates_created_at_index ON test_cac.type_templates(created_at);
CREATE INDEX IF NOT EXISTS type_templates_last_modifed_index ON test_cac.type_templates(last_modified);
INSERT INTO test_cac.type_templates(type_name, type_schema, created_by, created_at)
VALUES (
        'Number',
        '{"type": "integer"}',
        'user@superposition.io',
        NOW()
    ),
    (
        'Decimal',
        '{"type": "number"}',
        'user@superposition.io',
        NOW()
    ),
    (
        'Boolean',
        '{"type": "boolean"}',
        'user@superposition.io',
        NOW()
    ),
    (
        'Enum',
        '{"type": "string", "enum": ["android", "ios"]}',
        'user@superposition.io',
        NOW()
    ),
    (
        'Pattern',
        '{"type": "string", "pattern": ".*"}',
        'user@superposition.io',
        NOW()
    );

--
-- PostgreSQL database dump complete
--

