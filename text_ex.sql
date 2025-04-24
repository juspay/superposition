--
-- PostgreSQL database dump
--

-- Dumped from database version 12.18
-- Dumped by pg_dump version 12.17

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
-- Name: test_experimentation; Type: SCHEMA; Schema: -; Owner: postgres
--

CREATE SCHEMA test_experimentation;


ALTER SCHEMA test_experimentation OWNER TO postgres;

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

SET default_table_access_method = heap;

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
b798009a-ad87-456e-afab-ec087d20c172	experiments	postgres	2024-08-21 22:46:30.302158	INSERT	\N	{"id":7094634333213954048,"created_at":"2023-08-08T11:03:57+05:30","created_by":"cac.admin@juspay.in","last_modified":"2023-08-08T11:24:04+05:30","name":"test_1","override_keys":["patch_entries","hyperos_placeholder_tracker_ios"],"status":"CONCLUDED","traffic_percentage":10,"context":{"and":[{"==":[{"var":"os"},"ios"]},{"==":[{"var":"clientId"},"meesho"]}]},"variants":[{"context_id":"463654d2ae200d14fa893df925879db44ebcf5cdd1bbbdcdc228f496cf48992e","id":"7094634333213954048-test-3-android","override_id":"21dd2bb6a08033075af47314999010ed294a5aca9e77948f4324a82b009de836","overrides":{"hyperos_placeholder_tracker_ios":{"etag":"2a47799a6a98df972e3de0844153b4eb","path":["live","assets","in.juspay.hyperos.placeholder","tracker"],"value":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.49/v1-tracker.jsa"},"patch_entries":["hyperos_placeholder_tracker_ios"]},"variant_type":"EXPERIMENTAL"},{"context_id":"11ba7e1de84822ddfc9ab586dcfc2fb1f29f2eeb2dc990edcd91fa19cd2fe099","id":"7094634333213954048-control-3-android","override_id":"8267961e60033cc274f990558f10c0277c90ef737bfcfc0d1fe3be15d97937b9","overrides":{"hyperos_placeholder_tracker_ios":{"etag":"2a47799a6a98df972e3de0844153b4eb","path":["live","assets","in.juspay.hyperos.placeholder","tracker"],"value":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.48/v1-tracker.jsa"},"patch_entries":["hyperos_placeholder_tracker_ios"]},"variant_type":"CONTROL"}],"last_modified_by":"cac.admin@juspay.in","chosen_variant":""}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7094634333213954048,'2023-08-08 11:03:57+05:30','cac.admin@juspay.in','2023-08-08 11:24:04+05:30','test_1','{patch_entries,hyperos_placeholder_tracker_ios}','CONCLUDED',10,'{"and":[{"==":[{"var":"os"},"ios"]},{"==":[{"var":"clientId"},"meesho"]}]}','[{"context_id":"463654d2ae200d14fa893df925879db44ebcf5cdd1bbbdcdc228f496cf48992e","id":"7094634333213954048-test-3-android","override_id":"21dd2bb6a08033075af47314999010ed294a5aca9e77948f4324a82b009de836","overrides":{"hyperos_placeholder_tracker_ios":{"etag":"2a47799a6a98df972e3de0844153b4eb","path":["live","assets","in.juspay.hyperos.placeholder","tracker"],"value":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.49/v1-tracker.jsa"},"patch_entries":["hyperos_placeholder_tracker_ios"]},"variant_type":"EXPERIMENTAL"},{"context_id":"11ba7e1de84822ddfc9ab586dcfc2fb1f29f2eeb2dc990edcd91fa19cd2fe099","id":"7094634333213954048-control-3-android","override_id":"8267961e60033cc274f990558f10c0277c90ef737bfcfc0d1fe3be15d97937b9","overrides":{"hyperos_placeholder_tracker_ios":{"etag":"2a47799a6a98df972e3de0844153b4eb","path":["live","assets","in.juspay.hyperos.placeholder","tracker"],"value":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.48/v1-tracker.jsa"},"patch_entries":["hyperos_placeholder_tracker_ios"]},"variant_type":"CONTROL"}]','cac.admin@juspay.in','')
dc5c9a54-6cea-4085-86f6-7dae13ee1dda	experiments	postgres	2024-08-21 22:46:30.304843	INSERT	\N	{"id":7094645531124830208,"created_at":"2023-08-08T11:48:26+05:30","created_by":"cac.admin@juspay.in","last_modified":"2023-08-08T13:02:11+05:30","name":"test_1","override_keys":["patch_entries","hyperos_placeholder_tracker_ios"],"status":"CONCLUDED","traffic_percentage":5,"context":{"and":[{"==":[{"var":"os"},"ios"]},{"==":[{"var":"clientId"},"meesho"]}]},"variants":[{"context_id":"637a18a13b5b9073e108e2281cacc58bd1a8e822168adc3dcab295bfc5a4bc45","id":"7094645531124830208-test-3-android","override_id":"21dd2bb6a08033075af47314999010ed294a5aca9e77948f4324a82b009de836","overrides":{"hyperos_placeholder_tracker_ios":{"etag":"2a47799a6a98df972e3de0844153b4eb","path":["live","assets","in.juspay.hyperos.placeholder","tracker"],"value":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.49/v1-tracker.jsa"},"patch_entries":["hyperos_placeholder_tracker_ios"]},"variant_type":"EXPERIMENTAL"},{"context_id":"adffedeed3338aff48d25b7681455bb2cf8ad1ee3f866a98cc9ce13120174d5b","id":"7094645531124830208-control-3-android","override_id":"8267961e60033cc274f990558f10c0277c90ef737bfcfc0d1fe3be15d97937b9","overrides":{"hyperos_placeholder_tracker_ios":{"etag":"2a47799a6a98df972e3de0844153b4eb","path":["live","assets","in.juspay.hyperos.placeholder","tracker"],"value":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.48/v1-tracker.jsa"},"patch_entries":["hyperos_placeholder_tracker_ios"]},"variant_type":"CONTROL"}],"last_modified_by":"cac.admin@juspay.in","chosen_variant":""}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7094645531124830208,'2023-08-08 11:48:26+05:30','cac.admin@juspay.in','2023-08-08 13:02:11+05:30','test_1','{patch_entries,hyperos_placeholder_tracker_ios}','CONCLUDED',5,'{"and":[{"==":[{"var":"os"},"ios"]},{"==":[{"var":"clientId"},"meesho"]}]}','[{"context_id":"637a18a13b5b9073e108e2281cacc58bd1a8e822168adc3dcab295bfc5a4bc45","id":"7094645531124830208-test-3-android","override_id":"21dd2bb6a08033075af47314999010ed294a5aca9e77948f4324a82b009de836","overrides":{"hyperos_placeholder_tracker_ios":{"etag":"2a47799a6a98df972e3de0844153b4eb","path":["live","assets","in.juspay.hyperos.placeholder","tracker"],"value":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.49/v1-tracker.jsa"},"patch_entries":["hyperos_placeholder_tracker_ios"]},"variant_type":"EXPERIMENTAL"},{"context_id":"adffedeed3338aff48d25b7681455bb2cf8ad1ee3f866a98cc9ce13120174d5b","id":"7094645531124830208-control-3-android","override_id":"8267961e60033cc274f990558f10c0277c90ef737bfcfc0d1fe3be15d97937b9","overrides":{"hyperos_placeholder_tracker_ios":{"etag":"2a47799a6a98df972e3de0844153b4eb","path":["live","assets","in.juspay.hyperos.placeholder","tracker"],"value":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.48/v1-tracker.jsa"},"patch_entries":["hyperos_placeholder_tracker_ios"]},"variant_type":"CONTROL"}]','cac.admin@juspay.in','')
25d270c6-331d-43f1-8301-a786010eb2d9	experiments	postgres	2024-08-21 22:46:30.305287	INSERT	\N	{"id":7094665091173453824,"created_at":"2023-08-08T13:06:10+05:30","created_by":"cac.admin@juspay.in","last_modified":"2023-08-08T13:23:45+05:30","name":"test_1","override_keys":["patch_entries","hyperos_placeholder_tracker_ios"],"status":"CONCLUDED","traffic_percentage":10,"context":{"and":[{"==":[{"var":"os"},"ios"]},{"==":[{"var":"clientId"},"meesho"]}]},"variants":[{"context_id":"047e874ab4d7ed4c107223639833f26047f0a1733ec27505f6a7a3f3f2ab7ad7","id":"7094665091173453824-test-3-android","override_id":"e0e0fed9750c3fa7929e493acddf5ba2acb6db63b3b0bafd3f9697903c2ddaa3","overrides":{"hyperos_placeholder_tracker_ios":{"etag":"2a47799a6a98df972e3de0844153b4eb","path":["live","assets","in.juspay.hyperos.placeholder","tracker"],"value":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.51/v1-tracker.jsa"},"patch_entries":["hyperos_placeholder_tracker_ios"]},"variant_type":"EXPERIMENTAL"},{"context_id":"cf8e92b97557b0db1274c71240031036ca4bcc62674cffc2e9dc7ea2df56224f","id":"7094665091173453824-control-3-android","override_id":"bbfa5a8475ed19e6e907840a872b71ea49246ea3fe378676373acbe782264f7d","overrides":{"hyperos_placeholder_tracker_ios":{"etag":"2a47799a6a98df972e3de0844153b4eb","path":["live","assets","in.juspay.hyperos.placeholder","tracker"],"value":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.50/v1-tracker.jsa"},"patch_entries":["hyperos_placeholder_tracker_ios"]},"variant_type":"CONTROL"}],"last_modified_by":"cac.admin@juspay.in","chosen_variant":""}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7094665091173453824,'2023-08-08 13:06:10+05:30','cac.admin@juspay.in','2023-08-08 13:23:45+05:30','test_1','{patch_entries,hyperos_placeholder_tracker_ios}','CONCLUDED',10,'{"and":[{"==":[{"var":"os"},"ios"]},{"==":[{"var":"clientId"},"meesho"]}]}','[{"context_id":"047e874ab4d7ed4c107223639833f26047f0a1733ec27505f6a7a3f3f2ab7ad7","id":"7094665091173453824-test-3-android","override_id":"e0e0fed9750c3fa7929e493acddf5ba2acb6db63b3b0bafd3f9697903c2ddaa3","overrides":{"hyperos_placeholder_tracker_ios":{"etag":"2a47799a6a98df972e3de0844153b4eb","path":["live","assets","in.juspay.hyperos.placeholder","tracker"],"value":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.51/v1-tracker.jsa"},"patch_entries":["hyperos_placeholder_tracker_ios"]},"variant_type":"EXPERIMENTAL"},{"context_id":"cf8e92b97557b0db1274c71240031036ca4bcc62674cffc2e9dc7ea2df56224f","id":"7094665091173453824-control-3-android","override_id":"bbfa5a8475ed19e6e907840a872b71ea49246ea3fe378676373acbe782264f7d","overrides":{"hyperos_placeholder_tracker_ios":{"etag":"2a47799a6a98df972e3de0844153b4eb","path":["live","assets","in.juspay.hyperos.placeholder","tracker"],"value":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.50/v1-tracker.jsa"},"patch_entries":["hyperos_placeholder_tracker_ios"]},"variant_type":"CONTROL"}]','cac.admin@juspay.in','')
340bd2a9-cd2e-40d3-8f7e-df8c0867d9cf	experiments	postgres	2024-08-21 22:46:30.305657	INSERT	\N	{"id":7097892661364920320,"created_at":"2023-08-17T10:51:23+05:30","created_by":"cac.admin@juspay.in","last_modified":"2023-08-17T10:58:24+05:30","name":"experiment-1","override_keys":["hyperos_placeholder_tracker_android"],"status":"CONCLUDED","traffic_percentage":20,"context":{"and":[{"==":[{"var":"os"},"android"]},{"==":[{"var":"clientId"},"galactustest"]}]},"variants":[{"context_id":"4ab3d289c871751c3fc1fa69f48c6d0ffb7adf8e4e6776d59d7e3c7d43b9d539","id":"7097892661364920320-control","override_id":"78689505959cee24a30383f6445d6d169cb731fb2b0d525b3657920fdded3627","overrides":{"hyperos_placeholder_tracker_android":{"etag":"piyaz","path":["live","assets","in.juspay.hyperos.placeholder","tracker"],"value":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.50/v1-tracker.zip"}},"variant_type":"CONTROL"},{"context_id":"ca70564343c90923aa98078d4c133dbd262e1bc556efd9c8e91e4a1b57bbf795","id":"7097892661364920320-test1","override_id":"667227c74fff8101146fe1d1dfa2384e8344f5711e43abc63793e83c13d76a49","overrides":{"hyperos_placeholder_tracker_android":{"etag":"alloo","path":["live","assets","in.juspay.hyperos.placeholder","tracker"],"value":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.51/v1-tracker.zip"}},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"cac.admin@juspay.in","chosen_variant":""}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7097892661364920320,'2023-08-17 10:51:23+05:30','cac.admin@juspay.in','2023-08-17 10:58:24+05:30','experiment-1','{hyperos_placeholder_tracker_android}','CONCLUDED',20,'{"and":[{"==":[{"var":"os"},"android"]},{"==":[{"var":"clientId"},"galactustest"]}]}','[{"context_id":"4ab3d289c871751c3fc1fa69f48c6d0ffb7adf8e4e6776d59d7e3c7d43b9d539","id":"7097892661364920320-control","override_id":"78689505959cee24a30383f6445d6d169cb731fb2b0d525b3657920fdded3627","overrides":{"hyperos_placeholder_tracker_android":{"etag":"piyaz","path":["live","assets","in.juspay.hyperos.placeholder","tracker"],"value":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.50/v1-tracker.zip"}},"variant_type":"CONTROL"},{"context_id":"ca70564343c90923aa98078d4c133dbd262e1bc556efd9c8e91e4a1b57bbf795","id":"7097892661364920320-test1","override_id":"667227c74fff8101146fe1d1dfa2384e8344f5711e43abc63793e83c13d76a49","overrides":{"hyperos_placeholder_tracker_android":{"etag":"alloo","path":["live","assets","in.juspay.hyperos.placeholder","tracker"],"value":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.51/v1-tracker.zip"}},"variant_type":"EXPERIMENTAL"}]','cac.admin@juspay.in','')
5720ba21-614d-4da4-8080-b2cbe71f39d2	experiments	postgres	2024-08-21 22:46:30.306022	INSERT	\N	{"id":7097895312416706560,"created_at":"2023-08-17T11:01:55+05:30","created_by":"cac.admin@juspay.in","last_modified":"2023-08-17T11:13:31+05:30","name":"experiment-1","override_keys":["hyperos_placeholder_tracker_android"],"status":"CONCLUDED","traffic_percentage":20,"context":{"and":[{"==":[{"var":"os"},"android"]},{"==":[{"var":"clientId"},"galactustest"]}]},"variants":[{"context_id":"03422b5560e430d93b994d155759b70982fe116b61ac6e76012b699ee7cd64da","id":"7097895312416706560-control","override_id":"78689505959cee24a30383f6445d6d169cb731fb2b0d525b3657920fdded3627","overrides":{"hyperos_placeholder_tracker_android":{"etag":"piyaz","path":["live","assets","in.juspay.hyperos.placeholder","tracker"],"value":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.50/v1-tracker.zip"}},"variant_type":"CONTROL"},{"context_id":"85384c579c536e9b6917bc5825681145e8a1a5d2692ab435fae2a1e8ecb315ae","id":"7097895312416706560-test1","override_id":"667227c74fff8101146fe1d1dfa2384e8344f5711e43abc63793e83c13d76a49","overrides":{"hyperos_placeholder_tracker_android":{"etag":"alloo","path":["live","assets","in.juspay.hyperos.placeholder","tracker"],"value":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.51/v1-tracker.zip"}},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"cac.admin@juspay.in","chosen_variant":""}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7097895312416706560,'2023-08-17 11:01:55+05:30','cac.admin@juspay.in','2023-08-17 11:13:31+05:30','experiment-1','{hyperos_placeholder_tracker_android}','CONCLUDED',20,'{"and":[{"==":[{"var":"os"},"android"]},{"==":[{"var":"clientId"},"galactustest"]}]}','[{"context_id":"03422b5560e430d93b994d155759b70982fe116b61ac6e76012b699ee7cd64da","id":"7097895312416706560-control","override_id":"78689505959cee24a30383f6445d6d169cb731fb2b0d525b3657920fdded3627","overrides":{"hyperos_placeholder_tracker_android":{"etag":"piyaz","path":["live","assets","in.juspay.hyperos.placeholder","tracker"],"value":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.50/v1-tracker.zip"}},"variant_type":"CONTROL"},{"context_id":"85384c579c536e9b6917bc5825681145e8a1a5d2692ab435fae2a1e8ecb315ae","id":"7097895312416706560-test1","override_id":"667227c74fff8101146fe1d1dfa2384e8344f5711e43abc63793e83c13d76a49","overrides":{"hyperos_placeholder_tracker_android":{"etag":"alloo","path":["live","assets","in.juspay.hyperos.placeholder","tracker"],"value":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.51/v1-tracker.zip"}},"variant_type":"EXPERIMENTAL"}]','cac.admin@juspay.in','')
0163a58a-dd79-43f9-866d-5d870a420d56	experiments	postgres	2024-08-21 22:46:30.30636	INSERT	\N	{"id":7097905532270415872,"created_at":"2023-08-17T11:42:31+05:30","created_by":"cac.admin@juspay.in","last_modified":"2023-08-17T11:44:02+05:30","name":"experiment-1","override_keys":["hyperos_placeholder_tracker_android"],"status":"CONCLUDED","traffic_percentage":0,"context":{"and":[{"==":[{"var":"os"},"android"]},{"==":[{"var":"clientId"},"galactustest"]}]},"variants":[{"context_id":"291fda6700c80d4ae1e60c758e082cbcb51d5eccddce0741dba024ea02b8d7e0","id":"7097905532270415872-control","override_id":"78689505959cee24a30383f6445d6d169cb731fb2b0d525b3657920fdded3627","overrides":{"hyperos_placeholder_tracker_android":{"etag":"piyaz","path":["live","assets","in.juspay.hyperos.placeholder","tracker"],"value":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.50/v1-tracker.zip"}},"variant_type":"CONTROL"},{"context_id":"3a66454533ed80493d7d77af14233fdb8bbb9b15300c5d215702237e0777e308","id":"7097905532270415872-test1","override_id":"667227c74fff8101146fe1d1dfa2384e8344f5711e43abc63793e83c13d76a49","overrides":{"hyperos_placeholder_tracker_android":{"etag":"alloo","path":["live","assets","in.juspay.hyperos.placeholder","tracker"],"value":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.51/v1-tracker.zip"}},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"cac.admin@juspay.in","chosen_variant":""}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7097905532270415872,'2023-08-17 11:42:31+05:30','cac.admin@juspay.in','2023-08-17 11:44:02+05:30','experiment-1','{hyperos_placeholder_tracker_android}','CONCLUDED',0,'{"and":[{"==":[{"var":"os"},"android"]},{"==":[{"var":"clientId"},"galactustest"]}]}','[{"context_id":"291fda6700c80d4ae1e60c758e082cbcb51d5eccddce0741dba024ea02b8d7e0","id":"7097905532270415872-control","override_id":"78689505959cee24a30383f6445d6d169cb731fb2b0d525b3657920fdded3627","overrides":{"hyperos_placeholder_tracker_android":{"etag":"piyaz","path":["live","assets","in.juspay.hyperos.placeholder","tracker"],"value":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.50/v1-tracker.zip"}},"variant_type":"CONTROL"},{"context_id":"3a66454533ed80493d7d77af14233fdb8bbb9b15300c5d215702237e0777e308","id":"7097905532270415872-test1","override_id":"667227c74fff8101146fe1d1dfa2384e8344f5711e43abc63793e83c13d76a49","overrides":{"hyperos_placeholder_tracker_android":{"etag":"alloo","path":["live","assets","in.juspay.hyperos.placeholder","tracker"],"value":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.51/v1-tracker.zip"}},"variant_type":"EXPERIMENTAL"}]','cac.admin@juspay.in','')
fa744e0c-51f0-477a-8fde-65253552e04d	experiments	postgres	2024-08-21 22:46:30.306678	INSERT	\N	{"id":7099775689305493504,"created_at":"2023-08-22T15:33:51+05:30","created_by":"cac.admin@juspay.in","last_modified":"2023-08-22T15:47:16+05:30","name":"Godel_ACS_release","override_keys":["godel_acs_android","godel_config_android","godel_placeholder_acs_android","godel_placeholder_config_android","patch_entries"],"status":"INPROGRESS","traffic_percentage":20,"context":{"and":[{"==":[{"var":"os"},"android"]}]},"variants":[{"context_id":"7f408004fcffe6bf4c7f1c2038266b2f07df122d39a2fb14df7a708cde020bd5","id":"7099775689305493504-control","override_id":"8f9b7e2df82ea95b5eb2f162bfb3520d4eb1ae215c670ca60f67fe0fd9ac301e","overrides":{"godel_acs_android":{"etag":"2014efe3000a5fdc1ea7a70b9c9a9f69","path":["live","assets","in.juspay.godel","acs_js_source"],"value":"https://assets.juspay.in/hyper/bundles/acs/in.juspay.godel/1.2.99/v1-acs.zip"},"godel_config_android":{"etag":"acf7efedd2cdfd18d101db7edd154cdc","path":["live","assets","in.juspay.godel","config"],"value":"https://assets.juspay.in/hyper/bundles/config/in.juspay.godel/1.2.10/v1-config.zip"},"godel_placeholder_acs_android":{"etag":"2014efe3000a5fdc1ea7a70b9c9a9f69","path":["live","assets","in.juspay.godel.placeholder","acs_js_source"],"value":"https://assets.juspay.in/hyper/bundles/acs/in.juspay.godel/1.2.99/v1-acs.zip"},"godel_placeholder_config_android":{"etag":"acf7efedd2cdfd18d101db7edd154cdc","path":["live","assets","in.juspay.godel.placeholder","config"],"value":"https://assets.juspay.in/hyper/bundles/config/in.juspay.godel/1.2.10/v1-config.zip"},"patch_entries":["godel_bundle_android","godel_placeholder_bundle_android","godel_config_android","godel_placeholder_config_android","godel_acs_android","godel_placeholder_acs_android","hyperos_placeholder_tracker_android"]},"variant_type":"CONTROL"},{"context_id":"32b0795d4fe06e18c810298a75ef679304b192bc19880fb510f4c0564cc9732d","id":"7099775689305493504-test","override_id":"568f1c5e361db3c403f9df446b763efeb451f36c6bac07cb66f6720450d2bfe3","overrides":{"godel_acs_android":{"etag":"a16f12ac0c078e21eed52ab452cbf042","path":["live","assets","in.juspay.godel","acs_js_source"],"value":"https://assets.juspay.in/hyper/bundles/acs/in.juspay.godel/1.2.100/v1-acs.zip"},"godel_config_android":{"etag":"46c2b1a55e9d27445e3b1547ac0a4689","path":["live","assets","in.juspay.godel","config"],"value":"https://assets.juspay.in/hyper/bundles/config/in.juspay.godel/1.2.11/v1-config.zip"},"godel_placeholder_acs_android":{"etag":"a16f12ac0c078e21eed52ab452cbf042","path":["live","assets","in.juspay.godel.placeholder","acs_js_source"],"value":"https://assets.juspay.in/hyper/bundles/acs/in.juspay.godel/1.2.100/v1-acs.zip"},"godel_placeholder_config_android":{"etag":"46c2b1a55e9d27445e3b1547ac0a4689","path":["live","assets","in.juspay.godel.placeholder","config"],"value":"https://assets.juspay.in/hyper/bundles/config/in.juspay.godel/1.2.11/v1-config.zip"},"patch_entries":["godel_bundle_android","godel_placeholder_bundle_android","godel_config_android","godel_placeholder_config_android","godel_acs_android","godel_placeholder_acs_android","hyperos_placeholder_tracker_android"]},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"cac.admin@juspay.in","chosen_variant":""}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7099775689305493504,'2023-08-22 15:33:51+05:30','cac.admin@juspay.in','2023-08-22 15:47:16+05:30','Godel_ACS_release','{godel_acs_android,godel_config_android,godel_placeholder_acs_android,godel_placeholder_config_android,patch_entries}','INPROGRESS',20,'{"and":[{"==":[{"var":"os"},"android"]}]}','[{"context_id":"7f408004fcffe6bf4c7f1c2038266b2f07df122d39a2fb14df7a708cde020bd5","id":"7099775689305493504-control","override_id":"8f9b7e2df82ea95b5eb2f162bfb3520d4eb1ae215c670ca60f67fe0fd9ac301e","overrides":{"godel_acs_android":{"etag":"2014efe3000a5fdc1ea7a70b9c9a9f69","path":["live","assets","in.juspay.godel","acs_js_source"],"value":"https://assets.juspay.in/hyper/bundles/acs/in.juspay.godel/1.2.99/v1-acs.zip"},"godel_config_android":{"etag":"acf7efedd2cdfd18d101db7edd154cdc","path":["live","assets","in.juspay.godel","config"],"value":"https://assets.juspay.in/hyper/bundles/config/in.juspay.godel/1.2.10/v1-config.zip"},"godel_placeholder_acs_android":{"etag":"2014efe3000a5fdc1ea7a70b9c9a9f69","path":["live","assets","in.juspay.godel.placeholder","acs_js_source"],"value":"https://assets.juspay.in/hyper/bundles/acs/in.juspay.godel/1.2.99/v1-acs.zip"},"godel_placeholder_config_android":{"etag":"acf7efedd2cdfd18d101db7edd154cdc","path":["live","assets","in.juspay.godel.placeholder","config"],"value":"https://assets.juspay.in/hyper/bundles/config/in.juspay.godel/1.2.10/v1-config.zip"},"patch_entries":["godel_bundle_android","godel_placeholder_bundle_android","godel_config_android","godel_placeholder_config_android","godel_acs_android","godel_placeholder_acs_android","hyperos_placeholder_tracker_android"]},"variant_type":"CONTROL"},{"context_id":"32b0795d4fe06e18c810298a75ef679304b192bc19880fb510f4c0564cc9732d","id":"7099775689305493504-test","override_id":"568f1c5e361db3c403f9df446b763efeb451f36c6bac07cb66f6720450d2bfe3","overrides":{"godel_acs_android":{"etag":"a16f12ac0c078e21eed52ab452cbf042","path":["live","assets","in.juspay.godel","acs_js_source"],"value":"https://assets.juspay.in/hyper/bundles/acs/in.juspay.godel/1.2.100/v1-acs.zip"},"godel_config_android":{"etag":"46c2b1a55e9d27445e3b1547ac0a4689","path":["live","assets","in.juspay.godel","config"],"value":"https://assets.juspay.in/hyper/bundles/config/in.juspay.godel/1.2.11/v1-config.zip"},"godel_placeholder_acs_android":{"etag":"a16f12ac0c078e21eed52ab452cbf042","path":["live","assets","in.juspay.godel.placeholder","acs_js_source"],"value":"https://assets.juspay.in/hyper/bundles/acs/in.juspay.godel/1.2.100/v1-acs.zip"},"godel_placeholder_config_android":{"etag":"46c2b1a55e9d27445e3b1547ac0a4689","path":["live","assets","in.juspay.godel.placeholder","config"],"value":"https://assets.juspay.in/hyper/bundles/config/in.juspay.godel/1.2.11/v1-config.zip"},"patch_entries":["godel_bundle_android","godel_placeholder_bundle_android","godel_config_android","godel_placeholder_config_android","godel_acs_android","godel_placeholder_acs_android","hyperos_placeholder_tracker_android"]},"variant_type":"EXPERIMENTAL"}]','cac.admin@juspay.in','')
2023c30d-501c-41c9-8039-9072d7347086	experiments	postgres	2024-08-21 22:46:30.307423	INSERT	\N	{"id":7100097235819565056,"created_at":"2023-08-23T12:51:34+05:30","created_by":"cac.admin@juspay.in","last_modified":"2023-08-23T12:54:16+05:30","name":"Tracker_release","override_keys":["hyperos_placeholder_tracker_android","patch_entries"],"status":"CONCLUDED","traffic_percentage":0,"context":{"and":[{"==":[{"var":"os"},"android"]},{"==":[{"var":"clientId"},"poonawalla"]}]},"variants":[{"context_id":"5ff1ae83774a5bd997fcfb24abb57a8a8b9e7c140fd3ed07331029fad0774694","id":"7100097235819565056-control","override_id":"8f3101e5e15b0ced15b806037127158ad1bb4f538e04b612a9cf67760aa55d05","overrides":{"hyperos_placeholder_tracker_android":{"etag":"72e0db9d6d92d558b8a3dbb2e6abf0ec","path":["live","assets","in.juspay.hyperos.placeholder","tracker"],"value":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.50/v1-tracker.zip"},"patch_entries":["godel_bundle_android","godel_placeholder_bundle_android","godel_config_android","godel_placeholder_config_android","godel_acs_android","godel_placeholder_acs_android","hyperos_placeholder_tracker_android"]},"variant_type":"CONTROL"},{"context_id":"f3f9aabb93aa1c6a353de118195b63a2637d7ebbf9bb22c8a76c967398bc873a","id":"7100097235819565056-test","override_id":"f9081984a4fca32346e1c115aebcf67ac1ae098ffd6b26e66ad88906f06f5f33","overrides":{"hyperos_placeholder_tracker_android":{"etag":"39b4e3e33950355a8c32e9092a954eff","path":["live","assets","in.juspay.hyperos.placeholder","tracker"],"value":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.51/v1-tracker.zip"},"patch_entries":["godel_bundle_android","godel_placeholder_bundle_android","godel_config_android","godel_placeholder_config_android","godel_acs_android","godel_placeholder_acs_android","hyperos_placeholder_tracker_android"]},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"cac.admin@juspay.in","chosen_variant":""}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7100097235819565056,'2023-08-23 12:51:34+05:30','cac.admin@juspay.in','2023-08-23 12:54:16+05:30','Tracker_release','{hyperos_placeholder_tracker_android,patch_entries}','CONCLUDED',0,'{"and":[{"==":[{"var":"os"},"android"]},{"==":[{"var":"clientId"},"poonawalla"]}]}','[{"context_id":"5ff1ae83774a5bd997fcfb24abb57a8a8b9e7c140fd3ed07331029fad0774694","id":"7100097235819565056-control","override_id":"8f3101e5e15b0ced15b806037127158ad1bb4f538e04b612a9cf67760aa55d05","overrides":{"hyperos_placeholder_tracker_android":{"etag":"72e0db9d6d92d558b8a3dbb2e6abf0ec","path":["live","assets","in.juspay.hyperos.placeholder","tracker"],"value":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.50/v1-tracker.zip"},"patch_entries":["godel_bundle_android","godel_placeholder_bundle_android","godel_config_android","godel_placeholder_config_android","godel_acs_android","godel_placeholder_acs_android","hyperos_placeholder_tracker_android"]},"variant_type":"CONTROL"},{"context_id":"f3f9aabb93aa1c6a353de118195b63a2637d7ebbf9bb22c8a76c967398bc873a","id":"7100097235819565056-test","override_id":"f9081984a4fca32346e1c115aebcf67ac1ae098ffd6b26e66ad88906f06f5f33","overrides":{"hyperos_placeholder_tracker_android":{"etag":"39b4e3e33950355a8c32e9092a954eff","path":["live","assets","in.juspay.hyperos.placeholder","tracker"],"value":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.51/v1-tracker.zip"},"patch_entries":["godel_bundle_android","godel_placeholder_bundle_android","godel_config_android","godel_placeholder_config_android","godel_acs_android","godel_placeholder_acs_android","hyperos_placeholder_tracker_android"]},"variant_type":"EXPERIMENTAL"}]','cac.admin@juspay.in','')
52f366c8-46c4-4b7b-8f2b-72b87c5356f7	experiments	postgres	2024-08-21 22:46:30.307802	INSERT	\N	{"id":7100467927845048320,"created_at":"2023-08-24T13:24:34+05:30","created_by":"cac.admin@juspay.in","last_modified":"2023-08-24T13:25:45+05:30","name":"experiment-1","override_keys":["pmTestKey1","pmTestKey2"],"status":"CONCLUDED","traffic_percentage":0,"context":{"and":[{"==":[{"var":"os"},"ios"]},{"==":[{"var":"clientId"},"testClientCac1"]}]},"variants":[{"context_id":"e12e114451c36b1fc75364f27c123fc2d6bb7cc5002dd8898214cfc708d414f8","id":"7100467927845048320-control","override_id":"085e637459203733acb435d0bd921081b10a46a3eb192924a332e414bf266cf7","overrides":{"pmTestKey1":"value1-control","pmTestKey2":"value1-control"},"variant_type":"CONTROL"},{"context_id":"f0a4ac6d35cb5442fdbea7cdc6b3fd69048c1e5ed1d6420a4383667665b7f6a5","id":"7100467927845048320-test1","override_id":"65bcc9acaebd88355f850d7087e269f36d1316d7aeeddcf72cece682f99691c1","overrides":{"pmTestKey1":"value2-test","pmTestKey2":"value2-test"},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"cac.admin@juspay.in","chosen_variant":""}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7100467927845048320,'2023-08-24 13:24:34+05:30','cac.admin@juspay.in','2023-08-24 13:25:45+05:30','experiment-1','{pmTestKey1,pmTestKey2}','CONCLUDED',0,'{"and":[{"==":[{"var":"os"},"ios"]},{"==":[{"var":"clientId"},"testClientCac1"]}]}','[{"context_id":"e12e114451c36b1fc75364f27c123fc2d6bb7cc5002dd8898214cfc708d414f8","id":"7100467927845048320-control","override_id":"085e637459203733acb435d0bd921081b10a46a3eb192924a332e414bf266cf7","overrides":{"pmTestKey1":"value1-control","pmTestKey2":"value1-control"},"variant_type":"CONTROL"},{"context_id":"f0a4ac6d35cb5442fdbea7cdc6b3fd69048c1e5ed1d6420a4383667665b7f6a5","id":"7100467927845048320-test1","override_id":"65bcc9acaebd88355f850d7087e269f36d1316d7aeeddcf72cece682f99691c1","overrides":{"pmTestKey1":"value2-test","pmTestKey2":"value2-test"},"variant_type":"EXPERIMENTAL"}]','cac.admin@juspay.in','')
4e221b08-3c62-47a1-854f-ae40642e3579	experiments	postgres	2024-08-21 22:46:30.308113	INSERT	\N	{"id":7100418327268429824,"created_at":"2023-08-24T10:07:28+05:30","created_by":"cac.admin@juspay.in","last_modified":"2023-08-24T15:51:54+05:30","name":"experiment-1","override_keys":["pmTestKey1","pmTestKey2"],"status":"CONCLUDED","traffic_percentage":2,"context":{"and":[{"==":[{"var":"os"},"ios"]},{"==":[{"var":"clientId"},"testClientCac1"]}]},"variants":[{"context_id":"a43666147f7d1cd840ddc4c8da53cf19a351630cd843d74dbffe7d51c184ee51","id":"7100418327268429824-control","override_id":"085e637459203733acb435d0bd921081b10a46a3eb192924a332e414bf266cf7","overrides":{"pmTestKey1":"value1-control","pmTestKey2":"value1-control"},"variant_type":"CONTROL"},{"context_id":"f0b593cec64e50fe8d15675d53f39f75c9296c11304c2693711ae2b8b4b30546","id":"7100418327268429824-test1","override_id":"65bcc9acaebd88355f850d7087e269f36d1316d7aeeddcf72cece682f99691c1","overrides":{"pmTestKey1":"value2-test","pmTestKey2":"value2-test"},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"cac.admin@juspay.in","chosen_variant":""}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7100418327268429824,'2023-08-24 10:07:28+05:30','cac.admin@juspay.in','2023-08-24 15:51:54+05:30','experiment-1','{pmTestKey1,pmTestKey2}','CONCLUDED',2,'{"and":[{"==":[{"var":"os"},"ios"]},{"==":[{"var":"clientId"},"testClientCac1"]}]}','[{"context_id":"a43666147f7d1cd840ddc4c8da53cf19a351630cd843d74dbffe7d51c184ee51","id":"7100418327268429824-control","override_id":"085e637459203733acb435d0bd921081b10a46a3eb192924a332e414bf266cf7","overrides":{"pmTestKey1":"value1-control","pmTestKey2":"value1-control"},"variant_type":"CONTROL"},{"context_id":"f0b593cec64e50fe8d15675d53f39f75c9296c11304c2693711ae2b8b4b30546","id":"7100418327268429824-test1","override_id":"65bcc9acaebd88355f850d7087e269f36d1316d7aeeddcf72cece682f99691c1","overrides":{"pmTestKey1":"value2-test","pmTestKey2":"value2-test"},"variant_type":"EXPERIMENTAL"}]','cac.admin@juspay.in','')
27d17515-fa84-41d8-aa36-d28336a8cd9a	experiments	postgres	2024-08-21 22:46:30.308408	INSERT	\N	{"id":7100505076539723776,"created_at":"2023-08-24T15:52:11+05:30","created_by":"cac.admin@juspay.in","last_modified":"2023-08-24T15:52:35+05:30","name":"experiment-test","override_keys":["pmTestKey1","pmTestKey2"],"status":"CONCLUDED","traffic_percentage":10,"context":{"and":[{"==":[{"var":"os"},"ios"]},{"==":[{"var":"clientId"},"testClientCac1"]}]},"variants":[{"context_id":"ae7a885cca16c4e1b781d827c9e267741321f83f43a145b82c1380e9a9c63649","id":"7100505076539723776-control","override_id":"085e637459203733acb435d0bd921081b10a46a3eb192924a332e414bf266cf7","overrides":{"pmTestKey1":"value1-control","pmTestKey2":"value1-control"},"variant_type":"CONTROL"},{"context_id":"fb796d91a330050ea7112f6e415e5c5fd8b1489355c7a06bbe134a2370d47c67","id":"7100505076539723776-test1","override_id":"65bcc9acaebd88355f850d7087e269f36d1316d7aeeddcf72cece682f99691c1","overrides":{"pmTestKey1":"value2-test","pmTestKey2":"value2-test"},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"cac.admin@juspay.in","chosen_variant":""}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7100505076539723776,'2023-08-24 15:52:11+05:30','cac.admin@juspay.in','2023-08-24 15:52:35+05:30','experiment-test','{pmTestKey1,pmTestKey2}','CONCLUDED',10,'{"and":[{"==":[{"var":"os"},"ios"]},{"==":[{"var":"clientId"},"testClientCac1"]}]}','[{"context_id":"ae7a885cca16c4e1b781d827c9e267741321f83f43a145b82c1380e9a9c63649","id":"7100505076539723776-control","override_id":"085e637459203733acb435d0bd921081b10a46a3eb192924a332e414bf266cf7","overrides":{"pmTestKey1":"value1-control","pmTestKey2":"value1-control"},"variant_type":"CONTROL"},{"context_id":"fb796d91a330050ea7112f6e415e5c5fd8b1489355c7a06bbe134a2370d47c67","id":"7100505076539723776-test1","override_id":"65bcc9acaebd88355f850d7087e269f36d1316d7aeeddcf72cece682f99691c1","overrides":{"pmTestKey1":"value2-test","pmTestKey2":"value2-test"},"variant_type":"EXPERIMENTAL"}]','cac.admin@juspay.in','')
21505e61-fecf-4c0f-9fc4-173ceb25d9c9	experiments	postgres	2024-08-21 22:46:30.308688	INSERT	\N	{"id":7100505506371997696,"created_at":"2023-08-24T15:53:53+05:30","created_by":"cac.admin@juspay.in","last_modified":"2023-08-24T15:54:08+05:30","name":"experiment-test-2","override_keys":["pmTestKey1","pmTestKey2"],"status":"CONCLUDED","traffic_percentage":0,"context":{"and":[{"==":[{"var":"os"},"ios"]},{"==":[{"var":"clientId"},"testClientCac1"]}]},"variants":[{"context_id":"139618bad8aa5fe4e8c34591e55ea505613897d4ef79e508dd87544e7526042d","id":"7100505506371997696-control","override_id":"085e637459203733acb435d0bd921081b10a46a3eb192924a332e414bf266cf7","overrides":{"pmTestKey1":"value1-control","pmTestKey2":"value1-control"},"variant_type":"CONTROL"},{"context_id":"d164355dbb8d87da0c189471ace56578c7e5d4008f453f9dc248659a430b319c","id":"7100505506371997696-test1","override_id":"65bcc9acaebd88355f850d7087e269f36d1316d7aeeddcf72cece682f99691c1","overrides":{"pmTestKey1":"value2-test","pmTestKey2":"value2-test"},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"cac.admin@juspay.in","chosen_variant":""}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7100505506371997696,'2023-08-24 15:53:53+05:30','cac.admin@juspay.in','2023-08-24 15:54:08+05:30','experiment-test-2','{pmTestKey1,pmTestKey2}','CONCLUDED',0,'{"and":[{"==":[{"var":"os"},"ios"]},{"==":[{"var":"clientId"},"testClientCac1"]}]}','[{"context_id":"139618bad8aa5fe4e8c34591e55ea505613897d4ef79e508dd87544e7526042d","id":"7100505506371997696-control","override_id":"085e637459203733acb435d0bd921081b10a46a3eb192924a332e414bf266cf7","overrides":{"pmTestKey1":"value1-control","pmTestKey2":"value1-control"},"variant_type":"CONTROL"},{"context_id":"d164355dbb8d87da0c189471ace56578c7e5d4008f453f9dc248659a430b319c","id":"7100505506371997696-test1","override_id":"65bcc9acaebd88355f850d7087e269f36d1316d7aeeddcf72cece682f99691c1","overrides":{"pmTestKey1":"value2-test","pmTestKey2":"value2-test"},"variant_type":"EXPERIMENTAL"}]','cac.admin@juspay.in','')
8f96b3a6-3667-4291-9c42-6220c7bd5934	experiments	postgres	2024-08-21 22:46:30.308979	INSERT	\N	{"id":7100505592241983488,"created_at":"2023-08-24T15:54:14+05:30","created_by":"cac.admin@juspay.in","last_modified":"2023-08-24T15:55:13+05:30","name":"experiment-test-3","override_keys":["pmTestKey1","pmTestKey2"],"status":"CONCLUDED","traffic_percentage":41,"context":{"and":[{"==":[{"var":"os"},"ios"]},{"==":[{"var":"clientId"},"testClientCac1"]}]},"variants":[{"context_id":"d794c78d54a9bc079732d35263acb7b9892f65056846927d429c3fd39c8e6023","id":"7100505592241983488-control","override_id":"085e637459203733acb435d0bd921081b10a46a3eb192924a332e414bf266cf7","overrides":{"pmTestKey1":"value1-control","pmTestKey2":"value1-control"},"variant_type":"CONTROL"},{"context_id":"0a7e0196582254694e5c484ea20fe7cb32f5c536d67ee95f5ca03b1fb7bc356e","id":"7100505592241983488-test1","override_id":"65bcc9acaebd88355f850d7087e269f36d1316d7aeeddcf72cece682f99691c1","overrides":{"pmTestKey1":"value2-test","pmTestKey2":"value2-test"},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"cac.admin@juspay.in","chosen_variant":""}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7100505592241983488,'2023-08-24 15:54:14+05:30','cac.admin@juspay.in','2023-08-24 15:55:13+05:30','experiment-test-3','{pmTestKey1,pmTestKey2}','CONCLUDED',41,'{"and":[{"==":[{"var":"os"},"ios"]},{"==":[{"var":"clientId"},"testClientCac1"]}]}','[{"context_id":"d794c78d54a9bc079732d35263acb7b9892f65056846927d429c3fd39c8e6023","id":"7100505592241983488-control","override_id":"085e637459203733acb435d0bd921081b10a46a3eb192924a332e414bf266cf7","overrides":{"pmTestKey1":"value1-control","pmTestKey2":"value1-control"},"variant_type":"CONTROL"},{"context_id":"0a7e0196582254694e5c484ea20fe7cb32f5c536d67ee95f5ca03b1fb7bc356e","id":"7100505592241983488-test1","override_id":"65bcc9acaebd88355f850d7087e269f36d1316d7aeeddcf72cece682f99691c1","overrides":{"pmTestKey1":"value2-test","pmTestKey2":"value2-test"},"variant_type":"EXPERIMENTAL"}]','cac.admin@juspay.in','')
c2ab38d5-d842-4a49-889b-9189fefe68d3	experiments	postgres	2024-08-21 22:46:30.309302	INSERT	\N	{"id":7100505953140871168,"created_at":"2023-08-24T15:55:40+05:30","created_by":"cac.admin@juspay.in","last_modified":"2023-08-24T15:55:58+05:30","name":"experiment-test-4","override_keys":["pmTestKey1","pmTestKey2"],"status":"CONCLUDED","traffic_percentage":40,"context":{"and":[{"==":[{"var":"os"},"ios"]},{"==":[{"var":"clientId"},"testClientCac1"]}]},"variants":[{"context_id":"339ea286f17e67a0703e6009e0316a6b791693bb51c3cce4a7e608b8d7d6ff72","id":"7100505953140871168-control","override_id":"085e637459203733acb435d0bd921081b10a46a3eb192924a332e414bf266cf7","overrides":{"pmTestKey1":"value1-control","pmTestKey2":"value1-control"},"variant_type":"CONTROL"},{"context_id":"64d08764fa098a511fe31416b611d4ebc351c8a37e87e8ba230af998dd5412e3","id":"7100505953140871168-test1","override_id":"65bcc9acaebd88355f850d7087e269f36d1316d7aeeddcf72cece682f99691c1","overrides":{"pmTestKey1":"value2-test","pmTestKey2":"value2-test"},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"cac.admin@juspay.in","chosen_variant":""}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7100505953140871168,'2023-08-24 15:55:40+05:30','cac.admin@juspay.in','2023-08-24 15:55:58+05:30','experiment-test-4','{pmTestKey1,pmTestKey2}','CONCLUDED',40,'{"and":[{"==":[{"var":"os"},"ios"]},{"==":[{"var":"clientId"},"testClientCac1"]}]}','[{"context_id":"339ea286f17e67a0703e6009e0316a6b791693bb51c3cce4a7e608b8d7d6ff72","id":"7100505953140871168-control","override_id":"085e637459203733acb435d0bd921081b10a46a3eb192924a332e414bf266cf7","overrides":{"pmTestKey1":"value1-control","pmTestKey2":"value1-control"},"variant_type":"CONTROL"},{"context_id":"64d08764fa098a511fe31416b611d4ebc351c8a37e87e8ba230af998dd5412e3","id":"7100505953140871168-test1","override_id":"65bcc9acaebd88355f850d7087e269f36d1316d7aeeddcf72cece682f99691c1","overrides":{"pmTestKey1":"value2-test","pmTestKey2":"value2-test"},"variant_type":"EXPERIMENTAL"}]','cac.admin@juspay.in','')
64242a0f-8805-43df-ada7-ab3faadf6df9	experiments	postgres	2024-08-21 22:46:30.309588	INSERT	\N	{"id":7100506116404154368,"created_at":"2023-08-24T15:56:19+05:30","created_by":"cac.admin@juspay.in","last_modified":"2023-08-24T15:56:36+05:30","name":"experiment-test-5","override_keys":["pmTestKey1","pmTestKey2"],"status":"CONCLUDED","traffic_percentage":40,"context":{"and":[{"==":[{"var":"os"},"ios"]},{"==":[{"var":"clientId"},"testClientCac1"]}]},"variants":[{"context_id":"ada860c380d6d3b8249bc839f0b03916816bec3b549bcaa46dc13a9e14637c85","id":"7100506116404154368-control","override_id":"085e637459203733acb435d0bd921081b10a46a3eb192924a332e414bf266cf7","overrides":{"pmTestKey1":"value1-control","pmTestKey2":"value1-control"},"variant_type":"CONTROL"},{"context_id":"5e4c89b8968ca83720c0d3f9cb4e685039ecc6c849a87628951d08af2d5f659f","id":"7100506116404154368-test1","override_id":"65bcc9acaebd88355f850d7087e269f36d1316d7aeeddcf72cece682f99691c1","overrides":{"pmTestKey1":"value2-test","pmTestKey2":"value2-test"},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"cac.admin@juspay.in","chosen_variant":""}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7100506116404154368,'2023-08-24 15:56:19+05:30','cac.admin@juspay.in','2023-08-24 15:56:36+05:30','experiment-test-5','{pmTestKey1,pmTestKey2}','CONCLUDED',40,'{"and":[{"==":[{"var":"os"},"ios"]},{"==":[{"var":"clientId"},"testClientCac1"]}]}','[{"context_id":"ada860c380d6d3b8249bc839f0b03916816bec3b549bcaa46dc13a9e14637c85","id":"7100506116404154368-control","override_id":"085e637459203733acb435d0bd921081b10a46a3eb192924a332e414bf266cf7","overrides":{"pmTestKey1":"value1-control","pmTestKey2":"value1-control"},"variant_type":"CONTROL"},{"context_id":"5e4c89b8968ca83720c0d3f9cb4e685039ecc6c849a87628951d08af2d5f659f","id":"7100506116404154368-test1","override_id":"65bcc9acaebd88355f850d7087e269f36d1316d7aeeddcf72cece682f99691c1","overrides":{"pmTestKey1":"value2-test","pmTestKey2":"value2-test"},"variant_type":"EXPERIMENTAL"}]','cac.admin@juspay.in','')
ddd81cb4-2df0-4650-96b2-615e5200fdb9	experiments	postgres	2024-08-21 22:46:30.309861	INSERT	\N	{"id":7100551328161730560,"created_at":"2023-08-24T18:55:58+05:30","created_by":"cac.admin@juspay.in","last_modified":"2023-08-24T18:56:52+05:30","name":"experiment-test-6","override_keys":["pmTestKey1","pmTestKey2"],"status":"CONCLUDED","traffic_percentage":20,"context":{"and":[{"==":[{"var":"os"},"ios"]},{"==":[{"var":"clientId"},"testClientCac1"]}]},"variants":[{"context_id":"6801dab9dc504adab6993d1460a1e1c3b953ce89e6b1667028119fd365775d1c","id":"7100551328161730560-control","override_id":"085e637459203733acb435d0bd921081b10a46a3eb192924a332e414bf266cf7","overrides":{"pmTestKey1":"value1-control","pmTestKey2":"value1-control"},"variant_type":"CONTROL"},{"context_id":"f5b4a558f673e8768eeb2ad48c2a30b79cba2de1e9178fdc1cc52886217b0e01","id":"7100551328161730560-test1","override_id":"65bcc9acaebd88355f850d7087e269f36d1316d7aeeddcf72cece682f99691c1","overrides":{"pmTestKey1":"value2-test","pmTestKey2":"value2-test"},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"cac.admin@juspay.in","chosen_variant":""}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7100551328161730560,'2023-08-24 18:55:58+05:30','cac.admin@juspay.in','2023-08-24 18:56:52+05:30','experiment-test-6','{pmTestKey1,pmTestKey2}','CONCLUDED',20,'{"and":[{"==":[{"var":"os"},"ios"]},{"==":[{"var":"clientId"},"testClientCac1"]}]}','[{"context_id":"6801dab9dc504adab6993d1460a1e1c3b953ce89e6b1667028119fd365775d1c","id":"7100551328161730560-control","override_id":"085e637459203733acb435d0bd921081b10a46a3eb192924a332e414bf266cf7","overrides":{"pmTestKey1":"value1-control","pmTestKey2":"value1-control"},"variant_type":"CONTROL"},{"context_id":"f5b4a558f673e8768eeb2ad48c2a30b79cba2de1e9178fdc1cc52886217b0e01","id":"7100551328161730560-test1","override_id":"65bcc9acaebd88355f850d7087e269f36d1316d7aeeddcf72cece682f99691c1","overrides":{"pmTestKey1":"value2-test","pmTestKey2":"value2-test"},"variant_type":"EXPERIMENTAL"}]','cac.admin@juspay.in','')
1d71a7f4-d0af-4c77-9775-c1625db00f9e	experiments	postgres	2024-08-21 22:46:30.310124	INSERT	\N	{"id":7100741287271337984,"created_at":"2023-08-25T07:30:48+05:30","created_by":"cac.admin@juspay.in","last_modified":"2023-08-25T07:31:38+05:30","name":"experiment-test-7","override_keys":["pmTestKey1","pmTestKey2"],"status":"CONCLUDED","traffic_percentage":10,"context":{"and":[{"==":[{"var":"os"},"ios"]},{"==":[{"var":"clientId"},"testClientCac1"]}]},"variants":[{"context_id":"71dc96366d072d78ade59f7ad3aefffaea4f25d9bb864eec845c10b344463b4f","id":"7100741287271337984-control","override_id":"085e637459203733acb435d0bd921081b10a46a3eb192924a332e414bf266cf7","overrides":{"pmTestKey1":"value1-control","pmTestKey2":"value1-control"},"variant_type":"CONTROL"},{"context_id":"c35b1df90f0438aea2221097632223731e52a231f31a41cf24cf78ad624ee0b3","id":"7100741287271337984-test1","override_id":"65bcc9acaebd88355f850d7087e269f36d1316d7aeeddcf72cece682f99691c1","overrides":{"pmTestKey1":"value2-test","pmTestKey2":"value2-test"},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"cac.admin@juspay.in","chosen_variant":""}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7100741287271337984,'2023-08-25 07:30:48+05:30','cac.admin@juspay.in','2023-08-25 07:31:38+05:30','experiment-test-7','{pmTestKey1,pmTestKey2}','CONCLUDED',10,'{"and":[{"==":[{"var":"os"},"ios"]},{"==":[{"var":"clientId"},"testClientCac1"]}]}','[{"context_id":"71dc96366d072d78ade59f7ad3aefffaea4f25d9bb864eec845c10b344463b4f","id":"7100741287271337984-control","override_id":"085e637459203733acb435d0bd921081b10a46a3eb192924a332e414bf266cf7","overrides":{"pmTestKey1":"value1-control","pmTestKey2":"value1-control"},"variant_type":"CONTROL"},{"context_id":"c35b1df90f0438aea2221097632223731e52a231f31a41cf24cf78ad624ee0b3","id":"7100741287271337984-test1","override_id":"65bcc9acaebd88355f850d7087e269f36d1316d7aeeddcf72cece682f99691c1","overrides":{"pmTestKey1":"value2-test","pmTestKey2":"value2-test"},"variant_type":"EXPERIMENTAL"}]','cac.admin@juspay.in','')
1f977aa6-a36f-4759-9d7c-0bb6c9e1beef	experiments	postgres	2024-08-21 22:46:30.310402	INSERT	\N	{"id":7101895609925373952,"created_at":"2023-08-28T11:57:40+05:30","created_by":"cac.admin@juspay.in","last_modified":"2023-08-28T15:33:42+05:30","name":"Godel ACS Release","override_keys":["godel_bundle_android","godel_placeholder_bundle_android","godel_config_android","godel_placeholder_config_android","godel_acs_android","godel_placeholder_acs_android","hyperos_placeholder_tracker_android","patch_entries"],"status":"CONCLUDED","traffic_percentage":10,"context":{"and":[{"==":[{"var":"os"},"android"]},{"<":[{"var":"toss"},0]}]},"variants":[{"context_id":"62e09131d8d9a04988a5940bd380c12556c2f8069105b39484f74c8c9559d642","id":"7101895609925373952-control","override_id":"c116de90a3c8f79b1cd87a4a327eb5570dba41f738a0c5d8f56dc6c2b67da9fa","overrides":{"godel_acs_android":{"etag":"a16f12ac0c078e21eed52ab452cbf042","path":["live","assets","in.juspay.godel","acs_js_source"],"value":"https://assets.juspay.in/hyper/bundles/acs/in.juspay.godel/1.2.100/v1-acs.zip"},"godel_bundle_android":{"etag":"784aaa859ef08d604cf277ee3406f4cc","path":["live","package","in.juspay.godel"],"value":"https://assets.juspay.in/hyper/bundles/app/in.juspay.godel/1.2.23/android/v1-index_bundle.zip"},"godel_config_android":{"etag":"46c2b1a55e9d27445e3b1547ac0a4689","path":["live","assets","in.juspay.godel","config"],"value":"https://assets.juspay.in/hyper/bundles/config/in.juspay.godel/1.2.11/v1-config.zip"},"godel_placeholder_acs_android":{"etag":"a16f12ac0c078e21eed52ab452cbf042","path":["live","assets","in.juspay.godel.placeholder","acs_js_source"],"value":"https://assets.juspay.in/hyper/bundles/acs/in.juspay.godel/1.2.100/v1-acs.zip"},"godel_placeholder_bundle_android":{"etag":"784aaa859ef08d604cf277ee3406f4cc","path":["live","package","in.juspay.godel.placeholder"],"value":"https://assets.juspay.in/hyper/bundles/app/in.juspay.godel/1.2.23/android/v1-index_bundle.zip"},"godel_placeholder_config_android":{"etag":"46c2b1a55e9d27445e3b1547ac0a4689","path":["live","assets","in.juspay.godel.placeholder","config"],"value":"https://assets.juspay.in/hyper/bundles/config/in.juspay.godel/1.2.11/v1-config.zip"},"hyperos_placeholder_tracker_android":{"etag":"72e0db9d6d92d558b8a3dbb2e6abf0ec","path":["live","assets","in.juspay.hyperos.placeholder","tracker"],"value":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.50/v1-tracker.zip"},"patch_entries":["godel_bundle_android","godel_placeholder_bundle_android","godel_config_android","godel_placeholder_config_android","godel_acs_android","godel_placeholder_acs_android","hyperos_placeholder_tracker_android"]},"variant_type":"CONTROL"},{"context_id":"58991119af36b6acdeca68eb77a26ec51c3115f4794e8172e77e562e17f75281","id":"7101895609925373952-test","override_id":"4c0d1ead839ad25fe600fb76301b8ea63e4d0f35c58548d792a8e25eaa18af56","overrides":{"godel_acs_android":{"etag":"13b1c8287464d54e9417fcb18f6f21a1","path":["live","assets","in.juspay.godel","acs_js_source"],"value":"https://assets.juspay.in/hyper/bundles/acs/in.juspay.godel/1.2.102/v1-acs.zip"},"godel_bundle_android":{"etag":"5119e9bafb446b8d4a7e40823851684f","path":["live","package","in.juspay.godel"],"value":"https://assets.juspay.in/hyper/bundles/app/in.juspay.godel/1.2.24/android/v1-index_bundle.zip"},"godel_config_android":{"etag":"4bd5516b5ab247b99be2b89f34e8a733","path":["live","assets","in.juspay.godel","config"],"value":"https://assets.juspay.in/hyper/bundles/config/in.juspay.godel/1.2.12/v1-config.zip"},"godel_placeholder_acs_android":{"etag":"13b1c8287464d54e9417fcb18f6f21a1","path":["live","assets","in.juspay.godel.placeholder","acs_js_source"],"value":"https://assets.juspay.in/hyper/bundles/acs/in.juspay.godel/1.2.102/v1-acs.zip"},"godel_placeholder_bundle_android":{"etag":"5119e9bafb446b8d4a7e40823851684f","path":["live","package","in.juspay.godel.placeholder"],"value":"https://assets.juspay.in/hyper/bundles/app/in.juspay.godel/1.2.24/android/v1-index_bundle.zip"},"godel_placeholder_config_android":{"etag":"4bd5516b5ab247b99be2b89f34e8a733","path":["live","assets","in.juspay.godel.placeholder","config"],"value":"https://assets.juspay.in/hyper/bundles/config/in.juspay.godel/1.2.12/v1-config.zip"},"hyperos_placeholder_tracker_android":{"etag":"72e0db9d6d92d558b8a3dbb2e6abf0ec","path":["live","assets","in.juspay.hyperos.placeholder","tracker"],"value":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.50/v1-tracker.zip"},"patch_entries":["godel_bundle_android","godel_placeholder_bundle_android","godel_config_android","godel_placeholder_config_android","godel_acs_android","godel_placeholder_acs_android","hyperos_placeholder_tracker_android"]},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"cac.admin@juspay.in","chosen_variant":""}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7101895609925373952,'2023-08-28 11:57:40+05:30','cac.admin@juspay.in','2023-08-28 15:33:42+05:30','Godel ACS Release','{godel_bundle_android,godel_placeholder_bundle_android,godel_config_android,godel_placeholder_config_android,godel_acs_android,godel_placeholder_acs_android,hyperos_placeholder_tracker_android,patch_entries}','CONCLUDED',10,'{"and":[{"==":[{"var":"os"},"android"]},{"<":[{"var":"toss"},0]}]}','[{"context_id":"62e09131d8d9a04988a5940bd380c12556c2f8069105b39484f74c8c9559d642","id":"7101895609925373952-control","override_id":"c116de90a3c8f79b1cd87a4a327eb5570dba41f738a0c5d8f56dc6c2b67da9fa","overrides":{"godel_acs_android":{"etag":"a16f12ac0c078e21eed52ab452cbf042","path":["live","assets","in.juspay.godel","acs_js_source"],"value":"https://assets.juspay.in/hyper/bundles/acs/in.juspay.godel/1.2.100/v1-acs.zip"},"godel_bundle_android":{"etag":"784aaa859ef08d604cf277ee3406f4cc","path":["live","package","in.juspay.godel"],"value":"https://assets.juspay.in/hyper/bundles/app/in.juspay.godel/1.2.23/android/v1-index_bundle.zip"},"godel_config_android":{"etag":"46c2b1a55e9d27445e3b1547ac0a4689","path":["live","assets","in.juspay.godel","config"],"value":"https://assets.juspay.in/hyper/bundles/config/in.juspay.godel/1.2.11/v1-config.zip"},"godel_placeholder_acs_android":{"etag":"a16f12ac0c078e21eed52ab452cbf042","path":["live","assets","in.juspay.godel.placeholder","acs_js_source"],"value":"https://assets.juspay.in/hyper/bundles/acs/in.juspay.godel/1.2.100/v1-acs.zip"},"godel_placeholder_bundle_android":{"etag":"784aaa859ef08d604cf277ee3406f4cc","path":["live","package","in.juspay.godel.placeholder"],"value":"https://assets.juspay.in/hyper/bundles/app/in.juspay.godel/1.2.23/android/v1-index_bundle.zip"},"godel_placeholder_config_android":{"etag":"46c2b1a55e9d27445e3b1547ac0a4689","path":["live","assets","in.juspay.godel.placeholder","config"],"value":"https://assets.juspay.in/hyper/bundles/config/in.juspay.godel/1.2.11/v1-config.zip"},"hyperos_placeholder_tracker_android":{"etag":"72e0db9d6d92d558b8a3dbb2e6abf0ec","path":["live","assets","in.juspay.hyperos.placeholder","tracker"],"value":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.50/v1-tracker.zip"},"patch_entries":["godel_bundle_android","godel_placeholder_bundle_android","godel_config_android","godel_placeholder_config_android","godel_acs_android","godel_placeholder_acs_android","hyperos_placeholder_tracker_android"]},"variant_type":"CONTROL"},{"context_id":"58991119af36b6acdeca68eb77a26ec51c3115f4794e8172e77e562e17f75281","id":"7101895609925373952-test","override_id":"4c0d1ead839ad25fe600fb76301b8ea63e4d0f35c58548d792a8e25eaa18af56","overrides":{"godel_acs_android":{"etag":"13b1c8287464d54e9417fcb18f6f21a1","path":["live","assets","in.juspay.godel","acs_js_source"],"value":"https://assets.juspay.in/hyper/bundles/acs/in.juspay.godel/1.2.102/v1-acs.zip"},"godel_bundle_android":{"etag":"5119e9bafb446b8d4a7e40823851684f","path":["live","package","in.juspay.godel"],"value":"https://assets.juspay.in/hyper/bundles/app/in.juspay.godel/1.2.24/android/v1-index_bundle.zip"},"godel_config_android":{"etag":"4bd5516b5ab247b99be2b89f34e8a733","path":["live","assets","in.juspay.godel","config"],"value":"https://assets.juspay.in/hyper/bundles/config/in.juspay.godel/1.2.12/v1-config.zip"},"godel_placeholder_acs_android":{"etag":"13b1c8287464d54e9417fcb18f6f21a1","path":["live","assets","in.juspay.godel.placeholder","acs_js_source"],"value":"https://assets.juspay.in/hyper/bundles/acs/in.juspay.godel/1.2.102/v1-acs.zip"},"godel_placeholder_bundle_android":{"etag":"5119e9bafb446b8d4a7e40823851684f","path":["live","package","in.juspay.godel.placeholder"],"value":"https://assets.juspay.in/hyper/bundles/app/in.juspay.godel/1.2.24/android/v1-index_bundle.zip"},"godel_placeholder_config_android":{"etag":"4bd5516b5ab247b99be2b89f34e8a733","path":["live","assets","in.juspay.godel.placeholder","config"],"value":"https://assets.juspay.in/hyper/bundles/config/in.juspay.godel/1.2.12/v1-config.zip"},"hyperos_placeholder_tracker_android":{"etag":"72e0db9d6d92d558b8a3dbb2e6abf0ec","path":["live","assets","in.juspay.hyperos.placeholder","tracker"],"value":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.50/v1-tracker.zip"},"patch_entries":["godel_bundle_android","godel_placeholder_bundle_android","godel_config_android","godel_placeholder_config_android","godel_acs_android","godel_placeholder_acs_android","hyperos_placeholder_tracker_android"]},"variant_type":"EXPERIMENTAL"}]','cac.admin@juspay.in','')
80417c9e-3f23-4fa7-905d-84f6fdb8a9df	experiments	postgres	2024-08-21 22:46:30.310843	INSERT	\N	{"id":7101954441615642624,"created_at":"2023-08-28T15:51:26+05:30","created_by":"cac.admin@juspay.in","last_modified":"2023-08-28T15:51:26+05:30","name":"Godel ACS Release","override_keys":["godel_bundle_android","godel_placeholder_bundle_android","godel_config_android","godel_placeholder_config_android","godel_acs_android","godel_placeholder_acs_android","hyperos_placeholder_tracker_android","patch_entries"],"status":"CREATED","traffic_percentage":0,"context":{"and":[{"==":[{"var":"os"},"android"]}]},"variants":[{"context_id":"1cf58c1517bc9d6a2c581e88d0537f7d678761451e80bc04be122eee60a2d3c7","id":"7101954441615642624-control","override_id":"c116de90a3c8f79b1cd87a4a327eb5570dba41f738a0c5d8f56dc6c2b67da9fa","overrides":{"godel_acs_android":{"etag":"a16f12ac0c078e21eed52ab452cbf042","path":["live","assets","in.juspay.godel","acs_js_source"],"value":"https://assets.juspay.in/hyper/bundles/acs/in.juspay.godel/1.2.100/v1-acs.zip"},"godel_bundle_android":{"etag":"784aaa859ef08d604cf277ee3406f4cc","path":["live","package","in.juspay.godel"],"value":"https://assets.juspay.in/hyper/bundles/app/in.juspay.godel/1.2.23/android/v1-index_bundle.zip"},"godel_config_android":{"etag":"46c2b1a55e9d27445e3b1547ac0a4689","path":["live","assets","in.juspay.godel","config"],"value":"https://assets.juspay.in/hyper/bundles/config/in.juspay.godel/1.2.11/v1-config.zip"},"godel_placeholder_acs_android":{"etag":"a16f12ac0c078e21eed52ab452cbf042","path":["live","assets","in.juspay.godel.placeholder","acs_js_source"],"value":"https://assets.juspay.in/hyper/bundles/acs/in.juspay.godel/1.2.100/v1-acs.zip"},"godel_placeholder_bundle_android":{"etag":"784aaa859ef08d604cf277ee3406f4cc","path":["live","package","in.juspay.godel.placeholder"],"value":"https://assets.juspay.in/hyper/bundles/app/in.juspay.godel/1.2.23/android/v1-index_bundle.zip"},"godel_placeholder_config_android":{"etag":"46c2b1a55e9d27445e3b1547ac0a4689","path":["live","assets","in.juspay.godel.placeholder","config"],"value":"https://assets.juspay.in/hyper/bundles/config/in.juspay.godel/1.2.11/v1-config.zip"},"hyperos_placeholder_tracker_android":{"etag":"72e0db9d6d92d558b8a3dbb2e6abf0ec","path":["live","assets","in.juspay.hyperos.placeholder","tracker"],"value":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.50/v1-tracker.zip"},"patch_entries":["godel_bundle_android","godel_placeholder_bundle_android","godel_config_android","godel_placeholder_config_android","godel_acs_android","godel_placeholder_acs_android","hyperos_placeholder_tracker_android"]},"variant_type":"CONTROL"},{"context_id":"ba2063b6f829c11121d1b10e483d2256de5bd3777dcdb53ff45d482498878468","id":"7101954441615642624-test","override_id":"4884311e3d46b594a9e03013f70f675c646829dd79c0c02477ef813bccbacc25","overrides":{"godel_acs_android":{"etag":"13b1c8287464d54e9417fcb18f6f21a1","path":["live","assets","in.juspay.godel","acs_js_source"],"value":"https://assets.juspay.in/hyper/bundles/acs/in.juspay.godel/1.2.102/v1-acs.zip"},"godel_bundle_android":{"etag":"5119e9bafb446b8d4a7e40823851684f","path":["live","package","in.juspay.godel"],"value":"https://assets.juspay.in/hyper/bundles/app/in.juspay.godel/1.2.24/android/v1-index_bundle.zip"},"godel_config_android":{"etag":"4bd5516b5ab247b99be2b89f34e8a733","path":["live","assets","in.juspay.godel","config"],"value":"https://assets.juspay.in/hyper/bundles/config/in.juspay.godel/1.2.12/v1-config.zip"},"godel_placeholder_acs_android":{"etag":"13b1c8287464d54e9417fcb18f6f21a1","path":["live","assets","in.juspay.godel.placeholder","acs_js_source"],"value":"https://assets.juspay.in/hyper/bundles/acs/in.juspay.godel/1.2.102/v1-acs.zip"},"godel_placeholder_bundle_android":{"etag":"5119e9bafb446b8d4a7e40823851684f","path":["live","package","in.juspay.godel.placeholder"],"value":"https://assets.juspay.in/hyper/bundles/app/in.juspay.godel/1.2.24/android/v1-index_bundle.zip"},"godel_placeholder_config_android":{"etag":"4bd5516b5ab247b99be2b89f34e8a733","path":["live","assets","in.juspay.godel.placeholder","config"],"value":"https://assets.juspay.in/hyper/bundles/config/in.juspay.godel/1.2.12/v1-config.zip"},"hyperos_placeholder_tracker_android":{"etag":"39b4e3e33950355a8c32e9092a954eff","path":["live","assets","in.juspay.hyperos.placeholder","tracker"],"value":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.51/v1-tracker.zip"},"patch_entries":["godel_bundle_android","godel_placeholder_bundle_android","godel_config_android","godel_placeholder_config_android","godel_acs_android","godel_placeholder_acs_android","hyperos_placeholder_tracker_android"]},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"cac.admin@juspay.in","chosen_variant":""}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7101954441615642624,'2023-08-28 15:51:26+05:30','cac.admin@juspay.in','2023-08-28 15:51:26+05:30','Godel ACS Release','{godel_bundle_android,godel_placeholder_bundle_android,godel_config_android,godel_placeholder_config_android,godel_acs_android,godel_placeholder_acs_android,hyperos_placeholder_tracker_android,patch_entries}','CREATED',0,'{"and":[{"==":[{"var":"os"},"android"]}]}','[{"context_id":"1cf58c1517bc9d6a2c581e88d0537f7d678761451e80bc04be122eee60a2d3c7","id":"7101954441615642624-control","override_id":"c116de90a3c8f79b1cd87a4a327eb5570dba41f738a0c5d8f56dc6c2b67da9fa","overrides":{"godel_acs_android":{"etag":"a16f12ac0c078e21eed52ab452cbf042","path":["live","assets","in.juspay.godel","acs_js_source"],"value":"https://assets.juspay.in/hyper/bundles/acs/in.juspay.godel/1.2.100/v1-acs.zip"},"godel_bundle_android":{"etag":"784aaa859ef08d604cf277ee3406f4cc","path":["live","package","in.juspay.godel"],"value":"https://assets.juspay.in/hyper/bundles/app/in.juspay.godel/1.2.23/android/v1-index_bundle.zip"},"godel_config_android":{"etag":"46c2b1a55e9d27445e3b1547ac0a4689","path":["live","assets","in.juspay.godel","config"],"value":"https://assets.juspay.in/hyper/bundles/config/in.juspay.godel/1.2.11/v1-config.zip"},"godel_placeholder_acs_android":{"etag":"a16f12ac0c078e21eed52ab452cbf042","path":["live","assets","in.juspay.godel.placeholder","acs_js_source"],"value":"https://assets.juspay.in/hyper/bundles/acs/in.juspay.godel/1.2.100/v1-acs.zip"},"godel_placeholder_bundle_android":{"etag":"784aaa859ef08d604cf277ee3406f4cc","path":["live","package","in.juspay.godel.placeholder"],"value":"https://assets.juspay.in/hyper/bundles/app/in.juspay.godel/1.2.23/android/v1-index_bundle.zip"},"godel_placeholder_config_android":{"etag":"46c2b1a55e9d27445e3b1547ac0a4689","path":["live","assets","in.juspay.godel.placeholder","config"],"value":"https://assets.juspay.in/hyper/bundles/config/in.juspay.godel/1.2.11/v1-config.zip"},"hyperos_placeholder_tracker_android":{"etag":"72e0db9d6d92d558b8a3dbb2e6abf0ec","path":["live","assets","in.juspay.hyperos.placeholder","tracker"],"value":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.50/v1-tracker.zip"},"patch_entries":["godel_bundle_android","godel_placeholder_bundle_android","godel_config_android","godel_placeholder_config_android","godel_acs_android","godel_placeholder_acs_android","hyperos_placeholder_tracker_android"]},"variant_type":"CONTROL"},{"context_id":"ba2063b6f829c11121d1b10e483d2256de5bd3777dcdb53ff45d482498878468","id":"7101954441615642624-test","override_id":"4884311e3d46b594a9e03013f70f675c646829dd79c0c02477ef813bccbacc25","overrides":{"godel_acs_android":{"etag":"13b1c8287464d54e9417fcb18f6f21a1","path":["live","assets","in.juspay.godel","acs_js_source"],"value":"https://assets.juspay.in/hyper/bundles/acs/in.juspay.godel/1.2.102/v1-acs.zip"},"godel_bundle_android":{"etag":"5119e9bafb446b8d4a7e40823851684f","path":["live","package","in.juspay.godel"],"value":"https://assets.juspay.in/hyper/bundles/app/in.juspay.godel/1.2.24/android/v1-index_bundle.zip"},"godel_config_android":{"etag":"4bd5516b5ab247b99be2b89f34e8a733","path":["live","assets","in.juspay.godel","config"],"value":"https://assets.juspay.in/hyper/bundles/config/in.juspay.godel/1.2.12/v1-config.zip"},"godel_placeholder_acs_android":{"etag":"13b1c8287464d54e9417fcb18f6f21a1","path":["live","assets","in.juspay.godel.placeholder","acs_js_source"],"value":"https://assets.juspay.in/hyper/bundles/acs/in.juspay.godel/1.2.102/v1-acs.zip"},"godel_placeholder_bundle_android":{"etag":"5119e9bafb446b8d4a7e40823851684f","path":["live","package","in.juspay.godel.placeholder"],"value":"https://assets.juspay.in/hyper/bundles/app/in.juspay.godel/1.2.24/android/v1-index_bundle.zip"},"godel_placeholder_config_android":{"etag":"4bd5516b5ab247b99be2b89f34e8a733","path":["live","assets","in.juspay.godel.placeholder","config"],"value":"https://assets.juspay.in/hyper/bundles/config/in.juspay.godel/1.2.12/v1-config.zip"},"hyperos_placeholder_tracker_android":{"etag":"39b4e3e33950355a8c32e9092a954eff","path":["live","assets","in.juspay.hyperos.placeholder","tracker"],"value":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.51/v1-tracker.zip"},"patch_entries":["godel_bundle_android","godel_placeholder_bundle_android","godel_config_android","godel_placeholder_config_android","godel_acs_android","godel_placeholder_acs_android","hyperos_placeholder_tracker_android"]},"variant_type":"EXPERIMENTAL"}]','cac.admin@juspay.in','')
3bcd1808-5f83-4845-bdf5-293a8de5ded2	experiments	postgres	2024-08-21 22:46:30.311218	INSERT	\N	{"id":7102231926366408704,"created_at":"2023-08-29T10:14:04+05:30","created_by":"cac.admin@juspay.in","last_modified":"2023-08-29T10:14:04+05:30","name":"Godel ACS Release","override_keys":["hyperos_placeholder_tracker_android","patch_entries"],"status":"CREATED","traffic_percentage":0,"context":{"and":[{"==":[{"var":"os"},"android"]},{"==":[{"var":"clientId"},"meesho"]}]},"variants":[{"context_id":"4270402515ba13c3ff7fb0e1681f643d4f5fc263febe1802150f71471b8f3a8d","id":"7102231926366408704-control","override_id":"8f3101e5e15b0ced15b806037127158ad1bb4f538e04b612a9cf67760aa55d05","overrides":{"hyperos_placeholder_tracker_android":{"etag":"72e0db9d6d92d558b8a3dbb2e6abf0ec","path":["live","assets","in.juspay.hyperos.placeholder","tracker"],"value":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.50/v1-tracker.zip"},"patch_entries":["godel_bundle_android","godel_placeholder_bundle_android","godel_config_android","godel_placeholder_config_android","godel_acs_android","godel_placeholder_acs_android","hyperos_placeholder_tracker_android"]},"variant_type":"CONTROL"},{"context_id":"887c25ef2d7bbb294123679838e2ddcb4ee19e4ed7d491d9485d6ee8481f1350","id":"7102231926366408704-test","override_id":"f520553a4fd6cf08ca2248306d74f0365f618c3e76441ecabfa8f3f72ba5528d","overrides":{"hyperos_placeholder_tracker_android":{"etag":"0ccaa9fa32772930d4a55a82c9d08429","path":["live","assets","in.juspay.hyperos.placeholder","tracker"],"value":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.52/v1-tracker.zip"},"patch_entries":["godel_bundle_android","godel_placeholder_bundle_android","godel_config_android","godel_placeholder_config_android","godel_acs_android","godel_placeholder_acs_android","hyperos_placeholder_tracker_android"]},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"cac.admin@juspay.in","chosen_variant":""}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7102231926366408704,'2023-08-29 10:14:04+05:30','cac.admin@juspay.in','2023-08-29 10:14:04+05:30','Godel ACS Release','{hyperos_placeholder_tracker_android,patch_entries}','CREATED',0,'{"and":[{"==":[{"var":"os"},"android"]},{"==":[{"var":"clientId"},"meesho"]}]}','[{"context_id":"4270402515ba13c3ff7fb0e1681f643d4f5fc263febe1802150f71471b8f3a8d","id":"7102231926366408704-control","override_id":"8f3101e5e15b0ced15b806037127158ad1bb4f538e04b612a9cf67760aa55d05","overrides":{"hyperos_placeholder_tracker_android":{"etag":"72e0db9d6d92d558b8a3dbb2e6abf0ec","path":["live","assets","in.juspay.hyperos.placeholder","tracker"],"value":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.50/v1-tracker.zip"},"patch_entries":["godel_bundle_android","godel_placeholder_bundle_android","godel_config_android","godel_placeholder_config_android","godel_acs_android","godel_placeholder_acs_android","hyperos_placeholder_tracker_android"]},"variant_type":"CONTROL"},{"context_id":"887c25ef2d7bbb294123679838e2ddcb4ee19e4ed7d491d9485d6ee8481f1350","id":"7102231926366408704-test","override_id":"f520553a4fd6cf08ca2248306d74f0365f618c3e76441ecabfa8f3f72ba5528d","overrides":{"hyperos_placeholder_tracker_android":{"etag":"0ccaa9fa32772930d4a55a82c9d08429","path":["live","assets","in.juspay.hyperos.placeholder","tracker"],"value":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.52/v1-tracker.zip"},"patch_entries":["godel_bundle_android","godel_placeholder_bundle_android","godel_config_android","godel_placeholder_config_android","godel_acs_android","godel_placeholder_acs_android","hyperos_placeholder_tracker_android"]},"variant_type":"EXPERIMENTAL"}]','cac.admin@juspay.in','')
d742ff29-69b0-49f9-b0c4-6c3dea69ff8c	experiments	postgres	2024-08-21 22:46:30.311459	INSERT	\N	{"id":7110602278733541376,"created_at":"2023-09-21T12:34:51+05:30","created_by":"cac.admin@juspay.in","last_modified":"2023-09-22T10:09:28+05:30","name":"Enabling retry for Geddit Android","override_keys":["ec_base_html_android","hyperpay_base_html_android","upi_intent_base_html_android","patch_entries"],"status":"CONCLUDED","traffic_percentage":0,"context":{"and":[{"==":[{"var":"os"},"android"]},{"==":[{"var":"clientId"},"geddit"]}]},"variants":[{"context_id":"618320da8cca8abb370bf14e1d4725dd12e73340ad88f4c2fc12ea2cb66fbd25","id":"7110602278733541376-control","override_id":"d958db68123dd4b5b7a8be14b8668be42a25a94d8414c8aace4f9e239b6ebe3f","overrides":{"ec_base_html_android":{"etag":"","path":["dependencies","in.juspay.ec","default","src"],"value":""},"hyperpay_base_html_android":{"etag":"","path":["dependencies","in.juspay.hyperpay","default","src"],"value":""},"patch_entries":["ec_base_html_android","hyperpay_base_html_android","upi_base_html_android","hyperos_placeholder_tracker_android","godel_bundle_android","godel_placeholder_bundle_android","godel_config_android","godel_placeholder_config_android","godel_acs_android","godel_placeholder_acs_android","hyperos_placeholder_tracker_android"],"upi_intent_base_html_android":{"etag":"","path":["dependencies","in.juspay.upiintent","default","src"],"value":""}},"variant_type":"CONTROL"},{"context_id":"6683ce24a5cfd2b0d0fdf9f177028327ac79f8f945c05550805ea02efc46b994","id":"7110602278733541376-test","override_id":"b198985161d54953b36be0c2dcef163c071cd46fc861ac0e5634efff0ea1977a","overrides":{"ec_base_html_android":{"etag":"c486c8425929dbec13d2b5c469f274a7","path":["dependencies","in.juspay.ec","default","src"],"value":"https://assets.juspay.in/hyper/bundles/android/release/in.juspay.ec/1.0.4/base.html"},"hyperpay_base_html_android":{"etag":"d7c0cd9bc9a166e223efec78b94548ba","path":["dependencies","in.juspay.hyperpay","default","src"],"value":"https://assets.juspay.in/hyper/bundles/android/release/in.juspay.hyperpay/2.0.0/common/1.0.0/base.html"},"patch_entries":["ec_base_html_android","hyperpay_base_html_android","upi_intent_base_html_android","hyperos_placeholder_tracker_android","godel_bundle_android","godel_placeholder_bundle_android","godel_config_android","godel_placeholder_config_android","godel_acs_android","godel_placeholder_acs_android","hyperos_placeholder_tracker_android"],"upi_intent_base_html_android":{"etag":"5a00e9074edb9713389d26f53cbf97b2","path":["dependencies","in.juspay.upiintent","default","src"],"value":"https://assets.juspay.in/hyper/bundles/android/release/in.juspay.upiintent/1.0.4/base.html"}},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"cac.admin@juspay.in","chosen_variant":"7110602278733541376-test"}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7110602278733541376,'2023-09-21 12:34:51+05:30','cac.admin@juspay.in','2023-09-22 10:09:28+05:30','Enabling retry for Geddit Android','{ec_base_html_android,hyperpay_base_html_android,upi_intent_base_html_android,patch_entries}','CONCLUDED',0,'{"and":[{"==":[{"var":"os"},"android"]},{"==":[{"var":"clientId"},"geddit"]}]}','[{"context_id":"618320da8cca8abb370bf14e1d4725dd12e73340ad88f4c2fc12ea2cb66fbd25","id":"7110602278733541376-control","override_id":"d958db68123dd4b5b7a8be14b8668be42a25a94d8414c8aace4f9e239b6ebe3f","overrides":{"ec_base_html_android":{"etag":"","path":["dependencies","in.juspay.ec","default","src"],"value":""},"hyperpay_base_html_android":{"etag":"","path":["dependencies","in.juspay.hyperpay","default","src"],"value":""},"patch_entries":["ec_base_html_android","hyperpay_base_html_android","upi_base_html_android","hyperos_placeholder_tracker_android","godel_bundle_android","godel_placeholder_bundle_android","godel_config_android","godel_placeholder_config_android","godel_acs_android","godel_placeholder_acs_android","hyperos_placeholder_tracker_android"],"upi_intent_base_html_android":{"etag":"","path":["dependencies","in.juspay.upiintent","default","src"],"value":""}},"variant_type":"CONTROL"},{"context_id":"6683ce24a5cfd2b0d0fdf9f177028327ac79f8f945c05550805ea02efc46b994","id":"7110602278733541376-test","override_id":"b198985161d54953b36be0c2dcef163c071cd46fc861ac0e5634efff0ea1977a","overrides":{"ec_base_html_android":{"etag":"c486c8425929dbec13d2b5c469f274a7","path":["dependencies","in.juspay.ec","default","src"],"value":"https://assets.juspay.in/hyper/bundles/android/release/in.juspay.ec/1.0.4/base.html"},"hyperpay_base_html_android":{"etag":"d7c0cd9bc9a166e223efec78b94548ba","path":["dependencies","in.juspay.hyperpay","default","src"],"value":"https://assets.juspay.in/hyper/bundles/android/release/in.juspay.hyperpay/2.0.0/common/1.0.0/base.html"},"patch_entries":["ec_base_html_android","hyperpay_base_html_android","upi_intent_base_html_android","hyperos_placeholder_tracker_android","godel_bundle_android","godel_placeholder_bundle_android","godel_config_android","godel_placeholder_config_android","godel_acs_android","godel_placeholder_acs_android","hyperos_placeholder_tracker_android"],"upi_intent_base_html_android":{"etag":"5a00e9074edb9713389d26f53cbf97b2","path":["dependencies","in.juspay.upiintent","default","src"],"value":"https://assets.juspay.in/hyper/bundles/android/release/in.juspay.upiintent/1.0.4/base.html"}},"variant_type":"EXPERIMENTAL"}]','cac.admin@juspay.in','7110602278733541376-test')
df7267f5-b73e-4094-9d7e-0acc077a32ee	experiments	postgres	2024-08-21 22:46:30.31173	INSERT	\N	{"id":7112399555253424128,"created_at":"2023-09-26T11:36:35+05:30","created_by":"cac.admin@juspay.in","last_modified":"2023-09-26T16:20:21+05:30","name":"experiment-test-11","override_keys":["pmTestKey1","pmTestKey2"],"status":"CONCLUDED","traffic_percentage":0,"context":{"and":[{"==":[{"var":"os"},"ios"]},{"==":[{"var":"clientId"},"testClientCac1"]}]},"variants":[{"context_id":"8c270101c016e0f0458709779a0f4f510be06b56a6f63a49e081216c11b849bf","id":"7112399555253424128-control","override_id":"085e637459203733acb435d0bd921081b10a46a3eb192924a332e414bf266cf7","overrides":{"pmTestKey1":"value1-control","pmTestKey2":"value1-control"},"variant_type":"CONTROL"},{"context_id":"eb1263ccc04b5b39333bf76502732290379fedbea9e7434c9bce9a7afa0a5ec1","id":"7112399555253424128-test1","override_id":"65bcc9acaebd88355f850d7087e269f36d1316d7aeeddcf72cece682f99691c1","overrides":{"pmTestKey1":"value2-test","pmTestKey2":"value2-test"},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"cac.admin@juspay.in","chosen_variant":"7112399555253424128-control"}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7112399555253424128,'2023-09-26 11:36:35+05:30','cac.admin@juspay.in','2023-09-26 16:20:21+05:30','experiment-test-11','{pmTestKey1,pmTestKey2}','CONCLUDED',0,'{"and":[{"==":[{"var":"os"},"ios"]},{"==":[{"var":"clientId"},"testClientCac1"]}]}','[{"context_id":"8c270101c016e0f0458709779a0f4f510be06b56a6f63a49e081216c11b849bf","id":"7112399555253424128-control","override_id":"085e637459203733acb435d0bd921081b10a46a3eb192924a332e414bf266cf7","overrides":{"pmTestKey1":"value1-control","pmTestKey2":"value1-control"},"variant_type":"CONTROL"},{"context_id":"eb1263ccc04b5b39333bf76502732290379fedbea9e7434c9bce9a7afa0a5ec1","id":"7112399555253424128-test1","override_id":"65bcc9acaebd88355f850d7087e269f36d1316d7aeeddcf72cece682f99691c1","overrides":{"pmTestKey1":"value2-test","pmTestKey2":"value2-test"},"variant_type":"EXPERIMENTAL"}]','cac.admin@juspay.in','7112399555253424128-control')
cbcb7552-4303-4840-a633-bb50b8546702	experiments	postgres	2024-08-21 22:46:30.31195	INSERT	\N	{"id":7112471696935440384,"created_at":"2023-09-26T16:23:15+05:30","created_by":"cac.admin@juspay.in","last_modified":"2023-09-26T16:58:55+05:30","name":"test","override_keys":["arya_src"],"status":"CONCLUDED","traffic_percentage":0,"context":{"and":[{"==":[{"var":"os"},"Android"]}]},"variants":[{"context_id":"e00a26bdf1d9420a06add8cf031c8af0480a13ee9f2deaed1a80ec955549bf7a","id":"7112471696935440384-control","override_id":"d7351178aff995b70027fa92aea762eea3f9e9a385e898fe0261f9820a171278","overrides":{"arya_src":"dawdadw"},"variant_type":"CONTROL"},{"context_id":"c6cac52f2b2e4be1238535f217dc99a8dc6163e00607fb5eecf24c8d68b4970d","id":"7112471696935440384-test1","override_id":"2058952d37351296c72bb3ebb1f99a50560538c812b2049f7bf33d8f2eb77db6","overrides":{"arya_src":"default_str_ignore_this"},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"cac.admin@juspay.in","chosen_variant":"7112471696935440384-test1"}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7112471696935440384,'2023-09-26 16:23:15+05:30','cac.admin@juspay.in','2023-09-26 16:58:55+05:30','test','{arya_src}','CONCLUDED',0,'{"and":[{"==":[{"var":"os"},"Android"]}]}','[{"context_id":"e00a26bdf1d9420a06add8cf031c8af0480a13ee9f2deaed1a80ec955549bf7a","id":"7112471696935440384-control","override_id":"d7351178aff995b70027fa92aea762eea3f9e9a385e898fe0261f9820a171278","overrides":{"arya_src":"dawdadw"},"variant_type":"CONTROL"},{"context_id":"c6cac52f2b2e4be1238535f217dc99a8dc6163e00607fb5eecf24c8d68b4970d","id":"7112471696935440384-test1","override_id":"2058952d37351296c72bb3ebb1f99a50560538c812b2049f7bf33d8f2eb77db6","overrides":{"arya_src":"default_str_ignore_this"},"variant_type":"EXPERIMENTAL"}]','cac.admin@juspay.in','7112471696935440384-test1')
96bfd5fb-9c48-48a3-8380-210df1175757	experiments	postgres	2024-08-21 22:46:30.312197	INSERT	\N	{"id":7112483836291403776,"created_at":"2023-09-26T17:11:30+05:30","created_by":"cac.admin@juspay.in","last_modified":"2023-09-26T17:11:30+05:30","name":"Test Experiment","override_keys":["arya_bundle"],"status":"CREATED","traffic_percentage":0,"context":{"and":[{"==":[{"var":"os"},"Android"]}]},"variants":[{"context_id":"6b544a63ca728f828694a1a0f87ce87164ae34e520bd7e51575bc0308c6abd93","id":"7112483836291403776-control","override_id":"5d3dcf868ecda1553ec7ee28adbcc4c29326d95ecd164c6cbcbc59d8411fb9d0","overrides":{"arya_bundle":"dawdada"},"variant_type":"CONTROL"},{"context_id":"5e5d8a18d42f8f6ab615774fd0f406bb6cc8e1e7558c9072b900d9856f0c895f","id":"7112483836291403776-test1","override_id":"7cefaf3be8698894b5a92c24d51a1636056bd720f19d3d00aca6291285cb4504","overrides":{"arya_bundle":"default_str_ignore_this"},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"cac.admin@juspay.in","chosen_variant":""}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7112483836291403776,'2023-09-26 17:11:30+05:30','cac.admin@juspay.in','2023-09-26 17:11:30+05:30','Test Experiment','{arya_bundle}','CREATED',0,'{"and":[{"==":[{"var":"os"},"Android"]}]}','[{"context_id":"6b544a63ca728f828694a1a0f87ce87164ae34e520bd7e51575bc0308c6abd93","id":"7112483836291403776-control","override_id":"5d3dcf868ecda1553ec7ee28adbcc4c29326d95ecd164c6cbcbc59d8411fb9d0","overrides":{"arya_bundle":"dawdada"},"variant_type":"CONTROL"},{"context_id":"5e5d8a18d42f8f6ab615774fd0f406bb6cc8e1e7558c9072b900d9856f0c895f","id":"7112483836291403776-test1","override_id":"7cefaf3be8698894b5a92c24d51a1636056bd720f19d3d00aca6291285cb4504","overrides":{"arya_bundle":"default_str_ignore_this"},"variant_type":"EXPERIMENTAL"}]','cac.admin@juspay.in','')
a38bbf81-a9b6-46b8-9b2f-7447da3c40c4	experiments	postgres	2024-08-21 22:46:30.312406	INSERT	\N	{"id":7112482543506247680,"created_at":"2023-09-26T17:06:21+05:30","created_by":"cac.admin@juspay.in","last_modified":"2023-09-26T17:11:36+05:30","name":"","override_keys":["arya_required_apps"],"status":"CONCLUDED","traffic_percentage":0,"context":{"and":[{"==":[{"var":"os"},"Android"]}]},"variants":[{"context_id":"5cb4d24bba63bec8af6a681b994e0875ba60b6d3ec66ea34e1cb9d72fe5522b0","id":"7112482543506247680-control","override_id":"4bf7e41d1628e1c133234a34df8fff258bc7f5bc5c3ee0dfbe8a885242a7d42b","overrides":{"arya_required_apps":"override value"},"variant_type":"CONTROL"},{"context_id":"c3c308db28e8ac0fc7530c6179157f4507405eb8e5e399b9233c3a11fbbffc38","id":"7112482543506247680-test1","override_id":"5806515a24410d352ab4927493665d2bca544868b9eb0c2c283c4f98d93e1cbc","overrides":{"arya_required_apps":["default_array_ignore_this"]},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"cac.admin@juspay.in","chosen_variant":"7112482543506247680-control"}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7112482543506247680,'2023-09-26 17:06:21+05:30','cac.admin@juspay.in','2023-09-26 17:11:36+05:30','','{arya_required_apps}','CONCLUDED',0,'{"and":[{"==":[{"var":"os"},"Android"]}]}','[{"context_id":"5cb4d24bba63bec8af6a681b994e0875ba60b6d3ec66ea34e1cb9d72fe5522b0","id":"7112482543506247680-control","override_id":"4bf7e41d1628e1c133234a34df8fff258bc7f5bc5c3ee0dfbe8a885242a7d42b","overrides":{"arya_required_apps":"override value"},"variant_type":"CONTROL"},{"context_id":"c3c308db28e8ac0fc7530c6179157f4507405eb8e5e399b9233c3a11fbbffc38","id":"7112482543506247680-test1","override_id":"5806515a24410d352ab4927493665d2bca544868b9eb0c2c283c4f98d93e1cbc","overrides":{"arya_required_apps":["default_array_ignore_this"]},"variant_type":"EXPERIMENTAL"}]','cac.admin@juspay.in','7112482543506247680-control')
5ad09151-4c2e-46f4-824b-1b73b4cb3bac	experiments	postgres	2024-08-21 22:46:30.312642	INSERT	\N	{"id":7112484787039457280,"created_at":"2023-09-26T17:15:16+05:30","created_by":"cac.admin@juspay.in","last_modified":"2023-09-26T17:15:16+05:30","name":"testing","override_keys":["arya_bundle"],"status":"CREATED","traffic_percentage":0,"context":{"and":[{"==":[{"var":"os"},"IOS"]}]},"variants":[{"context_id":"e60e37fca9c2bea5eac657f14956c641d7e3e2fa7ede85815ff9705da7a8f86c","id":"7112484787039457280-control","override_id":"775dfe8d439fdfc8d3acbec91b8f7389e197a86851803546e99e5a419bf678be","overrides":{"arya_bundle":"","arya_bundle_etag":"dummy value"},"variant_type":"CONTROL"},{"context_id":"e524572fb1c1fdba2a68f1419e7fc3bf275bf4459ac896baa17e77528d76f9c1","id":"7112484787039457280-test1","override_id":"7cefaf3be8698894b5a92c24d51a1636056bd720f19d3d00aca6291285cb4504","overrides":{"arya_bundle":"default_str_ignore_this"},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"cac.admin@juspay.in","chosen_variant":""}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7112484787039457280,'2023-09-26 17:15:16+05:30','cac.admin@juspay.in','2023-09-26 17:15:16+05:30','testing','{arya_bundle}','CREATED',0,'{"and":[{"==":[{"var":"os"},"IOS"]}]}','[{"context_id":"e60e37fca9c2bea5eac657f14956c641d7e3e2fa7ede85815ff9705da7a8f86c","id":"7112484787039457280-control","override_id":"775dfe8d439fdfc8d3acbec91b8f7389e197a86851803546e99e5a419bf678be","overrides":{"arya_bundle":"","arya_bundle_etag":"dummy value"},"variant_type":"CONTROL"},{"context_id":"e524572fb1c1fdba2a68f1419e7fc3bf275bf4459ac896baa17e77528d76f9c1","id":"7112484787039457280-test1","override_id":"7cefaf3be8698894b5a92c24d51a1636056bd720f19d3d00aca6291285cb4504","overrides":{"arya_bundle":"default_str_ignore_this"},"variant_type":"EXPERIMENTAL"}]','cac.admin@juspay.in','')
336d0920-5fa5-4d24-9ccb-172031ade1f2	experiments	postgres	2024-08-21 22:46:30.312857	INSERT	\N	{"id":7112484915755606016,"created_at":"2023-09-26T17:15:47+05:30","created_by":"cac.admin@juspay.in","last_modified":"2023-09-26T17:15:47+05:30","name":"testing 2","override_keys":["arya_bundle"],"status":"CREATED","traffic_percentage":0,"context":{"and":[{"==":[{"var":"os"},"IOS"]}]},"variants":[{"context_id":"a2a3a4c7c780ae55f036a32ff935999ba265c4956b24d1eab824a01b01798072","id":"7112484915755606016-control","override_id":"d813868e6f17aaec05294b48a6965274e6734caac9b4a6b7fe71df536e95f150","overrides":{"arya_bundle":"adwadwawdad","arya_bundle_etag":"dummy value"},"variant_type":"CONTROL"},{"context_id":"a2bf80af5d13d9e5332b8a12e24d392860be8bfd360f72e56d8a5d7a612737aa","id":"7112484915755606016-test1","override_id":"7cefaf3be8698894b5a92c24d51a1636056bd720f19d3d00aca6291285cb4504","overrides":{"arya_bundle":"default_str_ignore_this"},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"cac.admin@juspay.in","chosen_variant":""}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7112484915755606016,'2023-09-26 17:15:47+05:30','cac.admin@juspay.in','2023-09-26 17:15:47+05:30','testing 2','{arya_bundle}','CREATED',0,'{"and":[{"==":[{"var":"os"},"IOS"]}]}','[{"context_id":"a2a3a4c7c780ae55f036a32ff935999ba265c4956b24d1eab824a01b01798072","id":"7112484915755606016-control","override_id":"d813868e6f17aaec05294b48a6965274e6734caac9b4a6b7fe71df536e95f150","overrides":{"arya_bundle":"adwadwawdad","arya_bundle_etag":"dummy value"},"variant_type":"CONTROL"},{"context_id":"a2bf80af5d13d9e5332b8a12e24d392860be8bfd360f72e56d8a5d7a612737aa","id":"7112484915755606016-test1","override_id":"7cefaf3be8698894b5a92c24d51a1636056bd720f19d3d00aca6291285cb4504","overrides":{"arya_bundle":"default_str_ignore_this"},"variant_type":"EXPERIMENTAL"}]','cac.admin@juspay.in','')
b7e2c740-ef35-472f-8df6-fb7a43bb4c93	experiments	postgres	2024-08-21 22:46:30.313057	INSERT	\N	{"id":7112523033254813696,"created_at":"2023-09-26T19:47:15+05:30","created_by":"cac.admin@juspay.in","last_modified":"2023-09-26T19:47:15+05:30","name":"Test Exp","override_keys":["arya_bundle"],"status":"CREATED","traffic_percentage":0,"context":{"and":[{"==":[{"var":"os"},"Android"]}]},"variants":[{"context_id":"853dc24076452538656695cde3d7763ae7279a3abfa04813fcc61e9666f7d613","id":"7112523033254813696-control","override_id":"c598f498cbd0b6e139240bb560005252646ec77b5f03942ad9f5a961f374a316","overrides":{"arya_bundle":"https://www.google.com","arya_bundle_etag":"dummy value"},"variant_type":"CONTROL"},{"context_id":"648e965b2882442af7af11d2c74bbbb071dbf6a0451e27ef13271812a9e4df21","id":"7112523033254813696-test1","override_id":"7cefaf3be8698894b5a92c24d51a1636056bd720f19d3d00aca6291285cb4504","overrides":{"arya_bundle":"default_str_ignore_this"},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"cac.admin@juspay.in","chosen_variant":""}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7112523033254813696,'2023-09-26 19:47:15+05:30','cac.admin@juspay.in','2023-09-26 19:47:15+05:30','Test Exp','{arya_bundle}','CREATED',0,'{"and":[{"==":[{"var":"os"},"Android"]}]}','[{"context_id":"853dc24076452538656695cde3d7763ae7279a3abfa04813fcc61e9666f7d613","id":"7112523033254813696-control","override_id":"c598f498cbd0b6e139240bb560005252646ec77b5f03942ad9f5a961f374a316","overrides":{"arya_bundle":"https://www.google.com","arya_bundle_etag":"dummy value"},"variant_type":"CONTROL"},{"context_id":"648e965b2882442af7af11d2c74bbbb071dbf6a0451e27ef13271812a9e4df21","id":"7112523033254813696-test1","override_id":"7cefaf3be8698894b5a92c24d51a1636056bd720f19d3d00aca6291285cb4504","overrides":{"arya_bundle":"default_str_ignore_this"},"variant_type":"EXPERIMENTAL"}]','cac.admin@juspay.in','')
7461e174-d936-49d2-be96-e9da5c6be64f	experiments	postgres	2024-08-21 22:46:30.313263	INSERT	\N	{"id":7112682390123143168,"created_at":"2023-09-27T06:20:29+05:30","created_by":"cac.admin@juspay.in","last_modified":"2023-09-27T06:20:29+05:30","name":"Enabling godel changes for Countrydelight + gameskraft","override_keys":["godel_bundle","godel_placeholder_bundle","godel_bundle_etag","godel_placeholder_bundle_etag"],"status":"CREATED","traffic_percentage":0,"context":{"and":[{"==":[{"var":"os"},"android"]},{"in":[{"var":"clientId"},["gameskraft","countrydelight"]]}]},"variants":[{"context_id":"a88e87c678b470c24b45cfcfc32c038b63d89318f318e10af0a1060735ed46f1","id":"7112682390123143168-test","override_id":"ae849a41b3c50faf9d0f3771ad661515fc04f8a171a0ae649df4b591c45e6929","overrides":{"godel_bundle":"https://assets.juspay.in/hyper/bundles/app/in.juspay.godel/1.2.27/android/v1-index_bundle.zip","godel_bundle_etag":"cb7c0aee7082a50760a380910d3922d4","godel_placeholder_bundle":"https://assets.juspay.in/hyper/bundles/app/in.juspay.godel/1.2.27/android/v1-index_bundle.zip","godel_placeholder_bundle_etag":"cb7c0aee7082a50760a380910d3922d4"},"variant_type":"EXPERIMENTAL"},{"context_id":"7c8f8acfaa91a158d59cca5ce81b670e45cd4bac7595f541dde19862421050dc","id":"7112682390123143168-control","override_id":"54c28d5455225caf40f161749301da3a182b09ec572a549fd02c6d9e2c920270","overrides":{"godel_bundle":"https://assets.juspay.in/hyper/bundles/app/in.juspay.godel/1.2.26/android/v1-index_bundle.zip","godel_bundle_etag":"3cb9454c5ec2e38e3b74e11b1ec1071c","godel_placeholder_bundle":"https://assets.juspay.in/hyper/bundles/app/in.juspay.godel/1.2.26/android/v1-index_bundle.zip","godel_placeholder_bundle_etag":"3cb9454c5ec2e38e3b74e11b1ec1071c"},"variant_type":"CONTROL"}],"last_modified_by":"cac.admin@juspay.in","chosen_variant":""}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7112682390123143168,'2023-09-27 06:20:29+05:30','cac.admin@juspay.in','2023-09-27 06:20:29+05:30','Enabling godel changes for Countrydelight + gameskraft','{godel_bundle,godel_placeholder_bundle,godel_bundle_etag,godel_placeholder_bundle_etag}','CREATED',0,'{"and":[{"==":[{"var":"os"},"android"]},{"in":[{"var":"clientId"},["gameskraft","countrydelight"]]}]}','[{"context_id":"a88e87c678b470c24b45cfcfc32c038b63d89318f318e10af0a1060735ed46f1","id":"7112682390123143168-test","override_id":"ae849a41b3c50faf9d0f3771ad661515fc04f8a171a0ae649df4b591c45e6929","overrides":{"godel_bundle":"https://assets.juspay.in/hyper/bundles/app/in.juspay.godel/1.2.27/android/v1-index_bundle.zip","godel_bundle_etag":"cb7c0aee7082a50760a380910d3922d4","godel_placeholder_bundle":"https://assets.juspay.in/hyper/bundles/app/in.juspay.godel/1.2.27/android/v1-index_bundle.zip","godel_placeholder_bundle_etag":"cb7c0aee7082a50760a380910d3922d4"},"variant_type":"EXPERIMENTAL"},{"context_id":"7c8f8acfaa91a158d59cca5ce81b670e45cd4bac7595f541dde19862421050dc","id":"7112682390123143168-control","override_id":"54c28d5455225caf40f161749301da3a182b09ec572a549fd02c6d9e2c920270","overrides":{"godel_bundle":"https://assets.juspay.in/hyper/bundles/app/in.juspay.godel/1.2.26/android/v1-index_bundle.zip","godel_bundle_etag":"3cb9454c5ec2e38e3b74e11b1ec1071c","godel_placeholder_bundle":"https://assets.juspay.in/hyper/bundles/app/in.juspay.godel/1.2.26/android/v1-index_bundle.zip","godel_placeholder_bundle_etag":"3cb9454c5ec2e38e3b74e11b1ec1071c"},"variant_type":"CONTROL"}]','cac.admin@juspay.in','')
8f24cd46-51f4-45bb-bd77-40421a49bb04	experiments	postgres	2024-08-21 22:46:30.313503	INSERT	\N	{"id":7112710543868051456,"created_at":"2023-09-27T08:12:21+05:30","created_by":"cac.admin@juspay.in","last_modified":"2023-09-27T08:24:53+05:30","name":"Updating UPI Intent for all web merchants","override_keys":["upi_intent_bundle_web","upi_intent_config_web","upi_intent_bundle_web_etag","upi_intent_config_web_etag"],"status":"CONCLUDED","traffic_percentage":0,"context":{"and":[{"==":[{"var":"os"},"web"]}]},"variants":[{"context_id":"f969e098c7d078e574c6f88a4d74079dfe7717f197f453fe517a6dbfad371ade","id":"7112710543868051456-test","override_id":"b061b5cd60ddfe1bae9ba1f1540eadfbac2bebb230314116b4e899af67b14093","overrides":{"upi_intent_bundle_web":"https://assets.juspay.in/hyper/bundles/app/in.juspay.upiintent/4.3.2/web/index.js","upi_intent_bundle_web_etag":"1bef9c71cb89700638234e5aa0e3e164","upi_intent_config_web":"https://assets.juspay.in/hyper/bundles/config/in.juspay.upiintent/common/2.2.12/config.js","upi_intent_config_web_etag":"40e640126342c39c020bf07f22af28dc"},"variant_type":"EXPERIMENTAL"},{"context_id":"9f22dedc869daf4d1d730038fd9a33a4fd4825fce2350272aa9811a5b8864bfd","id":"7112710543868051456-control","override_id":"b061b5cd60ddfe1bae9ba1f1540eadfbac2bebb230314116b4e899af67b14093","overrides":{"upi_intent_bundle_web":"https://assets.juspay.in/hyper/bundles/app/in.juspay.upiintent/4.3.2/web/index.js","upi_intent_bundle_web_etag":"1bef9c71cb89700638234e5aa0e3e164","upi_intent_config_web":"https://assets.juspay.in/hyper/bundles/config/in.juspay.upiintent/common/2.2.12/config.js","upi_intent_config_web_etag":"40e640126342c39c020bf07f22af28dc"},"variant_type":"CONTROL"}],"last_modified_by":"cac.admin@juspay.in","chosen_variant":"7112710543868051456-test"}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7112710543868051456,'2023-09-27 08:12:21+05:30','cac.admin@juspay.in','2023-09-27 08:24:53+05:30','Updating UPI Intent for all web merchants','{upi_intent_bundle_web,upi_intent_config_web,upi_intent_bundle_web_etag,upi_intent_config_web_etag}','CONCLUDED',0,'{"and":[{"==":[{"var":"os"},"web"]}]}','[{"context_id":"f969e098c7d078e574c6f88a4d74079dfe7717f197f453fe517a6dbfad371ade","id":"7112710543868051456-test","override_id":"b061b5cd60ddfe1bae9ba1f1540eadfbac2bebb230314116b4e899af67b14093","overrides":{"upi_intent_bundle_web":"https://assets.juspay.in/hyper/bundles/app/in.juspay.upiintent/4.3.2/web/index.js","upi_intent_bundle_web_etag":"1bef9c71cb89700638234e5aa0e3e164","upi_intent_config_web":"https://assets.juspay.in/hyper/bundles/config/in.juspay.upiintent/common/2.2.12/config.js","upi_intent_config_web_etag":"40e640126342c39c020bf07f22af28dc"},"variant_type":"EXPERIMENTAL"},{"context_id":"9f22dedc869daf4d1d730038fd9a33a4fd4825fce2350272aa9811a5b8864bfd","id":"7112710543868051456-control","override_id":"b061b5cd60ddfe1bae9ba1f1540eadfbac2bebb230314116b4e899af67b14093","overrides":{"upi_intent_bundle_web":"https://assets.juspay.in/hyper/bundles/app/in.juspay.upiintent/4.3.2/web/index.js","upi_intent_bundle_web_etag":"1bef9c71cb89700638234e5aa0e3e164","upi_intent_config_web":"https://assets.juspay.in/hyper/bundles/config/in.juspay.upiintent/common/2.2.12/config.js","upi_intent_config_web_etag":"40e640126342c39c020bf07f22af28dc"},"variant_type":"CONTROL"}]','cac.admin@juspay.in','7112710543868051456-test')
4cfaa164-5c59-4fb4-8cf2-48b188dfed5f	experiments	postgres	2024-08-21 22:46:30.313735	INSERT	\N	{"id":7112714599210156032,"created_at":"2023-09-27T08:28:28+05:30","created_by":"cac.admin@juspay.in","last_modified":"2023-09-27T08:35:53+05:30","name":"Updating UPI Intent for all web merchants","override_keys":["upiintent_bundle_web","upiintent_config_web","upiintent_bundle_web_etag","upiintent_config_web_etag"],"status":"CONCLUDED","traffic_percentage":0,"context":{"and":[{"==":[{"var":"os"},"web"]}]},"variants":[{"context_id":"9b84843d2c515bade29116438270867300bb19de2750fb696f39f391e462daca","id":"7112714599210156032-test","override_id":"a5e9d242d2f9a5fbe5545ae1c1486fdbdbe497ed11dabe9e3d4fc9558c5b98c6","overrides":{"upiintent_bundle_web":"https://assets.juspay.in/hyper/bundles/app/in.juspay.upiintent/4.3.2/web/index.js","upiintent_bundle_web_etag":"1bef9c71cb89700638234e5aa0e3e164","upiintent_config_web":"https://assets.juspay.in/hyper/bundles/config/in.juspay.upiintent/common/2.2.12/config.js","upiintent_config_web_etag":"40e640126342c39c020bf07f22af28dc"},"variant_type":"EXPERIMENTAL"},{"context_id":"bf2efeb7a5821273bcd6f7fdc572cbf304d0ba44dc224918559f7a23729358c3","id":"7112714599210156032-control","override_id":"a5e9d242d2f9a5fbe5545ae1c1486fdbdbe497ed11dabe9e3d4fc9558c5b98c6","overrides":{"upiintent_bundle_web":"https://assets.juspay.in/hyper/bundles/app/in.juspay.upiintent/4.3.2/web/index.js","upiintent_bundle_web_etag":"1bef9c71cb89700638234e5aa0e3e164","upiintent_config_web":"https://assets.juspay.in/hyper/bundles/config/in.juspay.upiintent/common/2.2.12/config.js","upiintent_config_web_etag":"40e640126342c39c020bf07f22af28dc"},"variant_type":"CONTROL"}],"last_modified_by":"cac.admin@juspay.in","chosen_variant":"7112714599210156032-test"}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7112714599210156032,'2023-09-27 08:28:28+05:30','cac.admin@juspay.in','2023-09-27 08:35:53+05:30','Updating UPI Intent for all web merchants','{upiintent_bundle_web,upiintent_config_web,upiintent_bundle_web_etag,upiintent_config_web_etag}','CONCLUDED',0,'{"and":[{"==":[{"var":"os"},"web"]}]}','[{"context_id":"9b84843d2c515bade29116438270867300bb19de2750fb696f39f391e462daca","id":"7112714599210156032-test","override_id":"a5e9d242d2f9a5fbe5545ae1c1486fdbdbe497ed11dabe9e3d4fc9558c5b98c6","overrides":{"upiintent_bundle_web":"https://assets.juspay.in/hyper/bundles/app/in.juspay.upiintent/4.3.2/web/index.js","upiintent_bundle_web_etag":"1bef9c71cb89700638234e5aa0e3e164","upiintent_config_web":"https://assets.juspay.in/hyper/bundles/config/in.juspay.upiintent/common/2.2.12/config.js","upiintent_config_web_etag":"40e640126342c39c020bf07f22af28dc"},"variant_type":"EXPERIMENTAL"},{"context_id":"bf2efeb7a5821273bcd6f7fdc572cbf304d0ba44dc224918559f7a23729358c3","id":"7112714599210156032-control","override_id":"a5e9d242d2f9a5fbe5545ae1c1486fdbdbe497ed11dabe9e3d4fc9558c5b98c6","overrides":{"upiintent_bundle_web":"https://assets.juspay.in/hyper/bundles/app/in.juspay.upiintent/4.3.2/web/index.js","upiintent_bundle_web_etag":"1bef9c71cb89700638234e5aa0e3e164","upiintent_config_web":"https://assets.juspay.in/hyper/bundles/config/in.juspay.upiintent/common/2.2.12/config.js","upiintent_config_web_etag":"40e640126342c39c020bf07f22af28dc"},"variant_type":"CONTROL"}]','cac.admin@juspay.in','7112714599210156032-test')
bee5cf4f-033f-4218-a3b2-13301271b6e1	experiments	postgres	2024-08-21 22:46:30.313979	INSERT	\N	{"id":7112716874713358336,"created_at":"2023-09-27T08:37:30+05:30","created_by":"cac.admin@juspay.in","last_modified":"2023-09-27T08:37:30+05:30","name":"Updating UPI Intent for all web merchants","override_keys":["upiintent_bundle","upiintent_config","upiintent_bundle_etag","upiintent_config_etag"],"status":"CREATED","traffic_percentage":0,"context":{"and":[{"==":[{"var":"os"},"web"]}]},"variants":[{"context_id":"290d273e26792b51d4a544c1cb18f60dca1e7ff8b14b0ec1a51a579758ea8502","id":"7112716874713358336-test","override_id":"66be851d74373f6bfd70ca23e3487d4330aae08ba114cff56cf34b1f09fb8489","overrides":{"upiintent_bundle":"https://assets.juspay.in/hyper/bundles/app/in.juspay.upiintent/4.3.2/web/index.js","upiintent_bundle_etag":"1bef9c71cb89700638234e5aa0e3e164","upiintent_config":"https://assets.juspay.in/hyper/bundles/config/in.juspay.upiintent/common/2.2.12/config.js","upiintent_config_etag":"40e640126342c39c020bf07f22af28dc"},"variant_type":"EXPERIMENTAL"},{"context_id":"8a718c378ce843933d867f1cfaacc9532eda73d0eb6cd15e65435dee1c5a5467","id":"7112716874713358336-control","override_id":"66be851d74373f6bfd70ca23e3487d4330aae08ba114cff56cf34b1f09fb8489","overrides":{"upiintent_bundle":"https://assets.juspay.in/hyper/bundles/app/in.juspay.upiintent/4.3.2/web/index.js","upiintent_bundle_etag":"1bef9c71cb89700638234e5aa0e3e164","upiintent_config":"https://assets.juspay.in/hyper/bundles/config/in.juspay.upiintent/common/2.2.12/config.js","upiintent_config_etag":"40e640126342c39c020bf07f22af28dc"},"variant_type":"CONTROL"}],"last_modified_by":"cac.admin@juspay.in","chosen_variant":""}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7112716874713358336,'2023-09-27 08:37:30+05:30','cac.admin@juspay.in','2023-09-27 08:37:30+05:30','Updating UPI Intent for all web merchants','{upiintent_bundle,upiintent_config,upiintent_bundle_etag,upiintent_config_etag}','CREATED',0,'{"and":[{"==":[{"var":"os"},"web"]}]}','[{"context_id":"290d273e26792b51d4a544c1cb18f60dca1e7ff8b14b0ec1a51a579758ea8502","id":"7112716874713358336-test","override_id":"66be851d74373f6bfd70ca23e3487d4330aae08ba114cff56cf34b1f09fb8489","overrides":{"upiintent_bundle":"https://assets.juspay.in/hyper/bundles/app/in.juspay.upiintent/4.3.2/web/index.js","upiintent_bundle_etag":"1bef9c71cb89700638234e5aa0e3e164","upiintent_config":"https://assets.juspay.in/hyper/bundles/config/in.juspay.upiintent/common/2.2.12/config.js","upiintent_config_etag":"40e640126342c39c020bf07f22af28dc"},"variant_type":"EXPERIMENTAL"},{"context_id":"8a718c378ce843933d867f1cfaacc9532eda73d0eb6cd15e65435dee1c5a5467","id":"7112716874713358336-control","override_id":"66be851d74373f6bfd70ca23e3487d4330aae08ba114cff56cf34b1f09fb8489","overrides":{"upiintent_bundle":"https://assets.juspay.in/hyper/bundles/app/in.juspay.upiintent/4.3.2/web/index.js","upiintent_bundle_etag":"1bef9c71cb89700638234e5aa0e3e164","upiintent_config":"https://assets.juspay.in/hyper/bundles/config/in.juspay.upiintent/common/2.2.12/config.js","upiintent_config_etag":"40e640126342c39c020bf07f22af28dc"},"variant_type":"CONTROL"}]','cac.admin@juspay.in','')
3d8bea38-76e8-4410-b1f0-e20454382970	experiments	postgres	2024-08-21 22:46:30.314224	INSERT	\N	{"id":7122932754380197888,"created_at":"2023-10-25T13:11:46+05:30","created_by":"ritick.madaan@juspay.in","last_modified":"2023-10-25T14:46:26+05:30","name":"test1","override_keys":["godel_config"],"status":"CONCLUDED","traffic_percentage":0,"context":{"and":[{"==":[{"var":"os"},"android"]},{"==":[{"var":"clientId"},"galactustest"]}]},"variants":[{"context_id":"2df7f921757cc2394e0d2e00b1a362c915b00b887473ebba9b245c8b4fcd9bf8","id":"7122932754380197888-control","override_id":"dece633f19bfbce7315e7dae457a58485af60f8d482270d74553801ab5e1d59e","overrides":{"godel_config":"abc"},"variant_type":"CONTROL"},{"context_id":"7d9457fc83bb768e864e7018a54a726807de131f6b2d570ddeb0309db53a1dc8","id":"7122932754380197888-test1","override_id":"e56a69a5abdd56ed760d6f15f476ac77e8a105e7a720601299747186f1043c92","overrides":{"godel_config":"def"},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"ritick.madaan@juspay.in","chosen_variant":"7122932754380197888-control"}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7122932754380197888,'2023-10-25 13:11:46+05:30','ritick.madaan@juspay.in','2023-10-25 14:46:26+05:30','test1','{godel_config}','CONCLUDED',0,'{"and":[{"==":[{"var":"os"},"android"]},{"==":[{"var":"clientId"},"galactustest"]}]}','[{"context_id":"2df7f921757cc2394e0d2e00b1a362c915b00b887473ebba9b245c8b4fcd9bf8","id":"7122932754380197888-control","override_id":"dece633f19bfbce7315e7dae457a58485af60f8d482270d74553801ab5e1d59e","overrides":{"godel_config":"abc"},"variant_type":"CONTROL"},{"context_id":"7d9457fc83bb768e864e7018a54a726807de131f6b2d570ddeb0309db53a1dc8","id":"7122932754380197888-test1","override_id":"e56a69a5abdd56ed760d6f15f476ac77e8a105e7a720601299747186f1043c92","overrides":{"godel_config":"def"},"variant_type":"EXPERIMENTAL"}]','ritick.madaan@juspay.in','7122932754380197888-control')
cabe9977-f8d3-448e-a5de-16b91dbf5197	experiments	postgres	2024-08-21 22:46:30.314458	INSERT	\N	{"id":7113445939770986496,"created_at":"2023-09-29T08:54:33+05:30","created_by":"cac.admin@juspay.in","last_modified":"2023-09-29T08:57:10+05:30","name":"Updating base html for myntra","override_keys":["ec_src","ec_src_etag"],"status":"CONCLUDED","traffic_percentage":0,"context":{"and":[{"==":[{"var":"os"},"android"]}]},"variants":[{"context_id":"ed433be0fdf140661506a7e4b5a0d5a575eb30bcc86abe2848419ccfe2a6624f","id":"7113445939770986496-test","override_id":"6e905efb616e6f96ba55bc15293a1e4d04f972112ac7ef9d1c0be0aafe29bf45","overrides":{"ec_src":"https://assets.juspay.in/hyper/bundles/android/release/in.juspay.ec/1.0.4/base.html","ec_src_etag":"c486c8425929dbec13d2b5c469f274a7"},"variant_type":"EXPERIMENTAL"},{"context_id":"b570452f14e86dbd3f0e63875049229c0fe309b34ea8482ce0ccfedaf8789153","id":"7113445939770986496-control","override_id":"6e905efb616e6f96ba55bc15293a1e4d04f972112ac7ef9d1c0be0aafe29bf45","overrides":{"ec_src":"https://assets.juspay.in/hyper/bundles/android/release/in.juspay.ec/1.0.4/base.html","ec_src_etag":"c486c8425929dbec13d2b5c469f274a7"},"variant_type":"CONTROL"}],"last_modified_by":"cac.admin@juspay.in","chosen_variant":"7113445939770986496-test"}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7113445939770986496,'2023-09-29 08:54:33+05:30','cac.admin@juspay.in','2023-09-29 08:57:10+05:30','Updating base html for myntra','{ec_src,ec_src_etag}','CONCLUDED',0,'{"and":[{"==":[{"var":"os"},"android"]}]}','[{"context_id":"ed433be0fdf140661506a7e4b5a0d5a575eb30bcc86abe2848419ccfe2a6624f","id":"7113445939770986496-test","override_id":"6e905efb616e6f96ba55bc15293a1e4d04f972112ac7ef9d1c0be0aafe29bf45","overrides":{"ec_src":"https://assets.juspay.in/hyper/bundles/android/release/in.juspay.ec/1.0.4/base.html","ec_src_etag":"c486c8425929dbec13d2b5c469f274a7"},"variant_type":"EXPERIMENTAL"},{"context_id":"b570452f14e86dbd3f0e63875049229c0fe309b34ea8482ce0ccfedaf8789153","id":"7113445939770986496-control","override_id":"6e905efb616e6f96ba55bc15293a1e4d04f972112ac7ef9d1c0be0aafe29bf45","overrides":{"ec_src":"https://assets.juspay.in/hyper/bundles/android/release/in.juspay.ec/1.0.4/base.html","ec_src_etag":"c486c8425929dbec13d2b5c469f274a7"},"variant_type":"CONTROL"}]','cac.admin@juspay.in','7113445939770986496-test')
3c4b9c5e-ec71-4b5c-bc26-9e80bca8b01c	experiments	postgres	2024-08-21 22:46:30.314724	INSERT	\N	{"id":7112802367099015168,"created_at":"2023-09-27T14:17:13+05:30","created_by":"cac.admin@juspay.in","last_modified":"2023-09-27T14:17:13+05:30","name":"test exp5","override_keys":["arya_bundle"],"status":"CREATED","traffic_percentage":0,"context":{"and":[{"==":[{"var":"os"},"Android"]}]},"variants":[{"context_id":"feea32b2444dbd69dc32d830dfcf37681423f0b39f207f260cf8027133ff7472","id":"7112802367099015168-test1","override_id":"880a00b726fc9ec4f146ed125bef65b919373acb0ef98ab49cbc692328efd3e7","overrides":{"arya_bundle":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/galactustest/android/release/config.jsone","arya_bundle_etag":""},"variant_type":"EXPERIMENTAL"},{"context_id":"e878398e97531e11137190932cda06b918a5e141919840633d46dcadda3093d2","id":"7112802367099015168-control","override_id":"f27a63e7518f70f840d57b4545a4cddde709bb3340d440fe21f584d5820710c5","overrides":{"arya_bundle":"default_str_ignore_this","arya_bundle_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}],"last_modified_by":"cac.admin@juspay.in","chosen_variant":""}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7112802367099015168,'2023-09-27 14:17:13+05:30','cac.admin@juspay.in','2023-09-27 14:17:13+05:30','test exp5','{arya_bundle}','CREATED',0,'{"and":[{"==":[{"var":"os"},"Android"]}]}','[{"context_id":"feea32b2444dbd69dc32d830dfcf37681423f0b39f207f260cf8027133ff7472","id":"7112802367099015168-test1","override_id":"880a00b726fc9ec4f146ed125bef65b919373acb0ef98ab49cbc692328efd3e7","overrides":{"arya_bundle":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/galactustest/android/release/config.jsone","arya_bundle_etag":""},"variant_type":"EXPERIMENTAL"},{"context_id":"e878398e97531e11137190932cda06b918a5e141919840633d46dcadda3093d2","id":"7112802367099015168-control","override_id":"f27a63e7518f70f840d57b4545a4cddde709bb3340d440fe21f584d5820710c5","overrides":{"arya_bundle":"default_str_ignore_this","arya_bundle_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}]','cac.admin@juspay.in','')
e520d222-bbe4-4f9a-abf1-32b058871ee6	experiments	postgres	2024-08-21 22:46:30.314963	INSERT	\N	{"id":7112842495807877120,"created_at":"2023-09-27T16:56:41+05:30","created_by":"cac.admin@juspay.in","last_modified":"2023-09-27T16:56:41+05:30","name":"Updating Tracker for all web merchants","override_keys":["hyperos_placeholder_tracker","hyperos_placeholder_tracker_etag"],"status":"CREATED","traffic_percentage":0,"context":{"and":[{"==":[{"var":"os"},"web"]}]},"variants":[{"context_id":"53e80339ed2798ea1c019d5200ce01d2ea66e81fb64b04122b4c1c89b865e847","id":"7112842495807877120-test","override_id":"b365a9858f95ea752d20df6278eda167bf4d801424e895ec4873697cf10c084a","overrides":{"hyperos_placeholder_tracker":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.57/tracker.js","hyperos_placeholder_tracker_etag":"ae93fa69fe617c47d33bd71fb9861f60"},"variant_type":"EXPERIMENTAL"},{"context_id":"85a55cb4f9218dd77d9067bcbb2469ae63cda9fbeb02091e004cca434095b4f4","id":"7112842495807877120-control","override_id":"b365a9858f95ea752d20df6278eda167bf4d801424e895ec4873697cf10c084a","overrides":{"hyperos_placeholder_tracker":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.57/tracker.js","hyperos_placeholder_tracker_etag":"ae93fa69fe617c47d33bd71fb9861f60"},"variant_type":"CONTROL"}],"last_modified_by":"cac.admin@juspay.in","chosen_variant":""}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7112842495807877120,'2023-09-27 16:56:41+05:30','cac.admin@juspay.in','2023-09-27 16:56:41+05:30','Updating Tracker for all web merchants','{hyperos_placeholder_tracker,hyperos_placeholder_tracker_etag}','CREATED',0,'{"and":[{"==":[{"var":"os"},"web"]}]}','[{"context_id":"53e80339ed2798ea1c019d5200ce01d2ea66e81fb64b04122b4c1c89b865e847","id":"7112842495807877120-test","override_id":"b365a9858f95ea752d20df6278eda167bf4d801424e895ec4873697cf10c084a","overrides":{"hyperos_placeholder_tracker":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.57/tracker.js","hyperos_placeholder_tracker_etag":"ae93fa69fe617c47d33bd71fb9861f60"},"variant_type":"EXPERIMENTAL"},{"context_id":"85a55cb4f9218dd77d9067bcbb2469ae63cda9fbeb02091e004cca434095b4f4","id":"7112842495807877120-control","override_id":"b365a9858f95ea752d20df6278eda167bf4d801424e895ec4873697cf10c084a","overrides":{"hyperos_placeholder_tracker":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.57/tracker.js","hyperos_placeholder_tracker_etag":"ae93fa69fe617c47d33bd71fb9861f60"},"variant_type":"CONTROL"}]','cac.admin@juspay.in','')
1e4e3106-59ba-46d2-bb41-48d22e17a368	experiments	postgres	2024-08-21 22:46:30.315348	INSERT	\N	{"id":7114209005786849280,"created_at":"2023-10-01T11:26:42+05:30","created_by":"cac.admin@juspay.in","last_modified":"2023-10-01T11:26:42+05:30","name":"Updating godel for a23Games","override_keys":["godel_config","godel_config_etag"],"status":"CREATED","traffic_percentage":0,"context":{"and":[{"==":[{"var":"os"},"android"]},{"==":[{"var":"clientId"},"a23games"]}]},"variants":[{"context_id":"afdfa1fbae908660055b374a323ab23198c3b1d0b18c6942103fb5e66b7712c5","id":"7114209005786849280-test","override_id":"126550b3f94cd406d2259d46eba11e6f0c8017b97f7bbfe2bfcd3e71d75df7d7","overrides":{"godel_config":"https://assets.juspay.in/hyper/bundles/config/in.juspay.godel/1.2.19/v1-config.zip","godel_config_etag":"50d9fa0a9ab30e3795dc8da0e6f90487"},"variant_type":"EXPERIMENTAL"},{"context_id":"c1452dd5094bfc89985f33422354644d674db651264f9a6dd8e7adf59a314b06","id":"7114209005786849280-control","override_id":"26524b54cb0b9a73173dc65f05330e3fed876fb694db019d2ca573c8ed819ee4","overrides":{"godel_config":"https://assets.juspay.in/hyper/bundles/config/in.juspay.godel/1.2.13/v1-config.zip","godel_config_etag":"afb52abf821aad45674ae78317ecf53d"},"variant_type":"CONTROL"}],"last_modified_by":"cac.admin@juspay.in","chosen_variant":""}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7114209005786849280,'2023-10-01 11:26:42+05:30','cac.admin@juspay.in','2023-10-01 11:26:42+05:30','Updating godel for a23Games','{godel_config,godel_config_etag}','CREATED',0,'{"and":[{"==":[{"var":"os"},"android"]},{"==":[{"var":"clientId"},"a23games"]}]}','[{"context_id":"afdfa1fbae908660055b374a323ab23198c3b1d0b18c6942103fb5e66b7712c5","id":"7114209005786849280-test","override_id":"126550b3f94cd406d2259d46eba11e6f0c8017b97f7bbfe2bfcd3e71d75df7d7","overrides":{"godel_config":"https://assets.juspay.in/hyper/bundles/config/in.juspay.godel/1.2.19/v1-config.zip","godel_config_etag":"50d9fa0a9ab30e3795dc8da0e6f90487"},"variant_type":"EXPERIMENTAL"},{"context_id":"c1452dd5094bfc89985f33422354644d674db651264f9a6dd8e7adf59a314b06","id":"7114209005786849280-control","override_id":"26524b54cb0b9a73173dc65f05330e3fed876fb694db019d2ca573c8ed819ee4","overrides":{"godel_config":"https://assets.juspay.in/hyper/bundles/config/in.juspay.godel/1.2.13/v1-config.zip","godel_config_etag":"afb52abf821aad45674ae78317ecf53d"},"variant_type":"CONTROL"}]','cac.admin@juspay.in','')
62d3c917-cb98-45e9-8f69-66edee3f5dc3	experiments	postgres	2024-08-21 22:46:30.31559	INSERT	\N	{"id":7114915282043125760,"created_at":"2023-10-03T10:13:12+05:30","created_by":"cac.admin@juspay.in","last_modified":"2023-10-03T10:13:12+05:30","name":"Updating godel for all Android","override_keys":["godel_placeholder_config","godel_placeholder_config_etag","godel_config","godel_config_etag","godel_acs","godel_acs_etag","godel_placeholder_acs","godel_placeholder_acs_etag"],"status":"CREATED","traffic_percentage":0,"context":{"and":[{"==":[{"var":"os"},"android"]}]},"variants":[{"context_id":"eda51638bac9d18931dc22611c90c245b0a92acbfc9734828d9cb04ab5fa978c","id":"7114915282043125760-test","override_id":"1a3c45127ec61377d38cfa5d61d161f4c1e9617aac5758288374844f81ce3c95","overrides":{"godel_acs":"https://assets.juspay.in/hyper/bundles/acs/in.juspay.godel/1.2.109/v1-acs.zip","godel_acs_etag":"7c16fd41bbc9af01bc53ddea98b75f31","godel_config":"https://assets.juspay.in/hyper/bundles/config/in.juspay.godel/1.2.18/v1-config.zip","godel_config_etag":"d4bbf0748060e6e873eb883af6a1ff09","godel_placeholder_acs":"https://assets.juspay.in/hyper/bundles/acs/in.juspay.godel/1.2.109/v1-acs.zip","godel_placeholder_acs_etag":"7c16fd41bbc9af01bc53ddea98b75f31","godel_placeholder_config":"https://assets.juspay.in/hyper/bundles/config/in.juspay.godel/1.2.18/v1-config.zip","godel_placeholder_config_etag":"d4bbf0748060e6e873eb883af6a1ff09"},"variant_type":"EXPERIMENTAL"},{"context_id":"5ff9b14b5b370e96ff3866f2e7795f0603939ba43e0491f0593b61ccbb054179","id":"7114915282043125760-control","override_id":"42a6fdf08e7ec3481e498211a9c8c51567c32c2afa4113ee85e391e74913e1b9","overrides":{"godel_acs":"https://assets.juspay.in/hyper/bundles/acs/in.juspay.godel/1.2.106/v1-acs.zip","godel_acs_etag":"3cb9454c5ec2e38e3b74e11b1ec1071c","godel_config":"https://assets.juspay.in/hyper/bundles/config/in.juspay.godel/1.2.13/v1-config.zip","godel_config_etag":"afb52abf821aad45674ae78317ecf53d","godel_placeholder_acs":"https://assets.juspay.in/hyper/bundles/acs/in.juspay.godel/1.2.106/v1-acs.zip","godel_placeholder_acs_etag":"3cb9454c5ec2e38e3b74e11b1ec1071c","godel_placeholder_config":"https://assets.juspay.in/hyper/bundles/config/in.juspay.godel/1.2.13/v1-config.zip","godel_placeholder_config_etag":"afb52abf821aad45674ae78317ecf53d"},"variant_type":"CONTROL"}],"last_modified_by":"cac.admin@juspay.in","chosen_variant":""}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7114915282043125760,'2023-10-03 10:13:12+05:30','cac.admin@juspay.in','2023-10-03 10:13:12+05:30','Updating godel for all Android','{godel_placeholder_config,godel_placeholder_config_etag,godel_config,godel_config_etag,godel_acs,godel_acs_etag,godel_placeholder_acs,godel_placeholder_acs_etag}','CREATED',0,'{"and":[{"==":[{"var":"os"},"android"]}]}','[{"context_id":"eda51638bac9d18931dc22611c90c245b0a92acbfc9734828d9cb04ab5fa978c","id":"7114915282043125760-test","override_id":"1a3c45127ec61377d38cfa5d61d161f4c1e9617aac5758288374844f81ce3c95","overrides":{"godel_acs":"https://assets.juspay.in/hyper/bundles/acs/in.juspay.godel/1.2.109/v1-acs.zip","godel_acs_etag":"7c16fd41bbc9af01bc53ddea98b75f31","godel_config":"https://assets.juspay.in/hyper/bundles/config/in.juspay.godel/1.2.18/v1-config.zip","godel_config_etag":"d4bbf0748060e6e873eb883af6a1ff09","godel_placeholder_acs":"https://assets.juspay.in/hyper/bundles/acs/in.juspay.godel/1.2.109/v1-acs.zip","godel_placeholder_acs_etag":"7c16fd41bbc9af01bc53ddea98b75f31","godel_placeholder_config":"https://assets.juspay.in/hyper/bundles/config/in.juspay.godel/1.2.18/v1-config.zip","godel_placeholder_config_etag":"d4bbf0748060e6e873eb883af6a1ff09"},"variant_type":"EXPERIMENTAL"},{"context_id":"5ff9b14b5b370e96ff3866f2e7795f0603939ba43e0491f0593b61ccbb054179","id":"7114915282043125760-control","override_id":"42a6fdf08e7ec3481e498211a9c8c51567c32c2afa4113ee85e391e74913e1b9","overrides":{"godel_acs":"https://assets.juspay.in/hyper/bundles/acs/in.juspay.godel/1.2.106/v1-acs.zip","godel_acs_etag":"3cb9454c5ec2e38e3b74e11b1ec1071c","godel_config":"https://assets.juspay.in/hyper/bundles/config/in.juspay.godel/1.2.13/v1-config.zip","godel_config_etag":"afb52abf821aad45674ae78317ecf53d","godel_placeholder_acs":"https://assets.juspay.in/hyper/bundles/acs/in.juspay.godel/1.2.106/v1-acs.zip","godel_placeholder_acs_etag":"3cb9454c5ec2e38e3b74e11b1ec1071c","godel_placeholder_config":"https://assets.juspay.in/hyper/bundles/config/in.juspay.godel/1.2.13/v1-config.zip","godel_placeholder_config_etag":"afb52abf821aad45674ae78317ecf53d"},"variant_type":"CONTROL"}]','cac.admin@juspay.in','')
de93230f-fb07-46ea-aefe-c4c5b3ac195a	experiments	postgres	2024-08-21 22:46:30.315984	INSERT	\N	{"id":7122929931712266240,"created_at":"2023-10-25T13:00:33+05:30","created_by":"ritick.madaan@juspay.in","last_modified":"2023-10-25T13:02:07+05:30","name":"experiment-1","override_keys":["pmTestKey1","pmTestKey2"],"status":"CONCLUDED","traffic_percentage":35,"context":{"and":[{"==":[{"var":"os"},"ios"]},{"==":[{"var":"client"},"testClientCac1"]}]},"variants":[{"context_id":"3d4e5429c33881be263a7583407acd9548af2bd83cbe730ac8c8fb5e97d6f079","id":"7122929931712266240-control","override_id":"085e637459203733acb435d0bd921081b10a46a3eb192924a332e414bf266cf7","overrides":{"pmTestKey1":"value1-control","pmTestKey2":"value1-control"},"variant_type":"CONTROL"},{"context_id":"1a7b31bd94857f09edf0c69130af94bc70d830b5b8ebf021f48d1a04eeab7dfc","id":"7122929931712266240-test1","override_id":"65bcc9acaebd88355f850d7087e269f36d1316d7aeeddcf72cece682f99691c1","overrides":{"pmTestKey1":"value2-test","pmTestKey2":"value2-test"},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"ritick.madaan@juspay.in","chosen_variant":"7122929931712266240-control"}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7122929931712266240,'2023-10-25 13:00:33+05:30','ritick.madaan@juspay.in','2023-10-25 13:02:07+05:30','experiment-1','{pmTestKey1,pmTestKey2}','CONCLUDED',35,'{"and":[{"==":[{"var":"os"},"ios"]},{"==":[{"var":"client"},"testClientCac1"]}]}','[{"context_id":"3d4e5429c33881be263a7583407acd9548af2bd83cbe730ac8c8fb5e97d6f079","id":"7122929931712266240-control","override_id":"085e637459203733acb435d0bd921081b10a46a3eb192924a332e414bf266cf7","overrides":{"pmTestKey1":"value1-control","pmTestKey2":"value1-control"},"variant_type":"CONTROL"},{"context_id":"1a7b31bd94857f09edf0c69130af94bc70d830b5b8ebf021f48d1a04eeab7dfc","id":"7122929931712266240-test1","override_id":"65bcc9acaebd88355f850d7087e269f36d1316d7aeeddcf72cece682f99691c1","overrides":{"pmTestKey1":"value2-test","pmTestKey2":"value2-test"},"variant_type":"EXPERIMENTAL"}]','ritick.madaan@juspay.in','7122929931712266240-control')
3ba263f9-1f51-44f5-b8d3-ac961987265b	experiments	postgres	2024-08-21 22:46:30.317671	INSERT	\N	{"id":7155868988351938560,"created_at":"2024-01-24T10:28:36+05:30","created_by":"aman.jain@juspay.in","last_modified":"2024-01-24T10:28:36+05:30","name":"test","override_keys":["arya_bundle_etag","arya_bundle"],"status":"CREATED","traffic_percentage":0,"context":{"and":[{"==":[{"var":"clientId"},"zee5"]},{"==":[{"var":"os"},"android"]}]},"variants":[{"context_id":"890eab4596bf892aeddbe2f72b25e307b24f1d6afec0086613964495fd98b907","id":"7155868988351938560-test","override_id":"1d2a19ffd7496afabb57adf22114bf46568b5a1736328ea2b4e6c29fa3035ffa","overrides":{"arya_bundle":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/tatasky/android/release/config.json","arya_bundle_etag":"94c8b7e65db60ec831fe580b3f52390b"},"variant_type":"EXPERIMENTAL"},{"context_id":"b9f8e3841f87d308a062f713b39564fafcffc5dd359204ea24ea8432e18cce75","id":"7155868988351938560-control","override_id":"298c25615106b78712c4753c98daf991a0551dae7c5bf42e177729cd348aafbf","overrides":{"arya_bundle":"default_str_ignore_this","arya_bundle_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}],"last_modified_by":"aman.jain@juspay.in","chosen_variant":""}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7155868988351938560,'2024-01-24 10:28:36+05:30','aman.jain@juspay.in','2024-01-24 10:28:36+05:30','test','{arya_bundle_etag,arya_bundle}','CREATED',0,'{"and":[{"==":[{"var":"clientId"},"zee5"]},{"==":[{"var":"os"},"android"]}]}','[{"context_id":"890eab4596bf892aeddbe2f72b25e307b24f1d6afec0086613964495fd98b907","id":"7155868988351938560-test","override_id":"1d2a19ffd7496afabb57adf22114bf46568b5a1736328ea2b4e6c29fa3035ffa","overrides":{"arya_bundle":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/tatasky/android/release/config.json","arya_bundle_etag":"94c8b7e65db60ec831fe580b3f52390b"},"variant_type":"EXPERIMENTAL"},{"context_id":"b9f8e3841f87d308a062f713b39564fafcffc5dd359204ea24ea8432e18cce75","id":"7155868988351938560-control","override_id":"298c25615106b78712c4753c98daf991a0551dae7c5bf42e177729cd348aafbf","overrides":{"arya_bundle":"default_str_ignore_this","arya_bundle_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}]','aman.jain@juspay.in','')
2719d960-f46d-4837-ad51-522a2f9559ff	experiments	postgres	2024-08-21 22:46:30.316391	INSERT	\N	{"id":7143475879880847360,"created_at":"2023-12-21T05:42:49+05:30","created_by":"rohan.pawar@juspay.in","last_modified":"2023-12-26T14:27:17+05:30","name":"mobius test 3","override_keys":["hyperpay_configuration_etag","hyperpay_configuration"],"status":"CONCLUDED","traffic_percentage":5,"context":{"and":[{"==":[{"var":"clientId"},"galactustest"]},{"==":[{"var":"os"},"android"]}]},"variants":[{"context_id":"2f7025565529ba3ee88273450aa4f74130bd03f6437c9a219023e09f57543e16","id":"7143475879880847360-test","override_id":"7ca1e210b75248afda6ae04f074f0dcc8a8f729f897e9f9113a3009e087c01b7","overrides":{"hyperpay_configuration":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/magicbrickspp/configuration/1.1/v1-configuration.zip","hyperpay_configuration_etag":"5f00e94dc26e6d96a95e4a2888b42f4d"},"variant_type":"EXPERIMENTAL"},{"context_id":"cbef7694ccc7ed44922ca7505dd0057904113b1545ab16488407598ced783b17","id":"7143475879880847360-control","override_id":"afb1f8291942d3276f2259b285f7a021e7ffb2a6bb4038b21976e3d59d6abde8","overrides":{"hyperpay_configuration":"default_str_ignore_this","hyperpay_configuration_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}],"last_modified_by":"mobius@juspay.in","chosen_variant":"7143475879880847360-control"}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7143475879880847360,'2023-12-21 05:42:49+05:30','rohan.pawar@juspay.in','2023-12-26 14:27:17+05:30','mobius test 3','{hyperpay_configuration_etag,hyperpay_configuration}','CONCLUDED',5,'{"and":[{"==":[{"var":"clientId"},"galactustest"]},{"==":[{"var":"os"},"android"]}]}','[{"context_id":"2f7025565529ba3ee88273450aa4f74130bd03f6437c9a219023e09f57543e16","id":"7143475879880847360-test","override_id":"7ca1e210b75248afda6ae04f074f0dcc8a8f729f897e9f9113a3009e087c01b7","overrides":{"hyperpay_configuration":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/magicbrickspp/configuration/1.1/v1-configuration.zip","hyperpay_configuration_etag":"5f00e94dc26e6d96a95e4a2888b42f4d"},"variant_type":"EXPERIMENTAL"},{"context_id":"cbef7694ccc7ed44922ca7505dd0057904113b1545ab16488407598ced783b17","id":"7143475879880847360-control","override_id":"afb1f8291942d3276f2259b285f7a021e7ffb2a6bb4038b21976e3d59d6abde8","overrides":{"hyperpay_configuration":"default_str_ignore_this","hyperpay_configuration_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}]','mobius@juspay.in','7143475879880847360-control')
13e91ac8-f004-41db-b37e-2075a4246a40	experiments	postgres	2024-08-21 22:46:30.316664	INSERT	\N	{"id":7115685286169178112,"created_at":"2023-10-05T13:12:55+05:30","created_by":"ritick.madaan@juspay.in","last_modified":"2023-10-05T13:43:26+05:30","name":"test","override_keys":["godel_config","godel_config_etag"],"status":"CONCLUDED","traffic_percentage":10,"context":{"and":[{"==":[{"var":"os"},"android"]}]},"variants":[{"context_id":"5d19199f9d94bd9fba2b1e5eb519ee2c19d7c50e16f2c9d61823f5e70dc29bbe","id":"7115685286169178112-test","override_id":"4745da36fa0f7521d691393089bf2675db65684ce5e6fbca8e4cd829aca81398","overrides":{"godel_config":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/bbdaily/ios/release/config.json","godel_config_etag":"49413dd07bf851cc25df695b2a387c15"},"variant_type":"EXPERIMENTAL"},{"context_id":"decde2da037eac150ad8907aefe2edd9d5917ec3e7dfeb0edb7dba296cdc58f7","id":"7115685286169178112-control","override_id":"c41790e9f50f0660a2c7c96cd3e4c55e7d1ba02c76081f3dc02fa68f9e280fbf","overrides":{"godel_config":"default_str_ignore_this","godel_config_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}],"last_modified_by":"ritick.madaan@juspay.in","chosen_variant":"7115685286169178112-test"}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7115685286169178112,'2023-10-05 13:12:55+05:30','ritick.madaan@juspay.in','2023-10-05 13:43:26+05:30','test','{godel_config,godel_config_etag}','CONCLUDED',10,'{"and":[{"==":[{"var":"os"},"android"]}]}','[{"context_id":"5d19199f9d94bd9fba2b1e5eb519ee2c19d7c50e16f2c9d61823f5e70dc29bbe","id":"7115685286169178112-test","override_id":"4745da36fa0f7521d691393089bf2675db65684ce5e6fbca8e4cd829aca81398","overrides":{"godel_config":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/bbdaily/ios/release/config.json","godel_config_etag":"49413dd07bf851cc25df695b2a387c15"},"variant_type":"EXPERIMENTAL"},{"context_id":"decde2da037eac150ad8907aefe2edd9d5917ec3e7dfeb0edb7dba296cdc58f7","id":"7115685286169178112-control","override_id":"c41790e9f50f0660a2c7c96cd3e4c55e7d1ba02c76081f3dc02fa68f9e280fbf","overrides":{"godel_config":"default_str_ignore_this","godel_config_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}]','ritick.madaan@juspay.in','7115685286169178112-test')
b4eb583e-c4d6-4fdc-8adc-19163685c68c	experiments	postgres	2024-08-21 22:46:30.316936	INSERT	\N	{"id":7115695675500355584,"created_at":"2023-10-05T13:54:12+05:30","created_by":"ritick.madaan@juspay.in","last_modified":"2023-10-05T13:55:19+05:30","name":"test","override_keys":["godel_config","godel_config_etag"],"status":"CONCLUDED","traffic_percentage":0,"context":{"and":[{"==":[{"var":"os"},"android"]}]},"variants":[{"context_id":"d83e251a8662237c502fcfd06bda4bb24d39c4fad6293bac28a7046bbb479eee","id":"7115695675500355584-test","override_id":"3223b05a93511419d7d27083425099179d5d8422c745edfab685470786a94951","overrides":{"godel_config":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/trell/web/release/config.json","godel_config_etag":"c5ca9a450c7ed1012422bfae3978fc02"},"variant_type":"EXPERIMENTAL"},{"context_id":"432f6c2004d98f8b4af8a8e8087488e9739d0ce9903dd81ab2935ab74dfce0b6","id":"7115695675500355584-control","override_id":"c41790e9f50f0660a2c7c96cd3e4c55e7d1ba02c76081f3dc02fa68f9e280fbf","overrides":{"godel_config":"default_str_ignore_this","godel_config_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}],"last_modified_by":"ritick.madaan@juspay.in","chosen_variant":"7115695675500355584-control"}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7115695675500355584,'2023-10-05 13:54:12+05:30','ritick.madaan@juspay.in','2023-10-05 13:55:19+05:30','test','{godel_config,godel_config_etag}','CONCLUDED',0,'{"and":[{"==":[{"var":"os"},"android"]}]}','[{"context_id":"d83e251a8662237c502fcfd06bda4bb24d39c4fad6293bac28a7046bbb479eee","id":"7115695675500355584-test","override_id":"3223b05a93511419d7d27083425099179d5d8422c745edfab685470786a94951","overrides":{"godel_config":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/trell/web/release/config.json","godel_config_etag":"c5ca9a450c7ed1012422bfae3978fc02"},"variant_type":"EXPERIMENTAL"},{"context_id":"432f6c2004d98f8b4af8a8e8087488e9739d0ce9903dd81ab2935ab74dfce0b6","id":"7115695675500355584-control","override_id":"c41790e9f50f0660a2c7c96cd3e4c55e7d1ba02c76081f3dc02fa68f9e280fbf","overrides":{"godel_config":"default_str_ignore_this","godel_config_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}]','ritick.madaan@juspay.in','7115695675500355584-control')
e8258cb9-1c6c-4145-bb7f-4f3b1d6feb3e	experiments	postgres	2024-08-21 22:46:30.3172	INSERT	\N	{"id":7115701752172015616,"created_at":"2023-10-05T14:18:21+05:30","created_by":"ritick.madaan@juspay.in","last_modified":"2023-10-05T14:18:25+05:30","name":"asgsad","override_keys":["arya_root"],"status":"CONCLUDED","traffic_percentage":0,"context":{"and":[{"==":[{"var":"os"},"android"]}]},"variants":[{"context_id":"e6aa84b6a7de16c38333364094fa385a11067d6231447b898dd84b7834a21a2e","id":"7115701752172015616-test","override_id":"e1dbfce657117e9d784d15c24927cdfcfb8b76d5e1a3d8bfa215cf7e6533efb2","overrides":{"arya_root":"https://google.com"},"variant_type":"EXPERIMENTAL"},{"context_id":"05c195c94c9bb76a8c78ba316112af165a44858a59fc86e719d9e672bb883e00","id":"7115701752172015616-control","override_id":"ddb5ef62eea8b7777d3379b4c450ce7a14676104298df2c05af5a4a99745b3d1","overrides":{"arya_root":"default_str_ignore_this"},"variant_type":"CONTROL"}],"last_modified_by":"ritick.madaan@juspay.in","chosen_variant":"7115701752172015616-control"}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7115701752172015616,'2023-10-05 14:18:21+05:30','ritick.madaan@juspay.in','2023-10-05 14:18:25+05:30','asgsad','{arya_root}','CONCLUDED',0,'{"and":[{"==":[{"var":"os"},"android"]}]}','[{"context_id":"e6aa84b6a7de16c38333364094fa385a11067d6231447b898dd84b7834a21a2e","id":"7115701752172015616-test","override_id":"e1dbfce657117e9d784d15c24927cdfcfb8b76d5e1a3d8bfa215cf7e6533efb2","overrides":{"arya_root":"https://google.com"},"variant_type":"EXPERIMENTAL"},{"context_id":"05c195c94c9bb76a8c78ba316112af165a44858a59fc86e719d9e672bb883e00","id":"7115701752172015616-control","override_id":"ddb5ef62eea8b7777d3379b4c450ce7a14676104298df2c05af5a4a99745b3d1","overrides":{"arya_root":"default_str_ignore_this"},"variant_type":"CONTROL"}]','ritick.madaan@juspay.in','7115701752172015616-control')
512cfdf7-d745-4301-b663-777739838b50	experiments	postgres	2024-08-21 22:46:30.317427	INSERT	\N	{"id":7115728357986885632,"created_at":"2023-10-05T16:04:04+05:30","created_by":"ritick.madaan@juspay.in","last_modified":"2023-10-05T16:09:00+05:30","name":"Ritick","override_keys":["godel_config","godel_config_etag"],"status":"CONCLUDED","traffic_percentage":0,"context":{"and":[{"==":[{"var":"clientId"},"galactustest"]},{"==":[{"var":"os"},"android"]}]},"variants":[{"context_id":"328b07bd59b58649487af92434bad944be704c64b30c723c218a1b5967d61e61","id":"7115728357986885632-test","override_id":"4745da36fa0f7521d691393089bf2675db65684ce5e6fbca8e4cd829aca81398","overrides":{"godel_config":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/bbdaily/ios/release/config.json","godel_config_etag":"49413dd07bf851cc25df695b2a387c15"},"variant_type":"EXPERIMENTAL"},{"context_id":"771659ba0a62a76889376e48ab6cf3c21fff5fd5f4135ddcace0695c022e3a50","id":"7115728357986885632-control","override_id":"c41790e9f50f0660a2c7c96cd3e4c55e7d1ba02c76081f3dc02fa68f9e280fbf","overrides":{"godel_config":"default_str_ignore_this","godel_config_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}],"last_modified_by":"ritick.madaan@juspay.in","chosen_variant":"7115728357986885632-test"}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7115728357986885632,'2023-10-05 16:04:04+05:30','ritick.madaan@juspay.in','2023-10-05 16:09:00+05:30','Ritick','{godel_config,godel_config_etag}','CONCLUDED',0,'{"and":[{"==":[{"var":"clientId"},"galactustest"]},{"==":[{"var":"os"},"android"]}]}','[{"context_id":"328b07bd59b58649487af92434bad944be704c64b30c723c218a1b5967d61e61","id":"7115728357986885632-test","override_id":"4745da36fa0f7521d691393089bf2675db65684ce5e6fbca8e4cd829aca81398","overrides":{"godel_config":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/bbdaily/ios/release/config.json","godel_config_etag":"49413dd07bf851cc25df695b2a387c15"},"variant_type":"EXPERIMENTAL"},{"context_id":"771659ba0a62a76889376e48ab6cf3c21fff5fd5f4135ddcace0695c022e3a50","id":"7115728357986885632-control","override_id":"c41790e9f50f0660a2c7c96cd3e4c55e7d1ba02c76081f3dc02fa68f9e280fbf","overrides":{"godel_config":"default_str_ignore_this","godel_config_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}]','ritick.madaan@juspay.in','7115728357986885632-test')
176e9547-17d9-4bcb-9fc4-361c8d75ebfc	experiments	postgres	2024-08-21 22:46:30.318001	INSERT	\N	{"id":7117130589057921024,"created_at":"2023-10-09T12:56:02+05:30","created_by":"aman.jain@juspay.in","last_modified":"2023-10-09T12:57:13+05:30","name":"test124","override_keys":["godel_config","godel_config_etag"],"status":"CONCLUDED","traffic_percentage":10,"context":{"and":[{"==":[{"var":"clientId"},"galactustest"]},{"==":[{"var":"os"},"android"]}]},"variants":[{"context_id":"cfa190d1ddd9f9720d159bd70c33215dc7c7d47342992dcadb3f83bb8d3fe3a6","id":"7117130589057921024-test","override_id":"64ab3462b7633f5163ce42aed966469e2d11aea2e4a19ae24e78d9b7d76ec771","overrides":{"godel_config":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.46/v1-tracker.jsa","godel_config_etag":"a80e7758201c7d654ad9a70049ae36"},"variant_type":"EXPERIMENTAL"},{"context_id":"33c7bb9bc4e13ebccfb00e78ad0ac41b905c05917030dcbe176fd464e77cb07b","id":"7117130589057921024-control","override_id":"4745da36fa0f7521d691393089bf2675db65684ce5e6fbca8e4cd829aca81398","overrides":{"godel_config":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/bbdaily/ios/release/config.json","godel_config_etag":"49413dd07bf851cc25df695b2a387c15"},"variant_type":"CONTROL"}],"last_modified_by":"aman.jain@juspay.in","chosen_variant":"7117130589057921024-control"}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7117130589057921024,'2023-10-09 12:56:02+05:30','aman.jain@juspay.in','2023-10-09 12:57:13+05:30','test124','{godel_config,godel_config_etag}','CONCLUDED',10,'{"and":[{"==":[{"var":"clientId"},"galactustest"]},{"==":[{"var":"os"},"android"]}]}','[{"context_id":"cfa190d1ddd9f9720d159bd70c33215dc7c7d47342992dcadb3f83bb8d3fe3a6","id":"7117130589057921024-test","override_id":"64ab3462b7633f5163ce42aed966469e2d11aea2e4a19ae24e78d9b7d76ec771","overrides":{"godel_config":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.46/v1-tracker.jsa","godel_config_etag":"a80e7758201c7d654ad9a70049ae36"},"variant_type":"EXPERIMENTAL"},{"context_id":"33c7bb9bc4e13ebccfb00e78ad0ac41b905c05917030dcbe176fd464e77cb07b","id":"7117130589057921024-control","override_id":"4745da36fa0f7521d691393089bf2675db65684ce5e6fbca8e4cd829aca81398","overrides":{"godel_config":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/bbdaily/ios/release/config.json","godel_config_etag":"49413dd07bf851cc25df695b2a387c15"},"variant_type":"CONTROL"}]','aman.jain@juspay.in','7117130589057921024-control')
49d50213-3343-4de1-a68a-63659eb3f526	experiments	postgres	2024-08-21 22:46:30.318328	INSERT	\N	{"id":7117131074927673344,"created_at":"2023-10-09T12:57:58+05:30","created_by":"aman.jain@juspay.in","last_modified":"2023-10-09T12:58:17+05:30","name":"test124","override_keys":["godel_config","godel_config_etag"],"status":"CONCLUDED","traffic_percentage":0,"context":{"and":[{"==":[{"var":"clientId"},"galactustest"]},{"==":[{"var":"os"},"android"]}]},"variants":[{"context_id":"8577f9b299b463f78c0b2d2d0d41945284470d7a4535d17b8163e8f6d023ca59","id":"7117131074927673344-test","override_id":"64ab3462b7633f5163ce42aed966469e2d11aea2e4a19ae24e78d9b7d76ec771","overrides":{"godel_config":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.46/v1-tracker.jsa","godel_config_etag":"a80e7758201c7d654ad9a70049ae36"},"variant_type":"EXPERIMENTAL"},{"context_id":"712cd296fba53d461229e0b31f201189ca54adae529bdf06ebb97efd370dc48c","id":"7117131074927673344-control","override_id":"4745da36fa0f7521d691393089bf2675db65684ce5e6fbca8e4cd829aca81398","overrides":{"godel_config":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/bbdaily/ios/release/config.json","godel_config_etag":"49413dd07bf851cc25df695b2a387c15"},"variant_type":"CONTROL"}],"last_modified_by":"aman.jain@juspay.in","chosen_variant":"7117131074927673344-test"}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7117131074927673344,'2023-10-09 12:57:58+05:30','aman.jain@juspay.in','2023-10-09 12:58:17+05:30','test124','{godel_config,godel_config_etag}','CONCLUDED',0,'{"and":[{"==":[{"var":"clientId"},"galactustest"]},{"==":[{"var":"os"},"android"]}]}','[{"context_id":"8577f9b299b463f78c0b2d2d0d41945284470d7a4535d17b8163e8f6d023ca59","id":"7117131074927673344-test","override_id":"64ab3462b7633f5163ce42aed966469e2d11aea2e4a19ae24e78d9b7d76ec771","overrides":{"godel_config":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.46/v1-tracker.jsa","godel_config_etag":"a80e7758201c7d654ad9a70049ae36"},"variant_type":"EXPERIMENTAL"},{"context_id":"712cd296fba53d461229e0b31f201189ca54adae529bdf06ebb97efd370dc48c","id":"7117131074927673344-control","override_id":"4745da36fa0f7521d691393089bf2675db65684ce5e6fbca8e4cd829aca81398","overrides":{"godel_config":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/bbdaily/ios/release/config.json","godel_config_etag":"49413dd07bf851cc25df695b2a387c15"},"variant_type":"CONTROL"}]','aman.jain@juspay.in','7117131074927673344-test')
317a467a-2520-4314-9255-1b488539e639	experiments	postgres	2024-08-21 22:46:30.318753	INSERT	\N	{"id":7155869386936647680,"created_at":"2024-01-24T10:30:11+05:30","created_by":"aman.jain@juspay.in","last_modified":"2024-01-24T10:30:11+05:30","name":"adadwd","override_keys":["arya_bundle_etag","arya_bundle"],"status":"CREATED","traffic_percentage":0,"context":{"and":[{"==":[{"var":"clientId"},"zee5"]},{"==":[{"var":"tier"},"t1"]},{"==":[{"var":"os"},"android"]},{"==":[{"var":"track"},"f1"]}]},"variants":[{"context_id":"70f7e361bd73d1a9cf66cdbd669ee367df6874ba250bfb444aead8977b8ac8cf","id":"7155869386936647680-test","override_id":"1d2a19ffd7496afabb57adf22114bf46568b5a1736328ea2b4e6c29fa3035ffa","overrides":{"arya_bundle":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/tatasky/android/release/config.json","arya_bundle_etag":"94c8b7e65db60ec831fe580b3f52390b"},"variant_type":"EXPERIMENTAL"},{"context_id":"772c7ad1913cd5f18b68267c8823b2ee28314048cd476202a348d2bbd0b71a4f","id":"7155869386936647680-control","override_id":"298c25615106b78712c4753c98daf991a0551dae7c5bf42e177729cd348aafbf","overrides":{"arya_bundle":"default_str_ignore_this","arya_bundle_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}],"last_modified_by":"aman.jain@juspay.in","chosen_variant":""}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7155869386936647680,'2024-01-24 10:30:11+05:30','aman.jain@juspay.in','2024-01-24 10:30:11+05:30','adadwd','{arya_bundle_etag,arya_bundle}','CREATED',0,'{"and":[{"==":[{"var":"clientId"},"zee5"]},{"==":[{"var":"tier"},"t1"]},{"==":[{"var":"os"},"android"]},{"==":[{"var":"track"},"f1"]}]}','[{"context_id":"70f7e361bd73d1a9cf66cdbd669ee367df6874ba250bfb444aead8977b8ac8cf","id":"7155869386936647680-test","override_id":"1d2a19ffd7496afabb57adf22114bf46568b5a1736328ea2b4e6c29fa3035ffa","overrides":{"arya_bundle":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/tatasky/android/release/config.json","arya_bundle_etag":"94c8b7e65db60ec831fe580b3f52390b"},"variant_type":"EXPERIMENTAL"},{"context_id":"772c7ad1913cd5f18b68267c8823b2ee28314048cd476202a348d2bbd0b71a4f","id":"7155869386936647680-control","override_id":"298c25615106b78712c4753c98daf991a0551dae7c5bf42e177729cd348aafbf","overrides":{"arya_bundle":"default_str_ignore_this","arya_bundle_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}]','aman.jain@juspay.in','')
86e6090a-a7f3-4d4b-9758-eddc4a5b35b7	experiments	postgres	2024-08-21 22:46:30.319211	INSERT	\N	{"id":7119946958782308352,"created_at":"2023-10-17T07:27:17+05:30","created_by":"aman.jain@juspay.in","last_modified":"2023-11-22T09:31:40+05:30","name":"dawd","override_keys":["arya_bundle","arya_bundle_etag"],"status":"CONCLUDED","traffic_percentage":0,"context":{"and":[{"==":[{"var":"os"},"android"]}]},"variants":[{"context_id":"b2cb511720c0bd3463d06fa32d638eeece42f2121827dbaeede7198ea9fe9835","id":"7119946958782308352-test","override_id":"8ea04458d6c10aae8233c100c888f8fda9232d4e993076d6d2cedba6a976df86","overrides":{"arya_bundle":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/tatasky/android/release/config.json","arya_bundle_etag":"6e4c0221f46858983520bf57008994d0"},"variant_type":"EXPERIMENTAL"},{"context_id":"010c36be24d11030d5c70aa617deb333825f68cba6f7c47bfcc85fcfcb0a2f47","id":"7119946958782308352-control","override_id":"f27a63e7518f70f840d57b4545a4cddde709bb3340d440fe21f584d5820710c5","overrides":{"arya_bundle":"default_str_ignore_this","arya_bundle_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}],"last_modified_by":"ritick.madaan@juspay.in","chosen_variant":"7119946958782308352-control"}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7119946958782308352,'2023-10-17 07:27:17+05:30','aman.jain@juspay.in','2023-11-22 09:31:40+05:30','dawd','{arya_bundle,arya_bundle_etag}','CONCLUDED',0,'{"and":[{"==":[{"var":"os"},"android"]}]}','[{"context_id":"b2cb511720c0bd3463d06fa32d638eeece42f2121827dbaeede7198ea9fe9835","id":"7119946958782308352-test","override_id":"8ea04458d6c10aae8233c100c888f8fda9232d4e993076d6d2cedba6a976df86","overrides":{"arya_bundle":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/tatasky/android/release/config.json","arya_bundle_etag":"6e4c0221f46858983520bf57008994d0"},"variant_type":"EXPERIMENTAL"},{"context_id":"010c36be24d11030d5c70aa617deb333825f68cba6f7c47bfcc85fcfcb0a2f47","id":"7119946958782308352-control","override_id":"f27a63e7518f70f840d57b4545a4cddde709bb3340d440fe21f584d5820710c5","overrides":{"arya_bundle":"default_str_ignore_this","arya_bundle_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}]','ritick.madaan@juspay.in','7119946958782308352-control')
a6c36091-ed48-4e78-be7a-2bd9d16d53fc	experiments	postgres	2024-08-21 22:46:30.319559	INSERT	\N	{"id":7117777147781951488,"created_at":"2023-10-11T07:45:13+05:30","created_by":"aman.jain@juspay.in","last_modified":"2023-11-22T09:31:55+05:30","name":"wdadad","override_keys":["arya_bundle","arya_bundle_etag"],"status":"CONCLUDED","traffic_percentage":0,"context":{"and":[{"==":[{"var":"os"},"android"]}]},"variants":[{"context_id":"fa53907e5710dcff8f793947868d3399767c1511afaa4742c1ad3a473a4a3b2b","id":"7117777147781951488-test","override_id":"fd704e6c2955c45b7c24602cebf461f7dfc4ec386a5e18247602af5d30086f30","overrides":{"arya_bundle":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/tatasky/android/release/config.json","arya_bundle_etag":"8eaf6b332b19bfa8684a125a0956dd84"},"variant_type":"EXPERIMENTAL"},{"context_id":"91aa3f53ec6c269a3d9e4311aa3634dbeb5b74af19f0d6f265163630914d9e97","id":"7117777147781951488-control","override_id":"f27a63e7518f70f840d57b4545a4cddde709bb3340d440fe21f584d5820710c5","overrides":{"arya_bundle":"default_str_ignore_this","arya_bundle_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}],"last_modified_by":"ritick.madaan@juspay.in","chosen_variant":"7117777147781951488-test"}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7117777147781951488,'2023-10-11 07:45:13+05:30','aman.jain@juspay.in','2023-11-22 09:31:55+05:30','wdadad','{arya_bundle,arya_bundle_etag}','CONCLUDED',0,'{"and":[{"==":[{"var":"os"},"android"]}]}','[{"context_id":"fa53907e5710dcff8f793947868d3399767c1511afaa4742c1ad3a473a4a3b2b","id":"7117777147781951488-test","override_id":"fd704e6c2955c45b7c24602cebf461f7dfc4ec386a5e18247602af5d30086f30","overrides":{"arya_bundle":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/tatasky/android/release/config.json","arya_bundle_etag":"8eaf6b332b19bfa8684a125a0956dd84"},"variant_type":"EXPERIMENTAL"},{"context_id":"91aa3f53ec6c269a3d9e4311aa3634dbeb5b74af19f0d6f265163630914d9e97","id":"7117777147781951488-control","override_id":"f27a63e7518f70f840d57b4545a4cddde709bb3340d440fe21f584d5820710c5","overrides":{"arya_bundle":"default_str_ignore_this","arya_bundle_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}]','ritick.madaan@juspay.in','7117777147781951488-test')
2dc220fc-11f6-4d41-8120-19d80a302702	experiments	postgres	2024-08-21 22:46:30.319847	INSERT	\N	{"id":7119946863389589504,"created_at":"2023-10-17T07:26:54+05:30","created_by":"aman.jain@juspay.in","last_modified":"2023-11-22T09:32:12+05:30","name":"qwertyui","override_keys":["godel_acs","godel_acs_etag"],"status":"CONCLUDED","traffic_percentage":0,"context":{"and":[{"==":[{"var":"os"},"android"]}]},"variants":[{"context_id":"eb53455e094812d911d219cd33d332e3217739cc9df7bde87757848de98b68cf","id":"7119946863389589504-test","override_id":"8f20a6fb87c619aacee503ddccf367be4b9a4195f6a9ae7dfa84ec455e060be5","overrides":{"godel_acs":"https://assets.juspay.in/hyper/bundles/acs/in.juspay.godel/1.2.112/v1-acs.zip","godel_acs_etag":"1fe12290c99844bade11626c391092"},"variant_type":"EXPERIMENTAL"},{"context_id":"bc2c177c4c136e3283efeea7d607f57bfbe860175ad26210c3d1c5697ceebf8e","id":"7119946863389589504-control","override_id":"3b0c2f917c497aa84f5ffd6ac18f6f65869e39325284839be7af9359a79efa75","overrides":{"godel_acs":"default_str_ignore_this","godel_acs_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}],"last_modified_by":"ritick.madaan@juspay.in","chosen_variant":"7119946863389589504-control"}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7119946863389589504,'2023-10-17 07:26:54+05:30','aman.jain@juspay.in','2023-11-22 09:32:12+05:30','qwertyui','{godel_acs,godel_acs_etag}','CONCLUDED',0,'{"and":[{"==":[{"var":"os"},"android"]}]}','[{"context_id":"eb53455e094812d911d219cd33d332e3217739cc9df7bde87757848de98b68cf","id":"7119946863389589504-test","override_id":"8f20a6fb87c619aacee503ddccf367be4b9a4195f6a9ae7dfa84ec455e060be5","overrides":{"godel_acs":"https://assets.juspay.in/hyper/bundles/acs/in.juspay.godel/1.2.112/v1-acs.zip","godel_acs_etag":"1fe12290c99844bade11626c391092"},"variant_type":"EXPERIMENTAL"},{"context_id":"bc2c177c4c136e3283efeea7d607f57bfbe860175ad26210c3d1c5697ceebf8e","id":"7119946863389589504-control","override_id":"3b0c2f917c497aa84f5ffd6ac18f6f65869e39325284839be7af9359a79efa75","overrides":{"godel_acs":"default_str_ignore_this","godel_acs_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}]','ritick.madaan@juspay.in','7119946863389589504-control')
16fafc37-e684-4016-b762-441597e3a40b	experiments	postgres	2024-08-21 22:46:30.327527	INSERT	\N	{"id":7145699356366950400,"created_at":"2023-12-27T08:58:07+05:30","created_by":"rohan.pawar@juspay.in","last_modified":"2024-01-01T14:09:52+05:30","name":"mobius test 3","override_keys":["hyperpay_configuration","hyperpay_configuration_etag"],"status":"CONCLUDED","traffic_percentage":50,"context":{"and":[{"==":[{"var":"clientId"},"galactustest"]},{"==":[{"var":"os"},"android"]}]},"variants":[{"context_id":"1e664cce4dbe6b662decaad992f1a074b756aa10bcc5080d0d648d48c0d76fa9","id":"7145699356366950400-test","override_id":"7ca1e210b75248afda6ae04f074f0dcc8a8f729f897e9f9113a3009e087c01b7","overrides":{"hyperpay_configuration":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/magicbrickspp/configuration/1.1/v1-configuration.zip","hyperpay_configuration_etag":"5f00e94dc26e6d96a95e4a2888b42f4d"},"variant_type":"EXPERIMENTAL"},{"context_id":"21052b5a5fb13110fa7479e824ef505ae8f1738eecf764543f47291d6d7948a3","id":"7145699356366950400-control","override_id":"afb1f8291942d3276f2259b285f7a021e7ffb2a6bb4038b21976e3d59d6abde8","overrides":{"hyperpay_configuration":"default_str_ignore_this","hyperpay_configuration_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}],"last_modified_by":"mobius@juspay.in","chosen_variant":"7145699356366950400-test"}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7145699356366950400,'2023-12-27 08:58:07+05:30','rohan.pawar@juspay.in','2024-01-01 14:09:52+05:30','mobius test 3','{hyperpay_configuration,hyperpay_configuration_etag}','CONCLUDED',50,'{"and":[{"==":[{"var":"clientId"},"galactustest"]},{"==":[{"var":"os"},"android"]}]}','[{"context_id":"1e664cce4dbe6b662decaad992f1a074b756aa10bcc5080d0d648d48c0d76fa9","id":"7145699356366950400-test","override_id":"7ca1e210b75248afda6ae04f074f0dcc8a8f729f897e9f9113a3009e087c01b7","overrides":{"hyperpay_configuration":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/magicbrickspp/configuration/1.1/v1-configuration.zip","hyperpay_configuration_etag":"5f00e94dc26e6d96a95e4a2888b42f4d"},"variant_type":"EXPERIMENTAL"},{"context_id":"21052b5a5fb13110fa7479e824ef505ae8f1738eecf764543f47291d6d7948a3","id":"7145699356366950400-control","override_id":"afb1f8291942d3276f2259b285f7a021e7ffb2a6bb4038b21976e3d59d6abde8","overrides":{"hyperpay_configuration":"default_str_ignore_this","hyperpay_configuration_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}]','mobius@juspay.in','7145699356366950400-test')
fe52b484-d71f-494e-b6c5-e007abfeebbd	experiments	postgres	2024-08-21 22:46:30.32014	INSERT	\N	{"id":7115731532762300416,"created_at":"2023-10-05T16:16:41+05:30","created_by":"ritick.madaan@juspay.in","last_modified":"2023-10-25T13:25:13+05:30","name":"demo","override_keys":["godel_config","godel_config_etag"],"status":"CONCLUDED","traffic_percentage":0,"context":{"and":[{"==":[{"var":"clientId"},"galactustest"]},{"==":[{"var":"os"},"android"]}]},"variants":[{"context_id":"4d9910251fd556e00163f2fc6166ba7b23d3576d92fadf1ba535ba0b7f078ebc","id":"7115731532762300416-test","override_id":"4745da36fa0f7521d691393089bf2675db65684ce5e6fbca8e4cd829aca81398","overrides":{"godel_config":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/bbdaily/ios/release/config.json","godel_config_etag":"49413dd07bf851cc25df695b2a387c15"},"variant_type":"EXPERIMENTAL"},{"context_id":"a6d7a86680d9786bf2775c8ea49c68b49d9e66f3df3412ea91bd1d577adc42b3","id":"7115731532762300416-control","override_id":"c41790e9f50f0660a2c7c96cd3e4c55e7d1ba02c76081f3dc02fa68f9e280fbf","overrides":{"godel_config":"default_str_ignore_this","godel_config_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}],"last_modified_by":"ritick.madaan@juspay.in","chosen_variant":"7115731532762300416-control"}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7115731532762300416,'2023-10-05 16:16:41+05:30','ritick.madaan@juspay.in','2023-10-25 13:25:13+05:30','demo','{godel_config,godel_config_etag}','CONCLUDED',0,'{"and":[{"==":[{"var":"clientId"},"galactustest"]},{"==":[{"var":"os"},"android"]}]}','[{"context_id":"4d9910251fd556e00163f2fc6166ba7b23d3576d92fadf1ba535ba0b7f078ebc","id":"7115731532762300416-test","override_id":"4745da36fa0f7521d691393089bf2675db65684ce5e6fbca8e4cd829aca81398","overrides":{"godel_config":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/bbdaily/ios/release/config.json","godel_config_etag":"49413dd07bf851cc25df695b2a387c15"},"variant_type":"EXPERIMENTAL"},{"context_id":"a6d7a86680d9786bf2775c8ea49c68b49d9e66f3df3412ea91bd1d577adc42b3","id":"7115731532762300416-control","override_id":"c41790e9f50f0660a2c7c96cd3e4c55e7d1ba02c76081f3dc02fa68f9e280fbf","overrides":{"godel_config":"default_str_ignore_this","godel_config_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}]','ritick.madaan@juspay.in','7115731532762300416-control')
1a8d79a3-676c-4a86-b5f8-f89d25835b52	experiments	postgres	2024-08-21 22:46:30.320438	INSERT	\N	{"id":7120300789818368000,"created_at":"2023-10-18T06:53:17+05:30","created_by":"aman.jain@juspay.in","last_modified":"2023-10-31T08:03:43+05:30","name":"test5","override_keys":["arya_bundle","godel_bundle","arya_bundle_etag","godel_bundle_etag"],"status":"CONCLUDED","traffic_percentage":0,"context":{"and":[{"==":[{"var":"os"},"android"]}]},"variants":[{"context_id":"fa7932cec2a2919dc950109e9400f1b7eefffd0b608bbda5455e96b63e67388a","id":"7120300789818368000-test","override_id":"e5ce1c03bb1d2a500b3dde34a3a135c86c7ee1798c14c378a3aad27cd72cfb60","overrides":{"arya_bundle":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/tatasky/android/release/config.json","arya_bundle_etag":"6e4c0221f46858983520bf57008994d0","godel_bundle":"https://assets.juspay.in/juspay/payments/in.juspay.dotp/release/v1-config.zip","godel_bundle_etag":"d1be46cb32c6ae215f492b0b963486b7"},"variant_type":"EXPERIMENTAL"},{"context_id":"043927f6216617dc6ce25ca0dde84f935fc0bb3d47937ace2ac3e902705c81f2","id":"7120300789818368000-control","override_id":"5c32fc6689590dd01a80c9c51cade1967de3080e6ac8ff5d9debbf752c7d31bb","overrides":{"arya_bundle":"default_str_ignore_this","arya_bundle_etag":"default_str_ignore_this","godel_bundle":"default_str_ignore_this","godel_bundle_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}],"last_modified_by":"bhawesh.agarwal@juspay.in","chosen_variant":"7120300789818368000-control"}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7120300789818368000,'2023-10-18 06:53:17+05:30','aman.jain@juspay.in','2023-10-31 08:03:43+05:30','test5','{arya_bundle,godel_bundle,arya_bundle_etag,godel_bundle_etag}','CONCLUDED',0,'{"and":[{"==":[{"var":"os"},"android"]}]}','[{"context_id":"fa7932cec2a2919dc950109e9400f1b7eefffd0b608bbda5455e96b63e67388a","id":"7120300789818368000-test","override_id":"e5ce1c03bb1d2a500b3dde34a3a135c86c7ee1798c14c378a3aad27cd72cfb60","overrides":{"arya_bundle":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/tatasky/android/release/config.json","arya_bundle_etag":"6e4c0221f46858983520bf57008994d0","godel_bundle":"https://assets.juspay.in/juspay/payments/in.juspay.dotp/release/v1-config.zip","godel_bundle_etag":"d1be46cb32c6ae215f492b0b963486b7"},"variant_type":"EXPERIMENTAL"},{"context_id":"043927f6216617dc6ce25ca0dde84f935fc0bb3d47937ace2ac3e902705c81f2","id":"7120300789818368000-control","override_id":"5c32fc6689590dd01a80c9c51cade1967de3080e6ac8ff5d9debbf752c7d31bb","overrides":{"arya_bundle":"default_str_ignore_this","arya_bundle_etag":"default_str_ignore_this","godel_bundle":"default_str_ignore_this","godel_bundle_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}]','bhawesh.agarwal@juspay.in','7120300789818368000-control')
a5d16b33-4d82-4948-9721-1c5511ad51e5	experiments	postgres	2024-08-21 22:46:30.32074	INSERT	\N	{"id":7188493072320155648,"created_at":"2024-04-23T11:05:04+05:30","created_by":"shubhranshu.sanjeev@juspay.in","last_modified":"2024-04-23T11:05:11+05:30","name":"hsbc sbx unified loader","override_keys":["hyperos_placeholder_loader"],"status":"CONCLUDED","traffic_percentage":0,"context":{"and":[{"==":[{"var":"clientId"},"testhsbc"]},{"==":[{"var":"os"},"web"]}]},"variants":[{"context_id":"9f6d73797bf206e037fed942bf1f8553a7dfe641ed661b5fe289f5e6844d80c3","id":"7188493072320155648-control","override_id":"8768472bfbb5a3783bd6e2d7c0c3e332fc1e4f3039ced882ca57d473204c9916","overrides":{"hyperos_placeholder_loader":"https://assets.juspay.in/hyper/bundles/app/loader/hsbc/1.0.4/web/loader.js"},"variant_type":"CONTROL"},{"context_id":"2bc11735f1d1d74bfc002a2a40ff391c9a2f0195197751e3dd8ea5b9f2f743bc","id":"7188493072320155648-experimental","override_id":"8768472bfbb5a3783bd6e2d7c0c3e332fc1e4f3039ced882ca57d473204c9916","overrides":{"hyperos_placeholder_loader":"https://assets.juspay.in/hyper/bundles/app/loader/hsbc/1.0.4/web/loader.js"},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"shubhranshu.sanjeev@juspay.in","chosen_variant":"7188493072320155648-experimental"}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7188493072320155648,'2024-04-23 11:05:04+05:30','shubhranshu.sanjeev@juspay.in','2024-04-23 11:05:11+05:30','hsbc sbx unified loader','{hyperos_placeholder_loader}','CONCLUDED',0,'{"and":[{"==":[{"var":"clientId"},"testhsbc"]},{"==":[{"var":"os"},"web"]}]}','[{"context_id":"9f6d73797bf206e037fed942bf1f8553a7dfe641ed661b5fe289f5e6844d80c3","id":"7188493072320155648-control","override_id":"8768472bfbb5a3783bd6e2d7c0c3e332fc1e4f3039ced882ca57d473204c9916","overrides":{"hyperos_placeholder_loader":"https://assets.juspay.in/hyper/bundles/app/loader/hsbc/1.0.4/web/loader.js"},"variant_type":"CONTROL"},{"context_id":"2bc11735f1d1d74bfc002a2a40ff391c9a2f0195197751e3dd8ea5b9f2f743bc","id":"7188493072320155648-experimental","override_id":"8768472bfbb5a3783bd6e2d7c0c3e332fc1e4f3039ced882ca57d473204c9916","overrides":{"hyperos_placeholder_loader":"https://assets.juspay.in/hyper/bundles/app/loader/hsbc/1.0.4/web/loader.js"},"variant_type":"EXPERIMENTAL"}]','shubhranshu.sanjeev@juspay.in','7188493072320155648-experimental')
f945cbf6-3700-43bd-865d-6f1f61e43e56	experiments	postgres	2024-08-21 22:46:30.321028	INSERT	\N	{"id":7188571544521814016,"created_at":"2024-04-23T16:16:53+05:30","created_by":"shubhranshu.sanjeev@juspay.in","last_modified":"2024-04-23T16:16:53+05:30","name":"test_experiment_with_multiple_clientids","override_keys":["pmTestKey1"],"status":"CREATED","traffic_percentage":0,"context":{"IN":[{"var":"clientId"},"galactustest,zee5"]},"variants":[{"context_id":"ab56204accbea23430e6e1ff1d80065772e52805db222f3d10d45e893c7958c4","id":"7188571544521814016-control","override_id":"7ead5b38c5401a989e990ccccdb01f7cd27017aee111160aee5c452bea6a6592","overrides":{"pmTestKey1":"abc"},"variant_type":"CONTROL"},{"context_id":"73f05457e20fab7000c44a3c58a4fca66819b9c93b7ae447fba936813d1c05ae","id":"7188571544521814016-experimental","override_id":"db62c9ec706fd492b89eee9400d0237c2144a908503f07fdf07bf94b9aad1ea2","overrides":{"pmTestKey1":"zxc"},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"shubhranshu.sanjeev@juspay.in","chosen_variant":""}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7188571544521814016,'2024-04-23 16:16:53+05:30','shubhranshu.sanjeev@juspay.in','2024-04-23 16:16:53+05:30','test_experiment_with_multiple_clientids','{pmTestKey1}','CREATED',0,'{"IN":[{"var":"clientId"},"galactustest,zee5"]}','[{"context_id":"ab56204accbea23430e6e1ff1d80065772e52805db222f3d10d45e893c7958c4","id":"7188571544521814016-control","override_id":"7ead5b38c5401a989e990ccccdb01f7cd27017aee111160aee5c452bea6a6592","overrides":{"pmTestKey1":"abc"},"variant_type":"CONTROL"},{"context_id":"73f05457e20fab7000c44a3c58a4fca66819b9c93b7ae447fba936813d1c05ae","id":"7188571544521814016-experimental","override_id":"db62c9ec706fd492b89eee9400d0237c2144a908503f07fdf07bf94b9aad1ea2","overrides":{"pmTestKey1":"zxc"},"variant_type":"EXPERIMENTAL"}]','shubhranshu.sanjeev@juspay.in','')
adc666ca-79be-423b-836a-12c742bd640a	experiments	postgres	2024-08-21 22:46:30.321292	INSERT	\N	{"id":7125099791835406336,"created_at":"2023-10-31T12:42:48+05:30","created_by":"kartik.gajendra@juspay.in","last_modified":"2023-11-22T09:31:03+05:30","name":"galactustest-experiment","override_keys":["godel_config"],"status":"CONCLUDED","traffic_percentage":0,"context":{"and":[{"==":[{"var":"os"},"android"]},{"==":[{"var":"clientId"},"galactustest"]}]},"variants":[{"context_id":"b9304183b5585116196c5aea6aa279990c52387d8ab10e76b73f54c1c7e2e91f","id":"7125099791835406336-control","override_id":"dece633f19bfbce7315e7dae457a58485af60f8d482270d74553801ab5e1d59e","overrides":{"godel_config":"abc"},"variant_type":"CONTROL"},{"context_id":"e22dbb26e053cba6963d36382fedfa5a2d6b53cba610d350ddaffa5444ec094c","id":"7125099791835406336-test1","override_id":"7e0ea3dabfdf0e5f03c85015c4cc62a56a46d04b5d00e1d7dfd17aaed635184e","overrides":{"godel_config":"godel_experimental"},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"ritick.madaan@juspay.in","chosen_variant":"7125099791835406336-control"}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7125099791835406336,'2023-10-31 12:42:48+05:30','kartik.gajendra@juspay.in','2023-11-22 09:31:03+05:30','galactustest-experiment','{godel_config}','CONCLUDED',0,'{"and":[{"==":[{"var":"os"},"android"]},{"==":[{"var":"clientId"},"galactustest"]}]}','[{"context_id":"b9304183b5585116196c5aea6aa279990c52387d8ab10e76b73f54c1c7e2e91f","id":"7125099791835406336-control","override_id":"dece633f19bfbce7315e7dae457a58485af60f8d482270d74553801ab5e1d59e","overrides":{"godel_config":"abc"},"variant_type":"CONTROL"},{"context_id":"e22dbb26e053cba6963d36382fedfa5a2d6b53cba610d350ddaffa5444ec094c","id":"7125099791835406336-test1","override_id":"7e0ea3dabfdf0e5f03c85015c4cc62a56a46d04b5d00e1d7dfd17aaed635184e","overrides":{"godel_config":"godel_experimental"},"variant_type":"EXPERIMENTAL"}]','ritick.madaan@juspay.in','7125099791835406336-control')
63e184a1-3b16-4c18-9dc0-3312350bef70	experiments	postgres	2024-08-21 22:46:30.321573	INSERT	\N	{"id":7119952192400715776,"created_at":"2023-10-17T07:48:05+05:30","created_by":"aman.jain@juspay.in","last_modified":"2023-11-22T09:31:14+05:30","name":"test3","override_keys":["godel_bundle","godel_bundle_etag"],"status":"CONCLUDED","traffic_percentage":0,"context":{"and":[{"==":[{"var":"clientId"},"zepto"]},{"==":[{"var":"os"},"android"]}]},"variants":[{"context_id":"7eb21477bc6a72cbdb7e9fec90d8ccb33c50ec6825419cb2357adc58c6663c9a","id":"7119952192400715776-test","override_id":"17017d6faf68fcd0790e166da1a5a4e858c81863042a87e0f3776c3fac284c7c","overrides":{"godel_bundle":"https://assets.juspay.in/juspay/payments/in.juspay.ec/release/v1-config.zip","godel_bundle_etag":"01ec08875f5d676ef172339e7d3f9b8f"},"variant_type":"EXPERIMENTAL"},{"context_id":"e49eef27a7536f03a7fe534912f43e4f7d28a473a684dc0f59b5567ca87a1c58","id":"7119952192400715776-control","override_id":"a811d64f4dc78535ff5a8a8ecf114089563a07e40a2311c44c8e99ce373eae20","overrides":{"godel_bundle":"default_str_ignore_this","godel_bundle_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}],"last_modified_by":"ritick.madaan@juspay.in","chosen_variant":"7119952192400715776-control"}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7119952192400715776,'2023-10-17 07:48:05+05:30','aman.jain@juspay.in','2023-11-22 09:31:14+05:30','test3','{godel_bundle,godel_bundle_etag}','CONCLUDED',0,'{"and":[{"==":[{"var":"clientId"},"zepto"]},{"==":[{"var":"os"},"android"]}]}','[{"context_id":"7eb21477bc6a72cbdb7e9fec90d8ccb33c50ec6825419cb2357adc58c6663c9a","id":"7119952192400715776-test","override_id":"17017d6faf68fcd0790e166da1a5a4e858c81863042a87e0f3776c3fac284c7c","overrides":{"godel_bundle":"https://assets.juspay.in/juspay/payments/in.juspay.ec/release/v1-config.zip","godel_bundle_etag":"01ec08875f5d676ef172339e7d3f9b8f"},"variant_type":"EXPERIMENTAL"},{"context_id":"e49eef27a7536f03a7fe534912f43e4f7d28a473a684dc0f59b5567ca87a1c58","id":"7119952192400715776-control","override_id":"a811d64f4dc78535ff5a8a8ecf114089563a07e40a2311c44c8e99ce373eae20","overrides":{"godel_bundle":"default_str_ignore_this","godel_bundle_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}]','ritick.madaan@juspay.in','7119952192400715776-control')
7d7d3bb0-610b-4d5e-a744-ea4fb920a0f9	experiments	postgres	2024-08-21 22:46:30.322012	INSERT	\N	{"id":7133465574847229952,"created_at":"2023-11-23T14:45:26+05:30","created_by":"mobius@juspay.in","last_modified":"2023-11-29T09:47:01+05:30","name":"experiment-1","override_keys":["pmTestKey1","pmTestKey2"],"status":"CONCLUDED","traffic_percentage":10,"context":{"and":[{"==":[{"var":"os"},"ios"]},{"==":[{"var":"client"},"testClientCac1"]}]},"variants":[{"context_id":"6923a97c98b435527b7a09a8a4fac353c89f5d450875b9813888c972d8149cb8","id":"7133465574847229952-control","override_id":"2123fadefc2e34d386e7ebb8851ab290ac4b2fad87e353745e3387025ffa81ff","overrides":{"pmTestKey1":"value1-control","pmTestKey2":"value1-control"},"variant_type":"CONTROL"},{"context_id":"829d5797818f5f8a67da6792317de3cc4c871acb82663528a5e56f33645a40af","id":"7133465574847229952-test1","override_id":"67d10870bc1289e3e0544fe29aa878b470344a319ed217d78cba21ff5c622db1","overrides":{"pmTestKey1":"value2-test","pmTestKey2":"value2-test"},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"mobius@juspay.in","chosen_variant":"7133465574847229952-test1"}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7133465574847229952,'2023-11-23 14:45:26+05:30','mobius@juspay.in','2023-11-29 09:47:01+05:30','experiment-1','{pmTestKey1,pmTestKey2}','CONCLUDED',10,'{"and":[{"==":[{"var":"os"},"ios"]},{"==":[{"var":"client"},"testClientCac1"]}]}','[{"context_id":"6923a97c98b435527b7a09a8a4fac353c89f5d450875b9813888c972d8149cb8","id":"7133465574847229952-control","override_id":"2123fadefc2e34d386e7ebb8851ab290ac4b2fad87e353745e3387025ffa81ff","overrides":{"pmTestKey1":"value1-control","pmTestKey2":"value1-control"},"variant_type":"CONTROL"},{"context_id":"829d5797818f5f8a67da6792317de3cc4c871acb82663528a5e56f33645a40af","id":"7133465574847229952-test1","override_id":"67d10870bc1289e3e0544fe29aa878b470344a319ed217d78cba21ff5c622db1","overrides":{"pmTestKey1":"value2-test","pmTestKey2":"value2-test"},"variant_type":"EXPERIMENTAL"}]','mobius@juspay.in','7133465574847229952-test1')
05d5d591-dc9d-4a69-96b4-e8cd469e28f6	experiments	postgres	2024-08-21 22:46:30.322319	INSERT	\N	{"id":7125136499787333632,"created_at":"2023-10-31T15:08:40+05:30","created_by":"ritick.madaan@juspay.in","last_modified":"2023-10-31T15:12:07+05:30","name":"galactustest test","override_keys":["godel_config","godel_config_etag"],"status":"CONCLUDED","traffic_percentage":0,"context":{"and":[{"==":[{"var":"clientId"},"galactustest"]},{"==":[{"var":"os"},"android"]}]},"variants":[{"context_id":"1a846c62e293d20b6bf1a1c3124a949b62c84873942f0fa13d1e5933563fa976","id":"7125136499787333632-test","override_id":"0844d58fec6b4f45def66d51acea3c88e2a40782a120301038c0f13c61da2746","overrides":{"godel_config":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/bbdaily/ios/release/config.json","godel_config_etag":"d574dfb82a2e851145a0c4070ef0d6e4"},"variant_type":"EXPERIMENTAL"},{"context_id":"a1fadb932e9154f4811b869b67fb5f4b5930eb120ec1a6183e49712369c48f23","id":"7125136499787333632-control","override_id":"5a59fbf006c9929350c7101f7621297b130206246ae69c8536f4e068778089b4","overrides":{"godel_config":"abc","godel_config_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}],"last_modified_by":"ritick.madaan@juspay.in","chosen_variant":"7125136499787333632-test"}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7125136499787333632,'2023-10-31 15:08:40+05:30','ritick.madaan@juspay.in','2023-10-31 15:12:07+05:30','galactustest test','{godel_config,godel_config_etag}','CONCLUDED',0,'{"and":[{"==":[{"var":"clientId"},"galactustest"]},{"==":[{"var":"os"},"android"]}]}','[{"context_id":"1a846c62e293d20b6bf1a1c3124a949b62c84873942f0fa13d1e5933563fa976","id":"7125136499787333632-test","override_id":"0844d58fec6b4f45def66d51acea3c88e2a40782a120301038c0f13c61da2746","overrides":{"godel_config":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/bbdaily/ios/release/config.json","godel_config_etag":"d574dfb82a2e851145a0c4070ef0d6e4"},"variant_type":"EXPERIMENTAL"},{"context_id":"a1fadb932e9154f4811b869b67fb5f4b5930eb120ec1a6183e49712369c48f23","id":"7125136499787333632-control","override_id":"5a59fbf006c9929350c7101f7621297b130206246ae69c8536f4e068778089b4","overrides":{"godel_config":"abc","godel_config_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}]','ritick.madaan@juspay.in','7125136499787333632-test')
a3ce5298-b9bb-4bcc-ad90-ee4d353e050b	experiments	postgres	2024-08-21 22:46:30.32262	INSERT	\N	{"id":7145416915827904512,"created_at":"2023-12-26T14:15:48+05:30","created_by":"rohan.pawar@juspay.in","last_modified":"2023-12-27T06:44:48+05:30","name":"mobius test 3","override_keys":["hyperpay_configuration","hyperpay_configuration_etag"],"status":"CONCLUDED","traffic_percentage":25,"context":{"and":[{"==":[{"var":"clientId"},"galactustest"]},{"==":[{"var":"os"},"android"]}]},"variants":[{"context_id":"ec8e160e307491e8dd3e379b4fa23987b4381bcd3753dfd4700871650314f141","id":"7145416915827904512-test","override_id":"7ca1e210b75248afda6ae04f074f0dcc8a8f729f897e9f9113a3009e087c01b7","overrides":{"hyperpay_configuration":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/magicbrickspp/configuration/1.1/v1-configuration.zip","hyperpay_configuration_etag":"5f00e94dc26e6d96a95e4a2888b42f4d"},"variant_type":"EXPERIMENTAL"},{"context_id":"3da48a2c5960757eac62187a38f371751ba9c52e30dd6bb3b77e65e41673eecd","id":"7145416915827904512-control","override_id":"afb1f8291942d3276f2259b285f7a021e7ffb2a6bb4038b21976e3d59d6abde8","overrides":{"hyperpay_configuration":"default_str_ignore_this","hyperpay_configuration_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}],"last_modified_by":"mobius@juspay.in","chosen_variant":"7145416915827904512-control"}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7145416915827904512,'2023-12-26 14:15:48+05:30','rohan.pawar@juspay.in','2023-12-27 06:44:48+05:30','mobius test 3','{hyperpay_configuration,hyperpay_configuration_etag}','CONCLUDED',25,'{"and":[{"==":[{"var":"clientId"},"galactustest"]},{"==":[{"var":"os"},"android"]}]}','[{"context_id":"ec8e160e307491e8dd3e379b4fa23987b4381bcd3753dfd4700871650314f141","id":"7145416915827904512-test","override_id":"7ca1e210b75248afda6ae04f074f0dcc8a8f729f897e9f9113a3009e087c01b7","overrides":{"hyperpay_configuration":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/magicbrickspp/configuration/1.1/v1-configuration.zip","hyperpay_configuration_etag":"5f00e94dc26e6d96a95e4a2888b42f4d"},"variant_type":"EXPERIMENTAL"},{"context_id":"3da48a2c5960757eac62187a38f371751ba9c52e30dd6bb3b77e65e41673eecd","id":"7145416915827904512-control","override_id":"afb1f8291942d3276f2259b285f7a021e7ffb2a6bb4038b21976e3d59d6abde8","overrides":{"hyperpay_configuration":"default_str_ignore_this","hyperpay_configuration_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}]','mobius@juspay.in','7145416915827904512-control')
e8416a3d-980a-4a0a-80f4-54dada9d7271	experiments	postgres	2024-08-21 22:46:30.32291	INSERT	\N	{"id":7169247805271527424,"created_at":"2024-03-01T08:31:15+05:30","created_by":"shubhranshu.sanjeev@juspay.in","last_modified":"2024-03-01T08:31:15+05:30","name":"etag-autofill-test","override_keys":["godel_config","godel_config_etag"],"status":"CREATED","traffic_percentage":0,"context":{"and":[{"==":[{"var":"clientId"},"galactustest"]},{"==":[{"var":"os"},"android"]}]},"variants":[{"context_id":"2c48644d9e00422d6fe4859cbd64dba57e40c090f15e7971e7e722829e1a25bb","id":"7169247805271527424-test","override_id":"badeef68ab9f6a694fb73a623ed3186c8c37df7e31aee1b0791f186c369a265f","overrides":{"godel_config":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/bbdaily/ios/release/config.json","godel_config_etag":"81243ede6c3fbbb38f3eca88084568c7"},"variant_type":"EXPERIMENTAL"},{"context_id":"3409119bfa87ce154c38d33eaf3a56a443d85e61fdab116e4c9a6f3393145581","id":"7169247805271527424-control","override_id":"81159838966447ace3e7be99a17e7b1a2e6a7ba2ecc98da4899f0e56f4539d59","overrides":{"godel_config":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/bbdaily/ios/release/config.json","godel_config_etag":"d574dfb82a2e851145a0c4070ef0d6e4"},"variant_type":"CONTROL"}],"last_modified_by":"shubhranshu.sanjeev@juspay.in","chosen_variant":""}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7169247805271527424,'2024-03-01 08:31:15+05:30','shubhranshu.sanjeev@juspay.in','2024-03-01 08:31:15+05:30','etag-autofill-test','{godel_config,godel_config_etag}','CREATED',0,'{"and":[{"==":[{"var":"clientId"},"galactustest"]},{"==":[{"var":"os"},"android"]}]}','[{"context_id":"2c48644d9e00422d6fe4859cbd64dba57e40c090f15e7971e7e722829e1a25bb","id":"7169247805271527424-test","override_id":"badeef68ab9f6a694fb73a623ed3186c8c37df7e31aee1b0791f186c369a265f","overrides":{"godel_config":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/bbdaily/ios/release/config.json","godel_config_etag":"81243ede6c3fbbb38f3eca88084568c7"},"variant_type":"EXPERIMENTAL"},{"context_id":"3409119bfa87ce154c38d33eaf3a56a443d85e61fdab116e4c9a6f3393145581","id":"7169247805271527424-control","override_id":"81159838966447ace3e7be99a17e7b1a2e6a7ba2ecc98da4899f0e56f4539d59","overrides":{"godel_config":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/bbdaily/ios/release/config.json","godel_config_etag":"d574dfb82a2e851145a0c4070ef0d6e4"},"variant_type":"CONTROL"}]','shubhranshu.sanjeev@juspay.in','')
66065221-4a1f-42bc-b417-a4f65870f29f	experiments	postgres	2024-08-21 22:46:30.323243	INSERT	\N	{"id":7189244532330844160,"created_at":"2024-04-25T12:51:06+05:30","created_by":"shubhranshu.sanjeev@juspay.in","last_modified":"2024-04-25T12:53:31+05:30","name":"dummyexperiment","override_keys":["arya_src_etag","arya_bundle","arya_src","arya_bundle_etag"],"status":"CREATED","traffic_percentage":0,"context":{"and":[{"==":[{"var":"os"},"ios"]},{"==":[{"var":"clientId"},"galactustest"]}]},"variants":[{"context_id":"93930c03ba039a477447e609820c97e8b6f51ddfb320d84c2b667fc22c549d67","id":"7189244532330844160-test","override_id":"28bac9d32e47ec65af26f53af1649ee731ebd9c22018bc493d5f708162c228e9","overrides":{"arya_bundle":"default_str_ignore_this","arya_bundle_etag":"default_str_ignore","arya_src":"default_str_ignore_this","arya_src_etag":"default_str_ignore"},"variant_type":"EXPERIMENTAL"},{"context_id":"497eef54d2cea2869dc3244d4b83c6031453ec453152392e62613a93a03775ce","id":"7189244532330844160-control","override_id":"28d94700c3c4e2e2c7f4d41154e992419bbc696c020cac765bdd02703ae36800","overrides":{"arya_bundle":"default_str_ignore_this","arya_bundle_etag":"default_str_ignore_this","arya_src":"default_str_ignore_this","arya_src_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}],"last_modified_by":"shubhranshu.sanjeev@juspay.in","chosen_variant":""}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7189244532330844160,'2024-04-25 12:51:06+05:30','shubhranshu.sanjeev@juspay.in','2024-04-25 12:53:31+05:30','dummyexperiment','{arya_src_etag,arya_bundle,arya_src,arya_bundle_etag}','CREATED',0,'{"and":[{"==":[{"var":"os"},"ios"]},{"==":[{"var":"clientId"},"galactustest"]}]}','[{"context_id":"93930c03ba039a477447e609820c97e8b6f51ddfb320d84c2b667fc22c549d67","id":"7189244532330844160-test","override_id":"28bac9d32e47ec65af26f53af1649ee731ebd9c22018bc493d5f708162c228e9","overrides":{"arya_bundle":"default_str_ignore_this","arya_bundle_etag":"default_str_ignore","arya_src":"default_str_ignore_this","arya_src_etag":"default_str_ignore"},"variant_type":"EXPERIMENTAL"},{"context_id":"497eef54d2cea2869dc3244d4b83c6031453ec453152392e62613a93a03775ce","id":"7189244532330844160-control","override_id":"28d94700c3c4e2e2c7f4d41154e992419bbc696c020cac765bdd02703ae36800","overrides":{"arya_bundle":"default_str_ignore_this","arya_bundle_etag":"default_str_ignore_this","arya_src":"default_str_ignore_this","arya_src_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}]','shubhranshu.sanjeev@juspay.in','')
a596fe9d-d009-4d80-be11-7adeae5c0c6e	experiments	postgres	2024-08-21 22:46:30.323555	INSERT	\N	{"id":7125752957852614656,"created_at":"2023-11-02T07:58:15+05:30","created_by":"kartik.gajendra@juspay.in","last_modified":"2023-11-16T14:09:22+05:30","name":"galactustest-experiment","override_keys":["godel_config"],"status":"CONCLUDED","traffic_percentage":46,"context":{"and":[{"==":[{"var":"os"},"android"]},{"==":[{"var":"clientId"},"galactustest"]}]},"variants":[{"context_id":"7c44f356718b19cd169b73eb9e7300597bd66ef2112da93adb1753adfce84031","id":"7125752957852614656-control","override_id":"dece633f19bfbce7315e7dae457a58485af60f8d482270d74553801ab5e1d59e","overrides":{"godel_config":"abc"},"variant_type":"CONTROL"},{"context_id":"8ab020d72267773d1755e252902514e58d7a4f4396c72305e0c029dcf53d2be9","id":"7125752957852614656-test1","override_id":"07d8ece9a73705f90c31b69488667f28b23e8e25f8510ff1ab9ff23cf58b8e71","overrides":{"godel_config":"godel_experimental_1"},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"mobius@juspay.in","chosen_variant":"7125752957852614656-control"}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7125752957852614656,'2023-11-02 07:58:15+05:30','kartik.gajendra@juspay.in','2023-11-16 14:09:22+05:30','galactustest-experiment','{godel_config}','CONCLUDED',46,'{"and":[{"==":[{"var":"os"},"android"]},{"==":[{"var":"clientId"},"galactustest"]}]}','[{"context_id":"7c44f356718b19cd169b73eb9e7300597bd66ef2112da93adb1753adfce84031","id":"7125752957852614656-control","override_id":"dece633f19bfbce7315e7dae457a58485af60f8d482270d74553801ab5e1d59e","overrides":{"godel_config":"abc"},"variant_type":"CONTROL"},{"context_id":"8ab020d72267773d1755e252902514e58d7a4f4396c72305e0c029dcf53d2be9","id":"7125752957852614656-test1","override_id":"07d8ece9a73705f90c31b69488667f28b23e8e25f8510ff1ab9ff23cf58b8e71","overrides":{"godel_config":"godel_experimental_1"},"variant_type":"EXPERIMENTAL"}]','mobius@juspay.in','7125752957852614656-control')
2ea42780-d64b-4f5d-a934-a294bf48f6b1	experiments	postgres	2024-08-21 22:46:30.323896	INSERT	\N	{"id":7127260768432291840,"created_at":"2023-11-06T11:49:45+05:30","created_by":"rohan.pawar@juspay.in","last_modified":"2023-11-22T09:30:44+05:30","name":"MobiusTestRelease","override_keys":["vies_entry"],"status":"CONCLUDED","traffic_percentage":0,"context":{"and":[{"==":[{"var":"os"},"android"]}]},"variants":[{"context_id":"66e211d98b9677c75a667613e99226a4b163195a9861adc8565fd22a74e8a624","id":"7127260768432291840-test","override_id":"a18eb9716d651eae11cf161f28db20ce789df6fbd95dd3de2a65a1171fd409ec","overrides":{"vies_entry":"test-value"},"variant_type":"EXPERIMENTAL"},{"context_id":"9b574714dff4c827455a1504d934ce4dd6efbd90cb297c72e4be8095b97b3099","id":"7127260768432291840-control","override_id":"80813c2b5591d1f4894a5f13bf14365be86f8c8fb7505b61908011bc6ca4cd44","overrides":{"vies_entry":"default_str_ignore_this"},"variant_type":"CONTROL"}],"last_modified_by":"ritick.madaan@juspay.in","chosen_variant":"7127260768432291840-control"}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7127260768432291840,'2023-11-06 11:49:45+05:30','rohan.pawar@juspay.in','2023-11-22 09:30:44+05:30','MobiusTestRelease','{vies_entry}','CONCLUDED',0,'{"and":[{"==":[{"var":"os"},"android"]}]}','[{"context_id":"66e211d98b9677c75a667613e99226a4b163195a9861adc8565fd22a74e8a624","id":"7127260768432291840-test","override_id":"a18eb9716d651eae11cf161f28db20ce789df6fbd95dd3de2a65a1171fd409ec","overrides":{"vies_entry":"test-value"},"variant_type":"EXPERIMENTAL"},{"context_id":"9b574714dff4c827455a1504d934ce4dd6efbd90cb297c72e4be8095b97b3099","id":"7127260768432291840-control","override_id":"80813c2b5591d1f4894a5f13bf14365be86f8c8fb7505b61908011bc6ca4cd44","overrides":{"vies_entry":"default_str_ignore_this"},"variant_type":"CONTROL"}]','ritick.madaan@juspay.in','7127260768432291840-control')
311aaa8d-27d5-4cec-bb16-da90be521d5a	experiments	postgres	2024-08-21 22:46:30.32418	INSERT	\N	{"id":7125752978719277056,"created_at":"2023-11-02T07:58:20+05:30","created_by":"kartik.gajendra@juspay.in","last_modified":"2023-11-22T09:30:48+05:30","name":"galactustest-experiment","override_keys":["godel_config"],"status":"CONCLUDED","traffic_percentage":0,"context":{"and":[{"==":[{"var":"os"},"android"]},{"==":[{"var":"clientId"},"galactustest"]}]},"variants":[{"context_id":"6c7fa8e753496acc9b09b023933d251e84b08bc954795fac885785b4e64674d3","id":"7125752978719277056-control","override_id":"dece633f19bfbce7315e7dae457a58485af60f8d482270d74553801ab5e1d59e","overrides":{"godel_config":"abc"},"variant_type":"CONTROL"},{"context_id":"38b782a1ddc8708e766af966c8a11f215b205412ebf389a07a4c419f2990c06f","id":"7125752978719277056-test1","override_id":"7e0ea3dabfdf0e5f03c85015c4cc62a56a46d04b5d00e1d7dfd17aaed635184e","overrides":{"godel_config":"godel_experimental"},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"ritick.madaan@juspay.in","chosen_variant":"7125752978719277056-control"}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7125752978719277056,'2023-11-02 07:58:20+05:30','kartik.gajendra@juspay.in','2023-11-22 09:30:48+05:30','galactustest-experiment','{godel_config}','CONCLUDED',0,'{"and":[{"==":[{"var":"os"},"android"]},{"==":[{"var":"clientId"},"galactustest"]}]}','[{"context_id":"6c7fa8e753496acc9b09b023933d251e84b08bc954795fac885785b4e64674d3","id":"7125752978719277056-control","override_id":"dece633f19bfbce7315e7dae457a58485af60f8d482270d74553801ab5e1d59e","overrides":{"godel_config":"abc"},"variant_type":"CONTROL"},{"context_id":"38b782a1ddc8708e766af966c8a11f215b205412ebf389a07a4c419f2990c06f","id":"7125752978719277056-test1","override_id":"7e0ea3dabfdf0e5f03c85015c4cc62a56a46d04b5d00e1d7dfd17aaed635184e","overrides":{"godel_config":"godel_experimental"},"variant_type":"EXPERIMENTAL"}]','ritick.madaan@juspay.in','7125752978719277056-control')
58adb365-be1a-438d-82ef-df2b53861c35	experiments	postgres	2024-08-21 22:46:30.324522	INSERT	\N	{"id":7128366584299397120,"created_at":"2023-11-09T13:03:52+05:30","created_by":"ritick.madaan@juspay.in","last_modified":"2023-11-09T13:27:52+05:30","name":"zee5 test","override_keys":["hyperos_placeholder_tracker_etag","hyperos_placeholder_tracker"],"status":"CONCLUDED","traffic_percentage":20,"context":{"and":[{"==":[{"var":"clientId"},"zee5"]},{"==":[{"var":"os"},"android"]}]},"variants":[{"context_id":"b28723ca5117a63bf96bd0d850f2af77a262d69b0d929d3dc17ab9a67c7ea1c3","id":"7128366584299397120-test","override_id":"5cc7746761839b528cc56948b0962e02da0abb49b9e33dda0954984d90830828","overrides":{"hyperos_placeholder_tracker":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/galactustest/android/release/config.json","hyperos_placeholder_tracker_etag":"f43c71f605bc96833153885453edbf47"},"variant_type":"EXPERIMENTAL"},{"context_id":"b57ffa231aaaf6d1f03f94eb24db990793bf632333025b9787a54100fcf8e216","id":"7128366584299397120-control","override_id":"25bbfdf9977c2087c7cfa786cfe826f14bbf4ec8f0ce2d03943661201325a4e5","overrides":{"hyperos_placeholder_tracker":"default_str_ignore_this","hyperos_placeholder_tracker_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}],"last_modified_by":"ritick.madaan@juspay.in","chosen_variant":"7128366584299397120-test"}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7128366584299397120,'2023-11-09 13:03:52+05:30','ritick.madaan@juspay.in','2023-11-09 13:27:52+05:30','zee5 test','{hyperos_placeholder_tracker_etag,hyperos_placeholder_tracker}','CONCLUDED',20,'{"and":[{"==":[{"var":"clientId"},"zee5"]},{"==":[{"var":"os"},"android"]}]}','[{"context_id":"b28723ca5117a63bf96bd0d850f2af77a262d69b0d929d3dc17ab9a67c7ea1c3","id":"7128366584299397120-test","override_id":"5cc7746761839b528cc56948b0962e02da0abb49b9e33dda0954984d90830828","overrides":{"hyperos_placeholder_tracker":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/galactustest/android/release/config.json","hyperos_placeholder_tracker_etag":"f43c71f605bc96833153885453edbf47"},"variant_type":"EXPERIMENTAL"},{"context_id":"b57ffa231aaaf6d1f03f94eb24db990793bf632333025b9787a54100fcf8e216","id":"7128366584299397120-control","override_id":"25bbfdf9977c2087c7cfa786cfe826f14bbf4ec8f0ce2d03943661201325a4e5","overrides":{"hyperos_placeholder_tracker":"default_str_ignore_this","hyperos_placeholder_tracker_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}]','ritick.madaan@juspay.in','7128366584299397120-test')
b9fb67c5-15e9-4264-a90a-9b8a1839b02e	experiments	postgres	2024-08-21 22:46:30.324859	INSERT	\N	{"id":7145699287093825536,"created_at":"2023-12-27T08:57:50+05:30","created_by":"rohan.pawar@juspay.in","last_modified":"2023-12-27T08:57:50+05:30","name":"mobius test 3","override_keys":["hyperpay_configuration","hyperpay_configuration_etag"],"status":"CREATED","traffic_percentage":0,"context":{"and":[{"==":[{"var":"clientId"},"galactustest"]},{"==":[{"var":"os"},"android"]}]},"variants":[{"context_id":"faec42a890e61480cecd6a7d80d02eba8b191274ae622f10576d9ee0e9f20273","id":"7145699287093825536-test","override_id":"7ca1e210b75248afda6ae04f074f0dcc8a8f729f897e9f9113a3009e087c01b7","overrides":{"hyperpay_configuration":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/magicbrickspp/configuration/1.1/v1-configuration.zip","hyperpay_configuration_etag":"5f00e94dc26e6d96a95e4a2888b42f4d"},"variant_type":"EXPERIMENTAL"},{"context_id":"c80d2c9783437b7d4dab17432608eaa67077d955f91de150a72b42c6d2d60248","id":"7145699287093825536-control","override_id":"afb1f8291942d3276f2259b285f7a021e7ffb2a6bb4038b21976e3d59d6abde8","overrides":{"hyperpay_configuration":"default_str_ignore_this","hyperpay_configuration_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}],"last_modified_by":"rohan.pawar@juspay.in","chosen_variant":""}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7145699287093825536,'2023-12-27 08:57:50+05:30','rohan.pawar@juspay.in','2023-12-27 08:57:50+05:30','mobius test 3','{hyperpay_configuration,hyperpay_configuration_etag}','CREATED',0,'{"and":[{"==":[{"var":"clientId"},"galactustest"]},{"==":[{"var":"os"},"android"]}]}','[{"context_id":"faec42a890e61480cecd6a7d80d02eba8b191274ae622f10576d9ee0e9f20273","id":"7145699287093825536-test","override_id":"7ca1e210b75248afda6ae04f074f0dcc8a8f729f897e9f9113a3009e087c01b7","overrides":{"hyperpay_configuration":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/magicbrickspp/configuration/1.1/v1-configuration.zip","hyperpay_configuration_etag":"5f00e94dc26e6d96a95e4a2888b42f4d"},"variant_type":"EXPERIMENTAL"},{"context_id":"c80d2c9783437b7d4dab17432608eaa67077d955f91de150a72b42c6d2d60248","id":"7145699287093825536-control","override_id":"afb1f8291942d3276f2259b285f7a021e7ffb2a6bb4038b21976e3d59d6abde8","overrides":{"hyperpay_configuration":"default_str_ignore_this","hyperpay_configuration_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}]','rohan.pawar@juspay.in','')
1b30436a-b470-47c4-8252-200b06ad42af	experiments	postgres	2024-08-21 22:46:30.325177	INSERT	\N	{"id":7132254992214142976,"created_at":"2023-11-20T06:35:01+05:30","created_by":"mobius@juspay.in","last_modified":"2023-11-22T09:29:33+05:30","name":"experiment-1","override_keys":["pmTestKey2","pmTestKey1"],"status":"CONCLUDED","traffic_percentage":0,"context":{"and":[{"==":[{"var":"os"},"ios"]},{"==":[{"var":"client"},"testClientCac1"]}]},"variants":[{"context_id":"b75bdce0d37e8897a73c29c13d1b465b3eddab197f8f4a49ae4a06d6d08feeee","id":"7132254992214142976-control","override_id":"085e637459203733acb435d0bd921081b10a46a3eb192924a332e414bf266cf7","overrides":{"pmTestKey1":"value1-control","pmTestKey2":"value1-control"},"variant_type":"CONTROL"},{"context_id":"c2a6845f03b2e86c917aa48ef4df2511d368fc1e501bfe674cf9d6196748bd23","id":"7132254992214142976-test1","override_id":"65bcc9acaebd88355f850d7087e269f36d1316d7aeeddcf72cece682f99691c1","overrides":{"pmTestKey1":"value2-test","pmTestKey2":"value2-test"},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"ritick.madaan@juspay.in","chosen_variant":"7132254992214142976-control"}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7132254992214142976,'2023-11-20 06:35:01+05:30','mobius@juspay.in','2023-11-22 09:29:33+05:30','experiment-1','{pmTestKey2,pmTestKey1}','CONCLUDED',0,'{"and":[{"==":[{"var":"os"},"ios"]},{"==":[{"var":"client"},"testClientCac1"]}]}','[{"context_id":"b75bdce0d37e8897a73c29c13d1b465b3eddab197f8f4a49ae4a06d6d08feeee","id":"7132254992214142976-control","override_id":"085e637459203733acb435d0bd921081b10a46a3eb192924a332e414bf266cf7","overrides":{"pmTestKey1":"value1-control","pmTestKey2":"value1-control"},"variant_type":"CONTROL"},{"context_id":"c2a6845f03b2e86c917aa48ef4df2511d368fc1e501bfe674cf9d6196748bd23","id":"7132254992214142976-test1","override_id":"65bcc9acaebd88355f850d7087e269f36d1316d7aeeddcf72cece682f99691c1","overrides":{"pmTestKey1":"value2-test","pmTestKey2":"value2-test"},"variant_type":"EXPERIMENTAL"}]','ritick.madaan@juspay.in','7132254992214142976-control')
5970a0ce-a715-4a73-a7a0-26bf1db02dce	experiments	postgres	2024-08-21 22:46:30.325488	INSERT	\N	{"id":7130919624796684288,"created_at":"2023-11-16T14:08:44+05:30","created_by":"mobius@juspay.in","last_modified":"2023-11-22T09:29:38+05:30","name":"experiment-1","override_keys":["pmTestKey1","pmTestKey2"],"status":"CONCLUDED","traffic_percentage":0,"context":{"and":[{"==":[{"var":"os"},"ios"]},{"==":[{"var":"client"},"testClientCac1"]}]},"variants":[{"context_id":"10da21e3309c736f4381a6420d62c8a1147dc12dc7ca57c5f98bfdffc1a594d0","id":"7130919624796684288-control","override_id":"085e637459203733acb435d0bd921081b10a46a3eb192924a332e414bf266cf7","overrides":{"pmTestKey1":"value1-control","pmTestKey2":"value1-control"},"variant_type":"CONTROL"},{"context_id":"b522637b6a345591568e4a454ae70b953cbc6bbe617fa28053f064799e4d24bd","id":"7130919624796684288-test1","override_id":"65bcc9acaebd88355f850d7087e269f36d1316d7aeeddcf72cece682f99691c1","overrides":{"pmTestKey1":"value2-test","pmTestKey2":"value2-test"},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"ritick.madaan@juspay.in","chosen_variant":"7130919624796684288-control"}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7130919624796684288,'2023-11-16 14:08:44+05:30','mobius@juspay.in','2023-11-22 09:29:38+05:30','experiment-1','{pmTestKey1,pmTestKey2}','CONCLUDED',0,'{"and":[{"==":[{"var":"os"},"ios"]},{"==":[{"var":"client"},"testClientCac1"]}]}','[{"context_id":"10da21e3309c736f4381a6420d62c8a1147dc12dc7ca57c5f98bfdffc1a594d0","id":"7130919624796684288-control","override_id":"085e637459203733acb435d0bd921081b10a46a3eb192924a332e414bf266cf7","overrides":{"pmTestKey1":"value1-control","pmTestKey2":"value1-control"},"variant_type":"CONTROL"},{"context_id":"b522637b6a345591568e4a454ae70b953cbc6bbe617fa28053f064799e4d24bd","id":"7130919624796684288-test1","override_id":"65bcc9acaebd88355f850d7087e269f36d1316d7aeeddcf72cece682f99691c1","overrides":{"pmTestKey1":"value2-test","pmTestKey2":"value2-test"},"variant_type":"EXPERIMENTAL"}]','ritick.madaan@juspay.in','7130919624796684288-control')
c392e85b-9d9e-4618-ac81-81462da4658f	experiments	postgres	2024-08-21 22:46:30.325787	INSERT	\N	{"id":7122930386611376128,"created_at":"2023-10-25T13:02:21+05:30","created_by":"ritick.madaan@juspay.in","last_modified":"2023-11-22T09:31:09+05:30","name":"experiment-1","override_keys":["pmTestKey1972","pmTestKey1999"],"status":"CONCLUDED","traffic_percentage":0,"context":{"and":[{"==":[{"var":"os"},"ios"]},{"==":[{"var":"client"},"testClientCac02"]}]},"variants":[{"context_id":"f87a94a3e2ac0cc624b04811aebccb68abf9277ccc61cdea0cca3a88b1c1248d","id":"7122930386611376128-control","override_id":"9802e3201f5948257637154d94b838b345150b9817604c10cf1312c804657a5e","overrides":{"pmTestKey1972":"value-7910-an-control","pmTestKey1999":"value-6910-an-control"},"variant_type":"CONTROL"},{"context_id":"fbf9abaffe4718957f565e2f1587b37db182e3ced7aeb55fa0e03f9f1ef36b6a","id":"7122930386611376128-test1","override_id":"c90c09c89b87e60ad6cec89219ebeb316d85f8dec45a427f809dc44b0b1a7fd4","overrides":{"pmTestKey1972":"value-7920-an-test","pmTestKey1999":"value-6930-an-test"},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"ritick.madaan@juspay.in","chosen_variant":"7122930386611376128-control"}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7122930386611376128,'2023-10-25 13:02:21+05:30','ritick.madaan@juspay.in','2023-11-22 09:31:09+05:30','experiment-1','{pmTestKey1972,pmTestKey1999}','CONCLUDED',0,'{"and":[{"==":[{"var":"os"},"ios"]},{"==":[{"var":"client"},"testClientCac02"]}]}','[{"context_id":"f87a94a3e2ac0cc624b04811aebccb68abf9277ccc61cdea0cca3a88b1c1248d","id":"7122930386611376128-control","override_id":"9802e3201f5948257637154d94b838b345150b9817604c10cf1312c804657a5e","overrides":{"pmTestKey1972":"value-7910-an-control","pmTestKey1999":"value-6910-an-control"},"variant_type":"CONTROL"},{"context_id":"fbf9abaffe4718957f565e2f1587b37db182e3ced7aeb55fa0e03f9f1ef36b6a","id":"7122930386611376128-test1","override_id":"c90c09c89b87e60ad6cec89219ebeb316d85f8dec45a427f809dc44b0b1a7fd4","overrides":{"pmTestKey1972":"value-7920-an-test","pmTestKey1999":"value-6930-an-test"},"variant_type":"EXPERIMENTAL"}]','ritick.madaan@juspay.in','7122930386611376128-control')
21673296-5db1-4302-adc1-51cf4ec67b5c	experiments	postgres	2024-08-21 22:46:30.326246	INSERT	\N	{"id":7133465816902021120,"created_at":"2023-11-23T14:46:24+05:30","created_by":"mobius@juspay.in","last_modified":"2023-11-29T09:47:34+05:30","name":"experiment-1","override_keys":["pmTestKey2","pmTestKey1"],"status":"CONCLUDED","traffic_percentage":0,"context":{"and":[{"==":[{"var":"os"},"ios"]},{"==":[{"var":"client"},"testClientCac1"]}]},"variants":[{"context_id":"6defc56f692448bb9e1c79aa8a2e02a9868361394ecdf8d29215c8a1b34764d8","id":"7133465816902021120-control","override_id":"2123fadefc2e34d386e7ebb8851ab290ac4b2fad87e353745e3387025ffa81ff","overrides":{"pmTestKey1":"value1-control","pmTestKey2":"value1-control"},"variant_type":"CONTROL"},{"context_id":"2239fc756f765b0d73aa28ea82eed075b7a6ee288703d2906503ca1ba5153e58","id":"7133465816902021120-test1","override_id":"67d10870bc1289e3e0544fe29aa878b470344a319ed217d78cba21ff5c622db1","overrides":{"pmTestKey1":"value2-test","pmTestKey2":"value2-test"},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"mobius@juspay.in","chosen_variant":"7133465816902021120-control"}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7133465816902021120,'2023-11-23 14:46:24+05:30','mobius@juspay.in','2023-11-29 09:47:34+05:30','experiment-1','{pmTestKey2,pmTestKey1}','CONCLUDED',0,'{"and":[{"==":[{"var":"os"},"ios"]},{"==":[{"var":"client"},"testClientCac1"]}]}','[{"context_id":"6defc56f692448bb9e1c79aa8a2e02a9868361394ecdf8d29215c8a1b34764d8","id":"7133465816902021120-control","override_id":"2123fadefc2e34d386e7ebb8851ab290ac4b2fad87e353745e3387025ffa81ff","overrides":{"pmTestKey1":"value1-control","pmTestKey2":"value1-control"},"variant_type":"CONTROL"},{"context_id":"2239fc756f765b0d73aa28ea82eed075b7a6ee288703d2906503ca1ba5153e58","id":"7133465816902021120-test1","override_id":"67d10870bc1289e3e0544fe29aa878b470344a319ed217d78cba21ff5c622db1","overrides":{"pmTestKey1":"value2-test","pmTestKey2":"value2-test"},"variant_type":"EXPERIMENTAL"}]','mobius@juspay.in','7133465816902021120-control')
c12c91b7-ccf6-44f3-8944-eb912b1f3a4b	experiments	postgres	2024-08-21 22:46:30.32661	INSERT	\N	{"id":7133449316409618432,"created_at":"2023-11-23T13:40:50+05:30","created_by":"mobius@juspay.in","last_modified":"2023-11-29T12:08:18+05:30","name":"experiment-1","override_keys":["pmTestKey1","pmTestKey2"],"status":"CONCLUDED","traffic_percentage":0,"context":{"and":[{"==":[{"var":"os"},"ios"]},{"==":[{"var":"client"},"testClientCac1"]}]},"variants":[{"context_id":"d1696342273aeb54fb16a9bafe939fefc54015e7e30c27502232186e4159726f","id":"7133449316409618432-control","override_id":"2123fadefc2e34d386e7ebb8851ab290ac4b2fad87e353745e3387025ffa81ff","overrides":{"pmTestKey1":"value1-control","pmTestKey2":"value1-control"},"variant_type":"CONTROL"},{"context_id":"48a6248edc895c858ac9f4dc988d81a5cd302e6a28c2a4adbb1df76e1d1d89d8","id":"7133449316409618432-test1","override_id":"67d10870bc1289e3e0544fe29aa878b470344a319ed217d78cba21ff5c622db1","overrides":{"pmTestKey1":"value2-test","pmTestKey2":"value2-test"},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"mobius@juspay.in","chosen_variant":"7133449316409618432-control"}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7133449316409618432,'2023-11-23 13:40:50+05:30','mobius@juspay.in','2023-11-29 12:08:18+05:30','experiment-1','{pmTestKey1,pmTestKey2}','CONCLUDED',0,'{"and":[{"==":[{"var":"os"},"ios"]},{"==":[{"var":"client"},"testClientCac1"]}]}','[{"context_id":"d1696342273aeb54fb16a9bafe939fefc54015e7e30c27502232186e4159726f","id":"7133449316409618432-control","override_id":"2123fadefc2e34d386e7ebb8851ab290ac4b2fad87e353745e3387025ffa81ff","overrides":{"pmTestKey1":"value1-control","pmTestKey2":"value1-control"},"variant_type":"CONTROL"},{"context_id":"48a6248edc895c858ac9f4dc988d81a5cd302e6a28c2a4adbb1df76e1d1d89d8","id":"7133449316409618432-test1","override_id":"67d10870bc1289e3e0544fe29aa878b470344a319ed217d78cba21ff5c622db1","overrides":{"pmTestKey1":"value2-test","pmTestKey2":"value2-test"},"variant_type":"EXPERIMENTAL"}]','mobius@juspay.in','7133449316409618432-control')
dee2e48b-77db-495c-abef-129efb9c0033	experiments	postgres	2024-08-21 22:46:30.327153	INSERT	\N	{"id":7137374889697832960,"created_at":"2023-12-04T09:39:39+05:30","created_by":"mobius@juspay.in","last_modified":"2023-12-04T09:53:45+05:30","name":"hyperpay version 3","override_keys":["pmTestKey1","pmTestKey2"],"status":"CONCLUDED","traffic_percentage":50,"context":{"and":[{"==":[{"var":"os"},"android"]},{"==":[{"var":"client"},"1mg"]}]},"variants":[{"context_id":"06c651d0f05bc1e81c480e7aa800fecb1d6cbef20d67c2d2a9cfe92bb19ab04b","id":"7137374889697832960-control","override_id":"20351707fa63907b8b5da4c4629e65cd3b248627394c0f3663fd6111e91b7600","overrides":{"pmTestKey1":"value1-control","pmTestKey2":"value2-control"},"variant_type":"CONTROL"},{"context_id":"0f58f73d8a022cf97a6c4a40a264473c0ae991453736196ae587aa680dead4f3","id":"7137374889697832960-test1","override_id":"5ae11a78d829df3744675dc6e97c2f118fe584496b3a566b7a2c418f69ad84cc","overrides":{"pmTestKey1":"value3-test","pmTestKey2":"value4-test"},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"mobius@juspay.in","chosen_variant":"7137374889697832960-control"}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7137374889697832960,'2023-12-04 09:39:39+05:30','mobius@juspay.in','2023-12-04 09:53:45+05:30','hyperpay version 3','{pmTestKey1,pmTestKey2}','CONCLUDED',50,'{"and":[{"==":[{"var":"os"},"android"]},{"==":[{"var":"client"},"1mg"]}]}','[{"context_id":"06c651d0f05bc1e81c480e7aa800fecb1d6cbef20d67c2d2a9cfe92bb19ab04b","id":"7137374889697832960-control","override_id":"20351707fa63907b8b5da4c4629e65cd3b248627394c0f3663fd6111e91b7600","overrides":{"pmTestKey1":"value1-control","pmTestKey2":"value2-control"},"variant_type":"CONTROL"},{"context_id":"0f58f73d8a022cf97a6c4a40a264473c0ae991453736196ae587aa680dead4f3","id":"7137374889697832960-test1","override_id":"5ae11a78d829df3744675dc6e97c2f118fe584496b3a566b7a2c418f69ad84cc","overrides":{"pmTestKey1":"value3-test","pmTestKey2":"value4-test"},"variant_type":"EXPERIMENTAL"}]','mobius@juspay.in','7137374889697832960-control')
146b26ec-e70d-4947-841b-6f2bd5ee925a	experiments	postgres	2024-08-21 22:46:30.327948	INSERT	\N	{"id":7145699449413390336,"created_at":"2023-12-27T08:58:29+05:30","created_by":"rohan.pawar@juspay.in","last_modified":"2024-02-15T06:01:23+05:30","name":"mobius test 3","override_keys":["hyperpay_configuration","hyperpay_configuration_etag"],"status":"CONCLUDED","traffic_percentage":50,"context":{"and":[{"==":[{"var":"clientId"},"galactustest"]},{"==":[{"var":"os"},"android"]}]},"variants":[{"context_id":"dbc71929676059eee48f4cd293fa0659762a434d59153a56fe9ab83214249041","id":"7145699449413390336-test","override_id":"7ca1e210b75248afda6ae04f074f0dcc8a8f729f897e9f9113a3009e087c01b7","overrides":{"hyperpay_configuration":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/magicbrickspp/configuration/1.1/v1-configuration.zip","hyperpay_configuration_etag":"5f00e94dc26e6d96a95e4a2888b42f4d"},"variant_type":"EXPERIMENTAL"},{"context_id":"43a9d79dfe3276ce8492897cb59ce23af0dc96aa1b0e28833ea45bdafa95a187","id":"7145699449413390336-control","override_id":"afb1f8291942d3276f2259b285f7a021e7ffb2a6bb4038b21976e3d59d6abde8","overrides":{"hyperpay_configuration":"default_str_ignore_this","hyperpay_configuration_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}],"last_modified_by":"mobius@juspay.in","chosen_variant":"7145699449413390336-test"}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7145699449413390336,'2023-12-27 08:58:29+05:30','rohan.pawar@juspay.in','2024-02-15 06:01:23+05:30','mobius test 3','{hyperpay_configuration,hyperpay_configuration_etag}','CONCLUDED',50,'{"and":[{"==":[{"var":"clientId"},"galactustest"]},{"==":[{"var":"os"},"android"]}]}','[{"context_id":"dbc71929676059eee48f4cd293fa0659762a434d59153a56fe9ab83214249041","id":"7145699449413390336-test","override_id":"7ca1e210b75248afda6ae04f074f0dcc8a8f729f897e9f9113a3009e087c01b7","overrides":{"hyperpay_configuration":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/magicbrickspp/configuration/1.1/v1-configuration.zip","hyperpay_configuration_etag":"5f00e94dc26e6d96a95e4a2888b42f4d"},"variant_type":"EXPERIMENTAL"},{"context_id":"43a9d79dfe3276ce8492897cb59ce23af0dc96aa1b0e28833ea45bdafa95a187","id":"7145699449413390336-control","override_id":"afb1f8291942d3276f2259b285f7a021e7ffb2a6bb4038b21976e3d59d6abde8","overrides":{"hyperpay_configuration":"default_str_ignore_this","hyperpay_configuration_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}]','mobius@juspay.in','7145699449413390336-test')
38e604c2-e648-42ca-aec7-1cd410775ca5	experiments	postgres	2024-08-21 22:46:30.32847	INSERT	\N	{"id":7202261923708796928,"created_at":"2024-05-31T10:57:34+05:30","created_by":"deepesh.maheshwari@juspay.in","last_modified":"2024-05-31T11:00:00+05:30","name":"EC and UPI intent update","override_keys":["upiintent_bundle","ec_bundle","ec_bundle_etag","upiintent_bundle_etag"],"status":"CONCLUDED","traffic_percentage":0,"context":{"and":[{"==":[{"var":"clientId"},"hdfcmaster"]},{"==":[{"var":"os"},"web"]}]},"variants":[{"context_id":"9620a9542ec1d24cf439de411943d27f1e326353ba40890fa602558bc43ebeb5","id":"7202261923708796928-test","override_id":"bc9fdc3c13a3569d0b30f3877934e3de94b145a1389be32e2f4617528283a20b","overrides":{"ec_bundle":"https://assets.juspay.in/hyper/bundles/app/in.juspay.ec/3.29.4/web/index.js","ec_bundle_etag":"e4b290873d9b7691a2471cdfa58023a4","upiintent_bundle":"https://assets.juspay.in/hyper/bundles/app/in.juspay.upiintent/4.9.9/web/index.js","upiintent_bundle_etag":"9774e9eaec3ec8221bfcb104a4534534"},"variant_type":"EXPERIMENTAL"},{"context_id":"0f8369ce5ae4fab3388300519c93300588fb5a6a5a3b72136e776593eca8d50e","id":"7202261923708796928-control","override_id":"e0f67abcfcb928bb6e84e0a14b8b7a9c44cb96c6e14b8672fab28b0022bf6a23","overrides":{"ec_bundle":"default_str_ignore_this","ec_bundle_etag":"default_str_ignore_this","upiintent_bundle":"default_str_ignore_this","upiintent_bundle_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}],"last_modified_by":"deepesh.maheshwari@juspay.in","chosen_variant":"7202261923708796928-test"}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7202261923708796928,'2024-05-31 10:57:34+05:30','deepesh.maheshwari@juspay.in','2024-05-31 11:00:00+05:30','EC and UPI intent update','{upiintent_bundle,ec_bundle,ec_bundle_etag,upiintent_bundle_etag}','CONCLUDED',0,'{"and":[{"==":[{"var":"clientId"},"hdfcmaster"]},{"==":[{"var":"os"},"web"]}]}','[{"context_id":"9620a9542ec1d24cf439de411943d27f1e326353ba40890fa602558bc43ebeb5","id":"7202261923708796928-test","override_id":"bc9fdc3c13a3569d0b30f3877934e3de94b145a1389be32e2f4617528283a20b","overrides":{"ec_bundle":"https://assets.juspay.in/hyper/bundles/app/in.juspay.ec/3.29.4/web/index.js","ec_bundle_etag":"e4b290873d9b7691a2471cdfa58023a4","upiintent_bundle":"https://assets.juspay.in/hyper/bundles/app/in.juspay.upiintent/4.9.9/web/index.js","upiintent_bundle_etag":"9774e9eaec3ec8221bfcb104a4534534"},"variant_type":"EXPERIMENTAL"},{"context_id":"0f8369ce5ae4fab3388300519c93300588fb5a6a5a3b72136e776593eca8d50e","id":"7202261923708796928-control","override_id":"e0f67abcfcb928bb6e84e0a14b8b7a9c44cb96c6e14b8672fab28b0022bf6a23","overrides":{"ec_bundle":"default_str_ignore_this","ec_bundle_etag":"default_str_ignore_this","upiintent_bundle":"default_str_ignore_this","upiintent_bundle_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}]','deepesh.maheshwari@juspay.in','7202261923708796928-test')
e8cc4e3c-6f4f-421f-b711-3695670b7d64	experiments	postgres	2024-08-21 22:46:30.328917	INSERT	\N	{"id":7138087493665431552,"created_at":"2023-12-06T08:51:17+05:30","created_by":"kartik.gajendra@juspay.in","last_modified":"2023-12-06T08:51:38+05:30","name":"mobius test","override_keys":["hyperpay_configuration","hyperpay_configuration_etag"],"status":"CONCLUDED","traffic_percentage":1,"context":{"and":[{"==":[{"var":"clientId"},"galactustest"]},{"==":[{"var":"os"},"android"]}]},"variants":[{"context_id":"c6e4355102dede5d1818547fb6b843c719a8a44e59dd3ba565cf5a483fc26b84","id":"7138087493665431552-test","override_id":"7ca1e210b75248afda6ae04f074f0dcc8a8f729f897e9f9113a3009e087c01b7","overrides":{"hyperpay_configuration":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/magicbrickspp/configuration/1.1/v1-configuration.zip","hyperpay_configuration_etag":"5f00e94dc26e6d96a95e4a2888b42f4d"},"variant_type":"EXPERIMENTAL"},{"context_id":"ce7d0b339a5367d61e88dd405cdf21d16f538a2df8d1a8029e206013e02655a6","id":"7138087493665431552-control","override_id":"afb1f8291942d3276f2259b285f7a021e7ffb2a6bb4038b21976e3d59d6abde8","overrides":{"hyperpay_configuration":"default_str_ignore_this","hyperpay_configuration_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}],"last_modified_by":"kartik.gajendra@juspay.in","chosen_variant":"7138087493665431552-control"}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7138087493665431552,'2023-12-06 08:51:17+05:30','kartik.gajendra@juspay.in','2023-12-06 08:51:38+05:30','mobius test','{hyperpay_configuration,hyperpay_configuration_etag}','CONCLUDED',1,'{"and":[{"==":[{"var":"clientId"},"galactustest"]},{"==":[{"var":"os"},"android"]}]}','[{"context_id":"c6e4355102dede5d1818547fb6b843c719a8a44e59dd3ba565cf5a483fc26b84","id":"7138087493665431552-test","override_id":"7ca1e210b75248afda6ae04f074f0dcc8a8f729f897e9f9113a3009e087c01b7","overrides":{"hyperpay_configuration":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/magicbrickspp/configuration/1.1/v1-configuration.zip","hyperpay_configuration_etag":"5f00e94dc26e6d96a95e4a2888b42f4d"},"variant_type":"EXPERIMENTAL"},{"context_id":"ce7d0b339a5367d61e88dd405cdf21d16f538a2df8d1a8029e206013e02655a6","id":"7138087493665431552-control","override_id":"afb1f8291942d3276f2259b285f7a021e7ffb2a6bb4038b21976e3d59d6abde8","overrides":{"hyperpay_configuration":"default_str_ignore_this","hyperpay_configuration_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}]','kartik.gajendra@juspay.in','7138087493665431552-control')
e14dae36-824e-4b1d-8486-a399c8f19edb	experiments	postgres	2024-08-21 22:46:30.329462	INSERT	\N	{"id":7146027985267212288,"created_at":"2023-12-28T06:43:58+05:30","created_by":"akash.singhania@juspay.in","last_modified":"2023-12-28T06:49:35+05:30","name":"Global Release - EC - Android","override_keys":["ec_bundle","ec_bundle_etag"],"status":"INPROGRESS","traffic_percentage":50,"context":{"and":[{"==":[{"var":"os"},"android"]},{"in":[{"var":"clientId"},["goindigo","mplgaming","gameskraft","jungleerummy","howzat","slice","rummytime","winzo","jar","lazypay","playship","confirmtkt","purplle.com","myteam11","railyatri","rapido","branch"]]}]},"variants":[{"context_id":"ef6ad4da7a263b60d8bad761f68723a159d155eb8542db8de0254cf56988c980","id":"7146027985267212288-test","override_id":"890f43a7096f1eecebf2c474b30ebd14c15a9b670408494214052a179ec601d4","overrides":{"ec_bundle":"https://assets.juspay.in/hyper/bundles/app/in.juspay.ec/3.16.18/android/v1-index_bundle.zip","ec_bundle_etag":"b864e0799319bb7a33c76fa3210bb352"},"variant_type":"EXPERIMENTAL"},{"context_id":"d5e4a08dbfed5db9c1676ebab29f1e1f51956de084a2c03c0f6d6704c793df2e","id":"7146027985267212288-control","override_id":"7e34b6da45dfb9ed09cb00290f9d28f3586aa9c93a4f4d1d1dcec2c67d37a529","overrides":{"ec_bundle":"default_str_ignore_this","ec_bundle_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}],"last_modified_by":"akash.singhania@juspay.in","chosen_variant":""}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7146027985267212288,'2023-12-28 06:43:58+05:30','akash.singhania@juspay.in','2023-12-28 06:49:35+05:30','Global Release - EC - Android','{ec_bundle,ec_bundle_etag}','INPROGRESS',50,'{"and":[{"==":[{"var":"os"},"android"]},{"in":[{"var":"clientId"},["goindigo","mplgaming","gameskraft","jungleerummy","howzat","slice","rummytime","winzo","jar","lazypay","playship","confirmtkt","purplle.com","myteam11","railyatri","rapido","branch"]]}]}','[{"context_id":"ef6ad4da7a263b60d8bad761f68723a159d155eb8542db8de0254cf56988c980","id":"7146027985267212288-test","override_id":"890f43a7096f1eecebf2c474b30ebd14c15a9b670408494214052a179ec601d4","overrides":{"ec_bundle":"https://assets.juspay.in/hyper/bundles/app/in.juspay.ec/3.16.18/android/v1-index_bundle.zip","ec_bundle_etag":"b864e0799319bb7a33c76fa3210bb352"},"variant_type":"EXPERIMENTAL"},{"context_id":"d5e4a08dbfed5db9c1676ebab29f1e1f51956de084a2c03c0f6d6704c793df2e","id":"7146027985267212288-control","override_id":"7e34b6da45dfb9ed09cb00290f9d28f3586aa9c93a4f4d1d1dcec2c67d37a529","overrides":{"ec_bundle":"default_str_ignore_this","ec_bundle_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}]','akash.singhania@juspay.in','')
b9b9b6b3-a680-4d87-824c-0ccbfb4b88c0	experiments	postgres	2024-08-21 22:46:30.329981	INSERT	\N	{"id":7138087705163210752,"created_at":"2023-12-06T08:52:08+05:30","created_by":"kartik.gajendra@juspay.in","last_modified":"2023-12-12T06:33:07+05:30","name":"mobius test 1","override_keys":["hyperpay_configuration","hyperpay_configuration_etag"],"status":"CONCLUDED","traffic_percentage":25,"context":{"and":[{"==":[{"var":"clientId"},"galactustest"]},{"==":[{"var":"os"},"android"]}]},"variants":[{"context_id":"332e9a95c0d2197d4a12db71d991ffd335b0093b75d0d77749c002c18e0a86cc","id":"7138087705163210752-test","override_id":"7ca1e210b75248afda6ae04f074f0dcc8a8f729f897e9f9113a3009e087c01b7","overrides":{"hyperpay_configuration":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/magicbrickspp/configuration/1.1/v1-configuration.zip","hyperpay_configuration_etag":"5f00e94dc26e6d96a95e4a2888b42f4d"},"variant_type":"EXPERIMENTAL"},{"context_id":"1826996a062c87f640b1f99d9580fc94a62e19982559e9a01696a8dec8191058","id":"7138087705163210752-control","override_id":"afb1f8291942d3276f2259b285f7a021e7ffb2a6bb4038b21976e3d59d6abde8","overrides":{"hyperpay_configuration":"default_str_ignore_this","hyperpay_configuration_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}],"last_modified_by":"mobius@juspay.in","chosen_variant":"7138087705163210752-control"}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7138087705163210752,'2023-12-06 08:52:08+05:30','kartik.gajendra@juspay.in','2023-12-12 06:33:07+05:30','mobius test 1','{hyperpay_configuration,hyperpay_configuration_etag}','CONCLUDED',25,'{"and":[{"==":[{"var":"clientId"},"galactustest"]},{"==":[{"var":"os"},"android"]}]}','[{"context_id":"332e9a95c0d2197d4a12db71d991ffd335b0093b75d0d77749c002c18e0a86cc","id":"7138087705163210752-test","override_id":"7ca1e210b75248afda6ae04f074f0dcc8a8f729f897e9f9113a3009e087c01b7","overrides":{"hyperpay_configuration":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/magicbrickspp/configuration/1.1/v1-configuration.zip","hyperpay_configuration_etag":"5f00e94dc26e6d96a95e4a2888b42f4d"},"variant_type":"EXPERIMENTAL"},{"context_id":"1826996a062c87f640b1f99d9580fc94a62e19982559e9a01696a8dec8191058","id":"7138087705163210752-control","override_id":"afb1f8291942d3276f2259b285f7a021e7ffb2a6bb4038b21976e3d59d6abde8","overrides":{"hyperpay_configuration":"default_str_ignore_this","hyperpay_configuration_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}]','mobius@juspay.in','7138087705163210752-control')
437c3a1d-de4f-4b61-90b6-a9e61441d5c0	experiments	postgres	2024-08-21 22:46:30.330583	INSERT	\N	{"id":7140228834636234752,"created_at":"2023-12-12T06:40:13+05:30","created_by":"aman.jain@juspay.in","last_modified":"2023-12-12T06:47:22+05:30","name":"test","override_keys":["arya_root"],"status":"CONCLUDED","traffic_percentage":20,"context":{"and":[{"==":[{"var":"os"},"android"]}]},"variants":[{"context_id":"dcd658aacbb4975f7c9348ed4c26e5bc0ec76d897fb42f654a6cfbdd50ce073e","id":"7140228834636234752-test","override_id":"334722013fece9c62a35f8ba84492df721d2dbc4af0102c3f3b588d79d0180f4","overrides":{"arya_root":"dwad"},"variant_type":"EXPERIMENTAL"},{"context_id":"dd9cb4f674ad0117190e738139f347cf493de37ff85dbe8e19b5b647bc3b6c4e","id":"7140228834636234752-control","override_id":"27bd63b346290504b882d0f5c336916fb99cb66f2a26d7f9f24482e1d6ea9c9c","overrides":{"arya_root":"default_str_ignore_this"},"variant_type":"CONTROL"}],"last_modified_by":"aman.jain@juspay.in","chosen_variant":"7140228834636234752-control"}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7140228834636234752,'2023-12-12 06:40:13+05:30','aman.jain@juspay.in','2023-12-12 06:47:22+05:30','test','{arya_root}','CONCLUDED',20,'{"and":[{"==":[{"var":"os"},"android"]}]}','[{"context_id":"dcd658aacbb4975f7c9348ed4c26e5bc0ec76d897fb42f654a6cfbdd50ce073e","id":"7140228834636234752-test","override_id":"334722013fece9c62a35f8ba84492df721d2dbc4af0102c3f3b588d79d0180f4","overrides":{"arya_root":"dwad"},"variant_type":"EXPERIMENTAL"},{"context_id":"dd9cb4f674ad0117190e738139f347cf493de37ff85dbe8e19b5b647bc3b6c4e","id":"7140228834636234752-control","override_id":"27bd63b346290504b882d0f5c336916fb99cb66f2a26d7f9f24482e1d6ea9c9c","overrides":{"arya_root":"default_str_ignore_this"},"variant_type":"CONTROL"}]','aman.jain@juspay.in','7140228834636234752-control')
94135003-9bdb-45dc-ae14-61f81ed58d62	experiments	postgres	2024-08-21 22:46:30.33103	INSERT	\N	{"id":7140253374879244288,"created_at":"2023-12-12T08:17:44+05:30","created_by":"aman.jain@juspay.in","last_modified":"2023-12-12T08:17:44+05:30","name":"dawdad","override_keys":["arya_root"],"status":"CREATED","traffic_percentage":0,"context":{"and":[{"==":[{"var":"os"},"android"]}]},"variants":[{"context_id":"c49528d88d4550af74eaea4ad42de49603941aa8b6fd235e0de1f165ebebac19","id":"7140253374879244288-test","override_id":"ffd0f8d439d613b932e136ee7627835a542fe506af89c87d186641779c5d31f5","overrides":{"arya_root":""},"variant_type":"EXPERIMENTAL"},{"context_id":"e8e8ee0a259fe80694014983234091645bbe4536fbee698524d9d0fb6db6ad1d","id":"7140253374879244288-control","override_id":"27bd63b346290504b882d0f5c336916fb99cb66f2a26d7f9f24482e1d6ea9c9c","overrides":{"arya_root":"default_str_ignore_this"},"variant_type":"CONTROL"}],"last_modified_by":"aman.jain@juspay.in","chosen_variant":""}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7140253374879244288,'2023-12-12 08:17:44+05:30','aman.jain@juspay.in','2023-12-12 08:17:44+05:30','dawdad','{arya_root}','CREATED',0,'{"and":[{"==":[{"var":"os"},"android"]}]}','[{"context_id":"c49528d88d4550af74eaea4ad42de49603941aa8b6fd235e0de1f165ebebac19","id":"7140253374879244288-test","override_id":"ffd0f8d439d613b932e136ee7627835a542fe506af89c87d186641779c5d31f5","overrides":{"arya_root":""},"variant_type":"EXPERIMENTAL"},{"context_id":"e8e8ee0a259fe80694014983234091645bbe4536fbee698524d9d0fb6db6ad1d","id":"7140253374879244288-control","override_id":"27bd63b346290504b882d0f5c336916fb99cb66f2a26d7f9f24482e1d6ea9c9c","overrides":{"arya_root":"default_str_ignore_this"},"variant_type":"CONTROL"}]','aman.jain@juspay.in','')
f464b46a-16e8-446a-bc3f-122e5b9fffc8	experiments	postgres	2024-08-21 22:46:30.33145	INSERT	\N	{"id":7140260528549928960,"created_at":"2023-12-12T08:46:09+05:30","created_by":"rohan.pawar@juspay.in","last_modified":"2023-12-13T08:06:12+05:30","name":"mobius test 2","override_keys":["hyperpay_configuration","hyperpay_configuration_etag"],"status":"CONCLUDED","traffic_percentage":50,"context":{"and":[{"==":[{"var":"clientId"},"galactustest"]},{"==":[{"var":"os"},"android"]}]},"variants":[{"context_id":"c6111ea9f2b0f445cc7c1b82bbf65efe3746df35a3e00841bace6e2c06c27196","id":"7140260528549928960-test","override_id":"7ca1e210b75248afda6ae04f074f0dcc8a8f729f897e9f9113a3009e087c01b7","overrides":{"hyperpay_configuration":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/magicbrickspp/configuration/1.1/v1-configuration.zip","hyperpay_configuration_etag":"5f00e94dc26e6d96a95e4a2888b42f4d"},"variant_type":"EXPERIMENTAL"},{"context_id":"4af6321459006311ec902f192ac9749b2fe8e070642f51086a65ea660fc2f6b8","id":"7140260528549928960-control","override_id":"afb1f8291942d3276f2259b285f7a021e7ffb2a6bb4038b21976e3d59d6abde8","overrides":{"hyperpay_configuration":"default_str_ignore_this","hyperpay_configuration_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}],"last_modified_by":"mobius@juspay.in","chosen_variant":"7140260528549928960-control"}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7140260528549928960,'2023-12-12 08:46:09+05:30','rohan.pawar@juspay.in','2023-12-13 08:06:12+05:30','mobius test 2','{hyperpay_configuration,hyperpay_configuration_etag}','CONCLUDED',50,'{"and":[{"==":[{"var":"clientId"},"galactustest"]},{"==":[{"var":"os"},"android"]}]}','[{"context_id":"c6111ea9f2b0f445cc7c1b82bbf65efe3746df35a3e00841bace6e2c06c27196","id":"7140260528549928960-test","override_id":"7ca1e210b75248afda6ae04f074f0dcc8a8f729f897e9f9113a3009e087c01b7","overrides":{"hyperpay_configuration":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/magicbrickspp/configuration/1.1/v1-configuration.zip","hyperpay_configuration_etag":"5f00e94dc26e6d96a95e4a2888b42f4d"},"variant_type":"EXPERIMENTAL"},{"context_id":"4af6321459006311ec902f192ac9749b2fe8e070642f51086a65ea660fc2f6b8","id":"7140260528549928960-control","override_id":"afb1f8291942d3276f2259b285f7a021e7ffb2a6bb4038b21976e3d59d6abde8","overrides":{"hyperpay_configuration":"default_str_ignore_this","hyperpay_configuration_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}]','mobius@juspay.in','7140260528549928960-control')
d7670523-a7b7-4f3a-92ca-0569d7f8dc9e	experiments	postgres	2024-08-21 22:46:30.331851	INSERT	\N	{"id":7140674404129603584,"created_at":"2023-12-13T12:10:45+05:30","created_by":"rohan.pawar@juspay.in","last_modified":"2023-12-14T08:05:38+05:30","name":"mobius test 3","override_keys":["hyperpay_configuration","hyperpay_configuration_etag"],"status":"CONCLUDED","traffic_percentage":50,"context":{"and":[{"==":[{"var":"clientId"},"galactustest"]},{"==":[{"var":"os"},"android"]}]},"variants":[{"context_id":"0fc1b55af5432ef9ab06a7bfa0b38bd4030d482fa76f54be660a0645dcf61561","id":"7140674404129603584-test","override_id":"7ca1e210b75248afda6ae04f074f0dcc8a8f729f897e9f9113a3009e087c01b7","overrides":{"hyperpay_configuration":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/magicbrickspp/configuration/1.1/v1-configuration.zip","hyperpay_configuration_etag":"5f00e94dc26e6d96a95e4a2888b42f4d"},"variant_type":"EXPERIMENTAL"},{"context_id":"262d7f53ab9c6cbb0c27673704b42220a8465d214de7547e2aa296efbf04d99d","id":"7140674404129603584-control","override_id":"afb1f8291942d3276f2259b285f7a021e7ffb2a6bb4038b21976e3d59d6abde8","overrides":{"hyperpay_configuration":"default_str_ignore_this","hyperpay_configuration_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}],"last_modified_by":"mobius@juspay.in","chosen_variant":"7140674404129603584-test"}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7140674404129603584,'2023-12-13 12:10:45+05:30','rohan.pawar@juspay.in','2023-12-14 08:05:38+05:30','mobius test 3','{hyperpay_configuration,hyperpay_configuration_etag}','CONCLUDED',50,'{"and":[{"==":[{"var":"clientId"},"galactustest"]},{"==":[{"var":"os"},"android"]}]}','[{"context_id":"0fc1b55af5432ef9ab06a7bfa0b38bd4030d482fa76f54be660a0645dcf61561","id":"7140674404129603584-test","override_id":"7ca1e210b75248afda6ae04f074f0dcc8a8f729f897e9f9113a3009e087c01b7","overrides":{"hyperpay_configuration":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/magicbrickspp/configuration/1.1/v1-configuration.zip","hyperpay_configuration_etag":"5f00e94dc26e6d96a95e4a2888b42f4d"},"variant_type":"EXPERIMENTAL"},{"context_id":"262d7f53ab9c6cbb0c27673704b42220a8465d214de7547e2aa296efbf04d99d","id":"7140674404129603584-control","override_id":"afb1f8291942d3276f2259b285f7a021e7ffb2a6bb4038b21976e3d59d6abde8","overrides":{"hyperpay_configuration":"default_str_ignore_this","hyperpay_configuration_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}]','mobius@juspay.in','7140674404129603584-test')
e9305b86-6176-41f2-8d32-33cb61c43797	experiments	postgres	2024-08-21 22:46:30.33234	INSERT	\N	{"id":7140416218094780416,"created_at":"2023-12-12T19:04:49+05:30","created_by":"aman.jain@juspay.in","last_modified":"2023-12-12T19:05:10+05:30","name":"testnew","override_keys":[],"status":"CONCLUDED","traffic_percentage":0,"context":{"and":[{"==":[{"var":"clientId"},"studio"]},{"==":[{"var":"os"},"android"]}]},"variants":[{"context_id":"3cd5bee206d2979f56fa4edefb339a179c7d77cf3240e5e67ba3f11c78ad35f7","id":"7140416218094780416-test","override_id":"af1349b9f5f9a1a6a0404dea36dcc9499bcb25c9adc112b7cc9a93cae41f3262","overrides":{},"variant_type":"EXPERIMENTAL"},{"context_id":"c37b61c013acc4a5ef4748a5e97678bb681c3a3ee2671876689ef5124a46667f","id":"7140416218094780416-control","override_id":"af1349b9f5f9a1a6a0404dea36dcc9499bcb25c9adc112b7cc9a93cae41f3262","overrides":{},"variant_type":"CONTROL"}],"last_modified_by":"aman.jain@juspay.in","chosen_variant":"7140416218094780416-test"}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7140416218094780416,'2023-12-12 19:04:49+05:30','aman.jain@juspay.in','2023-12-12 19:05:10+05:30','testnew','{}','CONCLUDED',0,'{"and":[{"==":[{"var":"clientId"},"studio"]},{"==":[{"var":"os"},"android"]}]}','[{"context_id":"3cd5bee206d2979f56fa4edefb339a179c7d77cf3240e5e67ba3f11c78ad35f7","id":"7140416218094780416-test","override_id":"af1349b9f5f9a1a6a0404dea36dcc9499bcb25c9adc112b7cc9a93cae41f3262","overrides":{},"variant_type":"EXPERIMENTAL"},{"context_id":"c37b61c013acc4a5ef4748a5e97678bb681c3a3ee2671876689ef5124a46667f","id":"7140416218094780416-control","override_id":"af1349b9f5f9a1a6a0404dea36dcc9499bcb25c9adc112b7cc9a93cae41f3262","overrides":{},"variant_type":"CONTROL"}]','aman.jain@juspay.in','7140416218094780416-test')
5cee9c6e-8e48-4674-8932-28121823b0a9	experiments	postgres	2024-08-21 22:46:30.332673	INSERT	\N	{"id":7140290618252574720,"created_at":"2023-12-12T10:45:43+05:30","created_by":"aman.jain@juspay.in","last_modified":"2023-12-13T09:14:21+05:30","name":"TestExp","override_keys":["consumer_bundle_etag","consumer_bundle"],"status":"CREATED","traffic_percentage":0,"context":{"and":[{"==":[{"var":"clientId"},"zee5"]},{"==":[{"var":"os"},"android"]}]},"variants":[{"context_id":"ef48186d8d6b64f0e5d7d8cfcfd6f1be2a5ec574e6bede4b956046bdab30294a","id":"7140290618252574720-test","override_id":"e56b74844f6aab978dc958d0b4e071c2e409d917e6c93194719211bddf8109b5","overrides":{"consumer_bundle":"https://assets.juspay.in/hyper/bundles/app/in.juspay.hyperpay/3.72.4-release-20231206.1/android/v1-index_bundle.zip","consumer_bundle_etag":"263a54c0c9c7693218f8ed63d5c942e2"},"variant_type":"EXPERIMENTAL"},{"context_id":"b8abee3e1f7938ea51fd5c88e7a70e0b558cda8bafdb4733d2ebdd766b08d6fa","id":"7140290618252574720-control","override_id":"34e82ba678d01bbded07536817e2c7c20d13cba03a8a8b8bdd9dad0fa3e92fb5","overrides":{"consumer_bundle":"default_str_ignore_this","consumer_bundle_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}],"last_modified_by":"aman.jain@juspay.in","chosen_variant":""}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7140290618252574720,'2023-12-12 10:45:43+05:30','aman.jain@juspay.in','2023-12-13 09:14:21+05:30','TestExp','{consumer_bundle_etag,consumer_bundle}','CREATED',0,'{"and":[{"==":[{"var":"clientId"},"zee5"]},{"==":[{"var":"os"},"android"]}]}','[{"context_id":"ef48186d8d6b64f0e5d7d8cfcfd6f1be2a5ec574e6bede4b956046bdab30294a","id":"7140290618252574720-test","override_id":"e56b74844f6aab978dc958d0b4e071c2e409d917e6c93194719211bddf8109b5","overrides":{"consumer_bundle":"https://assets.juspay.in/hyper/bundles/app/in.juspay.hyperpay/3.72.4-release-20231206.1/android/v1-index_bundle.zip","consumer_bundle_etag":"263a54c0c9c7693218f8ed63d5c942e2"},"variant_type":"EXPERIMENTAL"},{"context_id":"b8abee3e1f7938ea51fd5c88e7a70e0b558cda8bafdb4733d2ebdd766b08d6fa","id":"7140290618252574720-control","override_id":"34e82ba678d01bbded07536817e2c7c20d13cba03a8a8b8bdd9dad0fa3e92fb5","overrides":{"consumer_bundle":"default_str_ignore_this","consumer_bundle_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}]','aman.jain@juspay.in','')
f92ab261-0097-4c4e-ace1-58c55af0130e	experiments	postgres	2024-08-21 22:46:30.333068	INSERT	\N	{"id":7140662152812986368,"created_at":"2023-12-13T11:22:04+05:30","created_by":"aman.jain@juspay.in","last_modified":"2023-12-13T11:23:03+05:30","name":"testexp2","override_keys":["consumer_root","arya_root"],"status":"CREATED","traffic_percentage":0,"context":{"and":[{"==":[{"var":"clientId"},"studio"]},{"==":[{"var":"os"},"android"]}]},"variants":[{"context_id":"592bcf66c9ff8873dcdba6e95579a74c11c98e4185f8f909c0aa1b012310092e","id":"7140662152812986368-test","override_id":"1145cd058747c45bfc52369bcc3ebd5eec08b008eb7c0b6d6a999bd009acd863","overrides":{"arya_root":"adadadad","consumer_root":"test"},"variant_type":"EXPERIMENTAL"},{"context_id":"38116b8b23d457f0859241cd730047a05e164c81994d2a3654d0b0eb4255582b","id":"7140662152812986368-control","override_id":"8866d1d9d2df76dc2ee01a4caba29bbc83ced89d2c8f7c4710d118fab5b3ded9","overrides":{"arya_root":"default_str_ignore_this","consumer_root":"default_str_ignore_this"},"variant_type":"CONTROL"}],"last_modified_by":"aman.jain@juspay.in","chosen_variant":""}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7140662152812986368,'2023-12-13 11:22:04+05:30','aman.jain@juspay.in','2023-12-13 11:23:03+05:30','testexp2','{consumer_root,arya_root}','CREATED',0,'{"and":[{"==":[{"var":"clientId"},"studio"]},{"==":[{"var":"os"},"android"]}]}','[{"context_id":"592bcf66c9ff8873dcdba6e95579a74c11c98e4185f8f909c0aa1b012310092e","id":"7140662152812986368-test","override_id":"1145cd058747c45bfc52369bcc3ebd5eec08b008eb7c0b6d6a999bd009acd863","overrides":{"arya_root":"adadadad","consumer_root":"test"},"variant_type":"EXPERIMENTAL"},{"context_id":"38116b8b23d457f0859241cd730047a05e164c81994d2a3654d0b0eb4255582b","id":"7140662152812986368-control","override_id":"8866d1d9d2df76dc2ee01a4caba29bbc83ced89d2c8f7c4710d118fab5b3ded9","overrides":{"arya_root":"default_str_ignore_this","consumer_root":"default_str_ignore_this"},"variant_type":"CONTROL"}]','aman.jain@juspay.in','')
707bb24c-d17f-4c86-bc04-3e945f89dd42	experiments	postgres	2024-08-21 22:46:30.333438	INSERT	\N	{"id":7155192075930451968,"created_at":"2024-01-22T13:38:48+05:30","created_by":"rohan.pawar@juspay.in","last_modified":"2024-03-12T22:46:17+05:30","name":"mobius test 3","override_keys":["hyperpay_configuration_etag","hyperpay_configuration"],"status":"CONCLUDED","traffic_percentage":50,"context":{"and":[{"==":[{"var":"clientId"},"galactustest"]},{"==":[{"var":"os"},"android"]}]},"variants":[{"context_id":"931d479bf4533a6aff6b5c41d6a5a576354adcb37686c12c0331c9a7fc682dbc","id":"7155192075930451968-test","override_id":"7ca1e210b75248afda6ae04f074f0dcc8a8f729f897e9f9113a3009e087c01b7","overrides":{"hyperpay_configuration":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/magicbrickspp/configuration/1.1/v1-configuration.zip","hyperpay_configuration_etag":"5f00e94dc26e6d96a95e4a2888b42f4d"},"variant_type":"EXPERIMENTAL"},{"context_id":"e3f722b94963debafbf7afb729ca9ab7a85e050249f26a06a8f18dcff0302ae7","id":"7155192075930451968-control","override_id":"afb1f8291942d3276f2259b285f7a021e7ffb2a6bb4038b21976e3d59d6abde8","overrides":{"hyperpay_configuration":"default_str_ignore_this","hyperpay_configuration_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}],"last_modified_by":"mobius@juspay.in","chosen_variant":"7155192075930451968-test"}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7155192075930451968,'2024-01-22 13:38:48+05:30','rohan.pawar@juspay.in','2024-03-12 22:46:17+05:30','mobius test 3','{hyperpay_configuration_etag,hyperpay_configuration}','CONCLUDED',50,'{"and":[{"==":[{"var":"clientId"},"galactustest"]},{"==":[{"var":"os"},"android"]}]}','[{"context_id":"931d479bf4533a6aff6b5c41d6a5a576354adcb37686c12c0331c9a7fc682dbc","id":"7155192075930451968-test","override_id":"7ca1e210b75248afda6ae04f074f0dcc8a8f729f897e9f9113a3009e087c01b7","overrides":{"hyperpay_configuration":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/magicbrickspp/configuration/1.1/v1-configuration.zip","hyperpay_configuration_etag":"5f00e94dc26e6d96a95e4a2888b42f4d"},"variant_type":"EXPERIMENTAL"},{"context_id":"e3f722b94963debafbf7afb729ca9ab7a85e050249f26a06a8f18dcff0302ae7","id":"7155192075930451968-control","override_id":"afb1f8291942d3276f2259b285f7a021e7ffb2a6bb4038b21976e3d59d6abde8","overrides":{"hyperpay_configuration":"default_str_ignore_this","hyperpay_configuration_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}]','mobius@juspay.in','7155192075930451968-test')
59bea7b8-42f9-4f91-bd73-6e85e8db9f51	experiments	postgres	2024-08-21 22:46:30.333851	INSERT	\N	{"id":7202264306568204288,"created_at":"2024-05-31T11:07:02+05:30","created_by":"deepesh.maheshwari@juspay.in","last_modified":"2024-05-31T11:07:10+05:30","name":"EC and UPI intent update","override_keys":["ec_bundle_etag","upiintent_bundle_etag","upiintent_bundle","ec_bundle"],"status":"CONCLUDED","traffic_percentage":0,"context":{"and":[{"==":[{"var":"clientId"},"hdfcmaster"]},{"==":[{"var":"os"},"android"]}]},"variants":[{"context_id":"2ac1532160da0197dcc27df437225236932ff614809309752e0b26cc61a92f3e","id":"7202264306568204288-test","override_id":"388e2f29ff74d296d3fd033970bd79b8fcd587f57750cd2113b39f51d2f579d2","overrides":{"ec_bundle":"https://assets.juspay.in/hyper/bundles/app/in.juspay.ec/3.29.4/android/v1-index_bundle.zip","ec_bundle_etag":"6e673566ec9d34425bb9fadcac6e1927","upiintent_bundle":"https://assets.juspay.in/hyper/bundles/app/in.juspay.upiintent/4.9.9/android/v1-index_bundle.zip","upiintent_bundle_etag":"655d1b861dde675e69e6fe19dcde9804"},"variant_type":"EXPERIMENTAL"},{"context_id":"6b5f2a3761e248a859b6603773674065275f29ec748a4fd9b26c3666bc4b3f56","id":"7202264306568204288-control","override_id":"e0f67abcfcb928bb6e84e0a14b8b7a9c44cb96c6e14b8672fab28b0022bf6a23","overrides":{"ec_bundle":"default_str_ignore_this","ec_bundle_etag":"default_str_ignore_this","upiintent_bundle":"default_str_ignore_this","upiintent_bundle_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}],"last_modified_by":"deepesh.maheshwari@juspay.in","chosen_variant":"7202264306568204288-test"}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7202264306568204288,'2024-05-31 11:07:02+05:30','deepesh.maheshwari@juspay.in','2024-05-31 11:07:10+05:30','EC and UPI intent update','{ec_bundle_etag,upiintent_bundle_etag,upiintent_bundle,ec_bundle}','CONCLUDED',0,'{"and":[{"==":[{"var":"clientId"},"hdfcmaster"]},{"==":[{"var":"os"},"android"]}]}','[{"context_id":"2ac1532160da0197dcc27df437225236932ff614809309752e0b26cc61a92f3e","id":"7202264306568204288-test","override_id":"388e2f29ff74d296d3fd033970bd79b8fcd587f57750cd2113b39f51d2f579d2","overrides":{"ec_bundle":"https://assets.juspay.in/hyper/bundles/app/in.juspay.ec/3.29.4/android/v1-index_bundle.zip","ec_bundle_etag":"6e673566ec9d34425bb9fadcac6e1927","upiintent_bundle":"https://assets.juspay.in/hyper/bundles/app/in.juspay.upiintent/4.9.9/android/v1-index_bundle.zip","upiintent_bundle_etag":"655d1b861dde675e69e6fe19dcde9804"},"variant_type":"EXPERIMENTAL"},{"context_id":"6b5f2a3761e248a859b6603773674065275f29ec748a4fd9b26c3666bc4b3f56","id":"7202264306568204288-control","override_id":"e0f67abcfcb928bb6e84e0a14b8b7a9c44cb96c6e14b8672fab28b0022bf6a23","overrides":{"ec_bundle":"default_str_ignore_this","ec_bundle_etag":"default_str_ignore_this","upiintent_bundle":"default_str_ignore_this","upiintent_bundle_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}]','deepesh.maheshwari@juspay.in','7202264306568204288-test')
4bc6e725-174e-4fea-8987-8647a962d8c4	experiments	postgres	2024-08-21 22:46:30.334327	INSERT	\N	{"id":7202264873780318208,"created_at":"2024-05-31T11:09:17+05:30","created_by":"deepesh.maheshwari@juspay.in","last_modified":"2024-05-31T11:09:42+05:30","name":"EC and UPI intent update","override_keys":["ec_bundle","upiintent_bundle_etag","ec_bundle_etag","upiintent_bundle"],"status":"CONCLUDED","traffic_percentage":0,"context":{"and":[{"==":[{"var":"clientId"},"hdfcmaster"]},{"==":[{"var":"os"},"ios"]}]},"variants":[{"context_id":"0e688b11e09bb37396990355ae6e81999ffbde5f0eaf3df847ec8dc4150440f6","id":"7202264873780318208-test","override_id":"655b17f9f2c7df1481bf63505909c4874284a594daaa594f63477e310bb8fcc1","overrides":{"ec_bundle":"https://assets.juspay.in/hyper/bundles/app/in.juspay.ec/3.29.4/ios/v1-index_bundle.jsa","ec_bundle_etag":"230535f9eed9e9e977427d45e217de33","upiintent_bundle":"https://assets.juspay.in/hyper/bundles/app/in.juspay.upiintent/4.9.9/ios/v1-index_bundle.jsa","upiintent_bundle_etag":"d0838b52ce1073a6479f6af97c90167c"},"variant_type":"EXPERIMENTAL"},{"context_id":"c7b39a1865d73cf38ef645b3996425d4613ca3eab8bbf4312c6177d10c3bd3f5","id":"7202264873780318208-control","override_id":"e0f67abcfcb928bb6e84e0a14b8b7a9c44cb96c6e14b8672fab28b0022bf6a23","overrides":{"ec_bundle":"default_str_ignore_this","ec_bundle_etag":"default_str_ignore_this","upiintent_bundle":"default_str_ignore_this","upiintent_bundle_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}],"last_modified_by":"deepesh.maheshwari@juspay.in","chosen_variant":"7202264873780318208-test"}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7202264873780318208,'2024-05-31 11:09:17+05:30','deepesh.maheshwari@juspay.in','2024-05-31 11:09:42+05:30','EC and UPI intent update','{ec_bundle,upiintent_bundle_etag,ec_bundle_etag,upiintent_bundle}','CONCLUDED',0,'{"and":[{"==":[{"var":"clientId"},"hdfcmaster"]},{"==":[{"var":"os"},"ios"]}]}','[{"context_id":"0e688b11e09bb37396990355ae6e81999ffbde5f0eaf3df847ec8dc4150440f6","id":"7202264873780318208-test","override_id":"655b17f9f2c7df1481bf63505909c4874284a594daaa594f63477e310bb8fcc1","overrides":{"ec_bundle":"https://assets.juspay.in/hyper/bundles/app/in.juspay.ec/3.29.4/ios/v1-index_bundle.jsa","ec_bundle_etag":"230535f9eed9e9e977427d45e217de33","upiintent_bundle":"https://assets.juspay.in/hyper/bundles/app/in.juspay.upiintent/4.9.9/ios/v1-index_bundle.jsa","upiintent_bundle_etag":"d0838b52ce1073a6479f6af97c90167c"},"variant_type":"EXPERIMENTAL"},{"context_id":"c7b39a1865d73cf38ef645b3996425d4613ca3eab8bbf4312c6177d10c3bd3f5","id":"7202264873780318208-control","override_id":"e0f67abcfcb928bb6e84e0a14b8b7a9c44cb96c6e14b8672fab28b0022bf6a23","overrides":{"ec_bundle":"default_str_ignore_this","ec_bundle_etag":"default_str_ignore_this","upiintent_bundle":"default_str_ignore_this","upiintent_bundle_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}]','deepesh.maheshwari@juspay.in','7202264873780318208-test')
dc929e79-fbd0-4523-a6fc-fa33b6964f15	experiments	postgres	2024-08-21 22:46:30.334751	INSERT	\N	{"id":7214617329714196480,"created_at":"2024-07-04T13:13:32+05:30","created_by":"mounika.reddy@juspay.in","last_modified":"2024-07-04T13:13:39+05:30","name":"hsbc loader update","override_keys":["hyperos_placeholder_loader"],"status":"CONCLUDED","traffic_percentage":0,"context":{"and":[{"==":[{"var":"clientId"},"hsbc"]},{"==":[{"var":"os"},"web"]}]},"variants":[{"context_id":"416841469d118df4f5075a04e4726c538fd9c71b6f31d55cea2e89126a922f75","id":"7214617329714196480-test","override_id":"7c0a28beed2c4797fd1442fc680c57f544d05e1e3b158aaa144c5685ef8b4735","overrides":{"hyperos_placeholder_loader":"https://assets.juspay.in/hyper/bundles/app/loader/hsbc/1.0.6/web/loader.js"},"variant_type":"EXPERIMENTAL"},{"context_id":"2311d007b87a4e89f38da0fbda00929c5e6129413fb55982e493c29e6e965bd3","id":"7214617329714196480-control","override_id":"87a8752ec7ee4bcdf4ecef07658e9fd33267737ff6f16c60afe9c5b254edb398","overrides":{"hyperos_placeholder_loader":"default_str_ignore_this"},"variant_type":"CONTROL"}],"last_modified_by":"mounika.reddy@juspay.in","chosen_variant":"7214617329714196480-test"}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7214617329714196480,'2024-07-04 13:13:32+05:30','mounika.reddy@juspay.in','2024-07-04 13:13:39+05:30','hsbc loader update','{hyperos_placeholder_loader}','CONCLUDED',0,'{"and":[{"==":[{"var":"clientId"},"hsbc"]},{"==":[{"var":"os"},"web"]}]}','[{"context_id":"416841469d118df4f5075a04e4726c538fd9c71b6f31d55cea2e89126a922f75","id":"7214617329714196480-test","override_id":"7c0a28beed2c4797fd1442fc680c57f544d05e1e3b158aaa144c5685ef8b4735","overrides":{"hyperos_placeholder_loader":"https://assets.juspay.in/hyper/bundles/app/loader/hsbc/1.0.6/web/loader.js"},"variant_type":"EXPERIMENTAL"},{"context_id":"2311d007b87a4e89f38da0fbda00929c5e6129413fb55982e493c29e6e965bd3","id":"7214617329714196480-control","override_id":"87a8752ec7ee4bcdf4ecef07658e9fd33267737ff6f16c60afe9c5b254edb398","overrides":{"hyperos_placeholder_loader":"default_str_ignore_this"},"variant_type":"CONTROL"}]','mounika.reddy@juspay.in','7214617329714196480-test')
c1e3e24b-0baa-46ed-a5ce-63751acaea7e	experiments	postgres	2024-08-21 22:46:30.335162	INSERT	\N	{"id":7217465364171448320,"created_at":"2024-07-12T09:50:37+05:30","created_by":"shubhranshu.sanjeev@juspay.in","last_modified":"2024-07-12T09:52:39+05:30","name":"test-experiment-nnn","override_keys":["godel_config","godel_config_etag"],"status":"INPROGRESS","traffic_percentage":0,"context":{"and":[{"==":[{"var":"clientId"},"galactustest"]},{"==":[{"var":"scope"},"release"]},{"==":[{"var":"os"},"android"]}]},"variants":[{"context_id":"514540b487679173a51fe4e01c483fe1d6d3a9b931bdee2c0237dc6d9efbcf3b","id":"7217465364171448320-test","override_id":"1e79ffeded160b3c5e6250e1247116071c7b8aa73bab1119b532dd23ed168383","overrides":{"godel_config":"https://assets.juspay.in/hyper/bundles/config/in.juspay.godel/1.2.57/v1-config.zip","godel_config_etag":"77fd03350702e025c432e66842cd7521"},"variant_type":"EXPERIMENTAL"},{"context_id":"3d660c8f7ad00d1578269158aebc9686bd0754e79696e08945cce10664bbbcb7","id":"7217465364171448320-control","override_id":"81159838966447ace3e7be99a17e7b1a2e6a7ba2ecc98da4899f0e56f4539d59","overrides":{"godel_config":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/bbdaily/ios/release/config.json","godel_config_etag":"d574dfb82a2e851145a0c4070ef0d6e4"},"variant_type":"CONTROL"}],"last_modified_by":"shubhranshu.sanjeev@juspay.in","chosen_variant":""}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7217465364171448320,'2024-07-12 09:50:37+05:30','shubhranshu.sanjeev@juspay.in','2024-07-12 09:52:39+05:30','test-experiment-nnn','{godel_config,godel_config_etag}','INPROGRESS',0,'{"and":[{"==":[{"var":"clientId"},"galactustest"]},{"==":[{"var":"scope"},"release"]},{"==":[{"var":"os"},"android"]}]}','[{"context_id":"514540b487679173a51fe4e01c483fe1d6d3a9b931bdee2c0237dc6d9efbcf3b","id":"7217465364171448320-test","override_id":"1e79ffeded160b3c5e6250e1247116071c7b8aa73bab1119b532dd23ed168383","overrides":{"godel_config":"https://assets.juspay.in/hyper/bundles/config/in.juspay.godel/1.2.57/v1-config.zip","godel_config_etag":"77fd03350702e025c432e66842cd7521"},"variant_type":"EXPERIMENTAL"},{"context_id":"3d660c8f7ad00d1578269158aebc9686bd0754e79696e08945cce10664bbbcb7","id":"7217465364171448320-control","override_id":"81159838966447ace3e7be99a17e7b1a2e6a7ba2ecc98da4899f0e56f4539d59","overrides":{"godel_config":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/bbdaily/ios/release/config.json","godel_config_etag":"d574dfb82a2e851145a0c4070ef0d6e4"},"variant_type":"CONTROL"}]','shubhranshu.sanjeev@juspay.in','')
b2160a14-d7cd-4d16-92ba-fce7c0ef9f33	experiments	postgres	2024-08-21 22:46:30.335587	INSERT	\N	{"id":7189249119334772736,"created_at":"2024-04-25T13:09:20+05:30","created_by":"shubhranshu.sanjeev@juspay.in","last_modified":"2024-07-12T10:15:54+05:30","name":"dummy-experiment-2","override_keys":["arya_bundle","godel_bundle","godel_bundle_etag","arya_bundle_etag"],"status":"CREATED","traffic_percentage":0,"context":{"and":[{"==":[{"var":"os"},"android"]},{"==":[{"var":"clientId"},"galactustest"]}]},"variants":[{"context_id":"c6042f4af6bccf484576d12b0070b89645bb1e9b9680ce532068c010fb49e534","id":"7189249119334772736-test","override_id":"e359c27272515976eba9f7ba0b72c7a52a6eef6d4280ca3bbf3d49679773589c","overrides":{"arya_bundle":"default_str_ignore_this","arya_bundle_etag":"default_str_ignore","godel_bundle":"https://assets.juspay.in/hyper/bundles/config/in.juspay.godel/1.2.57/v1-config.zip","godel_bundle_etag":"77fd03350702e025c432e66842cd7521"},"variant_type":"EXPERIMENTAL"},{"context_id":"0ee3ec6af9d894d17b4a03c6fe574bf02a36bed43fea5b64a109ca79065050c5","id":"7189249119334772736-control","override_id":"1ee28fb8dfb4c808ab76ccee332c43d2378c6b4b1358af0f6bf2a42e38fada85","overrides":{"arya_bundle":"default_str_ignore_this","arya_bundle_etag":"default_str_ignore_this","godel_bundle":"default_str_ignore_this","godel_bundle_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}],"last_modified_by":"shubhranshu.sanjeev@juspay.in","chosen_variant":""}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7189249119334772736,'2024-04-25 13:09:20+05:30','shubhranshu.sanjeev@juspay.in','2024-07-12 10:15:54+05:30','dummy-experiment-2','{arya_bundle,godel_bundle,godel_bundle_etag,arya_bundle_etag}','CREATED',0,'{"and":[{"==":[{"var":"os"},"android"]},{"==":[{"var":"clientId"},"galactustest"]}]}','[{"context_id":"c6042f4af6bccf484576d12b0070b89645bb1e9b9680ce532068c010fb49e534","id":"7189249119334772736-test","override_id":"e359c27272515976eba9f7ba0b72c7a52a6eef6d4280ca3bbf3d49679773589c","overrides":{"arya_bundle":"default_str_ignore_this","arya_bundle_etag":"default_str_ignore","godel_bundle":"https://assets.juspay.in/hyper/bundles/config/in.juspay.godel/1.2.57/v1-config.zip","godel_bundle_etag":"77fd03350702e025c432e66842cd7521"},"variant_type":"EXPERIMENTAL"},{"context_id":"0ee3ec6af9d894d17b4a03c6fe574bf02a36bed43fea5b64a109ca79065050c5","id":"7189249119334772736-control","override_id":"1ee28fb8dfb4c808ab76ccee332c43d2378c6b4b1358af0f6bf2a42e38fada85","overrides":{"arya_bundle":"default_str_ignore_this","arya_bundle_etag":"default_str_ignore_this","godel_bundle":"default_str_ignore_this","godel_bundle_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}]','shubhranshu.sanjeev@juspay.in','')
3a4c8adb-a6d2-4d13-b23c-ff62ceccb929	experiments	postgres	2024-08-21 22:46:30.335989	INSERT	\N	{"id":7218596008003162112,"created_at":"2024-07-15T12:43:23+05:30","created_by":"prathamesh.rane@juspay.in","last_modified":"2024-07-15T12:43:45+05:30","name":"hdfcmaster enablement for sbx new upi change","override_keys":["hyperpay_bundle","hyperpay_bundle_etag"],"status":"CONCLUDED","traffic_percentage":0,"context":{"and":[{"==":[{"var":"clientId"},"hdfcmaster"]},{"==":[{"var":"os"},"web"]},{"==":[{"var":"resellerId"},"hdfc_reseller"]}]},"variants":[{"context_id":"f6b231e8bd66f2b834a2da7bc6e48d3cce15b3879f82be8cd9a992e094a10608","id":"7218596008003162112-test","override_id":"4203039ae6fe427da6adbb94bd5c6ee206d139776e1c71f2e1405ad251396637","overrides":{"hyperpay_bundle":"https://assets.juspay.in/hyper/bundles/app/in.juspay.hyperpay/4.24.3-release-20240422.3/web/prod-split_index.js","hyperpay_bundle_etag":"8ec671797de055489d4acbba008befc5"},"variant_type":"EXPERIMENTAL"},{"context_id":"80fb16c6247f183295961ae9b909e8f984daeb564cceba08dfc34eadecd0a990","id":"7218596008003162112-control","override_id":"2ff2d9f749cdcaec11f9798c14f325a7e467d8bab0468fa577ce491d3563fbe1","overrides":{"hyperpay_bundle":"default_str_ignore_this","hyperpay_bundle_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}],"last_modified_by":"prathamesh.rane@juspay.in","chosen_variant":"7218596008003162112-test"}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7218596008003162112,'2024-07-15 12:43:23+05:30','prathamesh.rane@juspay.in','2024-07-15 12:43:45+05:30','hdfcmaster enablement for sbx new upi change','{hyperpay_bundle,hyperpay_bundle_etag}','CONCLUDED',0,'{"and":[{"==":[{"var":"clientId"},"hdfcmaster"]},{"==":[{"var":"os"},"web"]},{"==":[{"var":"resellerId"},"hdfc_reseller"]}]}','[{"context_id":"f6b231e8bd66f2b834a2da7bc6e48d3cce15b3879f82be8cd9a992e094a10608","id":"7218596008003162112-test","override_id":"4203039ae6fe427da6adbb94bd5c6ee206d139776e1c71f2e1405ad251396637","overrides":{"hyperpay_bundle":"https://assets.juspay.in/hyper/bundles/app/in.juspay.hyperpay/4.24.3-release-20240422.3/web/prod-split_index.js","hyperpay_bundle_etag":"8ec671797de055489d4acbba008befc5"},"variant_type":"EXPERIMENTAL"},{"context_id":"80fb16c6247f183295961ae9b909e8f984daeb564cceba08dfc34eadecd0a990","id":"7218596008003162112-control","override_id":"2ff2d9f749cdcaec11f9798c14f325a7e467d8bab0468fa577ce491d3563fbe1","overrides":{"hyperpay_bundle":"default_str_ignore_this","hyperpay_bundle_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}]','prathamesh.rane@juspay.in','7218596008003162112-test')
8c9a82e3-bcaf-4d3a-9c7b-1467f8862284	experiments	postgres	2024-08-21 22:46:30.33639	INSERT	\N	{"id":7206198466018328576,"created_at":"2024-06-11T07:39:59+05:30","created_by":"mounika.reddy@juspay.in","last_modified":"2024-08-07T16:20:21+05:30","name":"hdfc hyperpay SBX","override_keys":["escrow_bundle","hyperpay_bundle_etag","upiintent_bundle_etag","ec_bundle","escrow_bundle_etag","hyperpay_bundle","ec_bundle_etag","upiintent_bundle"],"status":"CREATED","traffic_percentage":0,"context":{"and":[{"==":[{"var":"clientId"},"hdfcmaster"]},{"==":[{"var":"os"},"web"]},{"==":[{"var":"resellerId"},"hdfc_reseller"]}]},"variants":[{"context_id":"142023a66eadb6e0effb613b9b9c0292fb08230d609bd406a5a01c7f789ee891","id":"7206198466018328576-test","override_id":"7697748dedfd674e95b8848609a910cb152d49f89ac32d2102db5a3cba1ad107","overrides":{"ec_bundle":"https://assets.juspay.in/hyper/bundles/app/in.juspay.ec/3.33.1/web/index.js","ec_bundle_etag":"19a6301cb2893ce82ad42009859435ef","escrow_bundle":"https://assets.juspay.in/hyper/bundles/app/in.juspay.escrow/4.0.4/web/index.js","escrow_bundle_etag":"4b756ddc159a70672fea8dab5f0066ff","hyperpay_bundle":"https://assets.juspay.in/hyper/bundles/app/in.juspay.hyperpay/4.40.20-release-20240715.2/web/prod-split_index.js","hyperpay_bundle_etag":"c5c5c1ac957a20d2bf312ccfeb767571","upiintent_bundle":"https://assets.juspay.in/hyper/bundles/app/in.juspay.upiintent/4.10.2/web/index.js","upiintent_bundle_etag":"5617360b609834cf686ae75796aeafee"},"variant_type":"EXPERIMENTAL"},{"context_id":"e7c672fb9d580bcbfe90ce0ec029af2109b41a98972d13997cd90cea71bcd01e","id":"7206198466018328576-control","override_id":"221983082bcc13e5e24eae1c056686d5cb47abadae00e6187dad2b0bc482e88b","overrides":{"ec_bundle":"https://assets.juspay.in/hyper/bundles/app/in.juspay.ec/3.29.4/web/index.js","ec_bundle_etag":"e4b290873d9b7691a2471cdfa58023a4","escrow_bundle":"default_str_ignore_this","escrow_bundle_etag":"default_str_ignore_this","hyperpay_bundle":"https://assets.juspay.in/hyper/bundles/app/in.juspay.hyperpay/4.24.3-release-20240422.3/web/prod-split_index.js","hyperpay_bundle_etag":"8ec671797de055489d4acbba008befc5","upiintent_bundle":"https://assets.juspay.in/hyper/bundles/app/in.juspay.upiintent/4.9.9/web/index.js","upiintent_bundle_etag":"9774e9eaec3ec8221bfcb104a4534534"},"variant_type":"CONTROL"}],"last_modified_by":"srikanth.mitra@juspay.in","chosen_variant":""}	INSERT INTO test_experimentation.experiments (id,created_at,created_by,last_modified,"name",override_keys,status,traffic_percentage,context,variants,last_modified_by,chosen_variant)\n\tVALUES (7206198466018328576,'2024-06-11 07:39:59+05:30','mounika.reddy@juspay.in','2024-08-07 16:20:21+05:30','hdfc hyperpay SBX','{escrow_bundle,hyperpay_bundle_etag,upiintent_bundle_etag,ec_bundle,escrow_bundle_etag,hyperpay_bundle,ec_bundle_etag,upiintent_bundle}','CREATED',0,'{"and":[{"==":[{"var":"clientId"},"hdfcmaster"]},{"==":[{"var":"os"},"web"]},{"==":[{"var":"resellerId"},"hdfc_reseller"]}]}','[{"context_id":"142023a66eadb6e0effb613b9b9c0292fb08230d609bd406a5a01c7f789ee891","id":"7206198466018328576-test","override_id":"7697748dedfd674e95b8848609a910cb152d49f89ac32d2102db5a3cba1ad107","overrides":{"ec_bundle":"https://assets.juspay.in/hyper/bundles/app/in.juspay.ec/3.33.1/web/index.js","ec_bundle_etag":"19a6301cb2893ce82ad42009859435ef","escrow_bundle":"https://assets.juspay.in/hyper/bundles/app/in.juspay.escrow/4.0.4/web/index.js","escrow_bundle_etag":"4b756ddc159a70672fea8dab5f0066ff","hyperpay_bundle":"https://assets.juspay.in/hyper/bundles/app/in.juspay.hyperpay/4.40.20-release-20240715.2/web/prod-split_index.js","hyperpay_bundle_etag":"c5c5c1ac957a20d2bf312ccfeb767571","upiintent_bundle":"https://assets.juspay.in/hyper/bundles/app/in.juspay.upiintent/4.10.2/web/index.js","upiintent_bundle_etag":"5617360b609834cf686ae75796aeafee"},"variant_type":"EXPERIMENTAL"},{"context_id":"e7c672fb9d580bcbfe90ce0ec029af2109b41a98972d13997cd90cea71bcd01e","id":"7206198466018328576-control","override_id":"221983082bcc13e5e24eae1c056686d5cb47abadae00e6187dad2b0bc482e88b","overrides":{"ec_bundle":"https://assets.juspay.in/hyper/bundles/app/in.juspay.ec/3.29.4/web/index.js","ec_bundle_etag":"e4b290873d9b7691a2471cdfa58023a4","escrow_bundle":"default_str_ignore_this","escrow_bundle_etag":"default_str_ignore_this","hyperpay_bundle":"https://assets.juspay.in/hyper/bundles/app/in.juspay.hyperpay/4.24.3-release-20240422.3/web/prod-split_index.js","hyperpay_bundle_etag":"8ec671797de055489d4acbba008befc5","upiintent_bundle":"https://assets.juspay.in/hyper/bundles/app/in.juspay.upiintent/4.9.9/web/index.js","upiintent_bundle_etag":"9774e9eaec3ec8221bfcb104a4534534"},"variant_type":"CONTROL"}]','srikanth.mitra@juspay.in','')
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
7094634333213954048	2023-08-08 05:33:57+00	cac.admin@juspay.in	2023-08-08 05:54:04+00	test_1	{patch_entries,hyperos_placeholder_tracker_ios}	CONCLUDED	10	{"and":[{"==":[{"var":"os"},"ios"]},{"==":[{"var":"clientId"},"meesho"]}]}	[{"context_id":"463654d2ae200d14fa893df925879db44ebcf5cdd1bbbdcdc228f496cf48992e","id":"7094634333213954048-test-3-android","override_id":"21dd2bb6a08033075af47314999010ed294a5aca9e77948f4324a82b009de836","overrides":{"hyperos_placeholder_tracker_ios":{"etag":"2a47799a6a98df972e3de0844153b4eb","path":["live","assets","in.juspay.hyperos.placeholder","tracker"],"value":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.49/v1-tracker.jsa"},"patch_entries":["hyperos_placeholder_tracker_ios"]},"variant_type":"EXPERIMENTAL"},{"context_id":"11ba7e1de84822ddfc9ab586dcfc2fb1f29f2eeb2dc990edcd91fa19cd2fe099","id":"7094634333213954048-control-3-android","override_id":"8267961e60033cc274f990558f10c0277c90ef737bfcfc0d1fe3be15d97937b9","overrides":{"hyperos_placeholder_tracker_ios":{"etag":"2a47799a6a98df972e3de0844153b4eb","path":["live","assets","in.juspay.hyperos.placeholder","tracker"],"value":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.48/v1-tracker.jsa"},"patch_entries":["hyperos_placeholder_tracker_ios"]},"variant_type":"CONTROL"}]	cac.admin@juspay.in	
7094645531124830208	2023-08-08 06:18:26+00	cac.admin@juspay.in	2023-08-08 07:32:11+00	test_1	{patch_entries,hyperos_placeholder_tracker_ios}	CONCLUDED	5	{"and":[{"==":[{"var":"os"},"ios"]},{"==":[{"var":"clientId"},"meesho"]}]}	[{"context_id":"637a18a13b5b9073e108e2281cacc58bd1a8e822168adc3dcab295bfc5a4bc45","id":"7094645531124830208-test-3-android","override_id":"21dd2bb6a08033075af47314999010ed294a5aca9e77948f4324a82b009de836","overrides":{"hyperos_placeholder_tracker_ios":{"etag":"2a47799a6a98df972e3de0844153b4eb","path":["live","assets","in.juspay.hyperos.placeholder","tracker"],"value":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.49/v1-tracker.jsa"},"patch_entries":["hyperos_placeholder_tracker_ios"]},"variant_type":"EXPERIMENTAL"},{"context_id":"adffedeed3338aff48d25b7681455bb2cf8ad1ee3f866a98cc9ce13120174d5b","id":"7094645531124830208-control-3-android","override_id":"8267961e60033cc274f990558f10c0277c90ef737bfcfc0d1fe3be15d97937b9","overrides":{"hyperos_placeholder_tracker_ios":{"etag":"2a47799a6a98df972e3de0844153b4eb","path":["live","assets","in.juspay.hyperos.placeholder","tracker"],"value":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.48/v1-tracker.jsa"},"patch_entries":["hyperos_placeholder_tracker_ios"]},"variant_type":"CONTROL"}]	cac.admin@juspay.in	
7094665091173453824	2023-08-08 07:36:10+00	cac.admin@juspay.in	2023-08-08 07:53:45+00	test_1	{patch_entries,hyperos_placeholder_tracker_ios}	CONCLUDED	10	{"and":[{"==":[{"var":"os"},"ios"]},{"==":[{"var":"clientId"},"meesho"]}]}	[{"context_id":"047e874ab4d7ed4c107223639833f26047f0a1733ec27505f6a7a3f3f2ab7ad7","id":"7094665091173453824-test-3-android","override_id":"e0e0fed9750c3fa7929e493acddf5ba2acb6db63b3b0bafd3f9697903c2ddaa3","overrides":{"hyperos_placeholder_tracker_ios":{"etag":"2a47799a6a98df972e3de0844153b4eb","path":["live","assets","in.juspay.hyperos.placeholder","tracker"],"value":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.51/v1-tracker.jsa"},"patch_entries":["hyperos_placeholder_tracker_ios"]},"variant_type":"EXPERIMENTAL"},{"context_id":"cf8e92b97557b0db1274c71240031036ca4bcc62674cffc2e9dc7ea2df56224f","id":"7094665091173453824-control-3-android","override_id":"bbfa5a8475ed19e6e907840a872b71ea49246ea3fe378676373acbe782264f7d","overrides":{"hyperos_placeholder_tracker_ios":{"etag":"2a47799a6a98df972e3de0844153b4eb","path":["live","assets","in.juspay.hyperos.placeholder","tracker"],"value":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.50/v1-tracker.jsa"},"patch_entries":["hyperos_placeholder_tracker_ios"]},"variant_type":"CONTROL"}]	cac.admin@juspay.in	
7097892661364920320	2023-08-17 05:21:23+00	cac.admin@juspay.in	2023-08-17 05:28:24+00	experiment-1	{hyperos_placeholder_tracker_android}	CONCLUDED	20	{"and":[{"==":[{"var":"os"},"android"]},{"==":[{"var":"clientId"},"galactustest"]}]}	[{"context_id":"4ab3d289c871751c3fc1fa69f48c6d0ffb7adf8e4e6776d59d7e3c7d43b9d539","id":"7097892661364920320-control","override_id":"78689505959cee24a30383f6445d6d169cb731fb2b0d525b3657920fdded3627","overrides":{"hyperos_placeholder_tracker_android":{"etag":"piyaz","path":["live","assets","in.juspay.hyperos.placeholder","tracker"],"value":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.50/v1-tracker.zip"}},"variant_type":"CONTROL"},{"context_id":"ca70564343c90923aa98078d4c133dbd262e1bc556efd9c8e91e4a1b57bbf795","id":"7097892661364920320-test1","override_id":"667227c74fff8101146fe1d1dfa2384e8344f5711e43abc63793e83c13d76a49","overrides":{"hyperos_placeholder_tracker_android":{"etag":"alloo","path":["live","assets","in.juspay.hyperos.placeholder","tracker"],"value":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.51/v1-tracker.zip"}},"variant_type":"EXPERIMENTAL"}]	cac.admin@juspay.in	
7097895312416706560	2023-08-17 05:31:55+00	cac.admin@juspay.in	2023-08-17 05:43:31+00	experiment-1	{hyperos_placeholder_tracker_android}	CONCLUDED	20	{"and":[{"==":[{"var":"os"},"android"]},{"==":[{"var":"clientId"},"galactustest"]}]}	[{"context_id":"03422b5560e430d93b994d155759b70982fe116b61ac6e76012b699ee7cd64da","id":"7097895312416706560-control","override_id":"78689505959cee24a30383f6445d6d169cb731fb2b0d525b3657920fdded3627","overrides":{"hyperos_placeholder_tracker_android":{"etag":"piyaz","path":["live","assets","in.juspay.hyperos.placeholder","tracker"],"value":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.50/v1-tracker.zip"}},"variant_type":"CONTROL"},{"context_id":"85384c579c536e9b6917bc5825681145e8a1a5d2692ab435fae2a1e8ecb315ae","id":"7097895312416706560-test1","override_id":"667227c74fff8101146fe1d1dfa2384e8344f5711e43abc63793e83c13d76a49","overrides":{"hyperos_placeholder_tracker_android":{"etag":"alloo","path":["live","assets","in.juspay.hyperos.placeholder","tracker"],"value":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.51/v1-tracker.zip"}},"variant_type":"EXPERIMENTAL"}]	cac.admin@juspay.in	
7097905532270415872	2023-08-17 06:12:31+00	cac.admin@juspay.in	2023-08-17 06:14:02+00	experiment-1	{hyperos_placeholder_tracker_android}	CONCLUDED	0	{"and":[{"==":[{"var":"os"},"android"]},{"==":[{"var":"clientId"},"galactustest"]}]}	[{"context_id":"291fda6700c80d4ae1e60c758e082cbcb51d5eccddce0741dba024ea02b8d7e0","id":"7097905532270415872-control","override_id":"78689505959cee24a30383f6445d6d169cb731fb2b0d525b3657920fdded3627","overrides":{"hyperos_placeholder_tracker_android":{"etag":"piyaz","path":["live","assets","in.juspay.hyperos.placeholder","tracker"],"value":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.50/v1-tracker.zip"}},"variant_type":"CONTROL"},{"context_id":"3a66454533ed80493d7d77af14233fdb8bbb9b15300c5d215702237e0777e308","id":"7097905532270415872-test1","override_id":"667227c74fff8101146fe1d1dfa2384e8344f5711e43abc63793e83c13d76a49","overrides":{"hyperos_placeholder_tracker_android":{"etag":"alloo","path":["live","assets","in.juspay.hyperos.placeholder","tracker"],"value":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.51/v1-tracker.zip"}},"variant_type":"EXPERIMENTAL"}]	cac.admin@juspay.in	
7099775689305493504	2023-08-22 10:03:51+00	cac.admin@juspay.in	2023-08-22 10:17:16+00	Godel_ACS_release	{godel_acs_android,godel_config_android,godel_placeholder_acs_android,godel_placeholder_config_android,patch_entries}	INPROGRESS	20	{"and":[{"==":[{"var":"os"},"android"]}]}	[{"context_id":"7f408004fcffe6bf4c7f1c2038266b2f07df122d39a2fb14df7a708cde020bd5","id":"7099775689305493504-control","override_id":"8f9b7e2df82ea95b5eb2f162bfb3520d4eb1ae215c670ca60f67fe0fd9ac301e","overrides":{"godel_acs_android":{"etag":"2014efe3000a5fdc1ea7a70b9c9a9f69","path":["live","assets","in.juspay.godel","acs_js_source"],"value":"https://assets.juspay.in/hyper/bundles/acs/in.juspay.godel/1.2.99/v1-acs.zip"},"godel_config_android":{"etag":"acf7efedd2cdfd18d101db7edd154cdc","path":["live","assets","in.juspay.godel","config"],"value":"https://assets.juspay.in/hyper/bundles/config/in.juspay.godel/1.2.10/v1-config.zip"},"godel_placeholder_acs_android":{"etag":"2014efe3000a5fdc1ea7a70b9c9a9f69","path":["live","assets","in.juspay.godel.placeholder","acs_js_source"],"value":"https://assets.juspay.in/hyper/bundles/acs/in.juspay.godel/1.2.99/v1-acs.zip"},"godel_placeholder_config_android":{"etag":"acf7efedd2cdfd18d101db7edd154cdc","path":["live","assets","in.juspay.godel.placeholder","config"],"value":"https://assets.juspay.in/hyper/bundles/config/in.juspay.godel/1.2.10/v1-config.zip"},"patch_entries":["godel_bundle_android","godel_placeholder_bundle_android","godel_config_android","godel_placeholder_config_android","godel_acs_android","godel_placeholder_acs_android","hyperos_placeholder_tracker_android"]},"variant_type":"CONTROL"},{"context_id":"32b0795d4fe06e18c810298a75ef679304b192bc19880fb510f4c0564cc9732d","id":"7099775689305493504-test","override_id":"568f1c5e361db3c403f9df446b763efeb451f36c6bac07cb66f6720450d2bfe3","overrides":{"godel_acs_android":{"etag":"a16f12ac0c078e21eed52ab452cbf042","path":["live","assets","in.juspay.godel","acs_js_source"],"value":"https://assets.juspay.in/hyper/bundles/acs/in.juspay.godel/1.2.100/v1-acs.zip"},"godel_config_android":{"etag":"46c2b1a55e9d27445e3b1547ac0a4689","path":["live","assets","in.juspay.godel","config"],"value":"https://assets.juspay.in/hyper/bundles/config/in.juspay.godel/1.2.11/v1-config.zip"},"godel_placeholder_acs_android":{"etag":"a16f12ac0c078e21eed52ab452cbf042","path":["live","assets","in.juspay.godel.placeholder","acs_js_source"],"value":"https://assets.juspay.in/hyper/bundles/acs/in.juspay.godel/1.2.100/v1-acs.zip"},"godel_placeholder_config_android":{"etag":"46c2b1a55e9d27445e3b1547ac0a4689","path":["live","assets","in.juspay.godel.placeholder","config"],"value":"https://assets.juspay.in/hyper/bundles/config/in.juspay.godel/1.2.11/v1-config.zip"},"patch_entries":["godel_bundle_android","godel_placeholder_bundle_android","godel_config_android","godel_placeholder_config_android","godel_acs_android","godel_placeholder_acs_android","hyperos_placeholder_tracker_android"]},"variant_type":"EXPERIMENTAL"}]	cac.admin@juspay.in	
7100097235819565056	2023-08-23 07:21:34+00	cac.admin@juspay.in	2023-08-23 07:24:16+00	Tracker_release	{hyperos_placeholder_tracker_android,patch_entries}	CONCLUDED	0	{"and":[{"==":[{"var":"os"},"android"]},{"==":[{"var":"clientId"},"poonawalla"]}]}	[{"context_id":"5ff1ae83774a5bd997fcfb24abb57a8a8b9e7c140fd3ed07331029fad0774694","id":"7100097235819565056-control","override_id":"8f3101e5e15b0ced15b806037127158ad1bb4f538e04b612a9cf67760aa55d05","overrides":{"hyperos_placeholder_tracker_android":{"etag":"72e0db9d6d92d558b8a3dbb2e6abf0ec","path":["live","assets","in.juspay.hyperos.placeholder","tracker"],"value":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.50/v1-tracker.zip"},"patch_entries":["godel_bundle_android","godel_placeholder_bundle_android","godel_config_android","godel_placeholder_config_android","godel_acs_android","godel_placeholder_acs_android","hyperos_placeholder_tracker_android"]},"variant_type":"CONTROL"},{"context_id":"f3f9aabb93aa1c6a353de118195b63a2637d7ebbf9bb22c8a76c967398bc873a","id":"7100097235819565056-test","override_id":"f9081984a4fca32346e1c115aebcf67ac1ae098ffd6b26e66ad88906f06f5f33","overrides":{"hyperos_placeholder_tracker_android":{"etag":"39b4e3e33950355a8c32e9092a954eff","path":["live","assets","in.juspay.hyperos.placeholder","tracker"],"value":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.51/v1-tracker.zip"},"patch_entries":["godel_bundle_android","godel_placeholder_bundle_android","godel_config_android","godel_placeholder_config_android","godel_acs_android","godel_placeholder_acs_android","hyperos_placeholder_tracker_android"]},"variant_type":"EXPERIMENTAL"}]	cac.admin@juspay.in	
7100467927845048320	2023-08-24 07:54:34+00	cac.admin@juspay.in	2023-08-24 07:55:45+00	experiment-1	{pmTestKey1,pmTestKey2}	CONCLUDED	0	{"and":[{"==":[{"var":"os"},"ios"]},{"==":[{"var":"clientId"},"testClientCac1"]}]}	[{"context_id":"e12e114451c36b1fc75364f27c123fc2d6bb7cc5002dd8898214cfc708d414f8","id":"7100467927845048320-control","override_id":"085e637459203733acb435d0bd921081b10a46a3eb192924a332e414bf266cf7","overrides":{"pmTestKey1":"value1-control","pmTestKey2":"value1-control"},"variant_type":"CONTROL"},{"context_id":"f0a4ac6d35cb5442fdbea7cdc6b3fd69048c1e5ed1d6420a4383667665b7f6a5","id":"7100467927845048320-test1","override_id":"65bcc9acaebd88355f850d7087e269f36d1316d7aeeddcf72cece682f99691c1","overrides":{"pmTestKey1":"value2-test","pmTestKey2":"value2-test"},"variant_type":"EXPERIMENTAL"}]	cac.admin@juspay.in	
7100418327268429824	2023-08-24 04:37:28+00	cac.admin@juspay.in	2023-08-24 10:21:54+00	experiment-1	{pmTestKey1,pmTestKey2}	CONCLUDED	2	{"and":[{"==":[{"var":"os"},"ios"]},{"==":[{"var":"clientId"},"testClientCac1"]}]}	[{"context_id":"a43666147f7d1cd840ddc4c8da53cf19a351630cd843d74dbffe7d51c184ee51","id":"7100418327268429824-control","override_id":"085e637459203733acb435d0bd921081b10a46a3eb192924a332e414bf266cf7","overrides":{"pmTestKey1":"value1-control","pmTestKey2":"value1-control"},"variant_type":"CONTROL"},{"context_id":"f0b593cec64e50fe8d15675d53f39f75c9296c11304c2693711ae2b8b4b30546","id":"7100418327268429824-test1","override_id":"65bcc9acaebd88355f850d7087e269f36d1316d7aeeddcf72cece682f99691c1","overrides":{"pmTestKey1":"value2-test","pmTestKey2":"value2-test"},"variant_type":"EXPERIMENTAL"}]	cac.admin@juspay.in	
7100505076539723776	2023-08-24 10:22:11+00	cac.admin@juspay.in	2023-08-24 10:22:35+00	experiment-test	{pmTestKey1,pmTestKey2}	CONCLUDED	10	{"and":[{"==":[{"var":"os"},"ios"]},{"==":[{"var":"clientId"},"testClientCac1"]}]}	[{"context_id":"ae7a885cca16c4e1b781d827c9e267741321f83f43a145b82c1380e9a9c63649","id":"7100505076539723776-control","override_id":"085e637459203733acb435d0bd921081b10a46a3eb192924a332e414bf266cf7","overrides":{"pmTestKey1":"value1-control","pmTestKey2":"value1-control"},"variant_type":"CONTROL"},{"context_id":"fb796d91a330050ea7112f6e415e5c5fd8b1489355c7a06bbe134a2370d47c67","id":"7100505076539723776-test1","override_id":"65bcc9acaebd88355f850d7087e269f36d1316d7aeeddcf72cece682f99691c1","overrides":{"pmTestKey1":"value2-test","pmTestKey2":"value2-test"},"variant_type":"EXPERIMENTAL"}]	cac.admin@juspay.in	
7100505506371997696	2023-08-24 10:23:53+00	cac.admin@juspay.in	2023-08-24 10:24:08+00	experiment-test-2	{pmTestKey1,pmTestKey2}	CONCLUDED	0	{"and":[{"==":[{"var":"os"},"ios"]},{"==":[{"var":"clientId"},"testClientCac1"]}]}	[{"context_id":"139618bad8aa5fe4e8c34591e55ea505613897d4ef79e508dd87544e7526042d","id":"7100505506371997696-control","override_id":"085e637459203733acb435d0bd921081b10a46a3eb192924a332e414bf266cf7","overrides":{"pmTestKey1":"value1-control","pmTestKey2":"value1-control"},"variant_type":"CONTROL"},{"context_id":"d164355dbb8d87da0c189471ace56578c7e5d4008f453f9dc248659a430b319c","id":"7100505506371997696-test1","override_id":"65bcc9acaebd88355f850d7087e269f36d1316d7aeeddcf72cece682f99691c1","overrides":{"pmTestKey1":"value2-test","pmTestKey2":"value2-test"},"variant_type":"EXPERIMENTAL"}]	cac.admin@juspay.in	
7100505592241983488	2023-08-24 10:24:14+00	cac.admin@juspay.in	2023-08-24 10:25:13+00	experiment-test-3	{pmTestKey1,pmTestKey2}	CONCLUDED	41	{"and":[{"==":[{"var":"os"},"ios"]},{"==":[{"var":"clientId"},"testClientCac1"]}]}	[{"context_id":"d794c78d54a9bc079732d35263acb7b9892f65056846927d429c3fd39c8e6023","id":"7100505592241983488-control","override_id":"085e637459203733acb435d0bd921081b10a46a3eb192924a332e414bf266cf7","overrides":{"pmTestKey1":"value1-control","pmTestKey2":"value1-control"},"variant_type":"CONTROL"},{"context_id":"0a7e0196582254694e5c484ea20fe7cb32f5c536d67ee95f5ca03b1fb7bc356e","id":"7100505592241983488-test1","override_id":"65bcc9acaebd88355f850d7087e269f36d1316d7aeeddcf72cece682f99691c1","overrides":{"pmTestKey1":"value2-test","pmTestKey2":"value2-test"},"variant_type":"EXPERIMENTAL"}]	cac.admin@juspay.in	
7100505953140871168	2023-08-24 10:25:40+00	cac.admin@juspay.in	2023-08-24 10:25:58+00	experiment-test-4	{pmTestKey1,pmTestKey2}	CONCLUDED	40	{"and":[{"==":[{"var":"os"},"ios"]},{"==":[{"var":"clientId"},"testClientCac1"]}]}	[{"context_id":"339ea286f17e67a0703e6009e0316a6b791693bb51c3cce4a7e608b8d7d6ff72","id":"7100505953140871168-control","override_id":"085e637459203733acb435d0bd921081b10a46a3eb192924a332e414bf266cf7","overrides":{"pmTestKey1":"value1-control","pmTestKey2":"value1-control"},"variant_type":"CONTROL"},{"context_id":"64d08764fa098a511fe31416b611d4ebc351c8a37e87e8ba230af998dd5412e3","id":"7100505953140871168-test1","override_id":"65bcc9acaebd88355f850d7087e269f36d1316d7aeeddcf72cece682f99691c1","overrides":{"pmTestKey1":"value2-test","pmTestKey2":"value2-test"},"variant_type":"EXPERIMENTAL"}]	cac.admin@juspay.in	
7100506116404154368	2023-08-24 10:26:19+00	cac.admin@juspay.in	2023-08-24 10:26:36+00	experiment-test-5	{pmTestKey1,pmTestKey2}	CONCLUDED	40	{"and":[{"==":[{"var":"os"},"ios"]},{"==":[{"var":"clientId"},"testClientCac1"]}]}	[{"context_id":"ada860c380d6d3b8249bc839f0b03916816bec3b549bcaa46dc13a9e14637c85","id":"7100506116404154368-control","override_id":"085e637459203733acb435d0bd921081b10a46a3eb192924a332e414bf266cf7","overrides":{"pmTestKey1":"value1-control","pmTestKey2":"value1-control"},"variant_type":"CONTROL"},{"context_id":"5e4c89b8968ca83720c0d3f9cb4e685039ecc6c849a87628951d08af2d5f659f","id":"7100506116404154368-test1","override_id":"65bcc9acaebd88355f850d7087e269f36d1316d7aeeddcf72cece682f99691c1","overrides":{"pmTestKey1":"value2-test","pmTestKey2":"value2-test"},"variant_type":"EXPERIMENTAL"}]	cac.admin@juspay.in	
7100551328161730560	2023-08-24 13:25:58+00	cac.admin@juspay.in	2023-08-24 13:26:52+00	experiment-test-6	{pmTestKey1,pmTestKey2}	CONCLUDED	20	{"and":[{"==":[{"var":"os"},"ios"]},{"==":[{"var":"clientId"},"testClientCac1"]}]}	[{"context_id":"6801dab9dc504adab6993d1460a1e1c3b953ce89e6b1667028119fd365775d1c","id":"7100551328161730560-control","override_id":"085e637459203733acb435d0bd921081b10a46a3eb192924a332e414bf266cf7","overrides":{"pmTestKey1":"value1-control","pmTestKey2":"value1-control"},"variant_type":"CONTROL"},{"context_id":"f5b4a558f673e8768eeb2ad48c2a30b79cba2de1e9178fdc1cc52886217b0e01","id":"7100551328161730560-test1","override_id":"65bcc9acaebd88355f850d7087e269f36d1316d7aeeddcf72cece682f99691c1","overrides":{"pmTestKey1":"value2-test","pmTestKey2":"value2-test"},"variant_type":"EXPERIMENTAL"}]	cac.admin@juspay.in	
7100741287271337984	2023-08-25 02:00:48+00	cac.admin@juspay.in	2023-08-25 02:01:38+00	experiment-test-7	{pmTestKey1,pmTestKey2}	CONCLUDED	10	{"and":[{"==":[{"var":"os"},"ios"]},{"==":[{"var":"clientId"},"testClientCac1"]}]}	[{"context_id":"71dc96366d072d78ade59f7ad3aefffaea4f25d9bb864eec845c10b344463b4f","id":"7100741287271337984-control","override_id":"085e637459203733acb435d0bd921081b10a46a3eb192924a332e414bf266cf7","overrides":{"pmTestKey1":"value1-control","pmTestKey2":"value1-control"},"variant_type":"CONTROL"},{"context_id":"c35b1df90f0438aea2221097632223731e52a231f31a41cf24cf78ad624ee0b3","id":"7100741287271337984-test1","override_id":"65bcc9acaebd88355f850d7087e269f36d1316d7aeeddcf72cece682f99691c1","overrides":{"pmTestKey1":"value2-test","pmTestKey2":"value2-test"},"variant_type":"EXPERIMENTAL"}]	cac.admin@juspay.in	
7101895609925373952	2023-08-28 06:27:40+00	cac.admin@juspay.in	2023-08-28 10:03:42+00	Godel ACS Release	{godel_bundle_android,godel_placeholder_bundle_android,godel_config_android,godel_placeholder_config_android,godel_acs_android,godel_placeholder_acs_android,hyperos_placeholder_tracker_android,patch_entries}	CONCLUDED	10	{"and":[{"==":[{"var":"os"},"android"]},{"<":[{"var":"toss"},0]}]}	[{"context_id":"62e09131d8d9a04988a5940bd380c12556c2f8069105b39484f74c8c9559d642","id":"7101895609925373952-control","override_id":"c116de90a3c8f79b1cd87a4a327eb5570dba41f738a0c5d8f56dc6c2b67da9fa","overrides":{"godel_acs_android":{"etag":"a16f12ac0c078e21eed52ab452cbf042","path":["live","assets","in.juspay.godel","acs_js_source"],"value":"https://assets.juspay.in/hyper/bundles/acs/in.juspay.godel/1.2.100/v1-acs.zip"},"godel_bundle_android":{"etag":"784aaa859ef08d604cf277ee3406f4cc","path":["live","package","in.juspay.godel"],"value":"https://assets.juspay.in/hyper/bundles/app/in.juspay.godel/1.2.23/android/v1-index_bundle.zip"},"godel_config_android":{"etag":"46c2b1a55e9d27445e3b1547ac0a4689","path":["live","assets","in.juspay.godel","config"],"value":"https://assets.juspay.in/hyper/bundles/config/in.juspay.godel/1.2.11/v1-config.zip"},"godel_placeholder_acs_android":{"etag":"a16f12ac0c078e21eed52ab452cbf042","path":["live","assets","in.juspay.godel.placeholder","acs_js_source"],"value":"https://assets.juspay.in/hyper/bundles/acs/in.juspay.godel/1.2.100/v1-acs.zip"},"godel_placeholder_bundle_android":{"etag":"784aaa859ef08d604cf277ee3406f4cc","path":["live","package","in.juspay.godel.placeholder"],"value":"https://assets.juspay.in/hyper/bundles/app/in.juspay.godel/1.2.23/android/v1-index_bundle.zip"},"godel_placeholder_config_android":{"etag":"46c2b1a55e9d27445e3b1547ac0a4689","path":["live","assets","in.juspay.godel.placeholder","config"],"value":"https://assets.juspay.in/hyper/bundles/config/in.juspay.godel/1.2.11/v1-config.zip"},"hyperos_placeholder_tracker_android":{"etag":"72e0db9d6d92d558b8a3dbb2e6abf0ec","path":["live","assets","in.juspay.hyperos.placeholder","tracker"],"value":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.50/v1-tracker.zip"},"patch_entries":["godel_bundle_android","godel_placeholder_bundle_android","godel_config_android","godel_placeholder_config_android","godel_acs_android","godel_placeholder_acs_android","hyperos_placeholder_tracker_android"]},"variant_type":"CONTROL"},{"context_id":"58991119af36b6acdeca68eb77a26ec51c3115f4794e8172e77e562e17f75281","id":"7101895609925373952-test","override_id":"4c0d1ead839ad25fe600fb76301b8ea63e4d0f35c58548d792a8e25eaa18af56","overrides":{"godel_acs_android":{"etag":"13b1c8287464d54e9417fcb18f6f21a1","path":["live","assets","in.juspay.godel","acs_js_source"],"value":"https://assets.juspay.in/hyper/bundles/acs/in.juspay.godel/1.2.102/v1-acs.zip"},"godel_bundle_android":{"etag":"5119e9bafb446b8d4a7e40823851684f","path":["live","package","in.juspay.godel"],"value":"https://assets.juspay.in/hyper/bundles/app/in.juspay.godel/1.2.24/android/v1-index_bundle.zip"},"godel_config_android":{"etag":"4bd5516b5ab247b99be2b89f34e8a733","path":["live","assets","in.juspay.godel","config"],"value":"https://assets.juspay.in/hyper/bundles/config/in.juspay.godel/1.2.12/v1-config.zip"},"godel_placeholder_acs_android":{"etag":"13b1c8287464d54e9417fcb18f6f21a1","path":["live","assets","in.juspay.godel.placeholder","acs_js_source"],"value":"https://assets.juspay.in/hyper/bundles/acs/in.juspay.godel/1.2.102/v1-acs.zip"},"godel_placeholder_bundle_android":{"etag":"5119e9bafb446b8d4a7e40823851684f","path":["live","package","in.juspay.godel.placeholder"],"value":"https://assets.juspay.in/hyper/bundles/app/in.juspay.godel/1.2.24/android/v1-index_bundle.zip"},"godel_placeholder_config_android":{"etag":"4bd5516b5ab247b99be2b89f34e8a733","path":["live","assets","in.juspay.godel.placeholder","config"],"value":"https://assets.juspay.in/hyper/bundles/config/in.juspay.godel/1.2.12/v1-config.zip"},"hyperos_placeholder_tracker_android":{"etag":"72e0db9d6d92d558b8a3dbb2e6abf0ec","path":["live","assets","in.juspay.hyperos.placeholder","tracker"],"value":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.50/v1-tracker.zip"},"patch_entries":["godel_bundle_android","godel_placeholder_bundle_android","godel_config_android","godel_placeholder_config_android","godel_acs_android","godel_placeholder_acs_android","hyperos_placeholder_tracker_android"]},"variant_type":"EXPERIMENTAL"}]	cac.admin@juspay.in	
7112484787039457280	2023-09-26 11:45:16+00	cac.admin@juspay.in	2023-09-26 11:45:16+00	testing	{arya_bundle}	CREATED	0	{"and":[{"==":[{"var":"os"},"IOS"]}]}	[{"context_id":"e60e37fca9c2bea5eac657f14956c641d7e3e2fa7ede85815ff9705da7a8f86c","id":"7112484787039457280-control","override_id":"775dfe8d439fdfc8d3acbec91b8f7389e197a86851803546e99e5a419bf678be","overrides":{"arya_bundle":"","arya_bundle_etag":"dummy value"},"variant_type":"CONTROL"},{"context_id":"e524572fb1c1fdba2a68f1419e7fc3bf275bf4459ac896baa17e77528d76f9c1","id":"7112484787039457280-test1","override_id":"7cefaf3be8698894b5a92c24d51a1636056bd720f19d3d00aca6291285cb4504","overrides":{"arya_bundle":"default_str_ignore_this"},"variant_type":"EXPERIMENTAL"}]	cac.admin@juspay.in	
7101954441615642624	2023-08-28 10:21:26+00	cac.admin@juspay.in	2023-08-28 10:21:26+00	Godel ACS Release	{godel_bundle_android,godel_placeholder_bundle_android,godel_config_android,godel_placeholder_config_android,godel_acs_android,godel_placeholder_acs_android,hyperos_placeholder_tracker_android,patch_entries}	CREATED	0	{"and":[{"==":[{"var":"os"},"android"]}]}	[{"context_id":"1cf58c1517bc9d6a2c581e88d0537f7d678761451e80bc04be122eee60a2d3c7","id":"7101954441615642624-control","override_id":"c116de90a3c8f79b1cd87a4a327eb5570dba41f738a0c5d8f56dc6c2b67da9fa","overrides":{"godel_acs_android":{"etag":"a16f12ac0c078e21eed52ab452cbf042","path":["live","assets","in.juspay.godel","acs_js_source"],"value":"https://assets.juspay.in/hyper/bundles/acs/in.juspay.godel/1.2.100/v1-acs.zip"},"godel_bundle_android":{"etag":"784aaa859ef08d604cf277ee3406f4cc","path":["live","package","in.juspay.godel"],"value":"https://assets.juspay.in/hyper/bundles/app/in.juspay.godel/1.2.23/android/v1-index_bundle.zip"},"godel_config_android":{"etag":"46c2b1a55e9d27445e3b1547ac0a4689","path":["live","assets","in.juspay.godel","config"],"value":"https://assets.juspay.in/hyper/bundles/config/in.juspay.godel/1.2.11/v1-config.zip"},"godel_placeholder_acs_android":{"etag":"a16f12ac0c078e21eed52ab452cbf042","path":["live","assets","in.juspay.godel.placeholder","acs_js_source"],"value":"https://assets.juspay.in/hyper/bundles/acs/in.juspay.godel/1.2.100/v1-acs.zip"},"godel_placeholder_bundle_android":{"etag":"784aaa859ef08d604cf277ee3406f4cc","path":["live","package","in.juspay.godel.placeholder"],"value":"https://assets.juspay.in/hyper/bundles/app/in.juspay.godel/1.2.23/android/v1-index_bundle.zip"},"godel_placeholder_config_android":{"etag":"46c2b1a55e9d27445e3b1547ac0a4689","path":["live","assets","in.juspay.godel.placeholder","config"],"value":"https://assets.juspay.in/hyper/bundles/config/in.juspay.godel/1.2.11/v1-config.zip"},"hyperos_placeholder_tracker_android":{"etag":"72e0db9d6d92d558b8a3dbb2e6abf0ec","path":["live","assets","in.juspay.hyperos.placeholder","tracker"],"value":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.50/v1-tracker.zip"},"patch_entries":["godel_bundle_android","godel_placeholder_bundle_android","godel_config_android","godel_placeholder_config_android","godel_acs_android","godel_placeholder_acs_android","hyperos_placeholder_tracker_android"]},"variant_type":"CONTROL"},{"context_id":"ba2063b6f829c11121d1b10e483d2256de5bd3777dcdb53ff45d482498878468","id":"7101954441615642624-test","override_id":"4884311e3d46b594a9e03013f70f675c646829dd79c0c02477ef813bccbacc25","overrides":{"godel_acs_android":{"etag":"13b1c8287464d54e9417fcb18f6f21a1","path":["live","assets","in.juspay.godel","acs_js_source"],"value":"https://assets.juspay.in/hyper/bundles/acs/in.juspay.godel/1.2.102/v1-acs.zip"},"godel_bundle_android":{"etag":"5119e9bafb446b8d4a7e40823851684f","path":["live","package","in.juspay.godel"],"value":"https://assets.juspay.in/hyper/bundles/app/in.juspay.godel/1.2.24/android/v1-index_bundle.zip"},"godel_config_android":{"etag":"4bd5516b5ab247b99be2b89f34e8a733","path":["live","assets","in.juspay.godel","config"],"value":"https://assets.juspay.in/hyper/bundles/config/in.juspay.godel/1.2.12/v1-config.zip"},"godel_placeholder_acs_android":{"etag":"13b1c8287464d54e9417fcb18f6f21a1","path":["live","assets","in.juspay.godel.placeholder","acs_js_source"],"value":"https://assets.juspay.in/hyper/bundles/acs/in.juspay.godel/1.2.102/v1-acs.zip"},"godel_placeholder_bundle_android":{"etag":"5119e9bafb446b8d4a7e40823851684f","path":["live","package","in.juspay.godel.placeholder"],"value":"https://assets.juspay.in/hyper/bundles/app/in.juspay.godel/1.2.24/android/v1-index_bundle.zip"},"godel_placeholder_config_android":{"etag":"4bd5516b5ab247b99be2b89f34e8a733","path":["live","assets","in.juspay.godel.placeholder","config"],"value":"https://assets.juspay.in/hyper/bundles/config/in.juspay.godel/1.2.12/v1-config.zip"},"hyperos_placeholder_tracker_android":{"etag":"39b4e3e33950355a8c32e9092a954eff","path":["live","assets","in.juspay.hyperos.placeholder","tracker"],"value":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.51/v1-tracker.zip"},"patch_entries":["godel_bundle_android","godel_placeholder_bundle_android","godel_config_android","godel_placeholder_config_android","godel_acs_android","godel_placeholder_acs_android","hyperos_placeholder_tracker_android"]},"variant_type":"EXPERIMENTAL"}]	cac.admin@juspay.in	
7102231926366408704	2023-08-29 04:44:04+00	cac.admin@juspay.in	2023-08-29 04:44:04+00	Godel ACS Release	{hyperos_placeholder_tracker_android,patch_entries}	CREATED	0	{"and":[{"==":[{"var":"os"},"android"]},{"==":[{"var":"clientId"},"meesho"]}]}	[{"context_id":"4270402515ba13c3ff7fb0e1681f643d4f5fc263febe1802150f71471b8f3a8d","id":"7102231926366408704-control","override_id":"8f3101e5e15b0ced15b806037127158ad1bb4f538e04b612a9cf67760aa55d05","overrides":{"hyperos_placeholder_tracker_android":{"etag":"72e0db9d6d92d558b8a3dbb2e6abf0ec","path":["live","assets","in.juspay.hyperos.placeholder","tracker"],"value":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.50/v1-tracker.zip"},"patch_entries":["godel_bundle_android","godel_placeholder_bundle_android","godel_config_android","godel_placeholder_config_android","godel_acs_android","godel_placeholder_acs_android","hyperos_placeholder_tracker_android"]},"variant_type":"CONTROL"},{"context_id":"887c25ef2d7bbb294123679838e2ddcb4ee19e4ed7d491d9485d6ee8481f1350","id":"7102231926366408704-test","override_id":"f520553a4fd6cf08ca2248306d74f0365f618c3e76441ecabfa8f3f72ba5528d","overrides":{"hyperos_placeholder_tracker_android":{"etag":"0ccaa9fa32772930d4a55a82c9d08429","path":["live","assets","in.juspay.hyperos.placeholder","tracker"],"value":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.52/v1-tracker.zip"},"patch_entries":["godel_bundle_android","godel_placeholder_bundle_android","godel_config_android","godel_placeholder_config_android","godel_acs_android","godel_placeholder_acs_android","hyperos_placeholder_tracker_android"]},"variant_type":"EXPERIMENTAL"}]	cac.admin@juspay.in	
7110602278733541376	2023-09-21 07:04:51+00	cac.admin@juspay.in	2023-09-22 04:39:28+00	Enabling retry for Geddit Android	{ec_base_html_android,hyperpay_base_html_android,upi_intent_base_html_android,patch_entries}	CONCLUDED	0	{"and":[{"==":[{"var":"os"},"android"]},{"==":[{"var":"clientId"},"geddit"]}]}	[{"context_id":"618320da8cca8abb370bf14e1d4725dd12e73340ad88f4c2fc12ea2cb66fbd25","id":"7110602278733541376-control","override_id":"d958db68123dd4b5b7a8be14b8668be42a25a94d8414c8aace4f9e239b6ebe3f","overrides":{"ec_base_html_android":{"etag":"","path":["dependencies","in.juspay.ec","default","src"],"value":""},"hyperpay_base_html_android":{"etag":"","path":["dependencies","in.juspay.hyperpay","default","src"],"value":""},"patch_entries":["ec_base_html_android","hyperpay_base_html_android","upi_base_html_android","hyperos_placeholder_tracker_android","godel_bundle_android","godel_placeholder_bundle_android","godel_config_android","godel_placeholder_config_android","godel_acs_android","godel_placeholder_acs_android","hyperos_placeholder_tracker_android"],"upi_intent_base_html_android":{"etag":"","path":["dependencies","in.juspay.upiintent","default","src"],"value":""}},"variant_type":"CONTROL"},{"context_id":"6683ce24a5cfd2b0d0fdf9f177028327ac79f8f945c05550805ea02efc46b994","id":"7110602278733541376-test","override_id":"b198985161d54953b36be0c2dcef163c071cd46fc861ac0e5634efff0ea1977a","overrides":{"ec_base_html_android":{"etag":"c486c8425929dbec13d2b5c469f274a7","path":["dependencies","in.juspay.ec","default","src"],"value":"https://assets.juspay.in/hyper/bundles/android/release/in.juspay.ec/1.0.4/base.html"},"hyperpay_base_html_android":{"etag":"d7c0cd9bc9a166e223efec78b94548ba","path":["dependencies","in.juspay.hyperpay","default","src"],"value":"https://assets.juspay.in/hyper/bundles/android/release/in.juspay.hyperpay/2.0.0/common/1.0.0/base.html"},"patch_entries":["ec_base_html_android","hyperpay_base_html_android","upi_intent_base_html_android","hyperos_placeholder_tracker_android","godel_bundle_android","godel_placeholder_bundle_android","godel_config_android","godel_placeholder_config_android","godel_acs_android","godel_placeholder_acs_android","hyperos_placeholder_tracker_android"],"upi_intent_base_html_android":{"etag":"5a00e9074edb9713389d26f53cbf97b2","path":["dependencies","in.juspay.upiintent","default","src"],"value":"https://assets.juspay.in/hyper/bundles/android/release/in.juspay.upiintent/1.0.4/base.html"}},"variant_type":"EXPERIMENTAL"}]	cac.admin@juspay.in	7110602278733541376-test
7112399555253424128	2023-09-26 06:06:35+00	cac.admin@juspay.in	2023-09-26 10:50:21+00	experiment-test-11	{pmTestKey1,pmTestKey2}	CONCLUDED	0	{"and":[{"==":[{"var":"os"},"ios"]},{"==":[{"var":"clientId"},"testClientCac1"]}]}	[{"context_id":"8c270101c016e0f0458709779a0f4f510be06b56a6f63a49e081216c11b849bf","id":"7112399555253424128-control","override_id":"085e637459203733acb435d0bd921081b10a46a3eb192924a332e414bf266cf7","overrides":{"pmTestKey1":"value1-control","pmTestKey2":"value1-control"},"variant_type":"CONTROL"},{"context_id":"eb1263ccc04b5b39333bf76502732290379fedbea9e7434c9bce9a7afa0a5ec1","id":"7112399555253424128-test1","override_id":"65bcc9acaebd88355f850d7087e269f36d1316d7aeeddcf72cece682f99691c1","overrides":{"pmTestKey1":"value2-test","pmTestKey2":"value2-test"},"variant_type":"EXPERIMENTAL"}]	cac.admin@juspay.in	7112399555253424128-control
7112471696935440384	2023-09-26 10:53:15+00	cac.admin@juspay.in	2023-09-26 11:28:55+00	test	{arya_src}	CONCLUDED	0	{"and":[{"==":[{"var":"os"},"Android"]}]}	[{"context_id":"e00a26bdf1d9420a06add8cf031c8af0480a13ee9f2deaed1a80ec955549bf7a","id":"7112471696935440384-control","override_id":"d7351178aff995b70027fa92aea762eea3f9e9a385e898fe0261f9820a171278","overrides":{"arya_src":"dawdadw"},"variant_type":"CONTROL"},{"context_id":"c6cac52f2b2e4be1238535f217dc99a8dc6163e00607fb5eecf24c8d68b4970d","id":"7112471696935440384-test1","override_id":"2058952d37351296c72bb3ebb1f99a50560538c812b2049f7bf33d8f2eb77db6","overrides":{"arya_src":"default_str_ignore_this"},"variant_type":"EXPERIMENTAL"}]	cac.admin@juspay.in	7112471696935440384-test1
7112483836291403776	2023-09-26 11:41:30+00	cac.admin@juspay.in	2023-09-26 11:41:30+00	Test Experiment	{arya_bundle}	CREATED	0	{"and":[{"==":[{"var":"os"},"Android"]}]}	[{"context_id":"6b544a63ca728f828694a1a0f87ce87164ae34e520bd7e51575bc0308c6abd93","id":"7112483836291403776-control","override_id":"5d3dcf868ecda1553ec7ee28adbcc4c29326d95ecd164c6cbcbc59d8411fb9d0","overrides":{"arya_bundle":"dawdada"},"variant_type":"CONTROL"},{"context_id":"5e5d8a18d42f8f6ab615774fd0f406bb6cc8e1e7558c9072b900d9856f0c895f","id":"7112483836291403776-test1","override_id":"7cefaf3be8698894b5a92c24d51a1636056bd720f19d3d00aca6291285cb4504","overrides":{"arya_bundle":"default_str_ignore_this"},"variant_type":"EXPERIMENTAL"}]	cac.admin@juspay.in	
7112482543506247680	2023-09-26 11:36:21+00	cac.admin@juspay.in	2023-09-26 11:41:36+00		{arya_required_apps}	CONCLUDED	0	{"and":[{"==":[{"var":"os"},"Android"]}]}	[{"context_id":"5cb4d24bba63bec8af6a681b994e0875ba60b6d3ec66ea34e1cb9d72fe5522b0","id":"7112482543506247680-control","override_id":"4bf7e41d1628e1c133234a34df8fff258bc7f5bc5c3ee0dfbe8a885242a7d42b","overrides":{"arya_required_apps":"override value"},"variant_type":"CONTROL"},{"context_id":"c3c308db28e8ac0fc7530c6179157f4507405eb8e5e399b9233c3a11fbbffc38","id":"7112482543506247680-test1","override_id":"5806515a24410d352ab4927493665d2bca544868b9eb0c2c283c4f98d93e1cbc","overrides":{"arya_required_apps":["default_array_ignore_this"]},"variant_type":"EXPERIMENTAL"}]	cac.admin@juspay.in	7112482543506247680-control
7112484915755606016	2023-09-26 11:45:47+00	cac.admin@juspay.in	2023-09-26 11:45:47+00	testing 2	{arya_bundle}	CREATED	0	{"and":[{"==":[{"var":"os"},"IOS"]}]}	[{"context_id":"a2a3a4c7c780ae55f036a32ff935999ba265c4956b24d1eab824a01b01798072","id":"7112484915755606016-control","override_id":"d813868e6f17aaec05294b48a6965274e6734caac9b4a6b7fe71df536e95f150","overrides":{"arya_bundle":"adwadwawdad","arya_bundle_etag":"dummy value"},"variant_type":"CONTROL"},{"context_id":"a2bf80af5d13d9e5332b8a12e24d392860be8bfd360f72e56d8a5d7a612737aa","id":"7112484915755606016-test1","override_id":"7cefaf3be8698894b5a92c24d51a1636056bd720f19d3d00aca6291285cb4504","overrides":{"arya_bundle":"default_str_ignore_this"},"variant_type":"EXPERIMENTAL"}]	cac.admin@juspay.in	
7112523033254813696	2023-09-26 14:17:15+00	cac.admin@juspay.in	2023-09-26 14:17:15+00	Test Exp	{arya_bundle}	CREATED	0	{"and":[{"==":[{"var":"os"},"Android"]}]}	[{"context_id":"853dc24076452538656695cde3d7763ae7279a3abfa04813fcc61e9666f7d613","id":"7112523033254813696-control","override_id":"c598f498cbd0b6e139240bb560005252646ec77b5f03942ad9f5a961f374a316","overrides":{"arya_bundle":"https://www.google.com","arya_bundle_etag":"dummy value"},"variant_type":"CONTROL"},{"context_id":"648e965b2882442af7af11d2c74bbbb071dbf6a0451e27ef13271812a9e4df21","id":"7112523033254813696-test1","override_id":"7cefaf3be8698894b5a92c24d51a1636056bd720f19d3d00aca6291285cb4504","overrides":{"arya_bundle":"default_str_ignore_this"},"variant_type":"EXPERIMENTAL"}]	cac.admin@juspay.in	
7112682390123143168	2023-09-27 00:50:29+00	cac.admin@juspay.in	2023-09-27 00:50:29+00	Enabling godel changes for Countrydelight + gameskraft	{godel_bundle,godel_placeholder_bundle,godel_bundle_etag,godel_placeholder_bundle_etag}	CREATED	0	{"and":[{"==":[{"var":"os"},"android"]},{"in":[{"var":"clientId"},["gameskraft","countrydelight"]]}]}	[{"context_id":"a88e87c678b470c24b45cfcfc32c038b63d89318f318e10af0a1060735ed46f1","id":"7112682390123143168-test","override_id":"ae849a41b3c50faf9d0f3771ad661515fc04f8a171a0ae649df4b591c45e6929","overrides":{"godel_bundle":"https://assets.juspay.in/hyper/bundles/app/in.juspay.godel/1.2.27/android/v1-index_bundle.zip","godel_bundle_etag":"cb7c0aee7082a50760a380910d3922d4","godel_placeholder_bundle":"https://assets.juspay.in/hyper/bundles/app/in.juspay.godel/1.2.27/android/v1-index_bundle.zip","godel_placeholder_bundle_etag":"cb7c0aee7082a50760a380910d3922d4"},"variant_type":"EXPERIMENTAL"},{"context_id":"7c8f8acfaa91a158d59cca5ce81b670e45cd4bac7595f541dde19862421050dc","id":"7112682390123143168-control","override_id":"54c28d5455225caf40f161749301da3a182b09ec572a549fd02c6d9e2c920270","overrides":{"godel_bundle":"https://assets.juspay.in/hyper/bundles/app/in.juspay.godel/1.2.26/android/v1-index_bundle.zip","godel_bundle_etag":"3cb9454c5ec2e38e3b74e11b1ec1071c","godel_placeholder_bundle":"https://assets.juspay.in/hyper/bundles/app/in.juspay.godel/1.2.26/android/v1-index_bundle.zip","godel_placeholder_bundle_etag":"3cb9454c5ec2e38e3b74e11b1ec1071c"},"variant_type":"CONTROL"}]	cac.admin@juspay.in	
7112710543868051456	2023-09-27 02:42:21+00	cac.admin@juspay.in	2023-09-27 02:54:53+00	Updating UPI Intent for all web merchants	{upi_intent_bundle_web,upi_intent_config_web,upi_intent_bundle_web_etag,upi_intent_config_web_etag}	CONCLUDED	0	{"and":[{"==":[{"var":"os"},"web"]}]}	[{"context_id":"f969e098c7d078e574c6f88a4d74079dfe7717f197f453fe517a6dbfad371ade","id":"7112710543868051456-test","override_id":"b061b5cd60ddfe1bae9ba1f1540eadfbac2bebb230314116b4e899af67b14093","overrides":{"upi_intent_bundle_web":"https://assets.juspay.in/hyper/bundles/app/in.juspay.upiintent/4.3.2/web/index.js","upi_intent_bundle_web_etag":"1bef9c71cb89700638234e5aa0e3e164","upi_intent_config_web":"https://assets.juspay.in/hyper/bundles/config/in.juspay.upiintent/common/2.2.12/config.js","upi_intent_config_web_etag":"40e640126342c39c020bf07f22af28dc"},"variant_type":"EXPERIMENTAL"},{"context_id":"9f22dedc869daf4d1d730038fd9a33a4fd4825fce2350272aa9811a5b8864bfd","id":"7112710543868051456-control","override_id":"b061b5cd60ddfe1bae9ba1f1540eadfbac2bebb230314116b4e899af67b14093","overrides":{"upi_intent_bundle_web":"https://assets.juspay.in/hyper/bundles/app/in.juspay.upiintent/4.3.2/web/index.js","upi_intent_bundle_web_etag":"1bef9c71cb89700638234e5aa0e3e164","upi_intent_config_web":"https://assets.juspay.in/hyper/bundles/config/in.juspay.upiintent/common/2.2.12/config.js","upi_intent_config_web_etag":"40e640126342c39c020bf07f22af28dc"},"variant_type":"CONTROL"}]	cac.admin@juspay.in	7112710543868051456-test
7112714599210156032	2023-09-27 02:58:28+00	cac.admin@juspay.in	2023-09-27 03:05:53+00	Updating UPI Intent for all web merchants	{upiintent_bundle_web,upiintent_config_web,upiintent_bundle_web_etag,upiintent_config_web_etag}	CONCLUDED	0	{"and":[{"==":[{"var":"os"},"web"]}]}	[{"context_id":"9b84843d2c515bade29116438270867300bb19de2750fb696f39f391e462daca","id":"7112714599210156032-test","override_id":"a5e9d242d2f9a5fbe5545ae1c1486fdbdbe497ed11dabe9e3d4fc9558c5b98c6","overrides":{"upiintent_bundle_web":"https://assets.juspay.in/hyper/bundles/app/in.juspay.upiintent/4.3.2/web/index.js","upiintent_bundle_web_etag":"1bef9c71cb89700638234e5aa0e3e164","upiintent_config_web":"https://assets.juspay.in/hyper/bundles/config/in.juspay.upiintent/common/2.2.12/config.js","upiintent_config_web_etag":"40e640126342c39c020bf07f22af28dc"},"variant_type":"EXPERIMENTAL"},{"context_id":"bf2efeb7a5821273bcd6f7fdc572cbf304d0ba44dc224918559f7a23729358c3","id":"7112714599210156032-control","override_id":"a5e9d242d2f9a5fbe5545ae1c1486fdbdbe497ed11dabe9e3d4fc9558c5b98c6","overrides":{"upiintent_bundle_web":"https://assets.juspay.in/hyper/bundles/app/in.juspay.upiintent/4.3.2/web/index.js","upiintent_bundle_web_etag":"1bef9c71cb89700638234e5aa0e3e164","upiintent_config_web":"https://assets.juspay.in/hyper/bundles/config/in.juspay.upiintent/common/2.2.12/config.js","upiintent_config_web_etag":"40e640126342c39c020bf07f22af28dc"},"variant_type":"CONTROL"}]	cac.admin@juspay.in	7112714599210156032-test
7112716874713358336	2023-09-27 03:07:30+00	cac.admin@juspay.in	2023-09-27 03:07:30+00	Updating UPI Intent for all web merchants	{upiintent_bundle,upiintent_config,upiintent_bundle_etag,upiintent_config_etag}	CREATED	0	{"and":[{"==":[{"var":"os"},"web"]}]}	[{"context_id":"290d273e26792b51d4a544c1cb18f60dca1e7ff8b14b0ec1a51a579758ea8502","id":"7112716874713358336-test","override_id":"66be851d74373f6bfd70ca23e3487d4330aae08ba114cff56cf34b1f09fb8489","overrides":{"upiintent_bundle":"https://assets.juspay.in/hyper/bundles/app/in.juspay.upiintent/4.3.2/web/index.js","upiintent_bundle_etag":"1bef9c71cb89700638234e5aa0e3e164","upiintent_config":"https://assets.juspay.in/hyper/bundles/config/in.juspay.upiintent/common/2.2.12/config.js","upiintent_config_etag":"40e640126342c39c020bf07f22af28dc"},"variant_type":"EXPERIMENTAL"},{"context_id":"8a718c378ce843933d867f1cfaacc9532eda73d0eb6cd15e65435dee1c5a5467","id":"7112716874713358336-control","override_id":"66be851d74373f6bfd70ca23e3487d4330aae08ba114cff56cf34b1f09fb8489","overrides":{"upiintent_bundle":"https://assets.juspay.in/hyper/bundles/app/in.juspay.upiintent/4.3.2/web/index.js","upiintent_bundle_etag":"1bef9c71cb89700638234e5aa0e3e164","upiintent_config":"https://assets.juspay.in/hyper/bundles/config/in.juspay.upiintent/common/2.2.12/config.js","upiintent_config_etag":"40e640126342c39c020bf07f22af28dc"},"variant_type":"CONTROL"}]	cac.admin@juspay.in	
7122932754380197888	2023-10-25 07:41:46+00	ritick.madaan@juspay.in	2023-10-25 09:16:26+00	test1	{godel_config}	CONCLUDED	0	{"and":[{"==":[{"var":"os"},"android"]},{"==":[{"var":"clientId"},"galactustest"]}]}	[{"context_id":"2df7f921757cc2394e0d2e00b1a362c915b00b887473ebba9b245c8b4fcd9bf8","id":"7122932754380197888-control","override_id":"dece633f19bfbce7315e7dae457a58485af60f8d482270d74553801ab5e1d59e","overrides":{"godel_config":"abc"},"variant_type":"CONTROL"},{"context_id":"7d9457fc83bb768e864e7018a54a726807de131f6b2d570ddeb0309db53a1dc8","id":"7122932754380197888-test1","override_id":"e56a69a5abdd56ed760d6f15f476ac77e8a105e7a720601299747186f1043c92","overrides":{"godel_config":"def"},"variant_type":"EXPERIMENTAL"}]	ritick.madaan@juspay.in	7122932754380197888-control
7113445939770986496	2023-09-29 03:24:33+00	cac.admin@juspay.in	2023-09-29 03:27:10+00	Updating base html for myntra	{ec_src,ec_src_etag}	CONCLUDED	0	{"and":[{"==":[{"var":"os"},"android"]}]}	[{"context_id":"ed433be0fdf140661506a7e4b5a0d5a575eb30bcc86abe2848419ccfe2a6624f","id":"7113445939770986496-test","override_id":"6e905efb616e6f96ba55bc15293a1e4d04f972112ac7ef9d1c0be0aafe29bf45","overrides":{"ec_src":"https://assets.juspay.in/hyper/bundles/android/release/in.juspay.ec/1.0.4/base.html","ec_src_etag":"c486c8425929dbec13d2b5c469f274a7"},"variant_type":"EXPERIMENTAL"},{"context_id":"b570452f14e86dbd3f0e63875049229c0fe309b34ea8482ce0ccfedaf8789153","id":"7113445939770986496-control","override_id":"6e905efb616e6f96ba55bc15293a1e4d04f972112ac7ef9d1c0be0aafe29bf45","overrides":{"ec_src":"https://assets.juspay.in/hyper/bundles/android/release/in.juspay.ec/1.0.4/base.html","ec_src_etag":"c486c8425929dbec13d2b5c469f274a7"},"variant_type":"CONTROL"}]	cac.admin@juspay.in	7113445939770986496-test
7112802367099015168	2023-09-27 08:47:13+00	cac.admin@juspay.in	2023-09-27 08:47:13+00	test exp5	{arya_bundle}	CREATED	0	{"and":[{"==":[{"var":"os"},"Android"]}]}	[{"context_id":"feea32b2444dbd69dc32d830dfcf37681423f0b39f207f260cf8027133ff7472","id":"7112802367099015168-test1","override_id":"880a00b726fc9ec4f146ed125bef65b919373acb0ef98ab49cbc692328efd3e7","overrides":{"arya_bundle":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/galactustest/android/release/config.jsone","arya_bundle_etag":""},"variant_type":"EXPERIMENTAL"},{"context_id":"e878398e97531e11137190932cda06b918a5e141919840633d46dcadda3093d2","id":"7112802367099015168-control","override_id":"f27a63e7518f70f840d57b4545a4cddde709bb3340d440fe21f584d5820710c5","overrides":{"arya_bundle":"default_str_ignore_this","arya_bundle_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}]	cac.admin@juspay.in	
7112842495807877120	2023-09-27 11:26:41+00	cac.admin@juspay.in	2023-09-27 11:26:41+00	Updating Tracker for all web merchants	{hyperos_placeholder_tracker,hyperos_placeholder_tracker_etag}	CREATED	0	{"and":[{"==":[{"var":"os"},"web"]}]}	[{"context_id":"53e80339ed2798ea1c019d5200ce01d2ea66e81fb64b04122b4c1c89b865e847","id":"7112842495807877120-test","override_id":"b365a9858f95ea752d20df6278eda167bf4d801424e895ec4873697cf10c084a","overrides":{"hyperos_placeholder_tracker":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.57/tracker.js","hyperos_placeholder_tracker_etag":"ae93fa69fe617c47d33bd71fb9861f60"},"variant_type":"EXPERIMENTAL"},{"context_id":"85a55cb4f9218dd77d9067bcbb2469ae63cda9fbeb02091e004cca434095b4f4","id":"7112842495807877120-control","override_id":"b365a9858f95ea752d20df6278eda167bf4d801424e895ec4873697cf10c084a","overrides":{"hyperos_placeholder_tracker":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.57/tracker.js","hyperos_placeholder_tracker_etag":"ae93fa69fe617c47d33bd71fb9861f60"},"variant_type":"CONTROL"}]	cac.admin@juspay.in	
7114209005786849280	2023-10-01 05:56:42+00	cac.admin@juspay.in	2023-10-01 05:56:42+00	Updating godel for a23Games	{godel_config,godel_config_etag}	CREATED	0	{"and":[{"==":[{"var":"os"},"android"]},{"==":[{"var":"clientId"},"a23games"]}]}	[{"context_id":"afdfa1fbae908660055b374a323ab23198c3b1d0b18c6942103fb5e66b7712c5","id":"7114209005786849280-test","override_id":"126550b3f94cd406d2259d46eba11e6f0c8017b97f7bbfe2bfcd3e71d75df7d7","overrides":{"godel_config":"https://assets.juspay.in/hyper/bundles/config/in.juspay.godel/1.2.19/v1-config.zip","godel_config_etag":"50d9fa0a9ab30e3795dc8da0e6f90487"},"variant_type":"EXPERIMENTAL"},{"context_id":"c1452dd5094bfc89985f33422354644d674db651264f9a6dd8e7adf59a314b06","id":"7114209005786849280-control","override_id":"26524b54cb0b9a73173dc65f05330e3fed876fb694db019d2ca573c8ed819ee4","overrides":{"godel_config":"https://assets.juspay.in/hyper/bundles/config/in.juspay.godel/1.2.13/v1-config.zip","godel_config_etag":"afb52abf821aad45674ae78317ecf53d"},"variant_type":"CONTROL"}]	cac.admin@juspay.in	
7114915282043125760	2023-10-03 04:43:12+00	cac.admin@juspay.in	2023-10-03 04:43:12+00	Updating godel for all Android	{godel_placeholder_config,godel_placeholder_config_etag,godel_config,godel_config_etag,godel_acs,godel_acs_etag,godel_placeholder_acs,godel_placeholder_acs_etag}	CREATED	0	{"and":[{"==":[{"var":"os"},"android"]}]}	[{"context_id":"eda51638bac9d18931dc22611c90c245b0a92acbfc9734828d9cb04ab5fa978c","id":"7114915282043125760-test","override_id":"1a3c45127ec61377d38cfa5d61d161f4c1e9617aac5758288374844f81ce3c95","overrides":{"godel_acs":"https://assets.juspay.in/hyper/bundles/acs/in.juspay.godel/1.2.109/v1-acs.zip","godel_acs_etag":"7c16fd41bbc9af01bc53ddea98b75f31","godel_config":"https://assets.juspay.in/hyper/bundles/config/in.juspay.godel/1.2.18/v1-config.zip","godel_config_etag":"d4bbf0748060e6e873eb883af6a1ff09","godel_placeholder_acs":"https://assets.juspay.in/hyper/bundles/acs/in.juspay.godel/1.2.109/v1-acs.zip","godel_placeholder_acs_etag":"7c16fd41bbc9af01bc53ddea98b75f31","godel_placeholder_config":"https://assets.juspay.in/hyper/bundles/config/in.juspay.godel/1.2.18/v1-config.zip","godel_placeholder_config_etag":"d4bbf0748060e6e873eb883af6a1ff09"},"variant_type":"EXPERIMENTAL"},{"context_id":"5ff9b14b5b370e96ff3866f2e7795f0603939ba43e0491f0593b61ccbb054179","id":"7114915282043125760-control","override_id":"42a6fdf08e7ec3481e498211a9c8c51567c32c2afa4113ee85e391e74913e1b9","overrides":{"godel_acs":"https://assets.juspay.in/hyper/bundles/acs/in.juspay.godel/1.2.106/v1-acs.zip","godel_acs_etag":"3cb9454c5ec2e38e3b74e11b1ec1071c","godel_config":"https://assets.juspay.in/hyper/bundles/config/in.juspay.godel/1.2.13/v1-config.zip","godel_config_etag":"afb52abf821aad45674ae78317ecf53d","godel_placeholder_acs":"https://assets.juspay.in/hyper/bundles/acs/in.juspay.godel/1.2.106/v1-acs.zip","godel_placeholder_acs_etag":"3cb9454c5ec2e38e3b74e11b1ec1071c","godel_placeholder_config":"https://assets.juspay.in/hyper/bundles/config/in.juspay.godel/1.2.13/v1-config.zip","godel_placeholder_config_etag":"afb52abf821aad45674ae78317ecf53d"},"variant_type":"CONTROL"}]	cac.admin@juspay.in	
7122929931712266240	2023-10-25 07:30:33+00	ritick.madaan@juspay.in	2023-10-25 07:32:07+00	experiment-1	{pmTestKey1,pmTestKey2}	CONCLUDED	35	{"and":[{"==":[{"var":"os"},"ios"]},{"==":[{"var":"client"},"testClientCac1"]}]}	[{"context_id":"3d4e5429c33881be263a7583407acd9548af2bd83cbe730ac8c8fb5e97d6f079","id":"7122929931712266240-control","override_id":"085e637459203733acb435d0bd921081b10a46a3eb192924a332e414bf266cf7","overrides":{"pmTestKey1":"value1-control","pmTestKey2":"value1-control"},"variant_type":"CONTROL"},{"context_id":"1a7b31bd94857f09edf0c69130af94bc70d830b5b8ebf021f48d1a04eeab7dfc","id":"7122929931712266240-test1","override_id":"65bcc9acaebd88355f850d7087e269f36d1316d7aeeddcf72cece682f99691c1","overrides":{"pmTestKey1":"value2-test","pmTestKey2":"value2-test"},"variant_type":"EXPERIMENTAL"}]	ritick.madaan@juspay.in	7122929931712266240-control
7143475879880847360	2023-12-21 00:12:49+00	rohan.pawar@juspay.in	2023-12-26 08:57:17+00	mobius test 3	{hyperpay_configuration_etag,hyperpay_configuration}	CONCLUDED	5	{"and":[{"==":[{"var":"clientId"},"galactustest"]},{"==":[{"var":"os"},"android"]}]}	[{"context_id":"2f7025565529ba3ee88273450aa4f74130bd03f6437c9a219023e09f57543e16","id":"7143475879880847360-test","override_id":"7ca1e210b75248afda6ae04f074f0dcc8a8f729f897e9f9113a3009e087c01b7","overrides":{"hyperpay_configuration":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/magicbrickspp/configuration/1.1/v1-configuration.zip","hyperpay_configuration_etag":"5f00e94dc26e6d96a95e4a2888b42f4d"},"variant_type":"EXPERIMENTAL"},{"context_id":"cbef7694ccc7ed44922ca7505dd0057904113b1545ab16488407598ced783b17","id":"7143475879880847360-control","override_id":"afb1f8291942d3276f2259b285f7a021e7ffb2a6bb4038b21976e3d59d6abde8","overrides":{"hyperpay_configuration":"default_str_ignore_this","hyperpay_configuration_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}]	mobius@juspay.in	7143475879880847360-control
7115685286169178112	2023-10-05 07:42:55+00	ritick.madaan@juspay.in	2023-10-05 08:13:26+00	test	{godel_config,godel_config_etag}	CONCLUDED	10	{"and":[{"==":[{"var":"os"},"android"]}]}	[{"context_id":"5d19199f9d94bd9fba2b1e5eb519ee2c19d7c50e16f2c9d61823f5e70dc29bbe","id":"7115685286169178112-test","override_id":"4745da36fa0f7521d691393089bf2675db65684ce5e6fbca8e4cd829aca81398","overrides":{"godel_config":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/bbdaily/ios/release/config.json","godel_config_etag":"49413dd07bf851cc25df695b2a387c15"},"variant_type":"EXPERIMENTAL"},{"context_id":"decde2da037eac150ad8907aefe2edd9d5917ec3e7dfeb0edb7dba296cdc58f7","id":"7115685286169178112-control","override_id":"c41790e9f50f0660a2c7c96cd3e4c55e7d1ba02c76081f3dc02fa68f9e280fbf","overrides":{"godel_config":"default_str_ignore_this","godel_config_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}]	ritick.madaan@juspay.in	7115685286169178112-test
7115695675500355584	2023-10-05 08:24:12+00	ritick.madaan@juspay.in	2023-10-05 08:25:19+00	test	{godel_config,godel_config_etag}	CONCLUDED	0	{"and":[{"==":[{"var":"os"},"android"]}]}	[{"context_id":"d83e251a8662237c502fcfd06bda4bb24d39c4fad6293bac28a7046bbb479eee","id":"7115695675500355584-test","override_id":"3223b05a93511419d7d27083425099179d5d8422c745edfab685470786a94951","overrides":{"godel_config":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/trell/web/release/config.json","godel_config_etag":"c5ca9a450c7ed1012422bfae3978fc02"},"variant_type":"EXPERIMENTAL"},{"context_id":"432f6c2004d98f8b4af8a8e8087488e9739d0ce9903dd81ab2935ab74dfce0b6","id":"7115695675500355584-control","override_id":"c41790e9f50f0660a2c7c96cd3e4c55e7d1ba02c76081f3dc02fa68f9e280fbf","overrides":{"godel_config":"default_str_ignore_this","godel_config_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}]	ritick.madaan@juspay.in	7115695675500355584-control
7115701752172015616	2023-10-05 08:48:21+00	ritick.madaan@juspay.in	2023-10-05 08:48:25+00	asgsad	{arya_root}	CONCLUDED	0	{"and":[{"==":[{"var":"os"},"android"]}]}	[{"context_id":"e6aa84b6a7de16c38333364094fa385a11067d6231447b898dd84b7834a21a2e","id":"7115701752172015616-test","override_id":"e1dbfce657117e9d784d15c24927cdfcfb8b76d5e1a3d8bfa215cf7e6533efb2","overrides":{"arya_root":"https://google.com"},"variant_type":"EXPERIMENTAL"},{"context_id":"05c195c94c9bb76a8c78ba316112af165a44858a59fc86e719d9e672bb883e00","id":"7115701752172015616-control","override_id":"ddb5ef62eea8b7777d3379b4c450ce7a14676104298df2c05af5a4a99745b3d1","overrides":{"arya_root":"default_str_ignore_this"},"variant_type":"CONTROL"}]	ritick.madaan@juspay.in	7115701752172015616-control
7115728357986885632	2023-10-05 10:34:04+00	ritick.madaan@juspay.in	2023-10-05 10:39:00+00	Ritick	{godel_config,godel_config_etag}	CONCLUDED	0	{"and":[{"==":[{"var":"clientId"},"galactustest"]},{"==":[{"var":"os"},"android"]}]}	[{"context_id":"328b07bd59b58649487af92434bad944be704c64b30c723c218a1b5967d61e61","id":"7115728357986885632-test","override_id":"4745da36fa0f7521d691393089bf2675db65684ce5e6fbca8e4cd829aca81398","overrides":{"godel_config":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/bbdaily/ios/release/config.json","godel_config_etag":"49413dd07bf851cc25df695b2a387c15"},"variant_type":"EXPERIMENTAL"},{"context_id":"771659ba0a62a76889376e48ab6cf3c21fff5fd5f4135ddcace0695c022e3a50","id":"7115728357986885632-control","override_id":"c41790e9f50f0660a2c7c96cd3e4c55e7d1ba02c76081f3dc02fa68f9e280fbf","overrides":{"godel_config":"default_str_ignore_this","godel_config_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}]	ritick.madaan@juspay.in	7115728357986885632-test
7155868988351938560	2024-01-24 04:58:36+00	aman.jain@juspay.in	2024-01-24 04:58:36+00	test	{arya_bundle_etag,arya_bundle}	CREATED	0	{"and":[{"==":[{"var":"clientId"},"zee5"]},{"==":[{"var":"os"},"android"]}]}	[{"context_id":"890eab4596bf892aeddbe2f72b25e307b24f1d6afec0086613964495fd98b907","id":"7155868988351938560-test","override_id":"1d2a19ffd7496afabb57adf22114bf46568b5a1736328ea2b4e6c29fa3035ffa","overrides":{"arya_bundle":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/tatasky/android/release/config.json","arya_bundle_etag":"94c8b7e65db60ec831fe580b3f52390b"},"variant_type":"EXPERIMENTAL"},{"context_id":"b9f8e3841f87d308a062f713b39564fafcffc5dd359204ea24ea8432e18cce75","id":"7155868988351938560-control","override_id":"298c25615106b78712c4753c98daf991a0551dae7c5bf42e177729cd348aafbf","overrides":{"arya_bundle":"default_str_ignore_this","arya_bundle_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}]	aman.jain@juspay.in	
7117130589057921024	2023-10-09 07:26:02+00	aman.jain@juspay.in	2023-10-09 07:27:13+00	test124	{godel_config,godel_config_etag}	CONCLUDED	10	{"and":[{"==":[{"var":"clientId"},"galactustest"]},{"==":[{"var":"os"},"android"]}]}	[{"context_id":"cfa190d1ddd9f9720d159bd70c33215dc7c7d47342992dcadb3f83bb8d3fe3a6","id":"7117130589057921024-test","override_id":"64ab3462b7633f5163ce42aed966469e2d11aea2e4a19ae24e78d9b7d76ec771","overrides":{"godel_config":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.46/v1-tracker.jsa","godel_config_etag":"a80e7758201c7d654ad9a70049ae36"},"variant_type":"EXPERIMENTAL"},{"context_id":"33c7bb9bc4e13ebccfb00e78ad0ac41b905c05917030dcbe176fd464e77cb07b","id":"7117130589057921024-control","override_id":"4745da36fa0f7521d691393089bf2675db65684ce5e6fbca8e4cd829aca81398","overrides":{"godel_config":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/bbdaily/ios/release/config.json","godel_config_etag":"49413dd07bf851cc25df695b2a387c15"},"variant_type":"CONTROL"}]	aman.jain@juspay.in	7117130589057921024-control
7117131074927673344	2023-10-09 07:27:58+00	aman.jain@juspay.in	2023-10-09 07:28:17+00	test124	{godel_config,godel_config_etag}	CONCLUDED	0	{"and":[{"==":[{"var":"clientId"},"galactustest"]},{"==":[{"var":"os"},"android"]}]}	[{"context_id":"8577f9b299b463f78c0b2d2d0d41945284470d7a4535d17b8163e8f6d023ca59","id":"7117131074927673344-test","override_id":"64ab3462b7633f5163ce42aed966469e2d11aea2e4a19ae24e78d9b7d76ec771","overrides":{"godel_config":"https://assets.juspay.in/hyper/bundles/app/tracker/2.0.46/v1-tracker.jsa","godel_config_etag":"a80e7758201c7d654ad9a70049ae36"},"variant_type":"EXPERIMENTAL"},{"context_id":"712cd296fba53d461229e0b31f201189ca54adae529bdf06ebb97efd370dc48c","id":"7117131074927673344-control","override_id":"4745da36fa0f7521d691393089bf2675db65684ce5e6fbca8e4cd829aca81398","overrides":{"godel_config":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/bbdaily/ios/release/config.json","godel_config_etag":"49413dd07bf851cc25df695b2a387c15"},"variant_type":"CONTROL"}]	aman.jain@juspay.in	7117131074927673344-test
7155869386936647680	2024-01-24 05:00:11+00	aman.jain@juspay.in	2024-01-24 05:00:11+00	adadwd	{arya_bundle_etag,arya_bundle}	CREATED	0	{"and":[{"==":[{"var":"clientId"},"zee5"]},{"==":[{"var":"tier"},"t1"]},{"==":[{"var":"os"},"android"]},{"==":[{"var":"track"},"f1"]}]}	[{"context_id":"70f7e361bd73d1a9cf66cdbd669ee367df6874ba250bfb444aead8977b8ac8cf","id":"7155869386936647680-test","override_id":"1d2a19ffd7496afabb57adf22114bf46568b5a1736328ea2b4e6c29fa3035ffa","overrides":{"arya_bundle":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/tatasky/android/release/config.json","arya_bundle_etag":"94c8b7e65db60ec831fe580b3f52390b"},"variant_type":"EXPERIMENTAL"},{"context_id":"772c7ad1913cd5f18b68267c8823b2ee28314048cd476202a348d2bbd0b71a4f","id":"7155869386936647680-control","override_id":"298c25615106b78712c4753c98daf991a0551dae7c5bf42e177729cd348aafbf","overrides":{"arya_bundle":"default_str_ignore_this","arya_bundle_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}]	aman.jain@juspay.in	
7119946958782308352	2023-10-17 01:57:17+00	aman.jain@juspay.in	2023-11-22 04:01:40+00	dawd	{arya_bundle,arya_bundle_etag}	CONCLUDED	0	{"and":[{"==":[{"var":"os"},"android"]}]}	[{"context_id":"b2cb511720c0bd3463d06fa32d638eeece42f2121827dbaeede7198ea9fe9835","id":"7119946958782308352-test","override_id":"8ea04458d6c10aae8233c100c888f8fda9232d4e993076d6d2cedba6a976df86","overrides":{"arya_bundle":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/tatasky/android/release/config.json","arya_bundle_etag":"6e4c0221f46858983520bf57008994d0"},"variant_type":"EXPERIMENTAL"},{"context_id":"010c36be24d11030d5c70aa617deb333825f68cba6f7c47bfcc85fcfcb0a2f47","id":"7119946958782308352-control","override_id":"f27a63e7518f70f840d57b4545a4cddde709bb3340d440fe21f584d5820710c5","overrides":{"arya_bundle":"default_str_ignore_this","arya_bundle_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}]	ritick.madaan@juspay.in	7119946958782308352-control
7117777147781951488	2023-10-11 02:15:13+00	aman.jain@juspay.in	2023-11-22 04:01:55+00	wdadad	{arya_bundle,arya_bundle_etag}	CONCLUDED	0	{"and":[{"==":[{"var":"os"},"android"]}]}	[{"context_id":"fa53907e5710dcff8f793947868d3399767c1511afaa4742c1ad3a473a4a3b2b","id":"7117777147781951488-test","override_id":"fd704e6c2955c45b7c24602cebf461f7dfc4ec386a5e18247602af5d30086f30","overrides":{"arya_bundle":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/tatasky/android/release/config.json","arya_bundle_etag":"8eaf6b332b19bfa8684a125a0956dd84"},"variant_type":"EXPERIMENTAL"},{"context_id":"91aa3f53ec6c269a3d9e4311aa3634dbeb5b74af19f0d6f265163630914d9e97","id":"7117777147781951488-control","override_id":"f27a63e7518f70f840d57b4545a4cddde709bb3340d440fe21f584d5820710c5","overrides":{"arya_bundle":"default_str_ignore_this","arya_bundle_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}]	ritick.madaan@juspay.in	7117777147781951488-test
7119946863389589504	2023-10-17 01:56:54+00	aman.jain@juspay.in	2023-11-22 04:02:12+00	qwertyui	{godel_acs,godel_acs_etag}	CONCLUDED	0	{"and":[{"==":[{"var":"os"},"android"]}]}	[{"context_id":"eb53455e094812d911d219cd33d332e3217739cc9df7bde87757848de98b68cf","id":"7119946863389589504-test","override_id":"8f20a6fb87c619aacee503ddccf367be4b9a4195f6a9ae7dfa84ec455e060be5","overrides":{"godel_acs":"https://assets.juspay.in/hyper/bundles/acs/in.juspay.godel/1.2.112/v1-acs.zip","godel_acs_etag":"1fe12290c99844bade11626c391092"},"variant_type":"EXPERIMENTAL"},{"context_id":"bc2c177c4c136e3283efeea7d607f57bfbe860175ad26210c3d1c5697ceebf8e","id":"7119946863389589504-control","override_id":"3b0c2f917c497aa84f5ffd6ac18f6f65869e39325284839be7af9359a79efa75","overrides":{"godel_acs":"default_str_ignore_this","godel_acs_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}]	ritick.madaan@juspay.in	7119946863389589504-control
7115731532762300416	2023-10-05 10:46:41+00	ritick.madaan@juspay.in	2023-10-25 07:55:13+00	demo	{godel_config,godel_config_etag}	CONCLUDED	0	{"and":[{"==":[{"var":"clientId"},"galactustest"]},{"==":[{"var":"os"},"android"]}]}	[{"context_id":"4d9910251fd556e00163f2fc6166ba7b23d3576d92fadf1ba535ba0b7f078ebc","id":"7115731532762300416-test","override_id":"4745da36fa0f7521d691393089bf2675db65684ce5e6fbca8e4cd829aca81398","overrides":{"godel_config":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/bbdaily/ios/release/config.json","godel_config_etag":"49413dd07bf851cc25df695b2a387c15"},"variant_type":"EXPERIMENTAL"},{"context_id":"a6d7a86680d9786bf2775c8ea49c68b49d9e66f3df3412ea91bd1d577adc42b3","id":"7115731532762300416-control","override_id":"c41790e9f50f0660a2c7c96cd3e4c55e7d1ba02c76081f3dc02fa68f9e280fbf","overrides":{"godel_config":"default_str_ignore_this","godel_config_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}]	ritick.madaan@juspay.in	7115731532762300416-control
7120300789818368000	2023-10-18 01:23:17+00	aman.jain@juspay.in	2023-10-31 02:33:43+00	test5	{arya_bundle,godel_bundle,arya_bundle_etag,godel_bundle_etag}	CONCLUDED	0	{"and":[{"==":[{"var":"os"},"android"]}]}	[{"context_id":"fa7932cec2a2919dc950109e9400f1b7eefffd0b608bbda5455e96b63e67388a","id":"7120300789818368000-test","override_id":"e5ce1c03bb1d2a500b3dde34a3a135c86c7ee1798c14c378a3aad27cd72cfb60","overrides":{"arya_bundle":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/tatasky/android/release/config.json","arya_bundle_etag":"6e4c0221f46858983520bf57008994d0","godel_bundle":"https://assets.juspay.in/juspay/payments/in.juspay.dotp/release/v1-config.zip","godel_bundle_etag":"d1be46cb32c6ae215f492b0b963486b7"},"variant_type":"EXPERIMENTAL"},{"context_id":"043927f6216617dc6ce25ca0dde84f935fc0bb3d47937ace2ac3e902705c81f2","id":"7120300789818368000-control","override_id":"5c32fc6689590dd01a80c9c51cade1967de3080e6ac8ff5d9debbf752c7d31bb","overrides":{"arya_bundle":"default_str_ignore_this","arya_bundle_etag":"default_str_ignore_this","godel_bundle":"default_str_ignore_this","godel_bundle_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}]	bhawesh.agarwal@juspay.in	7120300789818368000-control
7188493072320155648	2024-04-23 05:35:04+00	shubhranshu.sanjeev@juspay.in	2024-04-23 05:35:11+00	hsbc sbx unified loader	{hyperos_placeholder_loader}	CONCLUDED	0	{"and":[{"==":[{"var":"clientId"},"testhsbc"]},{"==":[{"var":"os"},"web"]}]}	[{"context_id":"9f6d73797bf206e037fed942bf1f8553a7dfe641ed661b5fe289f5e6844d80c3","id":"7188493072320155648-control","override_id":"8768472bfbb5a3783bd6e2d7c0c3e332fc1e4f3039ced882ca57d473204c9916","overrides":{"hyperos_placeholder_loader":"https://assets.juspay.in/hyper/bundles/app/loader/hsbc/1.0.4/web/loader.js"},"variant_type":"CONTROL"},{"context_id":"2bc11735f1d1d74bfc002a2a40ff391c9a2f0195197751e3dd8ea5b9f2f743bc","id":"7188493072320155648-experimental","override_id":"8768472bfbb5a3783bd6e2d7c0c3e332fc1e4f3039ced882ca57d473204c9916","overrides":{"hyperos_placeholder_loader":"https://assets.juspay.in/hyper/bundles/app/loader/hsbc/1.0.4/web/loader.js"},"variant_type":"EXPERIMENTAL"}]	shubhranshu.sanjeev@juspay.in	7188493072320155648-experimental
7188571544521814016	2024-04-23 10:46:53+00	shubhranshu.sanjeev@juspay.in	2024-04-23 10:46:53+00	test_experiment_with_multiple_clientids	{pmTestKey1}	CREATED	0	{"IN":[{"var":"clientId"},"galactustest,zee5"]}	[{"context_id":"ab56204accbea23430e6e1ff1d80065772e52805db222f3d10d45e893c7958c4","id":"7188571544521814016-control","override_id":"7ead5b38c5401a989e990ccccdb01f7cd27017aee111160aee5c452bea6a6592","overrides":{"pmTestKey1":"abc"},"variant_type":"CONTROL"},{"context_id":"73f05457e20fab7000c44a3c58a4fca66819b9c93b7ae447fba936813d1c05ae","id":"7188571544521814016-experimental","override_id":"db62c9ec706fd492b89eee9400d0237c2144a908503f07fdf07bf94b9aad1ea2","overrides":{"pmTestKey1":"zxc"},"variant_type":"EXPERIMENTAL"}]	shubhranshu.sanjeev@juspay.in	
7125099791835406336	2023-10-31 07:12:48+00	kartik.gajendra@juspay.in	2023-11-22 04:01:03+00	galactustest-experiment	{godel_config}	CONCLUDED	0	{"and":[{"==":[{"var":"os"},"android"]},{"==":[{"var":"clientId"},"galactustest"]}]}	[{"context_id":"b9304183b5585116196c5aea6aa279990c52387d8ab10e76b73f54c1c7e2e91f","id":"7125099791835406336-control","override_id":"dece633f19bfbce7315e7dae457a58485af60f8d482270d74553801ab5e1d59e","overrides":{"godel_config":"abc"},"variant_type":"CONTROL"},{"context_id":"e22dbb26e053cba6963d36382fedfa5a2d6b53cba610d350ddaffa5444ec094c","id":"7125099791835406336-test1","override_id":"7e0ea3dabfdf0e5f03c85015c4cc62a56a46d04b5d00e1d7dfd17aaed635184e","overrides":{"godel_config":"godel_experimental"},"variant_type":"EXPERIMENTAL"}]	ritick.madaan@juspay.in	7125099791835406336-control
7119952192400715776	2023-10-17 02:18:05+00	aman.jain@juspay.in	2023-11-22 04:01:14+00	test3	{godel_bundle,godel_bundle_etag}	CONCLUDED	0	{"and":[{"==":[{"var":"clientId"},"zepto"]},{"==":[{"var":"os"},"android"]}]}	[{"context_id":"7eb21477bc6a72cbdb7e9fec90d8ccb33c50ec6825419cb2357adc58c6663c9a","id":"7119952192400715776-test","override_id":"17017d6faf68fcd0790e166da1a5a4e858c81863042a87e0f3776c3fac284c7c","overrides":{"godel_bundle":"https://assets.juspay.in/juspay/payments/in.juspay.ec/release/v1-config.zip","godel_bundle_etag":"01ec08875f5d676ef172339e7d3f9b8f"},"variant_type":"EXPERIMENTAL"},{"context_id":"e49eef27a7536f03a7fe534912f43e4f7d28a473a684dc0f59b5567ca87a1c58","id":"7119952192400715776-control","override_id":"a811d64f4dc78535ff5a8a8ecf114089563a07e40a2311c44c8e99ce373eae20","overrides":{"godel_bundle":"default_str_ignore_this","godel_bundle_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}]	ritick.madaan@juspay.in	7119952192400715776-control
7133465574847229952	2023-11-23 09:15:26+00	mobius@juspay.in	2023-11-29 04:17:01+00	experiment-1	{pmTestKey1,pmTestKey2}	CONCLUDED	10	{"and":[{"==":[{"var":"os"},"ios"]},{"==":[{"var":"client"},"testClientCac1"]}]}	[{"context_id":"6923a97c98b435527b7a09a8a4fac353c89f5d450875b9813888c972d8149cb8","id":"7133465574847229952-control","override_id":"2123fadefc2e34d386e7ebb8851ab290ac4b2fad87e353745e3387025ffa81ff","overrides":{"pmTestKey1":"value1-control","pmTestKey2":"value1-control"},"variant_type":"CONTROL"},{"context_id":"829d5797818f5f8a67da6792317de3cc4c871acb82663528a5e56f33645a40af","id":"7133465574847229952-test1","override_id":"67d10870bc1289e3e0544fe29aa878b470344a319ed217d78cba21ff5c622db1","overrides":{"pmTestKey1":"value2-test","pmTestKey2":"value2-test"},"variant_type":"EXPERIMENTAL"}]	mobius@juspay.in	7133465574847229952-test1
7125136499787333632	2023-10-31 09:38:40+00	ritick.madaan@juspay.in	2023-10-31 09:42:07+00	galactustest test	{godel_config,godel_config_etag}	CONCLUDED	0	{"and":[{"==":[{"var":"clientId"},"galactustest"]},{"==":[{"var":"os"},"android"]}]}	[{"context_id":"1a846c62e293d20b6bf1a1c3124a949b62c84873942f0fa13d1e5933563fa976","id":"7125136499787333632-test","override_id":"0844d58fec6b4f45def66d51acea3c88e2a40782a120301038c0f13c61da2746","overrides":{"godel_config":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/bbdaily/ios/release/config.json","godel_config_etag":"d574dfb82a2e851145a0c4070ef0d6e4"},"variant_type":"EXPERIMENTAL"},{"context_id":"a1fadb932e9154f4811b869b67fb5f4b5930eb120ec1a6183e49712369c48f23","id":"7125136499787333632-control","override_id":"5a59fbf006c9929350c7101f7621297b130206246ae69c8536f4e068778089b4","overrides":{"godel_config":"abc","godel_config_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}]	ritick.madaan@juspay.in	7125136499787333632-test
7145416915827904512	2023-12-26 08:45:48+00	rohan.pawar@juspay.in	2023-12-27 01:14:48+00	mobius test 3	{hyperpay_configuration,hyperpay_configuration_etag}	CONCLUDED	25	{"and":[{"==":[{"var":"clientId"},"galactustest"]},{"==":[{"var":"os"},"android"]}]}	[{"context_id":"ec8e160e307491e8dd3e379b4fa23987b4381bcd3753dfd4700871650314f141","id":"7145416915827904512-test","override_id":"7ca1e210b75248afda6ae04f074f0dcc8a8f729f897e9f9113a3009e087c01b7","overrides":{"hyperpay_configuration":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/magicbrickspp/configuration/1.1/v1-configuration.zip","hyperpay_configuration_etag":"5f00e94dc26e6d96a95e4a2888b42f4d"},"variant_type":"EXPERIMENTAL"},{"context_id":"3da48a2c5960757eac62187a38f371751ba9c52e30dd6bb3b77e65e41673eecd","id":"7145416915827904512-control","override_id":"afb1f8291942d3276f2259b285f7a021e7ffb2a6bb4038b21976e3d59d6abde8","overrides":{"hyperpay_configuration":"default_str_ignore_this","hyperpay_configuration_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}]	mobius@juspay.in	7145416915827904512-control
7169247805271527424	2024-03-01 03:01:15+00	shubhranshu.sanjeev@juspay.in	2024-03-01 03:01:15+00	etag-autofill-test	{godel_config,godel_config_etag}	CREATED	0	{"and":[{"==":[{"var":"clientId"},"galactustest"]},{"==":[{"var":"os"},"android"]}]}	[{"context_id":"2c48644d9e00422d6fe4859cbd64dba57e40c090f15e7971e7e722829e1a25bb","id":"7169247805271527424-test","override_id":"badeef68ab9f6a694fb73a623ed3186c8c37df7e31aee1b0791f186c369a265f","overrides":{"godel_config":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/bbdaily/ios/release/config.json","godel_config_etag":"81243ede6c3fbbb38f3eca88084568c7"},"variant_type":"EXPERIMENTAL"},{"context_id":"3409119bfa87ce154c38d33eaf3a56a443d85e61fdab116e4c9a6f3393145581","id":"7169247805271527424-control","override_id":"81159838966447ace3e7be99a17e7b1a2e6a7ba2ecc98da4899f0e56f4539d59","overrides":{"godel_config":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/bbdaily/ios/release/config.json","godel_config_etag":"d574dfb82a2e851145a0c4070ef0d6e4"},"variant_type":"CONTROL"}]	shubhranshu.sanjeev@juspay.in	
7189244532330844160	2024-04-25 07:21:06+00	shubhranshu.sanjeev@juspay.in	2024-04-25 07:23:31+00	dummyexperiment	{arya_src_etag,arya_bundle,arya_src,arya_bundle_etag}	CREATED	0	{"and":[{"==":[{"var":"os"},"ios"]},{"==":[{"var":"clientId"},"galactustest"]}]}	[{"context_id":"93930c03ba039a477447e609820c97e8b6f51ddfb320d84c2b667fc22c549d67","id":"7189244532330844160-test","override_id":"28bac9d32e47ec65af26f53af1649ee731ebd9c22018bc493d5f708162c228e9","overrides":{"arya_bundle":"default_str_ignore_this","arya_bundle_etag":"default_str_ignore","arya_src":"default_str_ignore_this","arya_src_etag":"default_str_ignore"},"variant_type":"EXPERIMENTAL"},{"context_id":"497eef54d2cea2869dc3244d4b83c6031453ec453152392e62613a93a03775ce","id":"7189244532330844160-control","override_id":"28d94700c3c4e2e2c7f4d41154e992419bbc696c020cac765bdd02703ae36800","overrides":{"arya_bundle":"default_str_ignore_this","arya_bundle_etag":"default_str_ignore_this","arya_src":"default_str_ignore_this","arya_src_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}]	shubhranshu.sanjeev@juspay.in	
7125752957852614656	2023-11-02 02:28:15+00	kartik.gajendra@juspay.in	2023-11-16 08:39:22+00	galactustest-experiment	{godel_config}	CONCLUDED	46	{"and":[{"==":[{"var":"os"},"android"]},{"==":[{"var":"clientId"},"galactustest"]}]}	[{"context_id":"7c44f356718b19cd169b73eb9e7300597bd66ef2112da93adb1753adfce84031","id":"7125752957852614656-control","override_id":"dece633f19bfbce7315e7dae457a58485af60f8d482270d74553801ab5e1d59e","overrides":{"godel_config":"abc"},"variant_type":"CONTROL"},{"context_id":"8ab020d72267773d1755e252902514e58d7a4f4396c72305e0c029dcf53d2be9","id":"7125752957852614656-test1","override_id":"07d8ece9a73705f90c31b69488667f28b23e8e25f8510ff1ab9ff23cf58b8e71","overrides":{"godel_config":"godel_experimental_1"},"variant_type":"EXPERIMENTAL"}]	mobius@juspay.in	7125752957852614656-control
7127260768432291840	2023-11-06 06:19:45+00	rohan.pawar@juspay.in	2023-11-22 04:00:44+00	MobiusTestRelease	{vies_entry}	CONCLUDED	0	{"and":[{"==":[{"var":"os"},"android"]}]}	[{"context_id":"66e211d98b9677c75a667613e99226a4b163195a9861adc8565fd22a74e8a624","id":"7127260768432291840-test","override_id":"a18eb9716d651eae11cf161f28db20ce789df6fbd95dd3de2a65a1171fd409ec","overrides":{"vies_entry":"test-value"},"variant_type":"EXPERIMENTAL"},{"context_id":"9b574714dff4c827455a1504d934ce4dd6efbd90cb297c72e4be8095b97b3099","id":"7127260768432291840-control","override_id":"80813c2b5591d1f4894a5f13bf14365be86f8c8fb7505b61908011bc6ca4cd44","overrides":{"vies_entry":"default_str_ignore_this"},"variant_type":"CONTROL"}]	ritick.madaan@juspay.in	7127260768432291840-control
7125752978719277056	2023-11-02 02:28:20+00	kartik.gajendra@juspay.in	2023-11-22 04:00:48+00	galactustest-experiment	{godel_config}	CONCLUDED	0	{"and":[{"==":[{"var":"os"},"android"]},{"==":[{"var":"clientId"},"galactustest"]}]}	[{"context_id":"6c7fa8e753496acc9b09b023933d251e84b08bc954795fac885785b4e64674d3","id":"7125752978719277056-control","override_id":"dece633f19bfbce7315e7dae457a58485af60f8d482270d74553801ab5e1d59e","overrides":{"godel_config":"abc"},"variant_type":"CONTROL"},{"context_id":"38b782a1ddc8708e766af966c8a11f215b205412ebf389a07a4c419f2990c06f","id":"7125752978719277056-test1","override_id":"7e0ea3dabfdf0e5f03c85015c4cc62a56a46d04b5d00e1d7dfd17aaed635184e","overrides":{"godel_config":"godel_experimental"},"variant_type":"EXPERIMENTAL"}]	ritick.madaan@juspay.in	7125752978719277056-control
7128366584299397120	2023-11-09 07:33:52+00	ritick.madaan@juspay.in	2023-11-09 07:57:52+00	zee5 test	{hyperos_placeholder_tracker_etag,hyperos_placeholder_tracker}	CONCLUDED	20	{"and":[{"==":[{"var":"clientId"},"zee5"]},{"==":[{"var":"os"},"android"]}]}	[{"context_id":"b28723ca5117a63bf96bd0d850f2af77a262d69b0d929d3dc17ab9a67c7ea1c3","id":"7128366584299397120-test","override_id":"5cc7746761839b528cc56948b0962e02da0abb49b9e33dda0954984d90830828","overrides":{"hyperos_placeholder_tracker":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/galactustest/android/release/config.json","hyperos_placeholder_tracker_etag":"f43c71f605bc96833153885453edbf47"},"variant_type":"EXPERIMENTAL"},{"context_id":"b57ffa231aaaf6d1f03f94eb24db990793bf632333025b9787a54100fcf8e216","id":"7128366584299397120-control","override_id":"25bbfdf9977c2087c7cfa786cfe826f14bbf4ec8f0ce2d03943661201325a4e5","overrides":{"hyperos_placeholder_tracker":"default_str_ignore_this","hyperos_placeholder_tracker_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}]	ritick.madaan@juspay.in	7128366584299397120-test
7145699287093825536	2023-12-27 03:27:50+00	rohan.pawar@juspay.in	2023-12-27 03:27:50+00	mobius test 3	{hyperpay_configuration,hyperpay_configuration_etag}	CREATED	0	{"and":[{"==":[{"var":"clientId"},"galactustest"]},{"==":[{"var":"os"},"android"]}]}	[{"context_id":"faec42a890e61480cecd6a7d80d02eba8b191274ae622f10576d9ee0e9f20273","id":"7145699287093825536-test","override_id":"7ca1e210b75248afda6ae04f074f0dcc8a8f729f897e9f9113a3009e087c01b7","overrides":{"hyperpay_configuration":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/magicbrickspp/configuration/1.1/v1-configuration.zip","hyperpay_configuration_etag":"5f00e94dc26e6d96a95e4a2888b42f4d"},"variant_type":"EXPERIMENTAL"},{"context_id":"c80d2c9783437b7d4dab17432608eaa67077d955f91de150a72b42c6d2d60248","id":"7145699287093825536-control","override_id":"afb1f8291942d3276f2259b285f7a021e7ffb2a6bb4038b21976e3d59d6abde8","overrides":{"hyperpay_configuration":"default_str_ignore_this","hyperpay_configuration_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}]	rohan.pawar@juspay.in	
7132254992214142976	2023-11-20 01:05:01+00	mobius@juspay.in	2023-11-22 03:59:33+00	experiment-1	{pmTestKey2,pmTestKey1}	CONCLUDED	0	{"and":[{"==":[{"var":"os"},"ios"]},{"==":[{"var":"client"},"testClientCac1"]}]}	[{"context_id":"b75bdce0d37e8897a73c29c13d1b465b3eddab197f8f4a49ae4a06d6d08feeee","id":"7132254992214142976-control","override_id":"085e637459203733acb435d0bd921081b10a46a3eb192924a332e414bf266cf7","overrides":{"pmTestKey1":"value1-control","pmTestKey2":"value1-control"},"variant_type":"CONTROL"},{"context_id":"c2a6845f03b2e86c917aa48ef4df2511d368fc1e501bfe674cf9d6196748bd23","id":"7132254992214142976-test1","override_id":"65bcc9acaebd88355f850d7087e269f36d1316d7aeeddcf72cece682f99691c1","overrides":{"pmTestKey1":"value2-test","pmTestKey2":"value2-test"},"variant_type":"EXPERIMENTAL"}]	ritick.madaan@juspay.in	7132254992214142976-control
7130919624796684288	2023-11-16 08:38:44+00	mobius@juspay.in	2023-11-22 03:59:38+00	experiment-1	{pmTestKey1,pmTestKey2}	CONCLUDED	0	{"and":[{"==":[{"var":"os"},"ios"]},{"==":[{"var":"client"},"testClientCac1"]}]}	[{"context_id":"10da21e3309c736f4381a6420d62c8a1147dc12dc7ca57c5f98bfdffc1a594d0","id":"7130919624796684288-control","override_id":"085e637459203733acb435d0bd921081b10a46a3eb192924a332e414bf266cf7","overrides":{"pmTestKey1":"value1-control","pmTestKey2":"value1-control"},"variant_type":"CONTROL"},{"context_id":"b522637b6a345591568e4a454ae70b953cbc6bbe617fa28053f064799e4d24bd","id":"7130919624796684288-test1","override_id":"65bcc9acaebd88355f850d7087e269f36d1316d7aeeddcf72cece682f99691c1","overrides":{"pmTestKey1":"value2-test","pmTestKey2":"value2-test"},"variant_type":"EXPERIMENTAL"}]	ritick.madaan@juspay.in	7130919624796684288-control
7122930386611376128	2023-10-25 07:32:21+00	ritick.madaan@juspay.in	2023-11-22 04:01:09+00	experiment-1	{pmTestKey1972,pmTestKey1999}	CONCLUDED	0	{"and":[{"==":[{"var":"os"},"ios"]},{"==":[{"var":"client"},"testClientCac02"]}]}	[{"context_id":"f87a94a3e2ac0cc624b04811aebccb68abf9277ccc61cdea0cca3a88b1c1248d","id":"7122930386611376128-control","override_id":"9802e3201f5948257637154d94b838b345150b9817604c10cf1312c804657a5e","overrides":{"pmTestKey1972":"value-7910-an-control","pmTestKey1999":"value-6910-an-control"},"variant_type":"CONTROL"},{"context_id":"fbf9abaffe4718957f565e2f1587b37db182e3ced7aeb55fa0e03f9f1ef36b6a","id":"7122930386611376128-test1","override_id":"c90c09c89b87e60ad6cec89219ebeb316d85f8dec45a427f809dc44b0b1a7fd4","overrides":{"pmTestKey1972":"value-7920-an-test","pmTestKey1999":"value-6930-an-test"},"variant_type":"EXPERIMENTAL"}]	ritick.madaan@juspay.in	7122930386611376128-control
7133465816902021120	2023-11-23 09:16:24+00	mobius@juspay.in	2023-11-29 04:17:34+00	experiment-1	{pmTestKey2,pmTestKey1}	CONCLUDED	0	{"and":[{"==":[{"var":"os"},"ios"]},{"==":[{"var":"client"},"testClientCac1"]}]}	[{"context_id":"6defc56f692448bb9e1c79aa8a2e02a9868361394ecdf8d29215c8a1b34764d8","id":"7133465816902021120-control","override_id":"2123fadefc2e34d386e7ebb8851ab290ac4b2fad87e353745e3387025ffa81ff","overrides":{"pmTestKey1":"value1-control","pmTestKey2":"value1-control"},"variant_type":"CONTROL"},{"context_id":"2239fc756f765b0d73aa28ea82eed075b7a6ee288703d2906503ca1ba5153e58","id":"7133465816902021120-test1","override_id":"67d10870bc1289e3e0544fe29aa878b470344a319ed217d78cba21ff5c622db1","overrides":{"pmTestKey1":"value2-test","pmTestKey2":"value2-test"},"variant_type":"EXPERIMENTAL"}]	mobius@juspay.in	7133465816902021120-control
7133449316409618432	2023-11-23 08:10:50+00	mobius@juspay.in	2023-11-29 06:38:18+00	experiment-1	{pmTestKey1,pmTestKey2}	CONCLUDED	0	{"and":[{"==":[{"var":"os"},"ios"]},{"==":[{"var":"client"},"testClientCac1"]}]}	[{"context_id":"d1696342273aeb54fb16a9bafe939fefc54015e7e30c27502232186e4159726f","id":"7133449316409618432-control","override_id":"2123fadefc2e34d386e7ebb8851ab290ac4b2fad87e353745e3387025ffa81ff","overrides":{"pmTestKey1":"value1-control","pmTestKey2":"value1-control"},"variant_type":"CONTROL"},{"context_id":"48a6248edc895c858ac9f4dc988d81a5cd302e6a28c2a4adbb1df76e1d1d89d8","id":"7133449316409618432-test1","override_id":"67d10870bc1289e3e0544fe29aa878b470344a319ed217d78cba21ff5c622db1","overrides":{"pmTestKey1":"value2-test","pmTestKey2":"value2-test"},"variant_type":"EXPERIMENTAL"}]	mobius@juspay.in	7133449316409618432-control
7137374889697832960	2023-12-04 04:09:39+00	mobius@juspay.in	2023-12-04 04:23:45+00	hyperpay version 3	{pmTestKey1,pmTestKey2}	CONCLUDED	50	{"and":[{"==":[{"var":"os"},"android"]},{"==":[{"var":"client"},"1mg"]}]}	[{"context_id":"06c651d0f05bc1e81c480e7aa800fecb1d6cbef20d67c2d2a9cfe92bb19ab04b","id":"7137374889697832960-control","override_id":"20351707fa63907b8b5da4c4629e65cd3b248627394c0f3663fd6111e91b7600","overrides":{"pmTestKey1":"value1-control","pmTestKey2":"value2-control"},"variant_type":"CONTROL"},{"context_id":"0f58f73d8a022cf97a6c4a40a264473c0ae991453736196ae587aa680dead4f3","id":"7137374889697832960-test1","override_id":"5ae11a78d829df3744675dc6e97c2f118fe584496b3a566b7a2c418f69ad84cc","overrides":{"pmTestKey1":"value3-test","pmTestKey2":"value4-test"},"variant_type":"EXPERIMENTAL"}]	mobius@juspay.in	7137374889697832960-control
7145699356366950400	2023-12-27 03:28:07+00	rohan.pawar@juspay.in	2024-01-01 08:39:52+00	mobius test 3	{hyperpay_configuration,hyperpay_configuration_etag}	CONCLUDED	50	{"and":[{"==":[{"var":"clientId"},"galactustest"]},{"==":[{"var":"os"},"android"]}]}	[{"context_id":"1e664cce4dbe6b662decaad992f1a074b756aa10bcc5080d0d648d48c0d76fa9","id":"7145699356366950400-test","override_id":"7ca1e210b75248afda6ae04f074f0dcc8a8f729f897e9f9113a3009e087c01b7","overrides":{"hyperpay_configuration":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/magicbrickspp/configuration/1.1/v1-configuration.zip","hyperpay_configuration_etag":"5f00e94dc26e6d96a95e4a2888b42f4d"},"variant_type":"EXPERIMENTAL"},{"context_id":"21052b5a5fb13110fa7479e824ef505ae8f1738eecf764543f47291d6d7948a3","id":"7145699356366950400-control","override_id":"afb1f8291942d3276f2259b285f7a021e7ffb2a6bb4038b21976e3d59d6abde8","overrides":{"hyperpay_configuration":"default_str_ignore_this","hyperpay_configuration_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}]	mobius@juspay.in	7145699356366950400-test
7145699449413390336	2023-12-27 03:28:29+00	rohan.pawar@juspay.in	2024-02-15 00:31:23+00	mobius test 3	{hyperpay_configuration,hyperpay_configuration_etag}	CONCLUDED	50	{"and":[{"==":[{"var":"clientId"},"galactustest"]},{"==":[{"var":"os"},"android"]}]}	[{"context_id":"dbc71929676059eee48f4cd293fa0659762a434d59153a56fe9ab83214249041","id":"7145699449413390336-test","override_id":"7ca1e210b75248afda6ae04f074f0dcc8a8f729f897e9f9113a3009e087c01b7","overrides":{"hyperpay_configuration":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/magicbrickspp/configuration/1.1/v1-configuration.zip","hyperpay_configuration_etag":"5f00e94dc26e6d96a95e4a2888b42f4d"},"variant_type":"EXPERIMENTAL"},{"context_id":"43a9d79dfe3276ce8492897cb59ce23af0dc96aa1b0e28833ea45bdafa95a187","id":"7145699449413390336-control","override_id":"afb1f8291942d3276f2259b285f7a021e7ffb2a6bb4038b21976e3d59d6abde8","overrides":{"hyperpay_configuration":"default_str_ignore_this","hyperpay_configuration_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}]	mobius@juspay.in	7145699449413390336-test
7202261923708796928	2024-05-31 05:27:34+00	deepesh.maheshwari@juspay.in	2024-05-31 05:30:00+00	EC and UPI intent update	{upiintent_bundle,ec_bundle,ec_bundle_etag,upiintent_bundle_etag}	CONCLUDED	0	{"and":[{"==":[{"var":"clientId"},"hdfcmaster"]},{"==":[{"var":"os"},"web"]}]}	[{"context_id":"9620a9542ec1d24cf439de411943d27f1e326353ba40890fa602558bc43ebeb5","id":"7202261923708796928-test","override_id":"bc9fdc3c13a3569d0b30f3877934e3de94b145a1389be32e2f4617528283a20b","overrides":{"ec_bundle":"https://assets.juspay.in/hyper/bundles/app/in.juspay.ec/3.29.4/web/index.js","ec_bundle_etag":"e4b290873d9b7691a2471cdfa58023a4","upiintent_bundle":"https://assets.juspay.in/hyper/bundles/app/in.juspay.upiintent/4.9.9/web/index.js","upiintent_bundle_etag":"9774e9eaec3ec8221bfcb104a4534534"},"variant_type":"EXPERIMENTAL"},{"context_id":"0f8369ce5ae4fab3388300519c93300588fb5a6a5a3b72136e776593eca8d50e","id":"7202261923708796928-control","override_id":"e0f67abcfcb928bb6e84e0a14b8b7a9c44cb96c6e14b8672fab28b0022bf6a23","overrides":{"ec_bundle":"default_str_ignore_this","ec_bundle_etag":"default_str_ignore_this","upiintent_bundle":"default_str_ignore_this","upiintent_bundle_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}]	deepesh.maheshwari@juspay.in	7202261923708796928-test
7138087493665431552	2023-12-06 03:21:17+00	kartik.gajendra@juspay.in	2023-12-06 03:21:38+00	mobius test	{hyperpay_configuration,hyperpay_configuration_etag}	CONCLUDED	1	{"and":[{"==":[{"var":"clientId"},"galactustest"]},{"==":[{"var":"os"},"android"]}]}	[{"context_id":"c6e4355102dede5d1818547fb6b843c719a8a44e59dd3ba565cf5a483fc26b84","id":"7138087493665431552-test","override_id":"7ca1e210b75248afda6ae04f074f0dcc8a8f729f897e9f9113a3009e087c01b7","overrides":{"hyperpay_configuration":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/magicbrickspp/configuration/1.1/v1-configuration.zip","hyperpay_configuration_etag":"5f00e94dc26e6d96a95e4a2888b42f4d"},"variant_type":"EXPERIMENTAL"},{"context_id":"ce7d0b339a5367d61e88dd405cdf21d16f538a2df8d1a8029e206013e02655a6","id":"7138087493665431552-control","override_id":"afb1f8291942d3276f2259b285f7a021e7ffb2a6bb4038b21976e3d59d6abde8","overrides":{"hyperpay_configuration":"default_str_ignore_this","hyperpay_configuration_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}]	kartik.gajendra@juspay.in	7138087493665431552-control
7146027985267212288	2023-12-28 01:13:58+00	akash.singhania@juspay.in	2023-12-28 01:19:35+00	Global Release - EC - Android	{ec_bundle,ec_bundle_etag}	INPROGRESS	50	{"and":[{"==":[{"var":"os"},"android"]},{"in":[{"var":"clientId"},["goindigo","mplgaming","gameskraft","jungleerummy","howzat","slice","rummytime","winzo","jar","lazypay","playship","confirmtkt","purplle.com","myteam11","railyatri","rapido","branch"]]}]}	[{"context_id":"ef6ad4da7a263b60d8bad761f68723a159d155eb8542db8de0254cf56988c980","id":"7146027985267212288-test","override_id":"890f43a7096f1eecebf2c474b30ebd14c15a9b670408494214052a179ec601d4","overrides":{"ec_bundle":"https://assets.juspay.in/hyper/bundles/app/in.juspay.ec/3.16.18/android/v1-index_bundle.zip","ec_bundle_etag":"b864e0799319bb7a33c76fa3210bb352"},"variant_type":"EXPERIMENTAL"},{"context_id":"d5e4a08dbfed5db9c1676ebab29f1e1f51956de084a2c03c0f6d6704c793df2e","id":"7146027985267212288-control","override_id":"7e34b6da45dfb9ed09cb00290f9d28f3586aa9c93a4f4d1d1dcec2c67d37a529","overrides":{"ec_bundle":"default_str_ignore_this","ec_bundle_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}]	akash.singhania@juspay.in	
7138087705163210752	2023-12-06 03:22:08+00	kartik.gajendra@juspay.in	2023-12-12 01:03:07+00	mobius test 1	{hyperpay_configuration,hyperpay_configuration_etag}	CONCLUDED	25	{"and":[{"==":[{"var":"clientId"},"galactustest"]},{"==":[{"var":"os"},"android"]}]}	[{"context_id":"332e9a95c0d2197d4a12db71d991ffd335b0093b75d0d77749c002c18e0a86cc","id":"7138087705163210752-test","override_id":"7ca1e210b75248afda6ae04f074f0dcc8a8f729f897e9f9113a3009e087c01b7","overrides":{"hyperpay_configuration":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/magicbrickspp/configuration/1.1/v1-configuration.zip","hyperpay_configuration_etag":"5f00e94dc26e6d96a95e4a2888b42f4d"},"variant_type":"EXPERIMENTAL"},{"context_id":"1826996a062c87f640b1f99d9580fc94a62e19982559e9a01696a8dec8191058","id":"7138087705163210752-control","override_id":"afb1f8291942d3276f2259b285f7a021e7ffb2a6bb4038b21976e3d59d6abde8","overrides":{"hyperpay_configuration":"default_str_ignore_this","hyperpay_configuration_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}]	mobius@juspay.in	7138087705163210752-control
7140228834636234752	2023-12-12 01:10:13+00	aman.jain@juspay.in	2023-12-12 01:17:22+00	test	{arya_root}	CONCLUDED	20	{"and":[{"==":[{"var":"os"},"android"]}]}	[{"context_id":"dcd658aacbb4975f7c9348ed4c26e5bc0ec76d897fb42f654a6cfbdd50ce073e","id":"7140228834636234752-test","override_id":"334722013fece9c62a35f8ba84492df721d2dbc4af0102c3f3b588d79d0180f4","overrides":{"arya_root":"dwad"},"variant_type":"EXPERIMENTAL"},{"context_id":"dd9cb4f674ad0117190e738139f347cf493de37ff85dbe8e19b5b647bc3b6c4e","id":"7140228834636234752-control","override_id":"27bd63b346290504b882d0f5c336916fb99cb66f2a26d7f9f24482e1d6ea9c9c","overrides":{"arya_root":"default_str_ignore_this"},"variant_type":"CONTROL"}]	aman.jain@juspay.in	7140228834636234752-control
7140253374879244288	2023-12-12 02:47:44+00	aman.jain@juspay.in	2023-12-12 02:47:44+00	dawdad	{arya_root}	CREATED	0	{"and":[{"==":[{"var":"os"},"android"]}]}	[{"context_id":"c49528d88d4550af74eaea4ad42de49603941aa8b6fd235e0de1f165ebebac19","id":"7140253374879244288-test","override_id":"ffd0f8d439d613b932e136ee7627835a542fe506af89c87d186641779c5d31f5","overrides":{"arya_root":""},"variant_type":"EXPERIMENTAL"},{"context_id":"e8e8ee0a259fe80694014983234091645bbe4536fbee698524d9d0fb6db6ad1d","id":"7140253374879244288-control","override_id":"27bd63b346290504b882d0f5c336916fb99cb66f2a26d7f9f24482e1d6ea9c9c","overrides":{"arya_root":"default_str_ignore_this"},"variant_type":"CONTROL"}]	aman.jain@juspay.in	
7140260528549928960	2023-12-12 03:16:09+00	rohan.pawar@juspay.in	2023-12-13 02:36:12+00	mobius test 2	{hyperpay_configuration,hyperpay_configuration_etag}	CONCLUDED	50	{"and":[{"==":[{"var":"clientId"},"galactustest"]},{"==":[{"var":"os"},"android"]}]}	[{"context_id":"c6111ea9f2b0f445cc7c1b82bbf65efe3746df35a3e00841bace6e2c06c27196","id":"7140260528549928960-test","override_id":"7ca1e210b75248afda6ae04f074f0dcc8a8f729f897e9f9113a3009e087c01b7","overrides":{"hyperpay_configuration":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/magicbrickspp/configuration/1.1/v1-configuration.zip","hyperpay_configuration_etag":"5f00e94dc26e6d96a95e4a2888b42f4d"},"variant_type":"EXPERIMENTAL"},{"context_id":"4af6321459006311ec902f192ac9749b2fe8e070642f51086a65ea660fc2f6b8","id":"7140260528549928960-control","override_id":"afb1f8291942d3276f2259b285f7a021e7ffb2a6bb4038b21976e3d59d6abde8","overrides":{"hyperpay_configuration":"default_str_ignore_this","hyperpay_configuration_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}]	mobius@juspay.in	7140260528549928960-control
7140674404129603584	2023-12-13 06:40:45+00	rohan.pawar@juspay.in	2023-12-14 02:35:38+00	mobius test 3	{hyperpay_configuration,hyperpay_configuration_etag}	CONCLUDED	50	{"and":[{"==":[{"var":"clientId"},"galactustest"]},{"==":[{"var":"os"},"android"]}]}	[{"context_id":"0fc1b55af5432ef9ab06a7bfa0b38bd4030d482fa76f54be660a0645dcf61561","id":"7140674404129603584-test","override_id":"7ca1e210b75248afda6ae04f074f0dcc8a8f729f897e9f9113a3009e087c01b7","overrides":{"hyperpay_configuration":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/magicbrickspp/configuration/1.1/v1-configuration.zip","hyperpay_configuration_etag":"5f00e94dc26e6d96a95e4a2888b42f4d"},"variant_type":"EXPERIMENTAL"},{"context_id":"262d7f53ab9c6cbb0c27673704b42220a8465d214de7547e2aa296efbf04d99d","id":"7140674404129603584-control","override_id":"afb1f8291942d3276f2259b285f7a021e7ffb2a6bb4038b21976e3d59d6abde8","overrides":{"hyperpay_configuration":"default_str_ignore_this","hyperpay_configuration_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}]	mobius@juspay.in	7140674404129603584-test
7140416218094780416	2023-12-12 13:34:49+00	aman.jain@juspay.in	2023-12-12 13:35:10+00	testnew	{}	CONCLUDED	0	{"and":[{"==":[{"var":"clientId"},"studio"]},{"==":[{"var":"os"},"android"]}]}	[{"context_id":"3cd5bee206d2979f56fa4edefb339a179c7d77cf3240e5e67ba3f11c78ad35f7","id":"7140416218094780416-test","override_id":"af1349b9f5f9a1a6a0404dea36dcc9499bcb25c9adc112b7cc9a93cae41f3262","overrides":{},"variant_type":"EXPERIMENTAL"},{"context_id":"c37b61c013acc4a5ef4748a5e97678bb681c3a3ee2671876689ef5124a46667f","id":"7140416218094780416-control","override_id":"af1349b9f5f9a1a6a0404dea36dcc9499bcb25c9adc112b7cc9a93cae41f3262","overrides":{},"variant_type":"CONTROL"}]	aman.jain@juspay.in	7140416218094780416-test
7140290618252574720	2023-12-12 05:15:43+00	aman.jain@juspay.in	2023-12-13 03:44:21+00	TestExp	{consumer_bundle_etag,consumer_bundle}	CREATED	0	{"and":[{"==":[{"var":"clientId"},"zee5"]},{"==":[{"var":"os"},"android"]}]}	[{"context_id":"ef48186d8d6b64f0e5d7d8cfcfd6f1be2a5ec574e6bede4b956046bdab30294a","id":"7140290618252574720-test","override_id":"e56b74844f6aab978dc958d0b4e071c2e409d917e6c93194719211bddf8109b5","overrides":{"consumer_bundle":"https://assets.juspay.in/hyper/bundles/app/in.juspay.hyperpay/3.72.4-release-20231206.1/android/v1-index_bundle.zip","consumer_bundle_etag":"263a54c0c9c7693218f8ed63d5c942e2"},"variant_type":"EXPERIMENTAL"},{"context_id":"b8abee3e1f7938ea51fd5c88e7a70e0b558cda8bafdb4733d2ebdd766b08d6fa","id":"7140290618252574720-control","override_id":"34e82ba678d01bbded07536817e2c7c20d13cba03a8a8b8bdd9dad0fa3e92fb5","overrides":{"consumer_bundle":"default_str_ignore_this","consumer_bundle_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}]	aman.jain@juspay.in	
7140662152812986368	2023-12-13 05:52:04+00	aman.jain@juspay.in	2023-12-13 05:53:03+00	testexp2	{consumer_root,arya_root}	CREATED	0	{"and":[{"==":[{"var":"clientId"},"studio"]},{"==":[{"var":"os"},"android"]}]}	[{"context_id":"592bcf66c9ff8873dcdba6e95579a74c11c98e4185f8f909c0aa1b012310092e","id":"7140662152812986368-test","override_id":"1145cd058747c45bfc52369bcc3ebd5eec08b008eb7c0b6d6a999bd009acd863","overrides":{"arya_root":"adadadad","consumer_root":"test"},"variant_type":"EXPERIMENTAL"},{"context_id":"38116b8b23d457f0859241cd730047a05e164c81994d2a3654d0b0eb4255582b","id":"7140662152812986368-control","override_id":"8866d1d9d2df76dc2ee01a4caba29bbc83ced89d2c8f7c4710d118fab5b3ded9","overrides":{"arya_root":"default_str_ignore_this","consumer_root":"default_str_ignore_this"},"variant_type":"CONTROL"}]	aman.jain@juspay.in	
7155192075930451968	2024-01-22 08:08:48+00	rohan.pawar@juspay.in	2024-03-12 17:16:17+00	mobius test 3	{hyperpay_configuration_etag,hyperpay_configuration}	CONCLUDED	50	{"and":[{"==":[{"var":"clientId"},"galactustest"]},{"==":[{"var":"os"},"android"]}]}	[{"context_id":"931d479bf4533a6aff6b5c41d6a5a576354adcb37686c12c0331c9a7fc682dbc","id":"7155192075930451968-test","override_id":"7ca1e210b75248afda6ae04f074f0dcc8a8f729f897e9f9113a3009e087c01b7","overrides":{"hyperpay_configuration":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/magicbrickspp/configuration/1.1/v1-configuration.zip","hyperpay_configuration_etag":"5f00e94dc26e6d96a95e4a2888b42f4d"},"variant_type":"EXPERIMENTAL"},{"context_id":"e3f722b94963debafbf7afb729ca9ab7a85e050249f26a06a8f18dcff0302ae7","id":"7155192075930451968-control","override_id":"afb1f8291942d3276f2259b285f7a021e7ffb2a6bb4038b21976e3d59d6abde8","overrides":{"hyperpay_configuration":"default_str_ignore_this","hyperpay_configuration_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}]	mobius@juspay.in	7155192075930451968-test
7202264306568204288	2024-05-31 05:37:02+00	deepesh.maheshwari@juspay.in	2024-05-31 05:37:10+00	EC and UPI intent update	{ec_bundle_etag,upiintent_bundle_etag,upiintent_bundle,ec_bundle}	CONCLUDED	0	{"and":[{"==":[{"var":"clientId"},"hdfcmaster"]},{"==":[{"var":"os"},"android"]}]}	[{"context_id":"2ac1532160da0197dcc27df437225236932ff614809309752e0b26cc61a92f3e","id":"7202264306568204288-test","override_id":"388e2f29ff74d296d3fd033970bd79b8fcd587f57750cd2113b39f51d2f579d2","overrides":{"ec_bundle":"https://assets.juspay.in/hyper/bundles/app/in.juspay.ec/3.29.4/android/v1-index_bundle.zip","ec_bundle_etag":"6e673566ec9d34425bb9fadcac6e1927","upiintent_bundle":"https://assets.juspay.in/hyper/bundles/app/in.juspay.upiintent/4.9.9/android/v1-index_bundle.zip","upiintent_bundle_etag":"655d1b861dde675e69e6fe19dcde9804"},"variant_type":"EXPERIMENTAL"},{"context_id":"6b5f2a3761e248a859b6603773674065275f29ec748a4fd9b26c3666bc4b3f56","id":"7202264306568204288-control","override_id":"e0f67abcfcb928bb6e84e0a14b8b7a9c44cb96c6e14b8672fab28b0022bf6a23","overrides":{"ec_bundle":"default_str_ignore_this","ec_bundle_etag":"default_str_ignore_this","upiintent_bundle":"default_str_ignore_this","upiintent_bundle_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}]	deepesh.maheshwari@juspay.in	7202264306568204288-test
7202264873780318208	2024-05-31 05:39:17+00	deepesh.maheshwari@juspay.in	2024-05-31 05:39:42+00	EC and UPI intent update	{ec_bundle,upiintent_bundle_etag,ec_bundle_etag,upiintent_bundle}	CONCLUDED	0	{"and":[{"==":[{"var":"clientId"},"hdfcmaster"]},{"==":[{"var":"os"},"ios"]}]}	[{"context_id":"0e688b11e09bb37396990355ae6e81999ffbde5f0eaf3df847ec8dc4150440f6","id":"7202264873780318208-test","override_id":"655b17f9f2c7df1481bf63505909c4874284a594daaa594f63477e310bb8fcc1","overrides":{"ec_bundle":"https://assets.juspay.in/hyper/bundles/app/in.juspay.ec/3.29.4/ios/v1-index_bundle.jsa","ec_bundle_etag":"230535f9eed9e9e977427d45e217de33","upiintent_bundle":"https://assets.juspay.in/hyper/bundles/app/in.juspay.upiintent/4.9.9/ios/v1-index_bundle.jsa","upiintent_bundle_etag":"d0838b52ce1073a6479f6af97c90167c"},"variant_type":"EXPERIMENTAL"},{"context_id":"c7b39a1865d73cf38ef645b3996425d4613ca3eab8bbf4312c6177d10c3bd3f5","id":"7202264873780318208-control","override_id":"e0f67abcfcb928bb6e84e0a14b8b7a9c44cb96c6e14b8672fab28b0022bf6a23","overrides":{"ec_bundle":"default_str_ignore_this","ec_bundle_etag":"default_str_ignore_this","upiintent_bundle":"default_str_ignore_this","upiintent_bundle_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}]	deepesh.maheshwari@juspay.in	7202264873780318208-test
7214617329714196480	2024-07-04 07:43:32+00	mounika.reddy@juspay.in	2024-07-04 07:43:39+00	hsbc loader update	{hyperos_placeholder_loader}	CONCLUDED	0	{"and":[{"==":[{"var":"clientId"},"hsbc"]},{"==":[{"var":"os"},"web"]}]}	[{"context_id":"416841469d118df4f5075a04e4726c538fd9c71b6f31d55cea2e89126a922f75","id":"7214617329714196480-test","override_id":"7c0a28beed2c4797fd1442fc680c57f544d05e1e3b158aaa144c5685ef8b4735","overrides":{"hyperos_placeholder_loader":"https://assets.juspay.in/hyper/bundles/app/loader/hsbc/1.0.6/web/loader.js"},"variant_type":"EXPERIMENTAL"},{"context_id":"2311d007b87a4e89f38da0fbda00929c5e6129413fb55982e493c29e6e965bd3","id":"7214617329714196480-control","override_id":"87a8752ec7ee4bcdf4ecef07658e9fd33267737ff6f16c60afe9c5b254edb398","overrides":{"hyperos_placeholder_loader":"default_str_ignore_this"},"variant_type":"CONTROL"}]	mounika.reddy@juspay.in	7214617329714196480-test
7217465364171448320	2024-07-12 04:20:37+00	shubhranshu.sanjeev@juspay.in	2024-07-12 04:22:39+00	test-experiment-nnn	{godel_config,godel_config_etag}	INPROGRESS	0	{"and":[{"==":[{"var":"clientId"},"galactustest"]},{"==":[{"var":"scope"},"release"]},{"==":[{"var":"os"},"android"]}]}	[{"context_id":"514540b487679173a51fe4e01c483fe1d6d3a9b931bdee2c0237dc6d9efbcf3b","id":"7217465364171448320-test","override_id":"1e79ffeded160b3c5e6250e1247116071c7b8aa73bab1119b532dd23ed168383","overrides":{"godel_config":"https://assets.juspay.in/hyper/bundles/config/in.juspay.godel/1.2.57/v1-config.zip","godel_config_etag":"77fd03350702e025c432e66842cd7521"},"variant_type":"EXPERIMENTAL"},{"context_id":"3d660c8f7ad00d1578269158aebc9686bd0754e79696e08945cce10664bbbcb7","id":"7217465364171448320-control","override_id":"81159838966447ace3e7be99a17e7b1a2e6a7ba2ecc98da4899f0e56f4539d59","overrides":{"godel_config":"https://assets.juspay.in/hyper/bundles/in.juspay.merchants/bbdaily/ios/release/config.json","godel_config_etag":"d574dfb82a2e851145a0c4070ef0d6e4"},"variant_type":"CONTROL"}]	shubhranshu.sanjeev@juspay.in	
7189249119334772736	2024-04-25 07:39:20+00	shubhranshu.sanjeev@juspay.in	2024-07-12 04:45:54+00	dummy-experiment-2	{arya_bundle,godel_bundle,godel_bundle_etag,arya_bundle_etag}	CREATED	0	{"and":[{"==":[{"var":"os"},"android"]},{"==":[{"var":"clientId"},"galactustest"]}]}	[{"context_id":"c6042f4af6bccf484576d12b0070b89645bb1e9b9680ce532068c010fb49e534","id":"7189249119334772736-test","override_id":"e359c27272515976eba9f7ba0b72c7a52a6eef6d4280ca3bbf3d49679773589c","overrides":{"arya_bundle":"default_str_ignore_this","arya_bundle_etag":"default_str_ignore","godel_bundle":"https://assets.juspay.in/hyper/bundles/config/in.juspay.godel/1.2.57/v1-config.zip","godel_bundle_etag":"77fd03350702e025c432e66842cd7521"},"variant_type":"EXPERIMENTAL"},{"context_id":"0ee3ec6af9d894d17b4a03c6fe574bf02a36bed43fea5b64a109ca79065050c5","id":"7189249119334772736-control","override_id":"1ee28fb8dfb4c808ab76ccee332c43d2378c6b4b1358af0f6bf2a42e38fada85","overrides":{"arya_bundle":"default_str_ignore_this","arya_bundle_etag":"default_str_ignore_this","godel_bundle":"default_str_ignore_this","godel_bundle_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}]	shubhranshu.sanjeev@juspay.in	
7218596008003162112	2024-07-15 07:13:23+00	prathamesh.rane@juspay.in	2024-07-15 07:13:45+00	hdfcmaster enablement for sbx new upi change	{hyperpay_bundle,hyperpay_bundle_etag}	CONCLUDED	0	{"and":[{"==":[{"var":"clientId"},"hdfcmaster"]},{"==":[{"var":"os"},"web"]},{"==":[{"var":"resellerId"},"hdfc_reseller"]}]}	[{"context_id":"f6b231e8bd66f2b834a2da7bc6e48d3cce15b3879f82be8cd9a992e094a10608","id":"7218596008003162112-test","override_id":"4203039ae6fe427da6adbb94bd5c6ee206d139776e1c71f2e1405ad251396637","overrides":{"hyperpay_bundle":"https://assets.juspay.in/hyper/bundles/app/in.juspay.hyperpay/4.24.3-release-20240422.3/web/prod-split_index.js","hyperpay_bundle_etag":"8ec671797de055489d4acbba008befc5"},"variant_type":"EXPERIMENTAL"},{"context_id":"80fb16c6247f183295961ae9b909e8f984daeb564cceba08dfc34eadecd0a990","id":"7218596008003162112-control","override_id":"2ff2d9f749cdcaec11f9798c14f325a7e467d8bab0468fa577ce491d3563fbe1","overrides":{"hyperpay_bundle":"default_str_ignore_this","hyperpay_bundle_etag":"default_str_ignore_this"},"variant_type":"CONTROL"}]	prathamesh.rane@juspay.in	7218596008003162112-test
7206198466018328576	2024-06-11 02:09:59+00	mounika.reddy@juspay.in	2024-08-07 10:50:21+00	hdfc hyperpay SBX	{escrow_bundle,hyperpay_bundle_etag,upiintent_bundle_etag,ec_bundle,escrow_bundle_etag,hyperpay_bundle,ec_bundle_etag,upiintent_bundle}	CREATED	0	{"and":[{"==":[{"var":"clientId"},"hdfcmaster"]},{"==":[{"var":"os"},"web"]},{"==":[{"var":"resellerId"},"hdfc_reseller"]}]}	[{"context_id":"142023a66eadb6e0effb613b9b9c0292fb08230d609bd406a5a01c7f789ee891","id":"7206198466018328576-test","override_id":"7697748dedfd674e95b8848609a910cb152d49f89ac32d2102db5a3cba1ad107","overrides":{"ec_bundle":"https://assets.juspay.in/hyper/bundles/app/in.juspay.ec/3.33.1/web/index.js","ec_bundle_etag":"19a6301cb2893ce82ad42009859435ef","escrow_bundle":"https://assets.juspay.in/hyper/bundles/app/in.juspay.escrow/4.0.4/web/index.js","escrow_bundle_etag":"4b756ddc159a70672fea8dab5f0066ff","hyperpay_bundle":"https://assets.juspay.in/hyper/bundles/app/in.juspay.hyperpay/4.40.20-release-20240715.2/web/prod-split_index.js","hyperpay_bundle_etag":"c5c5c1ac957a20d2bf312ccfeb767571","upiintent_bundle":"https://assets.juspay.in/hyper/bundles/app/in.juspay.upiintent/4.10.2/web/index.js","upiintent_bundle_etag":"5617360b609834cf686ae75796aeafee"},"variant_type":"EXPERIMENTAL"},{"context_id":"e7c672fb9d580bcbfe90ce0ec029af2109b41a98972d13997cd90cea71bcd01e","id":"7206198466018328576-control","override_id":"221983082bcc13e5e24eae1c056686d5cb47abadae00e6187dad2b0bc482e88b","overrides":{"ec_bundle":"https://assets.juspay.in/hyper/bundles/app/in.juspay.ec/3.29.4/web/index.js","ec_bundle_etag":"e4b290873d9b7691a2471cdfa58023a4","escrow_bundle":"default_str_ignore_this","escrow_bundle_etag":"default_str_ignore_this","hyperpay_bundle":"https://assets.juspay.in/hyper/bundles/app/in.juspay.hyperpay/4.24.3-release-20240422.3/web/prod-split_index.js","hyperpay_bundle_etag":"8ec671797de055489d4acbba008befc5","upiintent_bundle":"https://assets.juspay.in/hyper/bundles/app/in.juspay.upiintent/4.9.9/web/index.js","upiintent_bundle_etag":"9774e9eaec3ec8221bfcb104a4534534"},"variant_type":"CONTROL"}]	srikanth.mitra@juspay.in	
\.


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
-- Name: experiments experiments_audit; Type: TRIGGER; Schema: test_experimentation; Owner: postgres
--

CREATE TRIGGER experiments_audit AFTER INSERT OR DELETE OR UPDATE ON test_experimentation.experiments FOR EACH ROW EXECUTE FUNCTION test_experimentation.event_logger();


--
-- PostgreSQL database dump complete
--

