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
-- Name: dev_experimentation; Type: SCHEMA; Schema: -; Owner: postgres
--

CREATE SCHEMA dev_experimentation;


ALTER SCHEMA dev_experimentation OWNER TO postgres;

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

SET default_tablespace = '';

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

SET default_table_access_method = heap;

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
b46447bc-4389-4a40-bfcf-a0cbe11e6eed	experiments	postgres	2024-06-12 07:33:01.507048	INSERT	\N	{"id":7206559102640721920,"created_at":"2024-06-12T07:33:01.506776+00:00","created_by":"user@superposition.io","last_modified":"2024-06-12T07:33:01.506776+00:00","name":"experiment - 1","override_keys":["anotherKey","key1"],"status":"CREATED","traffic_percentage":0,"context":{"==":[{"var":"clientId"},"sss"]},"variants":[{"context_id":"4522703876fd1cd7a3c8f0d25cee567867edb9cf5dc3d5579316d15b850cf8c5","id":"7206559102640721920-control","override_id":"5632555ea94d3fe10d6040ddfe948e021bb2b4a32114ccb683a3ab7c164814a8","overrides":{"anotherKey":"ios","key1":345453},"variant_type":"CONTROL"},{"context_id":"ea70ec4a8bf98378fb05f596b3e1fee5143b59d8b07aa2c917e1ba4c068dee01","id":"7206559102640721920-experimental","override_id":"7b6ee30489a54b3befb50465afe0ed359c5e0989102775cd3be21175fd86332d","overrides":{"anotherKey":"android","key1":123},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"user@superposition.io","chosen_variant":null}	INSERT INTO "experiments" ("id", "created_at", "created_by", "last_modified", "name", "override_keys", "status", "traffic_percentage", "context", "variants", "last_modified_by", "chosen_variant") VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, DEFAULT) RETURNING "experiments"."id", "experiments"."created_at", "experiments"."created_by", "experiments"."last_modified", "experiments"."name", "experiments"."override_keys", "experiments"."status", "experiments"."traffic_percentage", "experiments"."context", "experiments"."variants", "experiments"."last_modified_by", "experiments"."chosen_variant"
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
98fa5ff0-6453-428f-ba51-b764befd3079	experiments	postgres	2024-09-04 12:23:14.162552	INSERT	\N	{"id":7237072717311250432,"created_at":"2024-09-04T12:23:14.162171+00:00","created_by":"user@superposition.io","last_modified":"2024-09-04T12:23:14.162172+00:00","name":"eeee","override_keys":["obj4444444","toogleee"],"status":"CREATED","traffic_percentage":0,"context":{"==":[{"var":"clientId"},"adasd"]},"variants":[{"context_id":"7323dc1d82bf2a743c86618b20f6abc4d600ce988c14243fad5174aba1a9f95e","id":"7237072717311250432-control","override_id":"2eae78ed69bab0f472599b36d0903b08f8ab45c551d31e1b2acad3fb8bfa3c26","overrides":{"obj4444444":{"a":1},"toogleee":true},"variant_type":"CONTROL"},{"context_id":"038da7f52fa098dca35a470198c5013b704bb24d7d41d55b96210c0f813d0691","id":"7237072717311250432-experimental","override_id":"382c717e3ee48b938e899f020bbb7685bb53de09c8bb3db1a44a882aa3c38ee2","overrides":{"obj4444444":{"a":123},"toogleee":true},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"user@superposition.io","chosen_variant":null}	INSERT INTO "experiments" ("id", "created_at", "created_by", "last_modified", "name", "override_keys", "status", "traffic_percentage", "context", "variants", "last_modified_by", "chosen_variant") VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, DEFAULT) RETURNING "experiments"."id", "experiments"."created_at", "experiments"."created_by", "experiments"."last_modified", "experiments"."name", "experiments"."override_keys", "experiments"."status", "experiments"."traffic_percentage", "experiments"."context", "experiments"."variants", "experiments"."last_modified_by", "experiments"."chosen_variant"
5673b504-dd18-43dd-b9b4-25241e156228	experiments	postgres	2024-09-04 12:41:30.618765	INSERT	\N	{"id":7237077316164259840,"created_at":"2024-09-04T12:41:30.618495+00:00","created_by":"user@superposition.io","last_modified":"2024-09-04T12:41:30.618496+00:00","name":"experiment-new-form","override_keys":["obbbbbbbj"],"status":"CREATED","traffic_percentage":0,"context":{"==":[{"var":"clientId"},"myclient"]},"variants":[{"context_id":"0e6f8bfc5c8e4313ebe12f128cb3ec03e048755982b8a2dfeb6d1d9d656789dc","id":"7237077316164259840-control","override_id":"276b4feca641faebf6214305720c22b331d53a113c5da5a41daa310f349b7f9a","overrides":{"obbbbbbbj":{"anc":"cna"}},"variant_type":"CONTROL"},{"context_id":"cc6237b4f4697fa5a53a27504eef0d2946b2aa7e522dbc6441300f5bbcfa78bf","id":"7237077316164259840-experimental","override_id":"f70f70f69b2f0133eab0af009371c64947df845e82bdc4433d5b2a2c09672563","overrides":{"obbbbbbbj":{"anc":"yeeee"}},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"user@superposition.io","chosen_variant":null}	INSERT INTO "experiments" ("id", "created_at", "created_by", "last_modified", "name", "override_keys", "status", "traffic_percentage", "context", "variants", "last_modified_by", "chosen_variant") VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, DEFAULT) RETURNING "experiments"."id", "experiments"."created_at", "experiments"."created_by", "experiments"."last_modified", "experiments"."name", "experiments"."override_keys", "experiments"."status", "experiments"."traffic_percentage", "experiments"."context", "experiments"."variants", "experiments"."last_modified_by", "experiments"."chosen_variant"
4e087cc5-4481-4269-b3c9-e05c8a642f34	experiments	postgres	2024-09-04 12:42:23.558013	UPDATE	{"id":7237077316164259840,"created_at":"2024-09-04T12:41:30.618495+00:00","created_by":"user@superposition.io","last_modified":"2024-09-04T12:41:30.618496+00:00","name":"experiment-new-form","override_keys":["obbbbbbbj"],"status":"CREATED","traffic_percentage":0,"context":{"==":[{"var":"clientId"},"myclient"]},"variants":[{"context_id":"0e6f8bfc5c8e4313ebe12f128cb3ec03e048755982b8a2dfeb6d1d9d656789dc","id":"7237077316164259840-control","override_id":"276b4feca641faebf6214305720c22b331d53a113c5da5a41daa310f349b7f9a","overrides":{"obbbbbbbj":{"anc":"cna"}},"variant_type":"CONTROL"},{"context_id":"cc6237b4f4697fa5a53a27504eef0d2946b2aa7e522dbc6441300f5bbcfa78bf","id":"7237077316164259840-experimental","override_id":"f70f70f69b2f0133eab0af009371c64947df845e82bdc4433d5b2a2c09672563","overrides":{"obbbbbbbj":{"anc":"yeeee"}},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"user@superposition.io","chosen_variant":null}	{"id":7237077316164259840,"created_at":"2024-09-04T12:41:30.618495+00:00","created_by":"user@superposition.io","last_modified":"2024-09-04T12:42:23.55782+00:00","name":"experiment-new-form","override_keys":["obbbbbbbj","StringBool"],"status":"CREATED","traffic_percentage":0,"context":{"==":[{"var":"clientId"},"myclient"]},"variants":[{"context_id":"0e6f8bfc5c8e4313ebe12f128cb3ec03e048755982b8a2dfeb6d1d9d656789dc","id":"7237077316164259840-control","override_id":"2b926a397c4407c4da33f235dbf4e9e202c3b53805f9af5740b8f9aff7a5a996","overrides":{"StringBool":"mystring","obbbbbbbj":{"anc":"cna"}},"variant_type":"CONTROL"},{"context_id":"cc6237b4f4697fa5a53a27504eef0d2946b2aa7e522dbc6441300f5bbcfa78bf","id":"7237077316164259840-experimental","override_id":"02ffbc4ea99079726233cc26cd3ee41df4f7fcddafdc278d3347ae052aaa3556","overrides":{"StringBool":true,"obbbbbbbj":{"anc":"yeeee"}},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"user@superposition.io","chosen_variant":null}	UPDATE "experiments" SET "variants" = $1, "override_keys" = $2, "last_modified" = $3, "last_modified_by" = $4 WHERE ("experiments"."id" = $5) RETURNING "experiments"."id", "experiments"."created_at", "experiments"."created_by", "experiments"."last_modified", "experiments"."name", "experiments"."override_keys", "experiments"."status", "experiments"."traffic_percentage", "experiments"."context", "experiments"."variants", "experiments"."last_modified_by", "experiments"."chosen_variant"
04443970-1ca6-46a8-a8b2-e675431f170a	experiments	postgres	2024-09-04 12:42:42.070812	UPDATE	{"id":7237077316164259840,"created_at":"2024-09-04T12:41:30.618495+00:00","created_by":"user@superposition.io","last_modified":"2024-09-04T12:42:23.55782+00:00","name":"experiment-new-form","override_keys":["obbbbbbbj","StringBool"],"status":"CREATED","traffic_percentage":0,"context":{"==":[{"var":"clientId"},"myclient"]},"variants":[{"context_id":"0e6f8bfc5c8e4313ebe12f128cb3ec03e048755982b8a2dfeb6d1d9d656789dc","id":"7237077316164259840-control","override_id":"2b926a397c4407c4da33f235dbf4e9e202c3b53805f9af5740b8f9aff7a5a996","overrides":{"StringBool":"mystring","obbbbbbbj":{"anc":"cna"}},"variant_type":"CONTROL"},{"context_id":"cc6237b4f4697fa5a53a27504eef0d2946b2aa7e522dbc6441300f5bbcfa78bf","id":"7237077316164259840-experimental","override_id":"02ffbc4ea99079726233cc26cd3ee41df4f7fcddafdc278d3347ae052aaa3556","overrides":{"StringBool":true,"obbbbbbbj":{"anc":"yeeee"}},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"user@superposition.io","chosen_variant":null}	{"id":7237077316164259840,"created_at":"2024-09-04T12:41:30.618495+00:00","created_by":"user@superposition.io","last_modified":"2024-09-04T12:42:42.070656+00:00","name":"experiment-new-form","override_keys":["obbbbbbbj","anotherKey","StringBool"],"status":"CREATED","traffic_percentage":0,"context":{"==":[{"var":"clientId"},"myclient"]},"variants":[{"context_id":"0e6f8bfc5c8e4313ebe12f128cb3ec03e048755982b8a2dfeb6d1d9d656789dc","id":"7237077316164259840-control","override_id":"623e19b729da364b43f6a2c34a646b14ff687b03ebadd1c8d7318f931bb4c980","overrides":{"StringBool":"mystring","anotherKey":"ios","obbbbbbbj":{"anc":"cna"}},"variant_type":"CONTROL"},{"context_id":"cc6237b4f4697fa5a53a27504eef0d2946b2aa7e522dbc6441300f5bbcfa78bf","id":"7237077316164259840-experimental","override_id":"ad92d509703099aa1b28cf33b9cce2d3537959ab02f43b9569737e559d064a27","overrides":{"StringBool":true,"anotherKey":"android","obbbbbbbj":{"anc":"yeeee"}},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"user@superposition.io","chosen_variant":null}	UPDATE "experiments" SET "variants" = $1, "override_keys" = $2, "last_modified" = $3, "last_modified_by" = $4 WHERE ("experiments"."id" = $5) RETURNING "experiments"."id", "experiments"."created_at", "experiments"."created_by", "experiments"."last_modified", "experiments"."name", "experiments"."override_keys", "experiments"."status", "experiments"."traffic_percentage", "experiments"."context", "experiments"."variants", "experiments"."last_modified_by", "experiments"."chosen_variant"
1b455d59-9be6-420d-9774-7b37425493bf	experiments	postgres	2024-09-04 12:43:06.759574	UPDATE	{"id":7237077316164259840,"created_at":"2024-09-04T12:41:30.618495+00:00","created_by":"user@superposition.io","last_modified":"2024-09-04T12:42:42.070656+00:00","name":"experiment-new-form","override_keys":["obbbbbbbj","anotherKey","StringBool"],"status":"CREATED","traffic_percentage":0,"context":{"==":[{"var":"clientId"},"myclient"]},"variants":[{"context_id":"0e6f8bfc5c8e4313ebe12f128cb3ec03e048755982b8a2dfeb6d1d9d656789dc","id":"7237077316164259840-control","override_id":"623e19b729da364b43f6a2c34a646b14ff687b03ebadd1c8d7318f931bb4c980","overrides":{"StringBool":"mystring","anotherKey":"ios","obbbbbbbj":{"anc":"cna"}},"variant_type":"CONTROL"},{"context_id":"cc6237b4f4697fa5a53a27504eef0d2946b2aa7e522dbc6441300f5bbcfa78bf","id":"7237077316164259840-experimental","override_id":"ad92d509703099aa1b28cf33b9cce2d3537959ab02f43b9569737e559d064a27","overrides":{"StringBool":true,"anotherKey":"android","obbbbbbbj":{"anc":"yeeee"}},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"user@superposition.io","chosen_variant":null}	{"id":7237077316164259840,"created_at":"2024-09-04T12:41:30.618495+00:00","created_by":"user@superposition.io","last_modified":"2024-09-04T12:43:06.759414+00:00","name":"experiment-new-form","override_keys":["anotherKey","StringBool","obbbbbbbj","NumberNull"],"status":"CREATED","traffic_percentage":0,"context":{"==":[{"var":"clientId"},"myclient"]},"variants":[{"context_id":"0e6f8bfc5c8e4313ebe12f128cb3ec03e048755982b8a2dfeb6d1d9d656789dc","id":"7237077316164259840-control","override_id":"39128c3b6b14bf355a25ec13972abd7b98da1536fc03a60a656ef99b5ec9477d","overrides":{"NumberNull":null,"StringBool":"mystring","anotherKey":"ios","obbbbbbbj":{"anc":"cna"}},"variant_type":"CONTROL"},{"context_id":"cc6237b4f4697fa5a53a27504eef0d2946b2aa7e522dbc6441300f5bbcfa78bf","id":"7237077316164259840-experimental","override_id":"ac97a5432a31fa75c5b853fcfa75da191034e0f79f831ad18fcc33ae1ae54dc7","overrides":{"NumberNull":123,"StringBool":true,"anotherKey":"android","obbbbbbbj":{"anc":"yeeee"}},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"user@superposition.io","chosen_variant":null}	UPDATE "experiments" SET "variants" = $1, "override_keys" = $2, "last_modified" = $3, "last_modified_by" = $4 WHERE ("experiments"."id" = $5) RETURNING "experiments"."id", "experiments"."created_at", "experiments"."created_by", "experiments"."last_modified", "experiments"."name", "experiments"."override_keys", "experiments"."status", "experiments"."traffic_percentage", "experiments"."context", "experiments"."variants", "experiments"."last_modified_by", "experiments"."chosen_variant"
d7107e91-237c-42a5-985b-dbbb65ecda67	experiments	postgres	2024-09-04 13:07:11.666067	INSERT	\N	{"id":7237083779775467520,"created_at":"2024-09-04T13:07:11.665844+00:00","created_by":"user@superposition.io","last_modified":"2024-09-04T13:07:11.665845+00:00","name":"experiment-2","override_keys":["obj4444444"],"status":"CREATED","traffic_percentage":0,"context":{"==":[{"var":"clientId"},"qwe123"]},"variants":[{"context_id":"af3b9878a3464ab6865968f8c85cd962443d63dc89b94879b0a792bbcc402d59","id":"7237083779775467520-control","override_id":"97b4c0d43a1cd31752200e6377c2943b1d6eb91addf40a3329ec2af4e411ffcc","overrides":{"obj4444444":{"a":1}},"variant_type":"CONTROL"},{"context_id":"e674ec1d12356911034477cc70026b5f17ec3f0c4769f00b234ec3b5614689c4","id":"7237083779775467520-experimental","override_id":"11ce044655b0f6b429853e6c314c10d1110e7b928dc5451971e2eef593b80fc2","overrides":{"obj4444444":{"a":2}},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"user@superposition.io","chosen_variant":null}	INSERT INTO "experiments" ("id", "created_at", "created_by", "last_modified", "name", "override_keys", "status", "traffic_percentage", "context", "variants", "last_modified_by", "chosen_variant") VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, DEFAULT) RETURNING "experiments"."id", "experiments"."created_at", "experiments"."created_by", "experiments"."last_modified", "experiments"."name", "experiments"."override_keys", "experiments"."status", "experiments"."traffic_percentage", "experiments"."context", "experiments"."variants", "experiments"."last_modified_by", "experiments"."chosen_variant"
7f973f30-9000-41f3-8b32-44ad659dee51	experiments	postgres	2024-09-04 13:17:06.560966	INSERT	\N	{"id":7237086274983694336,"created_at":"2024-09-04T13:17:06.560725+00:00","created_by":"user@superposition.io","last_modified":"2024-09-04T13:17:06.560726+00:00","name":"asdfgsgdfsgdfsgdfsgfsrawerawefsdfgdsfgsdfg","override_keys":["b123","obj4444444"],"status":"CREATED","traffic_percentage":0,"context":{"==":[{"var":"clientId"},"asdfasdfasdf"]},"variants":[{"context_id":"aa3567dce118aa69ca42016191b1ef4cf688cd0ae3c6778b5457c5b65c5c3b2e","id":"7237086274983694336-control","override_id":"20bca4710f51c50a2adf63500e68ac7e3d58d3b3be7f1fe55ffcee4c8a74220e","overrides":{"b123":"asdf","obj4444444":{"a":3}},"variant_type":"CONTROL"},{"context_id":"58040da1556f4858eda773955c79f89a8adc78dcd99752de98902d59b4c7d158","id":"7237086274983694336-experimental","override_id":"c3e41db2c3f8c5b8b6458728ce534d9fa8be27652b5e425032c0317b1aec1e1f","overrides":{"b123":"qwerqwer","obj4444444":{"a":2}},"variant_type":"EXPERIMENTAL"},{"context_id":"f729c78feb39253af94d1ccab11fdfa81414b5c3b638ae77c8070ffc8219bb1f","id":"7237086274983694336-bbbbbbb","override_id":"dfc271c180b02bdab4c85f7b91a783f440f5a8706f5973f6b6fc78c66bdddca4","overrides":{"b123":"fghjfghj","obj4444444":{"a":1}},"variant_type":"EXPERIMENTAL"},{"context_id":"7113ff078a25c37929cd4a33dd05d334a3d7b4d2d7d5a4b8d456be383196a482","id":"7237086274983694336-zcvzxcvzxcv","override_id":"9dbc930f43293216dced99241fe443eb67e0e17506b89ce273586c2a3a06532d","overrides":{"b123":"yuioyuioy","obj4444444":{"v":1}},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"user@superposition.io","chosen_variant":null}	INSERT INTO "experiments" ("id", "created_at", "created_by", "last_modified", "name", "override_keys", "status", "traffic_percentage", "context", "variants", "last_modified_by", "chosen_variant") VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, DEFAULT) RETURNING "experiments"."id", "experiments"."created_at", "experiments"."created_by", "experiments"."last_modified", "experiments"."name", "experiments"."override_keys", "experiments"."status", "experiments"."traffic_percentage", "experiments"."context", "experiments"."variants", "experiments"."last_modified_by", "experiments"."chosen_variant"
99bc9910-d5df-45e3-990d-cf8a7e106cba	experiments	postgres	2024-09-04 15:57:33.842852	INSERT	\N	{"id":7237126654693347328,"created_at":"2024-09-04T15:57:33.84263+00:00","created_by":"user@superposition.io","last_modified":"2024-09-04T15:57:33.84263+00:00","name":"asdfasdfadfasdfasdf33243113413342","override_keys":["StringBool","obj5555","toogleee","NumberNull","key1"],"status":"CREATED","traffic_percentage":0,"context":{"==":[{"var":"clientId"},"asdfasdf"]},"variants":[{"context_id":"3b56187b7183dc0641f20ae4eb2bf291ab70f3d414f1ec4fa1e407e0b142e6da","id":"7237126654693347328-control","override_id":"2738a8e3e671568081b8e51fc66948b406d9251e54c67dced3475fb7b30ead28","overrides":{"NumberNull":1,"StringBool":false,"key1":1,"obj5555":{"my":"my"},"toogleee":true},"variant_type":"CONTROL"},{"context_id":"ad653efc2fd72f66202c51b1ae25ee84fc174aa04e63591ce5741471d2515789","id":"7237126654693347328-experimental","override_id":"f32559aba85db44c4878b1526b42f0f0354523913c8449c0b6e2adacd4209e86","overrides":{"NumberNull":2,"StringBool":true,"key1":2,"obj5555":{"your":"nnn"},"toogleee":true},"variant_type":"EXPERIMENTAL"},{"context_id":"882f98970a2d827d9097ed415a98e4bc613a9237910445450b59d37a76ba95ac","id":"7237126654693347328-variantmineeee","override_id":"8cb3e91a9989e6ee08466a0c8fe49908663e4302d0f35bad3f2d4b6fc29cb843","overrides":{"NumberNull":3,"StringBool":"somestring","key1":3,"obj5555":{"him":"him"},"toogleee":true},"variant_type":"EXPERIMENTAL"},{"context_id":"d2407a40a2c75d878a03c633a704d0c0cb5a7a73a60b557f76ad5a60ffd2a816","id":"7237126654693347328-variantyyy","override_id":"f40790139ebfebf6dcb1e9fc6be44287fdeb11732b7bc5649acf033f6e2f186e","overrides":{"NumberNull":null,"StringBool":"aaaa","key1":4,"obj5555":{"her":"her"},"toogleee":true},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"user@superposition.io","chosen_variant":null}	INSERT INTO "experiments" ("id", "created_at", "created_by", "last_modified", "name", "override_keys", "status", "traffic_percentage", "context", "variants", "last_modified_by", "chosen_variant") VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, DEFAULT) RETURNING "experiments"."id", "experiments"."created_at", "experiments"."created_by", "experiments"."last_modified", "experiments"."name", "experiments"."override_keys", "experiments"."status", "experiments"."traffic_percentage", "experiments"."context", "experiments"."variants", "experiments"."last_modified_by", "experiments"."chosen_variant"
14dccd25-2612-4752-b7dc-bd05b59b7d3d	experiments	postgres	2024-09-04 16:24:54.902585	INSERT	\N	{"id":7237133537797869568,"created_at":"2024-09-04T16:24:54.902284+00:00","created_by":"user@superposition.io","last_modified":"2024-09-04T16:24:54.902285+00:00","name":"experiment-1","override_keys":["anotherKey","NumberNull","key1"],"status":"CREATED","traffic_percentage":0,"context":{"==":[{"var":"clientId"},"adfasdfasdfasdf"]},"variants":[{"context_id":"937dbf8c8d855550cfda16d52e4011caccc3da137fc2018ed753449961e8e23d","id":"7237133537797869568-control","override_id":"e1f1b55ed771818b9715b5a099fe241de94d78723b5af4763d7827902b61d0ee","overrides":{"NumberNull":123123,"anotherKey":"android","key1":123123123},"variant_type":"CONTROL"},{"context_id":"1d70e44733b3fd512f7cbcbd42e19936cf4661e02eaab3aaa5639c910af97090","id":"7237133537797869568-experimental","override_id":"e900df441fd7b854faf1517594b412399b8ad9c44ac4be4570b532f0970b5bca","overrides":{"NumberNull":123123,"anotherKey":"ios","key1":12},"variant_type":"EXPERIMENTAL"},{"context_id":"8f9b44ba8016e2d72ec076d27fdc530ba5e2f8e80ec374e59f7b2257cd9635e8","id":"7237133537797869568-asdfasdfasdf","override_id":"944e6fa628d8e1535db0c79bd492da9cfe406f7537ef9e3870568722b7062995","overrides":{"NumberNull":null,"anotherKey":"ios","key1":666},"variant_type":"EXPERIMENTAL"},{"context_id":"3fa2204e7511f2fdad7cc2f3a7de5fe7d62f1eb730378f733256ac626d2b67d0","id":"7237133537797869568-cvxcccvvcvcx","override_id":"e6d988a8e33c4a8555d547f558328e35bbe16fae688ee137c5d6b22bdff9d594","overrides":{"NumberNull":null,"anotherKey":"android","key1":123123},"variant_type":"EXPERIMENTAL"}],"last_modified_by":"user@superposition.io","chosen_variant":null}	INSERT INTO "experiments" ("id", "created_at", "created_by", "last_modified", "name", "override_keys", "status", "traffic_percentage", "context", "variants", "last_modified_by", "chosen_variant") VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, DEFAULT) RETURNING "experiments"."id", "experiments"."created_at", "experiments"."created_by", "experiments"."last_modified", "experiments"."name", "experiments"."override_keys", "experiments"."status", "experiments"."traffic_percentage", "experiments"."context", "experiments"."variants", "experiments"."last_modified_by", "experiments"."chosen_variant"
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
7206559102640721920	2024-06-12 07:33:01.506776+00	user@superposition.io	2024-06-12 07:33:01.506776+00	experiment - 1	{anotherKey,key1}	CREATED	0	{"==":[{"var":"clientId"},"sss"]}	[{"context_id":"4522703876fd1cd7a3c8f0d25cee567867edb9cf5dc3d5579316d15b850cf8c5","id":"7206559102640721920-control","override_id":"5632555ea94d3fe10d6040ddfe948e021bb2b4a32114ccb683a3ab7c164814a8","overrides":{"anotherKey":"ios","key1":345453},"variant_type":"CONTROL"},{"context_id":"ea70ec4a8bf98378fb05f596b3e1fee5143b59d8b07aa2c917e1ba4c068dee01","id":"7206559102640721920-experimental","override_id":"7b6ee30489a54b3befb50465afe0ed359c5e0989102775cd3be21175fd86332d","overrides":{"anotherKey":"android","key1":123},"variant_type":"EXPERIMENTAL"}]	user@superposition.io	\N
7237072717311250432	2024-09-04 12:23:14.162171+00	user@superposition.io	2024-09-04 12:23:14.162172+00	eeee	{obj4444444,toogleee}	CREATED	0	{"==":[{"var":"clientId"},"adasd"]}	[{"context_id":"7323dc1d82bf2a743c86618b20f6abc4d600ce988c14243fad5174aba1a9f95e","id":"7237072717311250432-control","override_id":"2eae78ed69bab0f472599b36d0903b08f8ab45c551d31e1b2acad3fb8bfa3c26","overrides":{"obj4444444":{"a":1},"toogleee":true},"variant_type":"CONTROL"},{"context_id":"038da7f52fa098dca35a470198c5013b704bb24d7d41d55b96210c0f813d0691","id":"7237072717311250432-experimental","override_id":"382c717e3ee48b938e899f020bbb7685bb53de09c8bb3db1a44a882aa3c38ee2","overrides":{"obj4444444":{"a":123},"toogleee":true},"variant_type":"EXPERIMENTAL"}]	user@superposition.io	\N
7237077316164259840	2024-09-04 12:41:30.618495+00	user@superposition.io	2024-09-04 12:43:06.759414+00	experiment-new-form	{anotherKey,StringBool,obbbbbbbj,NumberNull}	CREATED	0	{"==":[{"var":"clientId"},"myclient"]}	[{"context_id":"0e6f8bfc5c8e4313ebe12f128cb3ec03e048755982b8a2dfeb6d1d9d656789dc","id":"7237077316164259840-control","override_id":"39128c3b6b14bf355a25ec13972abd7b98da1536fc03a60a656ef99b5ec9477d","overrides":{"NumberNull":null,"StringBool":"mystring","anotherKey":"ios","obbbbbbbj":{"anc":"cna"}},"variant_type":"CONTROL"},{"context_id":"cc6237b4f4697fa5a53a27504eef0d2946b2aa7e522dbc6441300f5bbcfa78bf","id":"7237077316164259840-experimental","override_id":"ac97a5432a31fa75c5b853fcfa75da191034e0f79f831ad18fcc33ae1ae54dc7","overrides":{"NumberNull":123,"StringBool":true,"anotherKey":"android","obbbbbbbj":{"anc":"yeeee"}},"variant_type":"EXPERIMENTAL"}]	user@superposition.io	\N
7237083779775467520	2024-09-04 13:07:11.665844+00	user@superposition.io	2024-09-04 13:07:11.665845+00	experiment-2	{obj4444444}	CREATED	0	{"==":[{"var":"clientId"},"qwe123"]}	[{"context_id":"af3b9878a3464ab6865968f8c85cd962443d63dc89b94879b0a792bbcc402d59","id":"7237083779775467520-control","override_id":"97b4c0d43a1cd31752200e6377c2943b1d6eb91addf40a3329ec2af4e411ffcc","overrides":{"obj4444444":{"a":1}},"variant_type":"CONTROL"},{"context_id":"e674ec1d12356911034477cc70026b5f17ec3f0c4769f00b234ec3b5614689c4","id":"7237083779775467520-experimental","override_id":"11ce044655b0f6b429853e6c314c10d1110e7b928dc5451971e2eef593b80fc2","overrides":{"obj4444444":{"a":2}},"variant_type":"EXPERIMENTAL"}]	user@superposition.io	\N
7237086274983694336	2024-09-04 13:17:06.560725+00	user@superposition.io	2024-09-04 13:17:06.560726+00	asdfgsgdfsgdfsgdfsgfsrawerawefsdfgdsfgsdfg	{b123,obj4444444}	CREATED	0	{"==":[{"var":"clientId"},"asdfasdfasdf"]}	[{"context_id":"aa3567dce118aa69ca42016191b1ef4cf688cd0ae3c6778b5457c5b65c5c3b2e","id":"7237086274983694336-control","override_id":"20bca4710f51c50a2adf63500e68ac7e3d58d3b3be7f1fe55ffcee4c8a74220e","overrides":{"b123":"asdf","obj4444444":{"a":3}},"variant_type":"CONTROL"},{"context_id":"58040da1556f4858eda773955c79f89a8adc78dcd99752de98902d59b4c7d158","id":"7237086274983694336-experimental","override_id":"c3e41db2c3f8c5b8b6458728ce534d9fa8be27652b5e425032c0317b1aec1e1f","overrides":{"b123":"qwerqwer","obj4444444":{"a":2}},"variant_type":"EXPERIMENTAL"},{"context_id":"f729c78feb39253af94d1ccab11fdfa81414b5c3b638ae77c8070ffc8219bb1f","id":"7237086274983694336-bbbbbbb","override_id":"dfc271c180b02bdab4c85f7b91a783f440f5a8706f5973f6b6fc78c66bdddca4","overrides":{"b123":"fghjfghj","obj4444444":{"a":1}},"variant_type":"EXPERIMENTAL"},{"context_id":"7113ff078a25c37929cd4a33dd05d334a3d7b4d2d7d5a4b8d456be383196a482","id":"7237086274983694336-zcvzxcvzxcv","override_id":"9dbc930f43293216dced99241fe443eb67e0e17506b89ce273586c2a3a06532d","overrides":{"b123":"yuioyuioy","obj4444444":{"v":1}},"variant_type":"EXPERIMENTAL"}]	user@superposition.io	\N
7237126654693347328	2024-09-04 15:57:33.84263+00	user@superposition.io	2024-09-04 15:57:33.84263+00	asdfasdfadfasdfasdf33243113413342	{StringBool,obj5555,toogleee,NumberNull,key1}	CREATED	0	{"==":[{"var":"clientId"},"asdfasdf"]}	[{"context_id":"3b56187b7183dc0641f20ae4eb2bf291ab70f3d414f1ec4fa1e407e0b142e6da","id":"7237126654693347328-control","override_id":"2738a8e3e671568081b8e51fc66948b406d9251e54c67dced3475fb7b30ead28","overrides":{"NumberNull":1,"StringBool":false,"key1":1,"obj5555":{"my":"my"},"toogleee":true},"variant_type":"CONTROL"},{"context_id":"ad653efc2fd72f66202c51b1ae25ee84fc174aa04e63591ce5741471d2515789","id":"7237126654693347328-experimental","override_id":"f32559aba85db44c4878b1526b42f0f0354523913c8449c0b6e2adacd4209e86","overrides":{"NumberNull":2,"StringBool":true,"key1":2,"obj5555":{"your":"nnn"},"toogleee":true},"variant_type":"EXPERIMENTAL"},{"context_id":"882f98970a2d827d9097ed415a98e4bc613a9237910445450b59d37a76ba95ac","id":"7237126654693347328-variantmineeee","override_id":"8cb3e91a9989e6ee08466a0c8fe49908663e4302d0f35bad3f2d4b6fc29cb843","overrides":{"NumberNull":3,"StringBool":"somestring","key1":3,"obj5555":{"him":"him"},"toogleee":true},"variant_type":"EXPERIMENTAL"},{"context_id":"d2407a40a2c75d878a03c633a704d0c0cb5a7a73a60b557f76ad5a60ffd2a816","id":"7237126654693347328-variantyyy","override_id":"f40790139ebfebf6dcb1e9fc6be44287fdeb11732b7bc5649acf033f6e2f186e","overrides":{"NumberNull":null,"StringBool":"aaaa","key1":4,"obj5555":{"her":"her"},"toogleee":true},"variant_type":"EXPERIMENTAL"}]	user@superposition.io	\N
7237133537797869568	2024-09-04 16:24:54.902284+00	user@superposition.io	2024-09-04 16:24:54.902285+00	experiment-1	{anotherKey,NumberNull,key1}	CREATED	0	{"==":[{"var":"clientId"},"adfasdfasdfasdf"]}	[{"context_id":"937dbf8c8d855550cfda16d52e4011caccc3da137fc2018ed753449961e8e23d","id":"7237133537797869568-control","override_id":"e1f1b55ed771818b9715b5a099fe241de94d78723b5af4763d7827902b61d0ee","overrides":{"NumberNull":123123,"anotherKey":"android","key1":123123123},"variant_type":"CONTROL"},{"context_id":"1d70e44733b3fd512f7cbcbd42e19936cf4661e02eaab3aaa5639c910af97090","id":"7237133537797869568-experimental","override_id":"e900df441fd7b854faf1517594b412399b8ad9c44ac4be4570b532f0970b5bca","overrides":{"NumberNull":123123,"anotherKey":"ios","key1":12},"variant_type":"EXPERIMENTAL"},{"context_id":"8f9b44ba8016e2d72ec076d27fdc530ba5e2f8e80ec374e59f7b2257cd9635e8","id":"7237133537797869568-asdfasdfasdf","override_id":"944e6fa628d8e1535db0c79bd492da9cfe406f7537ef9e3870568722b7062995","overrides":{"NumberNull":null,"anotherKey":"ios","key1":666},"variant_type":"EXPERIMENTAL"},{"context_id":"3fa2204e7511f2fdad7cc2f3a7de5fe7d62f1eb730378f733256ac626d2b67d0","id":"7237133537797869568-cvxcccvvcvcx","override_id":"e6d988a8e33c4a8555d547f558328e35bbe16fae688ee137c5d6b22bdff9d594","overrides":{"NumberNull":null,"anotherKey":"android","key1":123123},"variant_type":"EXPERIMENTAL"}]	user@superposition.io	\N
\.


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
-- Name: experiments experiments_audit; Type: TRIGGER; Schema: dev_experimentation; Owner: postgres
--

CREATE TRIGGER experiments_audit AFTER INSERT OR DELETE OR UPDATE ON dev_experimentation.experiments FOR EACH ROW EXECUTE FUNCTION dev_experimentation.event_logger();


--
-- PostgreSQL database dump complete
--

