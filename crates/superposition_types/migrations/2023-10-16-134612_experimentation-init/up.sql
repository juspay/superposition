-- Your SQL goes here
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
--
-- Name: public; Type: SCHEMA; Schema: -; Owner: -
--
CREATE SCHEMA IF NOT EXISTS public;
--
-- Name: experiment_status_type; Type: TYPE; Schema: public; Owner: -
--
CREATE TYPE public.experiment_status_type AS ENUM (
'CREATED',
'CONCLUDED',
'INPROGRESS'
);
--
-- Name: not_null_text; Type: DOMAIN; Schema: public; Owner: -
--
CREATE DOMAIN public.not_null_text AS text NOT NULL;
--
-- Name: event_logger(); Type: FUNCTION; Schema: public; Owner: -
--
CREATE OR REPLACE FUNCTION public.event_logger() RETURNS trigger
LANGUAGE plpgsql AS $$
DECLARE old_data json;
new_data json;
BEGIN IF (TG_OP = 'UPDATE') THEN old_data := row_to_json(OLD);
new_data := row_to_json(NEW);
INSERT INTO public.event_log (
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
INSERT INTO public.event_log (
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
INSERT INTO public.event_log (table_name, user_name, action, new_data, query)
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
-- Name: experiments; Type: TABLE; Schema: public; Owner: -
--
CREATE TABLE public.experiments (
id bigint PRIMARY KEY,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
created_by text NOT NULL,
last_modified timestamp with time zone DEFAULT now() NOT NULL,
name text NOT NULL,
override_keys public.not_null_text [] NOT NULL,
status public.experiment_status_type NOT NULL,
traffic_percentage integer NOT NULL,
context json NOT NULL,
variants json NOT NULL,
last_modified_by text DEFAULT 'Null'::text NOT NULL,
chosen_variant text,
CONSTRAINT experiments_traffic_percentage_check CHECK ((traffic_percentage >= 0))
);
--
-- Name: experiment_created_date_index; Type: INDEX; Schema: public; Owner: -
--
CREATE INDEX experiment_created_date_index ON public.experiments USING btree (created_at) INCLUDE (id);
--
-- Name: experiment_last_modified_index; Type: INDEX; Schema: public; Owner: -
--
CREATE INDEX experiment_last_modified_index ON public.experiments USING btree (last_modified) INCLUDE (id, created_at);
--
-- Name: experiment_status_index; Type: INDEX; Schema: public; Owner: -
--
CREATE INDEX experiment_status_index ON public.experiments USING btree (status) INCLUDE (created_at, last_modified);

--
-- Name: experiments experiments_audit; Type: TRIGGER; Schema: public; Owner: -
--
CREATE TRIGGER experiments_audit AFTER INSERT OR DELETE OR UPDATE ON public.experiments FOR EACH ROW EXECUTE FUNCTION public.event_logger();