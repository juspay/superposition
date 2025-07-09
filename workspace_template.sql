-- Your SQL goes here
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
--
-- Name: {replaceme}; Type: SCHEMA; Schema: -; Owner: -
--
CREATE SCHEMA IF NOT EXISTS {replaceme};

--
-- Name: event_logger(); Type: FUNCTION; Schema: {replaceme}; Owner: -
--
CREATE OR REPLACE FUNCTION {replaceme}.event_logger() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
    old_data json;
    new_data json;
BEGIN
    IF (TG_OP = 'UPDATE') THEN
        old_data := row_to_json(OLD);
        new_data := row_to_json(NEW);
        INSERT INTO {replaceme}.event_log
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
        INSERT INTO {replaceme}.event_log
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
        INSERT INTO {replaceme}.event_log
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
-- Name: contexts; Type: TABLE; Schema: {replaceme}; Owner: -
--
CREATE TABLE IF NOT EXISTS {replaceme}.contexts  (
    id character varying PRIMARY KEY,
    value json NOT NULL,
    override_id character varying NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_by character varying NOT NULL,
    priority integer DEFAULT 1 NOT NULL,
    override json DEFAULT '{}'::json NOT NULL
);
--
-- Name: default_configs; Type: TABLE; Schema: {replaceme}; Owner: -
--
CREATE TABLE IF NOT EXISTS {replaceme}.default_configs  (
    key character varying PRIMARY KEY,
    value json NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_by character varying NOT NULL,
    schema json DEFAULT '{}'::json NOT NULL
);
--
-- Name: dimensions; Type: TABLE; Schema: {replaceme}; Owner: -
--
CREATE TABLE IF NOT EXISTS {replaceme}.dimensions (
    dimension character varying PRIMARY KEY,
    priority integer NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_by character varying NOT NULL,
    schema json DEFAULT '{}'::json NOT NULL
);
--
-- Name: event_log; Type: TABLE; Schema: {replaceme}; Owner: -
--
CREATE TABLE IF NOT EXISTS {replaceme}.event_log (
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
-- Name: event_log_action_index; Type: INDEX; Schema: {replaceme}; Owner: -
--
CREATE INDEX IF NOT EXISTS event_log_action_index ON ONLY {replaceme}.event_log USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_table_name_index; Type: INDEX; Schema: {replaceme}; Owner: -
--
CREATE INDEX IF NOT EXISTS event_log_table_name_index ON ONLY {replaceme}.event_log USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_timestamp_index; Type: INDEX; Schema: {replaceme}; Owner: -
--
CREATE INDEX IF NOT EXISTS event_log_timestamp_index ON ONLY {replaceme}.event_log USING btree ("timestamp") INCLUDE (action, table_name);

--
-- Name: contexts contexts_audit; Type: TRIGGER; Schema: {replaceme}; Owner: -
--
DO $$ BEGIN
    CREATE TRIGGER contexts_audit AFTER INSERT OR DELETE OR UPDATE ON {replaceme}.contexts FOR EACH ROW EXECUTE FUNCTION {replaceme}.event_logger();
EXCEPTION
    WHEN duplicate_object THEN null;
END $$;
--
-- Name: default_configs default_configs_audit; Type: TRIGGER; Schema: {replaceme}; Owner: -
--
DO $$ BEGIN
    CREATE TRIGGER default_configs_audit AFTER INSERT OR DELETE OR UPDATE ON {replaceme}.default_configs FOR EACH ROW EXECUTE FUNCTION {replaceme}.event_logger();
EXCEPTION
    WHEN duplicate_object THEN null;
END $$;
--
-- Name: dimensions dimensions_audit; Type: TRIGGER; Schema: {replaceme}; Owner: -
--
DO $$ BEGIN
    CREATE TRIGGER dimensions_audit AFTER INSERT OR DELETE OR UPDATE ON {replaceme}.dimensions FOR EACH ROW EXECUTE FUNCTION {replaceme}.event_logger();
EXCEPTION
    WHEN duplicate_object THEN null;
END $$;
-- Your SQL goes here
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
--
-- Name: {replaceme}; Type: SCHEMA; Schema: -; Owner: -
--
-- Name: experiment_status_type; Type: TYPE; Schema: {replaceme}; Owner: -
--

--
-- Name: not_null_text; Type: DOMAIN; Schema: {replaceme}; Owner: -
--
DO $$ BEGIN
    CREATE DOMAIN {replaceme}.not_null_text AS text NOT NULL;
EXCEPTION
    WHEN duplicate_object THEN null;
END $$;
--
-- Name: event_logger(); Type: FUNCTION; Schema: {replaceme}; Owner: -
--

DO $$ BEGIN
    CREATE TYPE public.experiment_status_type AS ENUM (
        'CREATED',
        'CONCLUDED',
        'INPROGRESS'
    );
EXCEPTION
    WHEN duplicate_object THEN null;
END $$;
--
-- Name: experiments; Type: TABLE; Schema: {replaceme}; Owner: -
--
CREATE TABLE IF NOT EXISTS {replaceme}.experiments (
    id bigint PRIMARY KEY,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_by text NOT NULL,
    last_modified timestamp with time zone DEFAULT now() NOT NULL,
    name text NOT NULL,
    override_keys {replaceme}.not_null_text [] NOT NULL,
    status public.experiment_status_type NOT NULL,
    traffic_percentage integer NOT NULL,
    context json NOT NULL,
    variants json NOT NULL,
    last_modified_by text DEFAULT 'Null'::text NOT NULL,
    chosen_variant text,
    CONSTRAINT experiments_traffic_percentage_check CHECK ((traffic_percentage >= 0))
);
--
-- Name: experiment_created_date_index; Type: INDEX; Schema: {replaceme}; Owner: -
--
CREATE INDEX IF NOT EXISTS experiment_created_date_index ON {replaceme}.experiments USING btree (created_at) INCLUDE (id);
--
-- Name: experiment_last_modified_index; Type: INDEX; Schema: {replaceme}; Owner: -
--
CREATE INDEX IF NOT EXISTS experiment_last_modified_index ON {replaceme}.experiments USING btree (last_modified) INCLUDE (id, created_at);
--
-- Name: experiment_status_index; Type: INDEX; Schema: {replaceme}; Owner: -
--
CREATE INDEX IF NOT EXISTS experiment_status_index ON {replaceme}.experiments USING btree (status) INCLUDE (created_at, last_modified);
--
-- Name: experiments experiments_audit; Type: TRIGGER; Schema: {replaceme}; Owner: -
--
DO $$ BEGIN
    CREATE TRIGGER experiments_audit AFTER INSERT OR DELETE OR UPDATE ON {replaceme}.experiments FOR EACH ROW EXECUTE FUNCTION {replaceme}.event_logger();
EXCEPTION
    WHEN duplicate_object THEN null;
END $$;

------------ Parititions for 2025 -----------
CREATE TABLE IF NOT EXISTS {replaceme}.event_log_y2025m01 PARTITION OF {replaceme}.event_log FOR
VALUES
FROM ('2025-01-01') TO ('2025-02-01');

CREATE TABLE IF NOT EXISTS {replaceme}.event_log_y2025m02 PARTITION OF {replaceme}.event_log FOR
VALUES
FROM ('2025-02-01') TO ('2025-03-01');

CREATE TABLE IF NOT EXISTS {replaceme}.event_log_y2025m03 PARTITION OF {replaceme}.event_log FOR
VALUES
FROM ('2025-03-01') TO ('2025-04-01');

CREATE TABLE IF NOT EXISTS {replaceme}.event_log_y2025m04 PARTITION OF {replaceme}.event_log FOR
VALUES
FROM ('2025-04-01') TO ('2025-05-01');

CREATE TABLE IF NOT EXISTS {replaceme}.event_log_y2025m05 PARTITION OF {replaceme}.event_log FOR
VALUES
FROM ('2025-05-01') TO ('2025-06-01');

CREATE TABLE IF NOT EXISTS {replaceme}.event_log_y2025m06 PARTITION OF {replaceme}.event_log FOR
VALUES
FROM ('2025-06-01') TO ('2025-07-01');

CREATE TABLE IF NOT EXISTS {replaceme}.event_log_y2025m07 PARTITION OF {replaceme}.event_log FOR
VALUES
FROM ('2025-07-01') TO ('2025-08-01');

CREATE TABLE IF NOT EXISTS {replaceme}.event_log_y2025m08 PARTITION OF {replaceme}.event_log FOR
VALUES
FROM ('2025-08-01') TO ('2025-09-01');

CREATE TABLE IF NOT EXISTS {replaceme}.event_log_y2025m09 PARTITION OF {replaceme}.event_log FOR
VALUES
FROM ('2025-09-01') TO ('2025-10-01');

CREATE TABLE IF NOT EXISTS {replaceme}.event_log_y2025m10 PARTITION OF {replaceme}.event_log FOR
VALUES
FROM ('2025-10-01') TO ('2025-11-01');

CREATE TABLE IF NOT EXISTS {replaceme}.event_log_y2025m11 PARTITION OF {replaceme}.event_log FOR
VALUES
FROM ('2025-11-01') TO ('2025-12-01');

CREATE TABLE IF NOT EXISTS {replaceme}.event_log_y2025m12 PARTITION OF {replaceme}.event_log FOR
VALUES
FROM ('2025-12-01') TO ('2026-01-01');

------------ Parititions for 2026 -----------
CREATE TABLE IF NOT EXISTS {replaceme}.event_log_y2026m01 PARTITION OF {replaceme}.event_log FOR
VALUES
FROM ('2026-01-01') TO ('2026-02-01');

CREATE TABLE IF NOT EXISTS {replaceme}.event_log_y2026m02 PARTITION OF {replaceme}.event_log FOR
VALUES
FROM ('2026-02-01') TO ('2026-03-01');

CREATE TABLE IF NOT EXISTS {replaceme}.event_log_y2026m03 PARTITION OF {replaceme}.event_log FOR
VALUES
FROM ('2026-03-01') TO ('2026-04-01');

CREATE TABLE IF NOT EXISTS {replaceme}.event_log_y2026m04 PARTITION OF {replaceme}.event_log FOR
VALUES
FROM ('2026-04-01') TO ('2026-05-01');

CREATE TABLE IF NOT EXISTS {replaceme}.event_log_y2026m05 PARTITION OF {replaceme}.event_log FOR
VALUES
FROM ('2026-05-01') TO ('2026-06-01');

CREATE TABLE IF NOT EXISTS {replaceme}.event_log_y2026m06 PARTITION OF {replaceme}.event_log FOR
VALUES
FROM ('2026-06-01') TO ('2026-07-01');

CREATE TABLE IF NOT EXISTS {replaceme}.event_log_y2026m07 PARTITION OF {replaceme}.event_log FOR
VALUES
FROM ('2026-07-01') TO ('2026-08-01');

CREATE TABLE IF NOT EXISTS {replaceme}.event_log_y2026m08 PARTITION OF {replaceme}.event_log FOR
VALUES
FROM ('2026-08-01') TO ('2026-09-01');

CREATE TABLE IF NOT EXISTS {replaceme}.event_log_y2026m09 PARTITION OF {replaceme}.event_log FOR
VALUES
FROM ('2026-09-01') TO ('2026-10-01');

CREATE TABLE IF NOT EXISTS {replaceme}.event_log_y2026m10 PARTITION OF {replaceme}.event_log FOR
VALUES
FROM ('2026-10-01') TO ('2026-11-01');

CREATE TABLE IF NOT EXISTS {replaceme}.event_log_y2026m11 PARTITION OF {replaceme}.event_log FOR
VALUES
FROM ('2026-11-01') TO ('2026-12-01');

CREATE TABLE IF NOT EXISTS {replaceme}.event_log_y2026m12 PARTITION OF {replaceme}.event_log FOR
VALUES
FROM ('2026-12-01') TO ('2027-01-01');

-- Your SQL goes here
-- Name: functions; Type: TABLE; Schema: {replaceme}; Owner: -
--
CREATE TABLE IF NOT EXISTS {replaceme}.functions (
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
-- Name: functions functions_audit; Type: TRIGGER; Schema: {replaceme}; Owner: -
--
DO $$ BEGIN
    CREATE TRIGGER functions_audit AFTER INSERT OR DELETE OR UPDATE ON {replaceme}.functions FOR EACH ROW EXECUTE FUNCTION {replaceme}.event_logger();
EXCEPTION
    WHEN duplicate_object THEN null;
END $$;
-- Your SQL goes here
ALTER TABLE {replaceme}.dimensions ADD COLUMN IF NOT EXISTS function_name text NULL;

DO $$ BEGIN
    ALTER TABLE {replaceme}.dimensions ADD FOREIGN KEY(function_name) REFERENCES {replaceme}.functions(function_name);
EXCEPTION
    WHEN duplicate_object THEN null;
END $$;

ALTER TABLE {replaceme}.default_configs ADD COLUMN IF NOT EXISTS function_name text NULL;

DO $$ BEGIN
    ALTER TABLE {replaceme}.default_configs ADD FOREIGN KEY(function_name) REFERENCES {replaceme}.functions(function_name);
EXCEPTION
    WHEN duplicate_object THEN null;
END $$;
-- Your SQL goes here
-- Name: functions; Type: TABLE; Schema: {replaceme}; Owner: -
--
CREATE TABLE IF NOT EXISTS {replaceme}.config_versions (
id bigint PRIMARY KEY,
config json NOT NULL,
config_hash TEXT NOT NULL,
tags varchar(100) [] check (array_position(tags, null) is null),
created_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
CREATE INDEX IF NOT EXISTS config_verions_tags_index ON {replaceme}.config_versions USING gin(tags);
CREATE INDEX IF NOT EXISTS config_versions_id_index ON {replaceme}.config_versions(id);
-- Your SQL goes here
CREATE TABLE IF NOT EXISTS {replaceme}.type_templates (
    type_name TEXT PRIMARY KEY,
    type_schema JSON NOT NULL,
    created_by TEXT NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    last_modified TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL
);
CREATE INDEX IF NOT EXISTS type_templates_index ON {replaceme}.type_templates(type_name);
CREATE INDEX IF NOT EXISTS type_templates_created_at_index ON {replaceme}.type_templates(created_at);

DO $$ BEGIN
    IF EXISTS(SELECT *
        FROM information_schema.columns
        WHERE table_name='type_templates' and table_schema='{replaceme}' and column_name='last_modified')
  THEN
    CREATE INDEX IF NOT EXISTS type_templates_last_modifed_index ON {replaceme}.type_templates(last_modified);
  END IF;
END $$;
-- Your SQL goes here

ALTER TABLE {replaceme}.functions
ADD COLUMN IF NOT EXISTS last_modified_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
ADD COLUMN IF NOT EXISTS last_modified_by varchar(200) not null default('null');

ALTER TABLE {replaceme}.dimensions
ADD COLUMN IF NOT EXISTS last_modified_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
ADD COLUMN IF NOT EXISTS last_modified_by varchar(200) not null default('null');

ALTER TABLE {replaceme}.contexts
ADD COLUMN IF NOT EXISTS last_modified_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
ADD COLUMN IF NOT EXISTS last_modified_by varchar(200) not null default('null');

ALTER TABLE {replaceme}.default_configs
ADD COLUMN IF NOT EXISTS last_modified_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
ADD COLUMN IF NOT EXISTS last_modified_by varchar(200) not null default('null');

DO $$ BEGIN
    IF EXISTS(SELECT *
        FROM information_schema.columns
        WHERE table_name='type_templates' and table_schema='{replaceme}' and column_name='last_modified')
  THEN
    ALTER TABLE {replaceme}.type_templates rename column last_modified to last_modified_at;
  END IF;
END $$;

ALTER TABLE {replaceme}.type_templates
ADD COLUMN IF NOT EXISTS last_modified_by varchar(200) not null default('null');
-- Your SQL goes here
ALTER TABLE {replaceme}.dimensions
ADD COLUMN IF NOT EXISTS position integer DEFAULT 0 NOT NULL;
ALTER TABLE {replaceme}.contexts
ADD COLUMN IF NOT EXISTS weight numeric(1000, 0) DEFAULT 1 NOT NULL;
CREATE INDEX IF NOT EXISTS idx_contexts_weight ON {replaceme}.contexts(weight);
-- Your SQL goes here
ALTER TABLE {replaceme}.dimensions
ALTER COLUMN priority SET DEFAULT 1;

DO $$ BEGIN
    ALTER TABLE {replaceme}.dimensions
    ADD CONSTRAINT dimension_unique_position UNIQUE (position) DEFERRABLE INITIALLY DEFERRED;
EXCEPTION
    WHEN duplicate_table THEN
    WHEN duplicate_object THEN null;
END $$;

ALTER TABLE {replaceme}.contexts ADD COLUMN IF NOT EXISTS description TEXT NOT NULL;
ALTER TABLE {replaceme}.contexts ADD COLUMN IF NOT EXISTS change_reason TEXT NOT NULL;

ALTER TABLE {replaceme}.dimensions ADD COLUMN IF NOT EXISTS description TEXT NOT NULL;
ALTER TABLE {replaceme}.dimensions ADD COLUMN IF NOT EXISTS change_reason TEXT NOT NULL;

ALTER TABLE {replaceme}.default_configs ADD COLUMN IF NOT EXISTS description TEXT NOT NULL;
ALTER TABLE {replaceme}.default_configs ADD COLUMN IF NOT EXISTS change_reason TEXT NOT NULL;

ALTER TABLE {replaceme}.type_templates ADD COLUMN IF NOT EXISTS description TEXT NOT NULL;
ALTER TABLE {replaceme}.type_templates ADD COLUMN IF NOT EXISTS change_reason TEXT NOT NULL;

DO $$ BEGIN
    IF EXISTS(SELECT *
        FROM information_schema.columns
        WHERE table_name='functions' and table_schema='{replaceme}' and column_name='function_description')
  THEN
    ALTER TABLE {replaceme}.functions RENAME COLUMN function_description TO description;
  END IF;
END $$;

ALTER TABLE {replaceme}.functions ADD COLUMN IF NOT EXISTS change_reason TEXT NOT NULL;

ALTER TABLE {replaceme}.config_versions ADD COLUMN IF NOT EXISTS description TEXT NOT NULL;

ALTER TABLE {replaceme}.experiments ADD COLUMN IF NOT EXISTS description TEXT NOT NULL;
ALTER TABLE {replaceme}.experiments ADD COLUMN IF NOT EXISTS change_reason TEXT NOT NULL;

DO $$ BEGIN
    IF NOT EXISTS(SELECT *
        FROM {replaceme}.dimensions
        WHERE dimension = 'variantIds')
    THEN
    INSERT INTO {replaceme}.dimensions (
            dimension,
            priority,
            created_at,
            created_by,
            schema,
            function_name,
            description,
            change_reason
        )
    VALUES (
            'variantIds',
            0,
            CURRENT_TIMESTAMP,
            'default@superposition.io',
            '{"type": "string","pattern": ".*"}'::json,
            null,
            'variantIds are used by experimentation module to manage and select variations',
            'initial setup'
    );
    END IF;
END $$;

INSERT INTO {replaceme}.type_templates(type_name, type_schema, created_by, created_at, last_modified_by, last_modified_at, description, change_reason)
VALUES (
        'Number',
        '{"type": "integer"}',
        'user@superposition.io',
        NOW(),
        'user@superposition.io',
        NOW(),
        'Number type is used to represent numeric values',
        'initial setup'
    ),
    (
        'Decimal',
        '{"type": "number"}',
        'user@superposition.io',
        NOW(),
        'user@superposition.io',
        NOW(),
        'Decimal type is used to represent decimal values',
        'initial setup'
    ),
    (
        'Boolean',
        '{"type": "boolean"}',
        'user@superposition.io',
        NOW(),
        'user@superposition.io',
        NOW(),
        'Boolean type is used to represent true/false values',
        'initial setup'
    ),
    (
        'Enum',
        '{"type": "string", "enum": ["android", "ios"]}',
        'user@superposition.io',
        NOW(),
        'user@superposition.io',
        NOW(),
        'Enum type is used to represent a fixed set of values',
        'initial setup'
    ),
    (
        'Pattern',
        '{"type": "string", "pattern": ".*"}',
        'user@superposition.io',
        NOW(),
        'user@superposition.io',
        NOW(),
        'Pattern type is used to represent a string that matches a specific pattern',
        'initial setup'
    ) ON CONFLICT DO NOTHING;

DO $$ BEGIN
    ALTER TYPE public.experiment_status_type ADD VALUE 'DISCARDED';
EXCEPTION
    WHEN duplicate_object THEN null;
END $$;

DO $$ BEGIN
    CREATE TYPE public.function_types AS ENUM (
    'VALIDATION',
    'AUTOCOMPLETE'
    );
EXCEPTION
    WHEN duplicate_object THEN null;
END $$;

ALTER TABLE {replaceme}.functions ADD COLUMN IF NOT EXISTS function_type public.FUNCTION_TYPES NOT NULL DEFAULT 'VALIDATION';

ALTER TABLE {replaceme}.dimensions
ADD COLUMN IF NOT EXISTS dependency_graph JSON default '{}'::json NOT NULL,
ADD COLUMN IF NOT EXISTS dependents TEXT[] default '{}' NOT NULL,
ADD COLUMN IF NOT EXISTS dependencies TEXT[] default '{}' NOT NULL;


ALTER TABLE {replaceme}.dimensions ADD COLUMN IF NOT EXISTS autocomplete_function_name text NULL;

DO $$ BEGIN
    ALTER TABLE {replaceme}.dimensions ADD FOREIGN KEY(autocomplete_function_name) REFERENCES {replaceme}.functions(function_name);
EXCEPTION
    WHEN duplicate_object THEN null;
END $$;

ALTER TABLE {replaceme}.default_configs ADD COLUMN IF NOT EXISTS autocomplete_function_name text NULL;

DO $$ BEGIN
    ALTER TABLE {replaceme}.default_configs ADD FOREIGN KEY(autocomplete_function_name) REFERENCES {replaceme}.functions(function_name);
EXCEPTION
    WHEN duplicate_object THEN null;
END $$;

DO $$ BEGIN
    CREATE TYPE public.http_method AS ENUM (
        'GET',
        'PUT',
        'POST',
        'DELETE',
        'PATCH',
        'HEAD'
    );
EXCEPTION
    WHEN duplicate_object THEN null;
END $$;

CREATE TABLE IF NOT EXISTS {replaceme}.webhooks (
    name text PRIMARY KEY,
    description text NOT NULL,
    enabled boolean NOT NULL,
    url text NOT NULL,
    method public.http_method NOT NULL DEFAULT 'POST',
    payload_version text NOT NULL,
    custom_headers json NOT NULL DEFAULT '{}'::json,
    events text[] NOT NULL,
    max_retries integer NOT NULL DEFAULT 0,
    last_triggered_at timestamp,
    change_reason TEXT NOT NULL,
    created_by text NOT NULL,
    created_at timestamp without time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
    last_modified_by text NOT NULL,
    last_modified_at timestamp without time zone NOT NULL DEFAULT CURRENT_TIMESTAMP
);

DO $$ BEGIN
    CREATE TRIGGER webhooks_audit AFTER INSERT OR DELETE OR UPDATE ON {replaceme}.webhooks FOR EACH ROW EXECUTE FUNCTION {replaceme}.event_logger();
EXCEPTION
    WHEN duplicate_object THEN null;
END $$;

ALTER TABLE {replaceme}.experiments
ADD COLUMN IF NOT EXISTS metrics JSON DEFAULT '{"enabled": false}'::json NOT NULL,
ADD COLUMN IF NOT EXISTS started_at TIMESTAMP,
ADD COLUMN IF NOT EXISTS started_by TEXT;

DO $$ BEGIN
    ALTER TYPE public.experiment_status_type ADD VALUE 'PAUSED';
EXCEPTION
    WHEN duplicate_object THEN null;
END $$;

DO $$ BEGIN
    CREATE TYPE public.experiment_type AS ENUM (
        'DEFAULT',
        'DELETE_OVERRIDES'
    );
EXCEPTION
    WHEN duplicate_object THEN null;
END $$;

ALTER TABLE {replaceme}.experiments ADD COLUMN IF NOT EXISTS experiment_type public.experiment_type NOT NULL DEFAULT 'DEFAULT';

CREATE TABLE IF NOT EXISTS {replaceme}.experiment_groups(
    id bigint PRIMARY KEY,
    context_hash TEXT NOT NULL,
    name TEXT NOT NULL,
    description TEXT NOT NULL,
    change_reason TEXT NOT NULL,
    context JSON NOT NULL,
    traffic_percentage integer NOT NULL CONSTRAINT traffic_percentage_range CHECK (traffic_percentage >= 0 AND traffic_percentage <= 100),
    member_experiment_ids bigint[] NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_by TEXT NOT NULL,
    last_modified_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    last_modified_by TEXT NOT NULL
);

ALTER TABLE {replaceme}.experiments ADD COLUMN IF NOT EXISTS experiment_group_id bigint;

DO $$ BEGIN
    CREATE TRIGGER experiment_groups_audit AFTER INSERT OR DELETE OR UPDATE ON {replaceme}.experiment_groups FOR EACH ROW EXECUTE FUNCTION {replaceme}.event_logger();
EXCEPTION
    WHEN duplicate_object THEN null;
END $$;

ALTER TABLE {replaceme}.experiment_groups ADD COLUMN IF NOT EXISTS buckets JSON[] DEFAULT array_fill(NULL::JSON, ARRAY[100]) NOT NULL CHECK (cardinality(buckets) = 100);

DO $$ BEGIN
    CREATE TYPE public.group_type AS ENUM (
        'USER_CREATED',
        'SYSTEM_GENERATED'
    );
EXCEPTION
    WHEN duplicate_object THEN null;
END $$;

ALTER TABLE {replaceme}.experiment_groups 
ADD COLUMN IF NOT EXISTS group_type public.group_type DEFAULT 'USER_CREATED';

ALTER TABLE {replaceme}.functions
ADD COLUMN IF NOT EXISTS created_at TIMESTAMP,
ADD COLUMN IF NOT EXISTS created_by TEXT NOT NULL DEFAULT 'NOT AVAILABLE';

-- drop the constraint so validations work, this for backfill
ALTER TABLE {replaceme}.functions ALTER COLUMN created_by DROP DEFAULT;

UPDATE {replaceme}.functions SET created_at = last_modified_at WHERE created_at IS NULL;

-- set constraints on created_at after backfill is done
ALTER TABLE {replaceme}.functions
ALTER COLUMN created_at SET DEFAULT CURRENT_TIMESTAMP,
ALTER COLUMN created_at SET NOT NULL;