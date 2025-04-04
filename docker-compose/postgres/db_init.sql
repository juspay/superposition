CREATE SCHEMA IF NOT EXISTS superposition;

CREATE TYPE superposition.org_status AS ENUM ('ACTIVE', 'INACTIVE', 'PENDING_KYB');

CREATE TABLE IF NOT EXISTS superposition.organisations (
    id VARCHAR(30) PRIMARY KEY NOT NULL,
    name TEXT NOT NULL,
    country_code VARCHAR(10),
    contact_email VARCHAR(255),
    contact_phone VARCHAR(15),
    created_by TEXT NOT NULL,
    admin_email TEXT NOT NULL,
    status superposition.org_status NOT NULL DEFAULT 'ACTIVE',
    sector VARCHAR(100),
    created_at TIMESTAMP WITHOUT TIME ZONE DEFAULT NOW () NOT NULL,
    updated_at TIMESTAMP WITHOUT TIME ZONE DEFAULT NOW () NOT NULL,
    updated_by TEXT NOT NULL
);

-- Indexes for optimizing queries
CREATE INDEX IF NOT EXISTS idx_organisation_contact_email ON superposition.organisations (contact_email);

CREATE INDEX IF NOT EXISTS idx_organisation_status ON superposition.organisations (status);

CREATE INDEX IF NOT EXISTS idx_organisation_created_at ON superposition.organisations (created_at);

CREATE INDEX IF NOT EXISTS idx_organisation_admin_email ON superposition.organisations (admin_email);

-- Create localorg organisation
INSERT INTO
    superposition.organisations (id, name, created_by, admin_email, updated_by)
VALUES
    (
        'localorg',
        'localorg',
        'admin@localorg.io',
        'admin@localorg.io',
        'admin@localorg.io'
    );

CREATE TYPE superposition.workspace_status AS ENUM ('ENABLED', 'DISABLED');
CREATE TABLE IF NOT EXISTS superposition.workspaces (
    organisation_id VARCHAR(30) NOT NULL,
    organisation_name TEXT NOT NULL,
    workspace_name VARCHAR(25) NOT NULL,
    workspace_schema_name TEXT NOT NULL,
    workspace_status superposition.WORKSPACE_STATUS NOT NULL,
    workspace_admin_email TEXT NOT NULL,
    created_by TEXT NOT NULL,
    last_modified_by TEXT NOT NULL,
    last_modified_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    mandatory_dimensions TEXT [],
    CONSTRAINT organisation_workspace_pkey PRIMARY KEY (organisation_id, workspace_name),
    CONSTRAINT unique_workspace_schema_name UNIQUE (workspace_schema_name),
    CONSTRAINT fk_organisation FOREIGN KEY (organisation_id) REFERENCES superposition.organisations (id) ON DELETE RESTRICT ON UPDATE CASCADE
);
CREATE INDEX IF NOT EXISTS idx_workspace_name ON superposition.workspaces (workspace_name);
CREATE INDEX IF NOT EXISTS idx_last_modified_created_by ON superposition.workspaces (last_modified_by, created_by);

BEGIN;
-- Setup workspace schema
-- Your SQL goes here
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
--
-- Name: localorg_test; Type: SCHEMA; Schema: -; Owner: -
--
CREATE SCHEMA IF NOT EXISTS localorg_test;

--
-- Name: event_logger(); Type: FUNCTION; Schema: localorg_test; Owner: -
--
CREATE OR REPLACE FUNCTION localorg_test.event_logger() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
    old_data json;
    new_data json;
BEGIN
    IF (TG_OP = 'UPDATE') THEN
        old_data := row_to_json(OLD);
        new_data := row_to_json(NEW);
        INSERT INTO localorg_test.event_log
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
        INSERT INTO localorg_test.event_log
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
        INSERT INTO localorg_test.event_log
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
-- Name: contexts; Type: TABLE; Schema: localorg_test; Owner: -
--
CREATE TABLE localorg_test.contexts (
    id character varying PRIMARY KEY,
    value json NOT NULL,
    override_id character varying NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_by character varying NOT NULL,
    priority integer DEFAULT 1 NOT NULL,
    override json DEFAULT '{}'::json NOT NULL
);
--
-- Name: default_configs; Type: TABLE; Schema: localorg_test; Owner: -
--
CREATE TABLE localorg_test.default_configs (
    key character varying PRIMARY KEY,
    value json NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_by character varying NOT NULL,
    schema json DEFAULT '{}'::json NOT NULL
);
--
-- Name: dimensions; Type: TABLE; Schema: localorg_test; Owner: -
--
CREATE TABLE localorg_test.dimensions (
    dimension character varying PRIMARY KEY,
    priority integer NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_by character varying NOT NULL,
    schema json DEFAULT '{}'::json NOT NULL
);
--
-- Name: event_log; Type: TABLE; Schema: localorg_test; Owner: -
--
CREATE TABLE IF NOT EXISTS localorg_test.event_log (
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
-- Name: event_log_action_index; Type: INDEX; Schema: localorg_test; Owner: -
--
CREATE INDEX IF NOT EXISTS event_log_action_index ON ONLY localorg_test.event_log USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_table_name_index; Type: INDEX; Schema: localorg_test; Owner: -
--
CREATE INDEX IF NOT EXISTS event_log_table_name_index ON ONLY localorg_test.event_log USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_timestamp_index; Type: INDEX; Schema: localorg_test; Owner: -
--
CREATE INDEX IF NOT EXISTS event_log_timestamp_index ON ONLY localorg_test.event_log USING btree ("timestamp") INCLUDE (action, table_name);

--
-- Name: contexts contexts_audit; Type: TRIGGER; Schema: localorg_test; Owner: -
--
CREATE TRIGGER contexts_audit AFTER INSERT OR DELETE OR UPDATE ON localorg_test.contexts FOR EACH ROW EXECUTE FUNCTION localorg_test.event_logger();
--
-- Name: default_configs default_configs_audit; Type: TRIGGER; Schema: localorg_test; Owner: -
--
CREATE TRIGGER default_configs_audit AFTER INSERT OR DELETE OR UPDATE ON localorg_test.default_configs FOR EACH ROW EXECUTE FUNCTION localorg_test.event_logger();
--
-- Name: dimensions dimensions_audit; Type: TRIGGER; Schema: localorg_test; Owner: -
--
CREATE TRIGGER dimensions_audit AFTER INSERT OR DELETE OR UPDATE ON localorg_test.dimensions FOR EACH ROW EXECUTE FUNCTION localorg_test.event_logger();
-- Your SQL goes here
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
--
-- Name: localorg_test; Type: SCHEMA; Schema: -; Owner: -
--
-- Name: experiment_status_type; Type: TYPE; Schema: localorg_test; Owner: -
--

--
-- Name: not_null_text; Type: DOMAIN; Schema: localorg_test; Owner: -
--
CREATE DOMAIN localorg_test.not_null_text AS text NOT NULL;
--
-- Name: event_logger(); Type: FUNCTION; Schema: localorg_test; Owner: -
--

DO $$ BEGIN
    CREATE TYPE public.experiment_status_type AS ENUM (
        'CREATED',
        'CONCLUDED',
        'INPROGRESS',
        'DISCARDED'
    );
EXCEPTION
    WHEN duplicate_object THEN null;
END $$;
--
-- Name: experiments; Type: TABLE; Schema: localorg_test; Owner: -
--
CREATE TABLE localorg_test.experiments (
    id bigint PRIMARY KEY,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_by text NOT NULL,
    last_modified timestamp with time zone DEFAULT now() NOT NULL,
    name text NOT NULL,
    override_keys localorg_test.not_null_text [] NOT NULL,
    status public.experiment_status_type NOT NULL,
    traffic_percentage integer NOT NULL,
    context json NOT NULL,
    variants json NOT NULL,
    last_modified_by text DEFAULT 'Null'::text NOT NULL,
    chosen_variant text,
    CONSTRAINT experiments_traffic_percentage_check CHECK ((traffic_percentage >= 0))
);
--
-- Name: experiment_created_date_index; Type: INDEX; Schema: localorg_test; Owner: -
--
CREATE INDEX experiment_created_date_index ON localorg_test.experiments USING btree (created_at) INCLUDE (id);
--
-- Name: experiment_last_modified_index; Type: INDEX; Schema: localorg_test; Owner: -
--
CREATE INDEX experiment_last_modified_index ON localorg_test.experiments USING btree (last_modified) INCLUDE (id, created_at);
--
-- Name: experiment_status_index; Type: INDEX; Schema: localorg_test; Owner: -
--
CREATE INDEX experiment_status_index ON localorg_test.experiments USING btree (status) INCLUDE (created_at, last_modified);
--
-- Name: experiments experiments_audit; Type: TRIGGER; Schema: localorg_test; Owner: -
--
CREATE TRIGGER experiments_audit AFTER INSERT OR DELETE OR UPDATE ON localorg_test.experiments FOR EACH ROW EXECUTE FUNCTION localorg_test.event_logger();

------------ Parititions for 2025 -----------
CREATE TABLE IF NOT EXISTS localorg_test.event_log_y2025m01 PARTITION OF localorg_test.event_log FOR
VALUES
FROM ('2025-01-01') TO ('2025-02-01');

CREATE TABLE IF NOT EXISTS localorg_test.event_log_y2025m02 PARTITION OF localorg_test.event_log FOR
VALUES
FROM ('2025-02-01') TO ('2025-03-01');

CREATE TABLE IF NOT EXISTS localorg_test.event_log_y2025m03 PARTITION OF localorg_test.event_log FOR
VALUES
FROM ('2025-03-01') TO ('2025-04-01');

CREATE TABLE IF NOT EXISTS localorg_test.event_log_y2025m04 PARTITION OF localorg_test.event_log FOR
VALUES
FROM ('2025-04-01') TO ('2025-05-01');

CREATE TABLE IF NOT EXISTS localorg_test.event_log_y2025m05 PARTITION OF localorg_test.event_log FOR
VALUES
FROM ('2025-05-01') TO ('2025-06-01');

CREATE TABLE IF NOT EXISTS localorg_test.event_log_y2025m06 PARTITION OF localorg_test.event_log FOR
VALUES
FROM ('2025-06-01') TO ('2025-07-01');

CREATE TABLE IF NOT EXISTS localorg_test.event_log_y2025m07 PARTITION OF localorg_test.event_log FOR
VALUES
FROM ('2025-07-01') TO ('2025-08-01');

CREATE TABLE IF NOT EXISTS localorg_test.event_log_y2025m08 PARTITION OF localorg_test.event_log FOR
VALUES
FROM ('2025-08-01') TO ('2025-09-01');

CREATE TABLE IF NOT EXISTS localorg_test.event_log_y2025m09 PARTITION OF localorg_test.event_log FOR
VALUES
FROM ('2025-09-01') TO ('2025-10-01');

CREATE TABLE IF NOT EXISTS localorg_test.event_log_y2025m10 PARTITION OF localorg_test.event_log FOR
VALUES
FROM ('2025-10-01') TO ('2025-11-01');

CREATE TABLE IF NOT EXISTS localorg_test.event_log_y2025m11 PARTITION OF localorg_test.event_log FOR
VALUES
FROM ('2025-11-01') TO ('2025-12-01');

CREATE TABLE IF NOT EXISTS localorg_test.event_log_y2025m12 PARTITION OF localorg_test.event_log FOR
VALUES
FROM ('2025-12-01') TO ('2026-01-01');

------------ Parititions for 2026 -----------
CREATE TABLE IF NOT EXISTS localorg_test.event_log_y2026m01 PARTITION OF localorg_test.event_log FOR
VALUES
FROM ('2026-01-01') TO ('2026-02-01');

CREATE TABLE IF NOT EXISTS localorg_test.event_log_y2026m02 PARTITION OF localorg_test.event_log FOR
VALUES
FROM ('2026-02-01') TO ('2026-03-01');

CREATE TABLE IF NOT EXISTS localorg_test.event_log_y2026m03 PARTITION OF localorg_test.event_log FOR
VALUES
FROM ('2026-03-01') TO ('2026-04-01');

CREATE TABLE IF NOT EXISTS localorg_test.event_log_y2026m04 PARTITION OF localorg_test.event_log FOR
VALUES
FROM ('2026-04-01') TO ('2026-05-01');

CREATE TABLE IF NOT EXISTS localorg_test.event_log_y2026m05 PARTITION OF localorg_test.event_log FOR
VALUES
FROM ('2026-05-01') TO ('2026-06-01');

CREATE TABLE IF NOT EXISTS localorg_test.event_log_y2026m06 PARTITION OF localorg_test.event_log FOR
VALUES
FROM ('2026-06-01') TO ('2026-07-01');

CREATE TABLE IF NOT EXISTS localorg_test.event_log_y2026m07 PARTITION OF localorg_test.event_log FOR
VALUES
FROM ('2026-07-01') TO ('2026-08-01');

CREATE TABLE IF NOT EXISTS localorg_test.event_log_y2026m08 PARTITION OF localorg_test.event_log FOR
VALUES
FROM ('2026-08-01') TO ('2026-09-01');

CREATE TABLE IF NOT EXISTS localorg_test.event_log_y2026m09 PARTITION OF localorg_test.event_log FOR
VALUES
FROM ('2026-09-01') TO ('2026-10-01');

CREATE TABLE IF NOT EXISTS localorg_test.event_log_y2026m10 PARTITION OF localorg_test.event_log FOR
VALUES
FROM ('2026-10-01') TO ('2026-11-01');

CREATE TABLE IF NOT EXISTS localorg_test.event_log_y2026m11 PARTITION OF localorg_test.event_log FOR
VALUES
FROM ('2026-11-01') TO ('2026-12-01');

CREATE TABLE IF NOT EXISTS localorg_test.event_log_y2026m12 PARTITION OF localorg_test.event_log FOR
VALUES
FROM ('2026-12-01') TO ('2027-01-01');

-- Your SQL goes here
-- Name: functions; Type: TABLE; Schema: localorg_test; Owner: -
--
CREATE TABLE localorg_test.functions (
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
-- Name: functions functions_audit; Type: TRIGGER; Schema: localorg_test; Owner: -
--
CREATE TRIGGER functions_audit AFTER INSERT OR DELETE OR UPDATE ON localorg_test.functions FOR EACH ROW EXECUTE FUNCTION localorg_test.event_logger();
-- Your SQL goes here
ALTER TABLE localorg_test.dimensions ADD COLUMN function_name text NULL;

ALTER TABLE localorg_test.dimensions ADD FOREIGN KEY(function_name) REFERENCES localorg_test.functions(function_name);

ALTER TABLE localorg_test.default_configs ADD COLUMN function_name text NULL;

ALTER TABLE localorg_test.default_configs ADD FOREIGN KEY(function_name) REFERENCES localorg_test.functions(function_name);
-- Your SQL goes here
-- Name: functions; Type: TABLE; Schema: localorg_test; Owner: -
--
CREATE TABLE localorg_test.config_versions (
id bigint PRIMARY KEY,
config json NOT NULL,
config_hash TEXT NOT NULL,
tags varchar(100) [] check (array_position(tags, null) is null),
created_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
CREATE INDEX IF NOT EXISTS config_verions_tags_index ON localorg_test.config_versions USING gin(tags);
CREATE INDEX IF NOT EXISTS config_versions_id_index ON localorg_test.config_versions(id);
-- Your SQL goes here
CREATE TABLE IF NOT EXISTS localorg_test.type_templates (
    type_name TEXT PRIMARY KEY,
    type_schema JSON NOT NULL,
    created_by TEXT NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    last_modified TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL
);
CREATE INDEX IF NOT EXISTS type_templates_index ON localorg_test.type_templates(type_name);
CREATE INDEX IF NOT EXISTS type_templates_created_at_index ON localorg_test.type_templates(created_at);
CREATE INDEX IF NOT EXISTS type_templates_last_modifed_index ON localorg_test.type_templates(last_modified);
INSERT INTO localorg_test.type_templates(type_name, type_schema, created_by, created_at, last_modified)
VALUES (
        'Number',
        '{"type": "integer"}',
        'user@superposition.io',
        NOW(),
        NOW()
    ),
    (
        'Decimal',
        '{"type": "number"}',
        'user@superposition.io',
        NOW(),
        NOW()
    ),
    (
        'Boolean',
        '{"type": "boolean"}',
        'user@superposition.io',
        NOW(),
        NOW()
    ),
    (
        'Enum',
        '{"type": "string", "enum": ["android", "ios"]}',
        'user@superposition.io',
        NOW(),
        NOW()
    ),
    (
        'Pattern',
        '{"type": "string", "pattern": ".*"}',
        'user@superposition.io',
        NOW(),
        NOW()
    );
-- Your SQL goes here

ALTER TABLE localorg_test.functions
add column last_modified_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
add column last_modified_by varchar(200) not null default('null');

ALTER TABLE localorg_test.dimensions
add column last_modified_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
add column last_modified_by varchar(200) not null default('null');

ALTER TABLE localorg_test.contexts
add column last_modified_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
add column last_modified_by varchar(200) not null default('null');

ALTER TABLE localorg_test.default_configs
add column last_modified_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
add column last_modified_by varchar(200) not null default('null');

ALTER TABLE localorg_test.type_templates
rename column last_modified to last_modified_at;

ALTER TABLE localorg_test.type_templates
add column last_modified_by varchar(200) not null default('null');
-- Your SQL goes here
ALTER TABLE localorg_test.dimensions
add column position integer DEFAULT 0 NOT NULL;
ALTER TABLE localorg_test.contexts
add column weight numeric(1000, 0) DEFAULT 1 NOT NULL;
CREATE INDEX IF NOT EXISTS idx_contexts_weight ON localorg_test.contexts(weight);
-- Your SQL goes here
ALTER TABLE localorg_test.dimensions
ALTER COLUMN priority SET DEFAULT 1;

ALTER TABLE localorg_test.dimensions
ADD CONSTRAINT dimension_unique_position UNIQUE (position);

ALTER TABLE localorg_test.contexts ADD COLUMN IF NOT EXISTS description TEXT DEFAULT '' NOT NULL;
ALTER TABLE localorg_test.contexts ADD COLUMN IF NOT EXISTS change_reason TEXT DEFAULT '' NOT NULL;

ALTER TABLE localorg_test.dimensions ADD COLUMN IF NOT EXISTS description TEXT DEFAULT '' NOT NULL;
ALTER TABLE localorg_test.dimensions ADD COLUMN IF NOT EXISTS change_reason TEXT DEFAULT '' NOT NULL;

ALTER TABLE localorg_test.default_configs ADD COLUMN IF NOT EXISTS description TEXT DEFAULT '' NOT NULL;
ALTER TABLE localorg_test.default_configs ADD COLUMN IF NOT EXISTS change_reason TEXT DEFAULT '' NOT NULL;

ALTER TABLE localorg_test.type_templates ADD COLUMN IF NOT EXISTS description TEXT DEFAULT '' NOT NULL;
ALTER TABLE localorg_test.type_templates ADD COLUMN IF NOT EXISTS change_reason TEXT DEFAULT '' NOT NULL;

ALTER TABLE localorg_test.functions RENAME COLUMN function_description TO description;
ALTER TABLE localorg_test.functions ADD COLUMN IF NOT EXISTS change_reason TEXT DEFAULT '' NOT NULL;

ALTER TABLE localorg_test.functions ALTER COLUMN description SET DEFAULT '';
ALTER TABLE localorg_test.functions ALTER COLUMN description SET NOT NULL;

ALTER TABLE localorg_test.config_versions ADD COLUMN IF NOT EXISTS description TEXT DEFAULT '' NOT NULL;

ALTER TABLE localorg_test.experiments ADD COLUMN IF NOT EXISTS description TEXT DEFAULT '' NOT NULL;
ALTER TABLE localorg_test.experiments ADD COLUMN IF NOT EXISTS change_reason TEXT DEFAULT '' NOT NULL;

INSERT INTO localorg_test.dimensions (
        dimension,
        priority,
        created_at,
        created_by,
        schema,
        function_name,
        description
    )
VALUES (
        'variantIds',
        0,
        CURRENT_TIMESTAMP,
        'default@superposition.io',
        '{"type": "string","pattern": ".*"}'::json,
        null,
        'variantIds are used by experimentation module to manage and select variations'
);

-- Create older partitions
-- 2023-2024
CREATE TABLE IF NOT EXISTS localorg_test.event_log_y2023m01 PARTITION OF localorg_test.event_log FOR
VALUES
FROM ('2023-01-01') TO ('2023-02-01');

CREATE TABLE IF NOT EXISTS localorg_test.event_log_y2023m02 PARTITION OF localorg_test.event_log FOR
VALUES
FROM ('2023-02-01') TO ('2023-03-01');

CREATE TABLE IF NOT EXISTS localorg_test.event_log_y2023m03 PARTITION OF localorg_test.event_log FOR
VALUES
FROM ('2023-03-01') TO ('2023-04-01');

CREATE TABLE IF NOT EXISTS localorg_test.event_log_y2023m04 PARTITION OF localorg_test.event_log FOR
VALUES
FROM ('2023-04-01') TO ('2023-05-01');

CREATE TABLE IF NOT EXISTS localorg_test.event_log_y2023m05 PARTITION OF localorg_test.event_log FOR
VALUES
FROM ('2023-05-01') TO ('2023-06-01');

CREATE TABLE IF NOT EXISTS localorg_test.event_log_y2023m06 PARTITION OF localorg_test.event_log FOR
VALUES
FROM ('2023-06-01') TO ('2023-07-01');

CREATE TABLE IF NOT EXISTS localorg_test.event_log_y2023m07 PARTITION OF localorg_test.event_log FOR
VALUES
FROM ('2023-07-01') TO ('2023-08-01');

CREATE TABLE IF NOT EXISTS localorg_test.event_log_y2023m08 PARTITION OF localorg_test.event_log FOR
VALUES
FROM ('2023-08-01') TO ('2023-09-01');

CREATE TABLE IF NOT EXISTS localorg_test.event_log_y2023m09 PARTITION OF localorg_test.event_log FOR
VALUES
FROM ('2023-09-01') TO ('2023-10-01');

CREATE TABLE IF NOT EXISTS localorg_test.event_log_y2023m10 PARTITION OF localorg_test.event_log FOR
VALUES
FROM ('2023-10-01') TO ('2023-11-01');

CREATE TABLE IF NOT EXISTS localorg_test.event_log_y2023m11 PARTITION OF localorg_test.event_log FOR
VALUES
FROM ('2023-11-01') TO ('2023-12-01');

CREATE TABLE IF NOT EXISTS localorg_test.event_log_y2023m12 PARTITION OF localorg_test.event_log FOR
VALUES
FROM ('2023-12-01') TO ('2024-01-01');

-- 2024-2025
CREATE TABLE IF NOT EXISTS localorg_test.event_log_y2024m01 PARTITION OF localorg_test.event_log FOR
VALUES
FROM ('2024-01-01') TO ('2024-02-01');

CREATE TABLE IF NOT EXISTS localorg_test.event_log_y2024m02 PARTITION OF localorg_test.event_log FOR
VALUES
FROM ('2024-02-01') TO ('2024-03-01');

CREATE TABLE IF NOT EXISTS localorg_test.event_log_y2024m03 PARTITION OF localorg_test.event_log FOR
VALUES
FROM ('2024-03-01') TO ('2024-04-01');

CREATE TABLE IF NOT EXISTS localorg_test.event_log_y2024m04 PARTITION OF localorg_test.event_log FOR
VALUES
FROM ('2024-04-01') TO ('2024-05-01');

CREATE TABLE IF NOT EXISTS localorg_test.event_log_y2024m05 PARTITION OF localorg_test.event_log FOR
VALUES
FROM ('2024-05-01') TO ('2024-06-01');

CREATE TABLE IF NOT EXISTS localorg_test.event_log_y2024m06 PARTITION OF localorg_test.event_log FOR
VALUES
FROM ('2024-06-01') TO ('2024-07-01');

CREATE TABLE IF NOT EXISTS localorg_test.event_log_y2024m07 PARTITION OF localorg_test.event_log FOR
VALUES
FROM ('2024-07-01') TO ('2024-08-01');

CREATE TABLE IF NOT EXISTS localorg_test.event_log_y2024m08 PARTITION OF localorg_test.event_log FOR
VALUES
FROM ('2024-08-01') TO ('2024-09-01');

CREATE TABLE IF NOT EXISTS localorg_test.event_log_y2024m09 PARTITION OF localorg_test.event_log FOR
VALUES
FROM ('2024-09-01') TO ('2024-10-01');

CREATE TABLE IF NOT EXISTS localorg_test.event_log_y2024m10 PARTITION OF localorg_test.event_log FOR
VALUES
FROM ('2024-10-01') TO ('2024-11-01');

CREATE TABLE IF NOT EXISTS localorg_test.event_log_y2024m11 PARTITION OF localorg_test.event_log FOR
VALUES
FROM ('2024-11-01') TO ('2024-12-01');

CREATE TABLE IF NOT EXISTS localorg_test.event_log_y2024m12 PARTITION OF localorg_test.event_log FOR
VALUES
FROM ('2024-12-01') TO ('2025-01-01');

-- Insert entry into workspaces table
INSERT INTO superposition.workspaces (
    organisation_id,
    organisation_name,
    workspace_name,
    workspace_schema_name,
    workspace_status,
    workspace_admin_email,
    created_by,
    last_modified_by,
    mandatory_dimensions
) VALUES (
    'localorg',
    'localorg',
    'test',
    'localorg_test',
    'ENABLED',
    'admin@localorg.io',
    'admin@localorg.io',
    'admin@localorg.io',
    null
);

-- Setup workspace schema
-- Your SQL goes here
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
--
-- Name: localorg_dev; Type: SCHEMA; Schema: -; Owner: -
--
CREATE SCHEMA IF NOT EXISTS localorg_dev;

--
-- Name: event_logger(); Type: FUNCTION; Schema: localorg_dev; Owner: -
--
CREATE OR REPLACE FUNCTION localorg_dev.event_logger() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
    old_data json;
    new_data json;
BEGIN
    IF (TG_OP = 'UPDATE') THEN
        old_data := row_to_json(OLD);
        new_data := row_to_json(NEW);
        INSERT INTO localorg_dev.event_log
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
        INSERT INTO localorg_dev.event_log
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
        INSERT INTO localorg_dev.event_log
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
-- Name: contexts; Type: TABLE; Schema: localorg_dev; Owner: -
--
CREATE TABLE localorg_dev.contexts (
    id character varying PRIMARY KEY,
    value json NOT NULL,
    override_id character varying NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_by character varying NOT NULL,
    priority integer DEFAULT 1 NOT NULL,
    override json DEFAULT '{}'::json NOT NULL
);
--
-- Name: default_configs; Type: TABLE; Schema: localorg_dev; Owner: -
--
CREATE TABLE localorg_dev.default_configs (
    key character varying PRIMARY KEY,
    value json NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_by character varying NOT NULL,
    schema json DEFAULT '{}'::json NOT NULL
);
--
-- Name: dimensions; Type: TABLE; Schema: localorg_dev; Owner: -
--
CREATE TABLE localorg_dev.dimensions (
    dimension character varying PRIMARY KEY,
    priority integer NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_by character varying NOT NULL,
    schema json DEFAULT '{}'::json NOT NULL
);
--
-- Name: event_log; Type: TABLE; Schema: localorg_dev; Owner: -
--
CREATE TABLE IF NOT EXISTS localorg_dev.event_log (
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
-- Name: event_log_action_index; Type: INDEX; Schema: localorg_dev; Owner: -
--
CREATE INDEX IF NOT EXISTS event_log_action_index ON ONLY localorg_dev.event_log USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_table_name_index; Type: INDEX; Schema: localorg_dev; Owner: -
--
CREATE INDEX IF NOT EXISTS event_log_table_name_index ON ONLY localorg_dev.event_log USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_timestamp_index; Type: INDEX; Schema: localorg_dev; Owner: -
--
CREATE INDEX IF NOT EXISTS event_log_timestamp_index ON ONLY localorg_dev.event_log USING btree ("timestamp") INCLUDE (action, table_name);

--
-- Name: contexts contexts_audit; Type: TRIGGER; Schema: localorg_dev; Owner: -
--
CREATE TRIGGER contexts_audit AFTER INSERT OR DELETE OR UPDATE ON localorg_dev.contexts FOR EACH ROW EXECUTE FUNCTION localorg_dev.event_logger();
--
-- Name: default_configs default_configs_audit; Type: TRIGGER; Schema: localorg_dev; Owner: -
--
CREATE TRIGGER default_configs_audit AFTER INSERT OR DELETE OR UPDATE ON localorg_dev.default_configs FOR EACH ROW EXECUTE FUNCTION localorg_dev.event_logger();
--
-- Name: dimensions dimensions_audit; Type: TRIGGER; Schema: localorg_dev; Owner: -
--
CREATE TRIGGER dimensions_audit AFTER INSERT OR DELETE OR UPDATE ON localorg_dev.dimensions FOR EACH ROW EXECUTE FUNCTION localorg_dev.event_logger();
-- Your SQL goes here
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
--
-- Name: localorg_dev; Type: SCHEMA; Schema: -; Owner: -
--
-- Name: experiment_status_type; Type: TYPE; Schema: localorg_dev; Owner: -
--

--
-- Name: not_null_text; Type: DOMAIN; Schema: localorg_dev; Owner: -
--
CREATE DOMAIN localorg_dev.not_null_text AS text NOT NULL;
--
-- Name: event_logger(); Type: FUNCTION; Schema: localorg_dev; Owner: -
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
-- Name: experiments; Type: TABLE; Schema: localorg_dev; Owner: -
--
CREATE TABLE localorg_dev.experiments (
    id bigint PRIMARY KEY,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_by text NOT NULL,
    last_modified timestamp with time zone DEFAULT now() NOT NULL,
    name text NOT NULL,
    override_keys localorg_dev.not_null_text [] NOT NULL,
    status public.experiment_status_type NOT NULL,
    traffic_percentage integer NOT NULL,
    context json NOT NULL,
    variants json NOT NULL,
    last_modified_by text DEFAULT 'Null'::text NOT NULL,
    chosen_variant text,
    CONSTRAINT experiments_traffic_percentage_check CHECK ((traffic_percentage >= 0))
);
--
-- Name: experiment_created_date_index; Type: INDEX; Schema: localorg_dev; Owner: -
--
CREATE INDEX experiment_created_date_index ON localorg_dev.experiments USING btree (created_at) INCLUDE (id);
--
-- Name: experiment_last_modified_index; Type: INDEX; Schema: localorg_dev; Owner: -
--
CREATE INDEX experiment_last_modified_index ON localorg_dev.experiments USING btree (last_modified) INCLUDE (id, created_at);
--
-- Name: experiment_status_index; Type: INDEX; Schema: localorg_dev; Owner: -
--
CREATE INDEX experiment_status_index ON localorg_dev.experiments USING btree (status) INCLUDE (created_at, last_modified);
--
-- Name: experiments experiments_audit; Type: TRIGGER; Schema: localorg_dev; Owner: -
--
CREATE TRIGGER experiments_audit AFTER INSERT OR DELETE OR UPDATE ON localorg_dev.experiments FOR EACH ROW EXECUTE FUNCTION localorg_dev.event_logger();

------------ Parititions for 2025 -----------
CREATE TABLE IF NOT EXISTS localorg_dev.event_log_y2025m01 PARTITION OF localorg_dev.event_log FOR
VALUES
FROM ('2025-01-01') TO ('2025-02-01');

CREATE TABLE IF NOT EXISTS localorg_dev.event_log_y2025m02 PARTITION OF localorg_dev.event_log FOR
VALUES
FROM ('2025-02-01') TO ('2025-03-01');

CREATE TABLE IF NOT EXISTS localorg_dev.event_log_y2025m03 PARTITION OF localorg_dev.event_log FOR
VALUES
FROM ('2025-03-01') TO ('2025-04-01');

CREATE TABLE IF NOT EXISTS localorg_dev.event_log_y2025m04 PARTITION OF localorg_dev.event_log FOR
VALUES
FROM ('2025-04-01') TO ('2025-05-01');

CREATE TABLE IF NOT EXISTS localorg_dev.event_log_y2025m05 PARTITION OF localorg_dev.event_log FOR
VALUES
FROM ('2025-05-01') TO ('2025-06-01');

CREATE TABLE IF NOT EXISTS localorg_dev.event_log_y2025m06 PARTITION OF localorg_dev.event_log FOR
VALUES
FROM ('2025-06-01') TO ('2025-07-01');

CREATE TABLE IF NOT EXISTS localorg_dev.event_log_y2025m07 PARTITION OF localorg_dev.event_log FOR
VALUES
FROM ('2025-07-01') TO ('2025-08-01');

CREATE TABLE IF NOT EXISTS localorg_dev.event_log_y2025m08 PARTITION OF localorg_dev.event_log FOR
VALUES
FROM ('2025-08-01') TO ('2025-09-01');

CREATE TABLE IF NOT EXISTS localorg_dev.event_log_y2025m09 PARTITION OF localorg_dev.event_log FOR
VALUES
FROM ('2025-09-01') TO ('2025-10-01');

CREATE TABLE IF NOT EXISTS localorg_dev.event_log_y2025m10 PARTITION OF localorg_dev.event_log FOR
VALUES
FROM ('2025-10-01') TO ('2025-11-01');

CREATE TABLE IF NOT EXISTS localorg_dev.event_log_y2025m11 PARTITION OF localorg_dev.event_log FOR
VALUES
FROM ('2025-11-01') TO ('2025-12-01');

CREATE TABLE IF NOT EXISTS localorg_dev.event_log_y2025m12 PARTITION OF localorg_dev.event_log FOR
VALUES
FROM ('2025-12-01') TO ('2026-01-01');

------------ Parititions for 2026 -----------
CREATE TABLE IF NOT EXISTS localorg_dev.event_log_y2026m01 PARTITION OF localorg_dev.event_log FOR
VALUES
FROM ('2026-01-01') TO ('2026-02-01');

CREATE TABLE IF NOT EXISTS localorg_dev.event_log_y2026m02 PARTITION OF localorg_dev.event_log FOR
VALUES
FROM ('2026-02-01') TO ('2026-03-01');

CREATE TABLE IF NOT EXISTS localorg_dev.event_log_y2026m03 PARTITION OF localorg_dev.event_log FOR
VALUES
FROM ('2026-03-01') TO ('2026-04-01');

CREATE TABLE IF NOT EXISTS localorg_dev.event_log_y2026m04 PARTITION OF localorg_dev.event_log FOR
VALUES
FROM ('2026-04-01') TO ('2026-05-01');

CREATE TABLE IF NOT EXISTS localorg_dev.event_log_y2026m05 PARTITION OF localorg_dev.event_log FOR
VALUES
FROM ('2026-05-01') TO ('2026-06-01');

CREATE TABLE IF NOT EXISTS localorg_dev.event_log_y2026m06 PARTITION OF localorg_dev.event_log FOR
VALUES
FROM ('2026-06-01') TO ('2026-07-01');

CREATE TABLE IF NOT EXISTS localorg_dev.event_log_y2026m07 PARTITION OF localorg_dev.event_log FOR
VALUES
FROM ('2026-07-01') TO ('2026-08-01');

CREATE TABLE IF NOT EXISTS localorg_dev.event_log_y2026m08 PARTITION OF localorg_dev.event_log FOR
VALUES
FROM ('2026-08-01') TO ('2026-09-01');

CREATE TABLE IF NOT EXISTS localorg_dev.event_log_y2026m09 PARTITION OF localorg_dev.event_log FOR
VALUES
FROM ('2026-09-01') TO ('2026-10-01');

CREATE TABLE IF NOT EXISTS localorg_dev.event_log_y2026m10 PARTITION OF localorg_dev.event_log FOR
VALUES
FROM ('2026-10-01') TO ('2026-11-01');

CREATE TABLE IF NOT EXISTS localorg_dev.event_log_y2026m11 PARTITION OF localorg_dev.event_log FOR
VALUES
FROM ('2026-11-01') TO ('2026-12-01');

CREATE TABLE IF NOT EXISTS localorg_dev.event_log_y2026m12 PARTITION OF localorg_dev.event_log FOR
VALUES
FROM ('2026-12-01') TO ('2027-01-01');

-- Your SQL goes here
-- Name: functions; Type: TABLE; Schema: localorg_dev; Owner: -
--
CREATE TABLE localorg_dev.functions (
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
-- Name: functions functions_audit; Type: TRIGGER; Schema: localorg_dev; Owner: -
--
CREATE TRIGGER functions_audit AFTER INSERT OR DELETE OR UPDATE ON localorg_dev.functions FOR EACH ROW EXECUTE FUNCTION localorg_dev.event_logger();
-- Your SQL goes here
ALTER TABLE localorg_dev.dimensions ADD COLUMN function_name text NULL;

ALTER TABLE localorg_dev.dimensions ADD FOREIGN KEY(function_name) REFERENCES localorg_dev.functions(function_name);

ALTER TABLE localorg_dev.default_configs ADD COLUMN function_name text NULL;

ALTER TABLE localorg_dev.default_configs ADD FOREIGN KEY(function_name) REFERENCES localorg_dev.functions(function_name);
-- Your SQL goes here
-- Name: functions; Type: TABLE; Schema: localorg_dev; Owner: -
--
CREATE TABLE localorg_dev.config_versions (
id bigint PRIMARY KEY,
config json NOT NULL,
config_hash TEXT NOT NULL,
tags varchar(100) [] check (array_position(tags, null) is null),
created_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
CREATE INDEX IF NOT EXISTS config_verions_tags_index ON localorg_dev.config_versions USING gin(tags);
CREATE INDEX IF NOT EXISTS config_versions_id_index ON localorg_dev.config_versions(id);
-- Your SQL goes here
CREATE TABLE IF NOT EXISTS localorg_dev.type_templates (
    type_name TEXT PRIMARY KEY,
    type_schema JSON NOT NULL,
    created_by TEXT NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    last_modified TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL
);
CREATE INDEX IF NOT EXISTS type_templates_index ON localorg_dev.type_templates(type_name);
CREATE INDEX IF NOT EXISTS type_templates_created_at_index ON localorg_dev.type_templates(created_at);
CREATE INDEX IF NOT EXISTS type_templates_last_modifed_index ON localorg_dev.type_templates(last_modified);
INSERT INTO localorg_dev.type_templates(type_name, type_schema, created_by, created_at, last_modified)
VALUES (
        'Number',
        '{"type": "integer"}',
        'user@superposition.io',
        NOW(),
        NOW()
    ),
    (
        'Decimal',
        '{"type": "number"}',
        'user@superposition.io',
        NOW(),
        NOW()
    ),
    (
        'Boolean',
        '{"type": "boolean"}',
        'user@superposition.io',
        NOW(),
        NOW()
    ),
    (
        'Enum',
        '{"type": "string", "enum": ["android", "ios"]}',
        'user@superposition.io',
        NOW(),
        NOW()
    ),
    (
        'Pattern',
        '{"type": "string", "pattern": ".*"}',
        'user@superposition.io',
        NOW(),
        NOW()
    );
-- Your SQL goes here

ALTER TABLE localorg_dev.functions
add column last_modified_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
add column last_modified_by varchar(200) not null default('null');

ALTER TABLE localorg_dev.dimensions
add column last_modified_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
add column last_modified_by varchar(200) not null default('null');

ALTER TABLE localorg_dev.contexts
add column last_modified_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
add column last_modified_by varchar(200) not null default('null');

ALTER TABLE localorg_dev.default_configs
add column last_modified_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
add column last_modified_by varchar(200) not null default('null');

ALTER TABLE localorg_dev.type_templates
rename column last_modified to last_modified_at;

ALTER TABLE localorg_dev.type_templates
add column last_modified_by varchar(200) not null default('null');
-- Your SQL goes here
ALTER TABLE localorg_dev.dimensions
add column position integer DEFAULT 0 NOT NULL;
ALTER TABLE localorg_dev.contexts
add column weight numeric(1000, 0) DEFAULT 1 NOT NULL;
CREATE INDEX IF NOT EXISTS idx_contexts_weight ON localorg_dev.contexts(weight);
-- Your SQL goes here
ALTER TABLE localorg_dev.dimensions
ALTER COLUMN priority SET DEFAULT 1;

ALTER TABLE localorg_dev.dimensions
ADD CONSTRAINT dimension_unique_position UNIQUE (position);

ALTER TABLE localorg_dev.contexts ADD COLUMN IF NOT EXISTS description TEXT DEFAULT '' NOT NULL;
ALTER TABLE localorg_dev.contexts ADD COLUMN IF NOT EXISTS change_reason TEXT DEFAULT '' NOT NULL;

ALTER TABLE localorg_dev.dimensions ADD COLUMN IF NOT EXISTS description TEXT DEFAULT '' NOT NULL;
ALTER TABLE localorg_dev.dimensions ADD COLUMN IF NOT EXISTS change_reason TEXT DEFAULT '' NOT NULL;

ALTER TABLE localorg_dev.default_configs ADD COLUMN IF NOT EXISTS description TEXT DEFAULT '' NOT NULL;
ALTER TABLE localorg_dev.default_configs ADD COLUMN IF NOT EXISTS change_reason TEXT DEFAULT '' NOT NULL;

ALTER TABLE localorg_dev.type_templates ADD COLUMN IF NOT EXISTS description TEXT DEFAULT '' NOT NULL;
ALTER TABLE localorg_dev.type_templates ADD COLUMN IF NOT EXISTS change_reason TEXT DEFAULT '' NOT NULL;

ALTER TABLE localorg_dev.functions RENAME COLUMN function_description TO description;
ALTER TABLE localorg_dev.functions ADD COLUMN IF NOT EXISTS change_reason TEXT DEFAULT '' NOT NULL;

ALTER TABLE localorg_dev.functions ALTER COLUMN description SET DEFAULT '';
ALTER TABLE localorg_dev.functions ALTER COLUMN description SET NOT NULL;

ALTER TABLE localorg_dev.config_versions ADD COLUMN IF NOT EXISTS description TEXT DEFAULT '' NOT NULL;

ALTER TABLE localorg_dev.experiments ADD COLUMN IF NOT EXISTS description TEXT DEFAULT '' NOT NULL;
ALTER TABLE localorg_dev.experiments ADD COLUMN IF NOT EXISTS change_reason TEXT DEFAULT '' NOT NULL;

INSERT INTO localorg_dev.dimensions (
        dimension,
        priority,
        created_at,
        created_by,
        schema,
        function_name,
        description
    )
VALUES (
        'variantIds',
        0,
        CURRENT_TIMESTAMP,
        'default@superposition.io',
        '{"type": "string","pattern": ".*"}'::json,
        null,
        'variantIds are used by experimentation module to manage and select variations'
);

-- Create older partitions
-- 2023-2024
CREATE TABLE IF NOT EXISTS localorg_dev.event_log_y2023m01 PARTITION OF localorg_dev.event_log FOR
VALUES
FROM ('2023-01-01') TO ('2023-02-01');

CREATE TABLE IF NOT EXISTS localorg_dev.event_log_y2023m02 PARTITION OF localorg_dev.event_log FOR
VALUES
FROM ('2023-02-01') TO ('2023-03-01');

CREATE TABLE IF NOT EXISTS localorg_dev.event_log_y2023m03 PARTITION OF localorg_dev.event_log FOR
VALUES
FROM ('2023-03-01') TO ('2023-04-01');

CREATE TABLE IF NOT EXISTS localorg_dev.event_log_y2023m04 PARTITION OF localorg_dev.event_log FOR
VALUES
FROM ('2023-04-01') TO ('2023-05-01');

CREATE TABLE IF NOT EXISTS localorg_dev.event_log_y2023m05 PARTITION OF localorg_dev.event_log FOR
VALUES
FROM ('2023-05-01') TO ('2023-06-01');

CREATE TABLE IF NOT EXISTS localorg_dev.event_log_y2023m06 PARTITION OF localorg_dev.event_log FOR
VALUES
FROM ('2023-06-01') TO ('2023-07-01');

CREATE TABLE IF NOT EXISTS localorg_dev.event_log_y2023m07 PARTITION OF localorg_dev.event_log FOR
VALUES
FROM ('2023-07-01') TO ('2023-08-01');

CREATE TABLE IF NOT EXISTS localorg_dev.event_log_y2023m08 PARTITION OF localorg_dev.event_log FOR
VALUES
FROM ('2023-08-01') TO ('2023-09-01');

CREATE TABLE IF NOT EXISTS localorg_dev.event_log_y2023m09 PARTITION OF localorg_dev.event_log FOR
VALUES
FROM ('2023-09-01') TO ('2023-10-01');

CREATE TABLE IF NOT EXISTS localorg_dev.event_log_y2023m10 PARTITION OF localorg_dev.event_log FOR
VALUES
FROM ('2023-10-01') TO ('2023-11-01');

CREATE TABLE IF NOT EXISTS localorg_dev.event_log_y2023m11 PARTITION OF localorg_dev.event_log FOR
VALUES
FROM ('2023-11-01') TO ('2023-12-01');

CREATE TABLE IF NOT EXISTS localorg_dev.event_log_y2023m12 PARTITION OF localorg_dev.event_log FOR
VALUES
FROM ('2023-12-01') TO ('2024-01-01');

-- 2024-2025
CREATE TABLE IF NOT EXISTS localorg_dev.event_log_y2024m01 PARTITION OF localorg_dev.event_log FOR
VALUES
FROM ('2024-01-01') TO ('2024-02-01');

CREATE TABLE IF NOT EXISTS localorg_dev.event_log_y2024m02 PARTITION OF localorg_dev.event_log FOR
VALUES
FROM ('2024-02-01') TO ('2024-03-01');

CREATE TABLE IF NOT EXISTS localorg_dev.event_log_y2024m03 PARTITION OF localorg_dev.event_log FOR
VALUES
FROM ('2024-03-01') TO ('2024-04-01');

CREATE TABLE IF NOT EXISTS localorg_dev.event_log_y2024m04 PARTITION OF localorg_dev.event_log FOR
VALUES
FROM ('2024-04-01') TO ('2024-05-01');

CREATE TABLE IF NOT EXISTS localorg_dev.event_log_y2024m05 PARTITION OF localorg_dev.event_log FOR
VALUES
FROM ('2024-05-01') TO ('2024-06-01');

CREATE TABLE IF NOT EXISTS localorg_dev.event_log_y2024m06 PARTITION OF localorg_dev.event_log FOR
VALUES
FROM ('2024-06-01') TO ('2024-07-01');

CREATE TABLE IF NOT EXISTS localorg_dev.event_log_y2024m07 PARTITION OF localorg_dev.event_log FOR
VALUES
FROM ('2024-07-01') TO ('2024-08-01');

CREATE TABLE IF NOT EXISTS localorg_dev.event_log_y2024m08 PARTITION OF localorg_dev.event_log FOR
VALUES
FROM ('2024-08-01') TO ('2024-09-01');

CREATE TABLE IF NOT EXISTS localorg_dev.event_log_y2024m09 PARTITION OF localorg_dev.event_log FOR
VALUES
FROM ('2024-09-01') TO ('2024-10-01');

CREATE TABLE IF NOT EXISTS localorg_dev.event_log_y2024m10 PARTITION OF localorg_dev.event_log FOR
VALUES
FROM ('2024-10-01') TO ('2024-11-01');

CREATE TABLE IF NOT EXISTS localorg_dev.event_log_y2024m11 PARTITION OF localorg_dev.event_log FOR
VALUES
FROM ('2024-11-01') TO ('2024-12-01');

CREATE TABLE IF NOT EXISTS localorg_dev.event_log_y2024m12 PARTITION OF localorg_dev.event_log FOR
VALUES
FROM ('2024-12-01') TO ('2025-01-01');

-- Insert entry into workspaces table
INSERT INTO superposition.workspaces (
    organisation_id,
    organisation_name,
    workspace_name,
    workspace_schema_name,
    workspace_status,
    workspace_admin_email,
    created_by,
    last_modified_by,
    mandatory_dimensions
) VALUES (
    'localorg',
    'localorg',
    'dev',
    'localorg_dev',
    'ENABLED',
    'admin@localorg.io',
    'admin@localorg.io',
    'admin@localorg.io',
    null
);

CREATE TYPE public.function_types AS ENUM (
'VALIDATION',
'AUTOCOMPLETE'
);

ALTER TABLE localorg_dev.functions ADD COLUMN function_type public.FUNCTION_TYPES NOT NULL DEFAULT 'VALIDATION';


COMMIT

ALTER TABLE localorg_test.dimensions 
ADD COLUMN dependency_graph JSON default '{}'::json NOT NULL,
ADD COLUMN dependents TEXT[] default '{}' NOT NULL,
ADD COLUMN immediate_childrens TEXT[] default '{}' NOT NULL;

ALTER TABLE localorg_dev.dimensions 
ADD COLUMN dependency_graph JSON default '{}'::json NOT NULL,
ADD COLUMN dependents TEXT[] default '{}' NOT NULL,
ADD COLUMN immediate_childrens TEXT[] default '{}' NOT NULL;