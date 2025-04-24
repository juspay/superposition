-- Your SQL goes here
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
--
-- Name: ayushjain; Type: SCHEMA; Schema: -; Owner: -
--
CREATE SCHEMA IF NOT EXISTS ayushjain;
--
-- Name: dimension_type; Type: TYPE; Schema: ayushjain; Owner: -
--
CREATE TYPE ayushjain.dimension_type AS ENUM (
    'NULL',
    'BOOL',
    'NUMBER',
    'STRING',
    'ARRAY',
    'OBJECT'
);
--
-- Name: event_logger(); Type: FUNCTION; Schema: ayushjain; Owner: -
--
CREATE OR REPLACE FUNCTION ayushjain.event_logger() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
DECLARE
    old_data json;
    new_data json;
BEGIN
    IF (TG_OP = 'UPDATE') THEN
        old_data := row_to_json(OLD);
        new_data := row_to_json(NEW);
        INSERT INTO ayushjain.event_log
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
        INSERT INTO ayushjain.event_log
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
        INSERT INTO ayushjain.event_log
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
-- Name: contexts; Type: TABLE; Schema: ayushjain; Owner: -
--
CREATE TABLE ayushjain.contexts (
    id character varying PRIMARY KEY,
    value json NOT NULL,
    override_id character varying NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_by character varying NOT NULL,
    priority integer DEFAULT 1 NOT NULL,
    override json DEFAULT '{}'::json NOT NULL
);
--
-- Name: default_configs; Type: TABLE; Schema: ayushjain; Owner: -
--
CREATE TABLE ayushjain.default_configs (
    key character varying PRIMARY KEY,
    value json NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_by character varying NOT NULL,
    schema json DEFAULT '{}'::json NOT NULL
);
--
-- Name: dimensions; Type: TABLE; Schema: ayushjain; Owner: -
--
CREATE TABLE ayushjain.dimensions (
    dimension character varying PRIMARY KEY,
    priority integer NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_by character varying NOT NULL,
    schema json DEFAULT '{}'::json NOT NULL
);
--
-- Name: event_log; Type: TABLE; Schema: ayushjain; Owner: -
--
CREATE TABLE IF NOT EXISTS ayushjain.event_log (
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
-- Name: event_log_action_index; Type: INDEX; Schema: ayushjain; Owner: -
--
CREATE INDEX IF NOT EXISTS event_log_action_index ON ONLY ayushjain.event_log USING btree (action) INCLUDE ("timestamp", table_name);
--
-- Name: event_log_table_name_index; Type: INDEX; Schema: ayushjain; Owner: -
--
CREATE INDEX IF NOT EXISTS event_log_table_name_index ON ONLY ayushjain.event_log USING btree (table_name) INCLUDE (action, "timestamp");
--
-- Name: event_log_timestamp_index; Type: INDEX; Schema: ayushjain; Owner: -
--
CREATE INDEX IF NOT EXISTS event_log_timestamp_index ON ONLY ayushjain.event_log USING btree ("timestamp") INCLUDE (action, table_name);

--
-- Event log parititons
--
CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2023m08 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2023-08-01') TO ('2023-09-01');

CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2023m09 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2023-09-01') TO ('2023-10-01');

CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2023m10 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2023-10-01') TO ('2023-11-01');

CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2023m11 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2023-11-01') TO ('2023-12-01');

CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2023m12 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2023-12-01') TO ('2024-01-01');

CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2024m01 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2024-01-01') TO ('2024-02-01');

CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2024m02 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2024-02-01') TO ('2024-03-01');

CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2024m03 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2024-03-01') TO ('2024-04-01');

CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2024m04 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2024-04-01') TO ('2024-05-01');

CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2024m05 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2024-05-01') TO ('2024-06-01');

CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2024m06 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2024-06-01') TO ('2024-07-01');

CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2024m07 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2024-07-01') TO ('2024-08-01');

--
-- Name: contexts contexts_audit; Type: TRIGGER; Schema: ayushjain; Owner: -
--
CREATE TRIGGER contexts_audit AFTER INSERT OR DELETE OR UPDATE ON ayushjain.contexts FOR EACH ROW EXECUTE FUNCTION ayushjain.event_logger();
--
-- Name: default_configs default_configs_audit; Type: TRIGGER; Schema: ayushjain; Owner: -
--
CREATE TRIGGER default_configs_audit AFTER INSERT OR DELETE OR UPDATE ON ayushjain.default_configs FOR EACH ROW EXECUTE FUNCTION ayushjain.event_logger();
--
-- Name: dimensions dimensions_audit; Type: TRIGGER; Schema: ayushjain; Owner: -
--
CREATE TRIGGER dimensions_audit AFTER INSERT OR DELETE OR UPDATE ON ayushjain.dimensions FOR EACH ROW EXECUTE FUNCTION ayushjain.event_logger();
-- Your SQL goes here
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
--
-- Name: ayushjain; Type: SCHEMA; Schema: -; Owner: -
--
CREATE SCHEMA IF NOT EXISTS ayushjain;
--
-- Name: experiment_status_type; Type: TYPE; Schema: ayushjain; Owner: -
--
CREATE TYPE ayushjain.experiment_status_type AS ENUM (
    'CREATED',
    'CONCLUDED',
    'INPROGRESS'
);
--
-- Name: not_null_text; Type: DOMAIN; Schema: ayushjain; Owner: -
--
CREATE DOMAIN ayushjain.not_null_text AS text NOT NULL;
--
-- Name: event_logger(); Type: FUNCTION; Schema: ayushjain; Owner: -
--
CREATE OR REPLACE FUNCTION ayushjain.event_logger() RETURNS trigger
LANGUAGE plpgsql AS $$
DECLARE old_data json;
new_data json;
BEGIN IF (TG_OP = 'UPDATE') THEN old_data := row_to_json(OLD);
new_data := row_to_json(NEW);
INSERT INTO ayushjain.event_log (
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
INSERT INTO ayushjain.event_log (
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
INSERT INTO ayushjain.event_log (table_name, user_name, action, new_data, query)
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
-- Name: experiments; Type: TABLE; Schema: ayushjain; Owner: -
--
CREATE TABLE ayushjain.experiments (
    id bigint PRIMARY KEY,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_by text NOT NULL,
    last_modified timestamp with time zone DEFAULT now() NOT NULL,
    name text NOT NULL,
    override_keys ayushjain.not_null_text [] NOT NULL,
    status ayushjain.experiment_status_type NOT NULL,
    traffic_percentage integer NOT NULL,
    context json NOT NULL,
    variants json NOT NULL,
    last_modified_by text DEFAULT 'Null'::text NOT NULL,
    chosen_variant text,
    CONSTRAINT experiments_traffic_percentage_check CHECK ((traffic_percentage >= 0))
);
--
-- Name: experiment_created_date_index; Type: INDEX; Schema: ayushjain; Owner: -
--
CREATE INDEX experiment_created_date_index ON ayushjain.experiments USING btree (created_at) INCLUDE (id);
--
-- Name: experiment_last_modified_index; Type: INDEX; Schema: ayushjain; Owner: -
--
CREATE INDEX experiment_last_modified_index ON ayushjain.experiments USING btree (last_modified) INCLUDE (id, created_at);
--
-- Name: experiment_status_index; Type: INDEX; Schema: ayushjain; Owner: -
--
CREATE INDEX experiment_status_index ON ayushjain.experiments USING btree (status) INCLUDE (created_at, last_modified);
--
-- Name: experiments experiments_audit; Type: TRIGGER; Schema: ayushjain; Owner: -
--
CREATE TRIGGER experiments_audit AFTER INSERT OR DELETE OR UPDATE ON ayushjain.experiments FOR EACH ROW EXECUTE FUNCTION ayushjain.event_logger();
-- Your SQL goes here
CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2024m08 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2024-08-01') TO ('2024-09-01');

CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2024m09 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2024-09-01') TO ('2024-10-01');

CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2024m10 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2024-10-01') TO ('2024-11-01');

CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2024m11 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2024-11-01') TO ('2024-12-01');

CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2024m12 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2024-12-01') TO ('2025-01-01');

------------ Parititions for 2025 -----------
CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2025m01 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2025-01-01') TO ('2025-02-01');

CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2025m02 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2025-02-01') TO ('2025-03-01');

CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2025m03 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2025-03-01') TO ('2025-04-01');

CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2025m04 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2025-04-01') TO ('2025-05-01');

CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2025m05 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2025-05-01') TO ('2025-06-01');

CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2025m06 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2025-06-01') TO ('2025-07-01');

CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2025m07 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2025-07-01') TO ('2025-08-01');

CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2025m08 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2025-08-01') TO ('2025-09-01');

CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2025m09 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2025-09-01') TO ('2025-10-01');

CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2025m10 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2025-10-01') TO ('2025-11-01');

CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2025m11 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2025-11-01') TO ('2025-12-01');

CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2025m12 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2025-12-01') TO ('2026-01-01');

------------ Parititions for 2026 -----------
CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2026m01 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2026-01-01') TO ('2026-02-01');

CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2026m02 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2026-02-01') TO ('2026-03-01');

CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2026m03 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2026-03-01') TO ('2026-04-01');

CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2026m04 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2026-04-01') TO ('2026-05-01');

CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2026m05 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2026-05-01') TO ('2026-06-01');

CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2026m06 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2026-06-01') TO ('2026-07-01');

CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2026m07 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2026-07-01') TO ('2026-08-01');

CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2026m08 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2026-08-01') TO ('2026-09-01');

CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2026m09 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2026-09-01') TO ('2026-10-01');

CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2026m10 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2026-10-01') TO ('2026-11-01');

CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2026m11 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2026-11-01') TO ('2026-12-01');

CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2026m12 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2026-12-01') TO ('2027-01-01');
-- Your SQL goes here
CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2024m08 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2024-08-01') TO ('2024-09-01');

CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2024m09 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2024-09-01') TO ('2024-10-01');

CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2024m10 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2024-10-01') TO ('2024-11-01');

CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2024m11 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2024-11-01') TO ('2024-12-01');

CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2024m12 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2024-12-01') TO ('2025-01-01');

------------ Parititions for 2025 -----------
CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2025m01 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2025-01-01') TO ('2025-02-01');

CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2025m02 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2025-02-01') TO ('2025-03-01');

CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2025m03 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2025-03-01') TO ('2025-04-01');

CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2025m04 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2025-04-01') TO ('2025-05-01');

CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2025m05 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2025-05-01') TO ('2025-06-01');

CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2025m06 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2025-06-01') TO ('2025-07-01');

CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2025m07 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2025-07-01') TO ('2025-08-01');

CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2025m08 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2025-08-01') TO ('2025-09-01');

CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2025m09 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2025-09-01') TO ('2025-10-01');

CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2025m10 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2025-10-01') TO ('2025-11-01');

CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2025m11 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2025-11-01') TO ('2025-12-01');

CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2025m12 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2025-12-01') TO ('2026-01-01');

------------ Parititions for 2026 -----------
CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2026m01 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2026-01-01') TO ('2026-02-01');

CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2026m02 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2026-02-01') TO ('2026-03-01');

CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2026m03 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2026-03-01') TO ('2026-04-01');

CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2026m04 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2026-04-01') TO ('2026-05-01');

CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2026m05 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2026-05-01') TO ('2026-06-01');

CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2026m06 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2026-06-01') TO ('2026-07-01');

CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2026m07 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2026-07-01') TO ('2026-08-01');

CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2026m08 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2026-08-01') TO ('2026-09-01');

CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2026m09 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2026-09-01') TO ('2026-10-01');

CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2026m10 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2026-10-01') TO ('2026-11-01');

CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2026m11 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2026-11-01') TO ('2026-12-01');

CREATE TABLE IF NOT EXISTS ayushjain.event_log_y2026m12 PARTITION OF ayushjain.event_log FOR
VALUES
FROM ('2026-12-01') TO ('2027-01-01');
-- Your SQL goes here
-- Name: functions; Type: TABLE; Schema: ayushjain; Owner: -
--
CREATE TABLE ayushjain.functions (
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
-- Name: functions functions_audit; Type: TRIGGER; Schema: ayushjain; Owner: -
--
CREATE TRIGGER functions_audit AFTER INSERT OR DELETE OR UPDATE ON ayushjain.functions FOR EACH ROW EXECUTE FUNCTION ayushjain.event_logger();
-- Your SQL goes here
ALTER TABLE ayushjain.dimensions ADD COLUMN function_name text NULL;

ALTER TABLE ayushjain.dimensions ADD FOREIGN KEY(function_name) REFERENCES ayushjain.functions(function_name);

ALTER TABLE ayushjain.default_configs ADD COLUMN function_name text NULL;

ALTER TABLE ayushjain.default_configs ADD FOREIGN KEY(function_name) REFERENCES ayushjain.functions(function_name);
-- Your SQL goes here
-- Name: functions; Type: TABLE; Schema: ayushjain; Owner: -
--
CREATE TABLE ayushjain.config_versions (
id bigint PRIMARY KEY,
config json NOT NULL,
config_hash TEXT NOT NULL,
tags varchar(100) [] check (array_position(tags, null) is null),
created_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);
CREATE INDEX IF NOT EXISTS config_verions_tags_index ON ayushjain.config_versions USING gin(tags);
CREATE INDEX IF NOT EXISTS config_versions_id_index ON ayushjain.config_versions(id);
-- Your SQL goes here
CREATE TABLE IF NOT EXISTS ayushjain.type_templates (
    type_name TEXT PRIMARY KEY,
    type_schema JSON NOT NULL,
    created_by TEXT NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    last_modified TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL
);
CREATE INDEX IF NOT EXISTS type_templates_index ON ayushjain.type_templates(type_name);
CREATE INDEX IF NOT EXISTS type_templates_created_at_index ON ayushjain.type_templates(created_at);
CREATE INDEX IF NOT EXISTS type_templates_last_modifed_index ON ayushjain.type_templates(last_modified);
INSERT INTO ayushjain.type_templates(type_name, type_schema, created_by, created_at)
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
-- Your SQL goes here

ALTER TABLE ayushjain.functions
add column last_modified_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
add column last_modified_by varchar(200) not null default('null');

ALTER TABLE ayushjain.dimensions
add column last_modified_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
add column last_modified_by varchar(200) not null default('null');

ALTER TABLE ayushjain.contexts
add column last_modified_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
add column last_modified_by varchar(200) not null default('null');

ALTER TABLE ayushjain.default_configs
add column last_modified_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
add column last_modified_by varchar(200) not null default('null');

ALTER TABLE ayushjain.type_templates
rename column last_modified to last_modified_at;

ALTER TABLE ayushjain.type_templates
add column last_modified_by varchar(200) not null default('null');
-- Your SQL goes here
ALTER TABLE ayushjain.dimensions
add column position integer DEFAULT 0 NOT NULL;
ALTER TABLE ayushjain.contexts
add column weight numeric(1000, 0) DEFAULT 1 NOT NULL;
CREATE INDEX IF NOT EXISTS idx_contexts_weight ON ayushjain.contexts(weight);
-- Your SQL goes here
ALTER TABLE ayushjain.dimensions
ALTER COLUMN priority SET DEFAULT 1;

ALTER TABLE ayushjain.dimensions
ADD CONSTRAINT dimension_unique_position UNIQUE (position) DEFERRABLE INITIALLY IMMEDIATE;

SET CONSTRAINTS dimension_unique_position DEFERRED;
-- up.sql
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
    created_at TIMESTAMP WITHOUT TIME ZONE DEFAULT NOW() NOT NULL,
    updated_at TIMESTAMP WITHOUT TIME ZONE DEFAULT NOW() NOT NULL,
    updated_by TEXT NOT NULL
);

-- Indexes for optimizing queries
CREATE INDEX IF NOT EXISTS idx_organisation_contact_email ON superposition.organisations (contact_email);
CREATE INDEX IF NOT EXISTS idx_organisation_status ON superposition.organisations (status);
CREATE INDEX IF NOT EXISTS idx_organisation_created_at ON superposition.organisations (created_at);
CREATE INDEX IF NOT EXISTS idx_organisation_admin_email ON superposition.organisations (admin_email);
CREATE SCHEMA IF NOT EXISTS superposition;
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
-- Your SQL goes here
ALTER TABLE ayushjain.contexts ADD COLUMN IF NOT EXISTS description TEXT DEFAULT '' NOT NULL;
ALTER TABLE ayushjain.contexts ADD COLUMN IF NOT EXISTS change_reason TEXT DEFAULT '' NOT NULL;

ALTER TABLE ayushjain.dimensions ADD COLUMN IF NOT EXISTS description TEXT DEFAULT '' NOT NULL;
ALTER TABLE ayushjain.dimensions ADD COLUMN IF NOT EXISTS change_reason TEXT DEFAULT '' NOT NULL;

ALTER TABLE ayushjain.default_configs ADD COLUMN IF NOT EXISTS description TEXT DEFAULT '' NOT NULL;
ALTER TABLE ayushjain.default_configs ADD COLUMN IF NOT EXISTS change_reason TEXT DEFAULT '' NOT NULL;

ALTER TABLE ayushjain.type_templates ADD COLUMN IF NOT EXISTS description TEXT DEFAULT '' NOT NULL;
ALTER TABLE ayushjain.type_templates ADD COLUMN IF NOT EXISTS change_reason TEXT DEFAULT '' NOT NULL;

ALTER TABLE ayushjain.functions RENAME COLUMN function_description TO description;
ALTER TABLE ayushjain.functions ADD COLUMN IF NOT EXISTS change_reason TEXT DEFAULT '' NOT NULL;

ALTER TABLE ayushjain.functions ALTER COLUMN description SET DEFAULT '';
ALTER TABLE ayushjain.functions ALTER COLUMN description SET NOT NULL; 

ALTER TABLE ayushjain.config_versions ADD COLUMN IF NOT EXISTS description TEXT DEFAULT '' NOT NULL;
ALTER TABLE ayushjain.config_versions ADD COLUMN IF NOT EXISTS change_reason TEXT DEFAULT '' NOT NULL;

ALTER TABLE ayushjain.experiments ADD COLUMN IF NOT EXISTS description TEXT DEFAULT '' NOT NULL;
ALTER TABLE ayushjain.experiments ADD COLUMN IF NOT EXISTS change_reason TEXT DEFAULT '' NOT NULL;
-- Your SQL goes here
ALTER TYPE ayushjain.experiment_status_type ADD VALUE 'DISCARDED';
-- Your SQL goes here
ALTER TABLE ayushjain.config_versions
DROP COLUMN description;
