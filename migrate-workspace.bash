org_id="juspay"
org_name="juspay"
ws_name=$1
tenant_cac_schema="${ws_name}_cac";
tenant_exp_schema="${ws_name}_exp";
ws_schema="${org_id}_${ws_name}"
ws_setup_sql=$(sed -E "s/\{replaceme\}/${ws_schema}/g" < workspace_template.sql)
admin_email="admin@juspay.in"

echo "
-- Setup workspace schema
${ws_setup_sql}

-- Create older partitions
-- 2023-2024
CREATE TABLE IF NOT EXISTS ${ws_schema}.event_log_y2023m01 PARTITION OF ${ws_schema}.event_log FOR
VALUES
FROM ('2023-01-01') TO ('2023-02-01');

CREATE TABLE IF NOT EXISTS ${ws_schema}.event_log_y2023m02 PARTITION OF ${ws_schema}.event_log FOR
VALUES
FROM ('2023-02-01') TO ('2023-03-01');

CREATE TABLE IF NOT EXISTS ${ws_schema}.event_log_y2023m03 PARTITION OF ${ws_schema}.event_log FOR
VALUES
FROM ('2023-03-01') TO ('2023-04-01');

CREATE TABLE IF NOT EXISTS ${ws_schema}.event_log_y2023m04 PARTITION OF ${ws_schema}.event_log FOR
VALUES
FROM ('2023-04-01') TO ('2023-05-01');

CREATE TABLE IF NOT EXISTS ${ws_schema}.event_log_y2023m05 PARTITION OF ${ws_schema}.event_log FOR
VALUES
FROM ('2023-05-01') TO ('2023-06-01');

CREATE TABLE IF NOT EXISTS ${ws_schema}.event_log_y2023m06 PARTITION OF ${ws_schema}.event_log FOR
VALUES
FROM ('2023-06-01') TO ('2023-07-01');

CREATE TABLE IF NOT EXISTS ${ws_schema}.event_log_y2023m07 PARTITION OF ${ws_schema}.event_log FOR
VALUES
FROM ('2023-07-01') TO ('2023-08-01');

CREATE TABLE IF NOT EXISTS ${ws_schema}.event_log_y2023m08 PARTITION OF ${ws_schema}.event_log FOR
VALUES
FROM ('2023-08-01') TO ('2023-09-01');

CREATE TABLE IF NOT EXISTS ${ws_schema}.event_log_y2023m09 PARTITION OF ${ws_schema}.event_log FOR
VALUES
FROM ('2023-09-01') TO ('2023-10-01');

CREATE TABLE IF NOT EXISTS ${ws_schema}.event_log_y2023m10 PARTITION OF ${ws_schema}.event_log FOR
VALUES
FROM ('2023-10-01') TO ('2023-11-01');

CREATE TABLE IF NOT EXISTS ${ws_schema}.event_log_y2023m11 PARTITION OF ${ws_schema}.event_log FOR
VALUES
FROM ('2023-11-01') TO ('2023-12-01');

CREATE TABLE IF NOT EXISTS ${ws_schema}.event_log_y2023m12 PARTITION OF ${ws_schema}.event_log FOR
VALUES
FROM ('2023-12-01') TO ('2024-01-01');

-- 2024-2025
CREATE TABLE IF NOT EXISTS ${ws_schema}.event_log_y2024m01 PARTITION OF ${ws_schema}.event_log FOR
VALUES
FROM ('2024-01-01') TO ('2024-02-01');

CREATE TABLE IF NOT EXISTS ${ws_schema}.event_log_y2024m02 PARTITION OF ${ws_schema}.event_log FOR
VALUES
FROM ('2024-02-01') TO ('2024-03-01');

CREATE TABLE IF NOT EXISTS ${ws_schema}.event_log_y2024m03 PARTITION OF ${ws_schema}.event_log FOR
VALUES
FROM ('2024-03-01') TO ('2024-04-01');

CREATE TABLE IF NOT EXISTS ${ws_schema}.event_log_y2024m04 PARTITION OF ${ws_schema}.event_log FOR
VALUES
FROM ('2024-04-01') TO ('2024-05-01');

CREATE TABLE IF NOT EXISTS ${ws_schema}.event_log_y2024m05 PARTITION OF ${ws_schema}.event_log FOR
VALUES
FROM ('2024-05-01') TO ('2024-06-01');

CREATE TABLE IF NOT EXISTS ${ws_schema}.event_log_y2024m06 PARTITION OF ${ws_schema}.event_log FOR
VALUES
FROM ('2024-06-01') TO ('2024-07-01');

CREATE TABLE IF NOT EXISTS ${ws_schema}.event_log_y2024m07 PARTITION OF ${ws_schema}.event_log FOR
VALUES
FROM ('2024-07-01') TO ('2024-08-01');

CREATE TABLE IF NOT EXISTS ${ws_schema}.event_log_y2024m08 PARTITION OF ${ws_schema}.event_log FOR
VALUES
FROM ('2024-08-01') TO ('2024-09-01');

CREATE TABLE IF NOT EXISTS ${ws_schema}.event_log_y2024m09 PARTITION OF ${ws_schema}.event_log FOR
VALUES
FROM ('2024-09-01') TO ('2024-10-01');

CREATE TABLE IF NOT EXISTS ${ws_schema}.event_log_y2024m10 PARTITION OF ${ws_schema}.event_log FOR
VALUES
FROM ('2024-10-01') TO ('2024-11-01');

CREATE TABLE IF NOT EXISTS ${ws_schema}.event_log_y2024m11 PARTITION OF ${ws_schema}.event_log FOR
VALUES
FROM ('2024-11-01') TO ('2024-12-01');

CREATE TABLE IF NOT EXISTS ${ws_schema}.event_log_y2024m12 PARTITION OF ${ws_schema}.event_log FOR
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
    '${org_id}',
    '${org_name}',
    '${ws_name}',
    '${ws_schema}',
    superposition.workspace_status.ENABLED,
    '${admin_email}',
    '${admin_email}',
    '${admin_email}',
    null
);

-- Migrate contexts
INSERT INTO ${ws_schema}.contexts
SELECT * FROM ${tenant_cac_schema}.contexts;

-- Migrate dimensions
INSERT INTO ${ws_schema}.dimensions
SELECT * FROM ${tenant_cac_schema}.dimensions;

-- Migrate functions
INSERT INTO ${ws_schema}.functions
SELECT * FROM ${tenant_cac_schema}.functions;

-- Migrate config-versions
INSERT INTO ${ws_schema}.config_versions
SELECT * FROM ${tenant_cac_schema}.config_versions;

-- Migrate type-templates
INSERT INTO ${ws_schema}.type_templates
SELECT * FROM ${tenant_cac_schema}.type_templates;

-- Migrate experiments
INSERT INTO ${ws_schema}.experiments
SELECT
    *,
    status::public.experiment_status_type
FROM ${tenant_exp_schema}.experiments;

-- Migrate event log
INSERT INTO ${ws_schema}.event_log
SELECT * FROM ${tenant_cac_schema}.event_log;
INSERT INTO ${ws_schema}.event_log
SELECT * FROM ${tenant_exp_schema}.event_log;
"
