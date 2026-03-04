DO $$
BEGIN
    BEGIN
        CREATE TABLE casbin_rule (
            id SERIAL PRIMARY KEY,
            ptype VARCHAR NOT NULL,
            v0 VARCHAR NOT NULL,
            v1 VARCHAR NOT NULL,
            v2 VARCHAR NOT NULL,
            v3 VARCHAR NOT NULL,
            v4 VARCHAR NOT NULL,
            v5 VARCHAR NOT NULL,
            CONSTRAINT unique_key_diesel_adapter UNIQUE(ptype, v0, v1, v2, v3, v4, v5)
        );

        -- Insert this only on first-time table creation.
        INSERT INTO casbin_rule (ptype, v0, v1, v2, v3, v4, v5)
        VALUES ('g', 'user@superposition.io', 'admin', '*', '', '', '')
        ON CONFLICT (ptype, v0, v1, v2, v3, v4, v5) DO NOTHING;
    EXCEPTION
        WHEN duplicate_table THEN
            -- Table already exists; do not insert the root admin mapping.
            NULL;
    END;
END
$$;

-- Seed rules (idempotent). Note: this schema stores up to 6 values (v0..v5);
-- rules with fewer fields use empty-string placeholders.

INSERT INTO casbin_rule (ptype, v0, v1, v2, v3, v4, v5)
VALUES
    -- Global/admin and generic read policies
    ('p', 'admin', '*', '*', '*', '*', ''), -- admin can do anything
    ('p', '*', '*', 'workspace', 'read', '*', ''), -- anyone can read workspaces (for listing)
    ('p', 'reader', '*', '*', 'read', '*', ''), -- generic read access for readers

    -- context
    ('g2', 'context:get', '*', 'read', '', '', ''),
    ('g2', 'context:get_from_context', '*', 'read', '', '', ''), 
    ('g2', 'context:list', '*', 'read', '', '', ''),
    ('p', 'context_reader', '*', 'context', 'read', '*', ''),

    -- dimension
    ('g2', 'dimension:get', '*', 'read', '', '', ''),
    ('g2', 'dimension:list', '*', 'read', '', '', ''),
    ('p', 'dimension_reader', '*', 'dimension', 'read', '*', ''),

    -- default_config
    ('g2', 'default_config:get', '*', 'read', '', '', ''),
    ('g2', 'default_config:list', '*', 'read', '', '', ''),
    ('p', 'default_config_reader', '*', 'default_config', 'read', '*', ''),

    -- config
    ('g2', 'config:get', '*', 'read', '', '', ''),
    ('g2', 'config:resolve', '*', 'read', '', '', ''),
    ('g2', 'config:list_version', '*', 'read', '', '', ''),
    ('g2', 'config:get_version', '*', 'read', '', '', ''),
    ('p', 'config_reader', '*', 'config', 'read', '*', ''),

    -- audit_log
    ('g2', 'audit_log:list', '*', 'read', '', '', ''),
    ('p', 'audit_log_reader', '*', 'audit_log', 'read', '*', ''),

    -- function
    ('g2', 'function:get', '*', 'read', '', '', ''),
    ('g2', 'function:list', '*', 'read', '', '', ''),
    ('p', 'function_reader', '*', 'function', 'read', '*', ''),

    -- type_template
    ('g2', 'type_template:get', '*', 'read', '', '', ''),
    ('g2', 'type_template:list', '*', 'read', '', '', ''),
    ('p', 'type_template_reader', '*', 'type_template', 'read', '*', ''),

    -- experiment
    ('g2', 'experiment:get', '*', 'read', '', '', ''),
    ('g2', 'experiment:list', '*', 'read', '', '', ''),
    ('g2', 'experiment:get_applicable_variants', '*', 'read', '', '', ''),
    ('p', 'experiment_reader', '*', 'experiment', 'read', '*', ''),

    -- experiment_group
    ('g2', 'experiment_group:get', '*', 'read', '', '', ''),
    ('g2', 'experiment_group:list', '*', 'read', '', '', ''),
    ('p', 'experiment_group_reader', '*', 'experiment_group', 'read', '*', ''),

    -- variable
    ('g2', 'variable:get', '*', 'read', '', '', ''),
    ('g2', 'variable:list', '*', 'read', '', '', ''),
    ('p', 'variable_reader', '*', 'variable', 'read', '*', ''),

    -- secret
    ('g2', 'secret:get', '*', 'read', '', '', ''),
    ('g2', 'secret:list', '*', 'read', '', '', ''),
    ('p', 'secret_reader', '*', 'secret', 'read', '*', ''),

    -- webhook
    ('g2', 'webhook:get', '*', 'read', '', '', ''),
    ('g2', 'webhook:list', '*', 'read', '', '', ''),
    ('g2', 'webhook:get_by_event', '*', 'read', '', '', ''),
    ('p', 'webhook_reader', '*', 'webhook', 'read', '*', ''),

    -- workspace
    ('g2', 'workspace:get', '*', 'read', '', '', ''),
    ('g2', 'workspace:list', '*', 'read', '', '', ''),
    ('p', 'workspace_reader', '*', 'workspace', 'read', '*', '')

    -- TODO: Figure out what to do about Auth and Organisation resources, which don't fit neatly into the "workspace" schema. For now, we can use a wildcard schema ("*") for these resources in the policy rules, but this is something we may want to revisit for more fine-grained control in the future.
ON CONFLICT (ptype, v0, v1, v2, v3, v4, v5) DO NOTHING;

