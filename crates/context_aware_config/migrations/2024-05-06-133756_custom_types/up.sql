-- Your SQL goes here
CREATE TABLE jsonschema_types (
    type_name TEXT PRIMARY KEY,
    type_schema JSON NOT NULL,
    created_by TEXT NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    last_modified TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL
);
CREATE INDEX IF NOT EXISTS json_schema_types_index ON public.jsonschema_types(type_name);
CREATE INDEX IF NOT EXISTS json_schema_types_created_at_index ON public.jsonschema_types(created_at);
CREATE INDEX IF NOT EXISTS json_schema_types_last_modifed_index ON public.jsonschema_types(last_modified);
INSERT INTO jsonschema_types(type_name, type_schema, created_by, created_at)
VALUES (
        'Number',
        '{"type": "number"}',
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
        '{"type": "string", "enum": "$replacement"}',
        'user@superposition.io',
        NOW()
    ),
    (
        'Pattern',
        '{"type": "string", "pattern": "$replacement"}',
        'user@superposition.io',
        NOW()
    );