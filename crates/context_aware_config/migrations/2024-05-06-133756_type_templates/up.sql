-- Your SQL goes here
CREATE TABLE type_templates (
    type_name TEXT PRIMARY KEY,
    type_schema JSON NOT NULL,
    created_by TEXT NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    last_modified TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL
);
CREATE INDEX IF NOT EXISTS type_templates_index ON public.type_templates(type_name);
CREATE INDEX IF NOT EXISTS type_templates_created_at_index ON public.type_templates(created_at);
CREATE INDEX IF NOT EXISTS type_templates_last_modifed_index ON public.type_templates(last_modified);
INSERT INTO type_templates(type_name, type_schema, created_by, created_at)
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
