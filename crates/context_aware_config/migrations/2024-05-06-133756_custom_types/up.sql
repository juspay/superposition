-- Your SQL goes here
CREATE TABLE jsonschema_types (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
    type_name TEXT NOT NULL,
    display_name TEXT NOT NULL,
    type_schema JSON NOT NULL,
    created_by TEXT NOT NULL,
    created_at TIMESTAMP NOT NULL,
    last_modified TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL
);
ALTER TABLE jsonschema_types
ADD CONSTRAINT unique_display_name UNIQUE (display_name);
CREATE INDEX IF NOT EXISTS json_schema_types_index ON ONLY public.jsonschema_types(type_name, created_at);
INSERT INTO jsonschema_types(type_name, display_name, type_schema, created_by, created_at)
VALUES (
        'number',
        'Number',
        '{"type": "number"}',
        'user@superposition.io',
        NOW()
    ),
    (
        'number',
        'Decimal',
        '{"type": "number"}',
        'user@superposition.io',
        NOW()
    ),
    (
        'boolean',
        'Boolean',
        '{"type": "boolean"}',
        'user@superposition.io',
        NOW()
    ),
    (
        'enum',
        'String (Enum)',
        '{"type": "string", "enum": "$replacement"}',
        'user@superposition.io',
        NOW()
    ),
    (
        'pattern',
        'String (Regex)',
        '{"type": "string", "pattern": "$replacement"}',
        'user@superposition.io',
        NOW()
    );