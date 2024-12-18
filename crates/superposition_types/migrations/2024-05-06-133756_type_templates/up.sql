-- Your SQL goes here
CREATE TABLE IF NOT EXISTS public.type_templates (
    type_name TEXT PRIMARY KEY,
    type_schema JSON NOT NULL,
    created_by TEXT NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    last_modified TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL
);
CREATE INDEX IF NOT EXISTS type_templates_index ON public.type_templates(type_name);
CREATE INDEX IF NOT EXISTS type_templates_created_at_index ON public.type_templates(created_at);
CREATE INDEX IF NOT EXISTS type_templates_last_modifed_index ON public.type_templates(last_modified);
INSERT INTO public.type_templates(type_name, type_schema, created_by, created_at, last_modified)
VALUES (
        'Number',
        '{"type": "integer"}',
        'user@superposition.io',
        '2024-10-18 10:34:00.376562+00',
        '2024-10-18 10:34:00.376562+00'
    ),
    (
        'Decimal',
        '{"type": "number"}',
        'user@superposition.io',
        '2024-10-18 10:35:00.376562+00',
        '2024-10-18 10:35:00.376562+00'
    ),
    (
        'Boolean',
        '{"type": "boolean"}',
        'user@superposition.io',
        '2024-10-18 10:36:00.376562+00',
        '2024-10-18 10:36:00.376562+00'
    ),
    (
        'Enum',
        '{"type": "string", "enum": ["android", "ios"]}',
        'user@superposition.io',
        '2024-10-18 10:37:00.376562+00',
        '2024-10-18 10:37:00.376562+00'
    ),
    (
        'Pattern',
        '{"type": "string", "pattern": ".*"}',
        'user@superposition.io',
        '2024-10-18 10:38:00.376562+00',
        '2024-10-18 10:38:00.376562+00'
    );