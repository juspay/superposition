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


INSERT INTO superposition.organisations(id, admin_email, name, created_by, updated_by) values(
    'testorg',
    'test@example.com',
    'testorg',
    'test_user',
    'test_user'
);