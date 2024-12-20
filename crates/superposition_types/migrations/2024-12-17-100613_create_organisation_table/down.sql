-- down.sql
DROP INDEX IF EXISTS superposition.idx_organisation_admin_email;
DROP INDEX IF EXISTS superposition.idx_organisation_created_at;
DROP INDEX IF EXISTS superposition.idx_organisation_status;
DROP INDEX IF EXISTS superposition.idx_organisation_contact_email;
DROP TABLE IF EXISTS superposition.organisation;
DROP TYPE IF EXISTS superposition.org_status;
DROP SCHEMA IF EXISTS superposition;