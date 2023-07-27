ALTER TABLE contexts ADD override JSON NOT NULL DEFAULT '{}';
DROP TABLE overrides;
