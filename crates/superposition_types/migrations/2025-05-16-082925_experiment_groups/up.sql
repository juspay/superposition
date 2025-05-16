-- Your SQL goes here
CREATE TABLE IF NOT EXISTS public.experiment_groups(
    hash TEXT PRIMARY KEY,
    name TEXT NOT NULL,
    description TEXT NOT NULL,
    context JSON NOT NULL,
    traffic_percentage integer NOT NULL CONSTRAINT traffic_percentage_range CHECK (traffic_percentage >= 0 AND traffic_percentage <= 100),
    experiments_inside_ids TEXT[] NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_by TEXT NOT NULL,
    last_modified_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    last_modified_by TEXT NOT NULL
);