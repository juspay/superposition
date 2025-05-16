-- Your SQL goes here
CREATE TABLE IF NOT EXISTS public.experiment_groups(
    experiment_group_id TEXT PRIMARY KEY,
    name TEXT NOT NULL,
    description TEXT NOT NULL,
    change_reason TEXT NOT NULL,
    context JSON NOT NULL,
    traffic_percentage integer NOT NULL CONSTRAINT traffic_percentage_range CHECK (traffic_percentage >= 0 AND traffic_percentage <= 100),
    member_experiment_ids TEXT[] NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_by TEXT NOT NULL,
    last_modified_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    last_modified_by TEXT NOT NULL
);