CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

-- DROP TYPE {{schema}}.experiment_status_type;

CREATE TYPE {{schema}}.experiment_status_type AS ENUM (
	'CREATED',
	'CONCLUDED',
	'INPROGRESS');

-- DROP DOMAIN {{schema}}.not_null_text;

CREATE DOMAIN {{schema}}.not_null_text AS text not null;


-- {{schema}}.event_log definition

-- Drop table

-- DROP TABLE {{schema}}.event_log;

CREATE TABLE {{schema}}.event_log (
	id uuid NOT NULL DEFAULT uuid_generate_v4(),
	table_name text NOT NULL,
	user_name text NOT NULL,
	"timestamp" timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
	"action" text NOT NULL,
	original_data json NULL,
	new_data json NULL,
	query text NOT NULL,
	CONSTRAINT event_log_pkey PRIMARY KEY (id, "timestamp")
)
PARTITION BY RANGE ("timestamp");
CREATE INDEX event_log_action_index ON ONLY {{schema}}.event_log USING btree (action) INCLUDE ("timestamp", table_name);
CREATE INDEX event_log_table_name_index ON ONLY {{schema}}.event_log USING btree (table_name) INCLUDE (action, "timestamp");
CREATE INDEX event_log_timestamp_index ON ONLY {{schema}}.event_log USING btree ("timestamp") INCLUDE (action, table_name);

CREATE OR REPLACE FUNCTION {{schema}}.event_logger()
 RETURNS trigger
 LANGUAGE plpgsql
AS $function$
DECLARE
    old_data json;
    new_data json;
BEGIN
    IF (TG_OP = 'UPDATE') THEN
        old_data := row_to_json(OLD);
        new_data := row_to_json(NEW);

        INSERT INTO {{schema}}.event_log
            (table_name, user_name, action, original_data, new_data, query)
            VALUES (
                TG_TABLE_NAME::TEXT,
                session_user::TEXT,
                TG_OP,
                old_data,
                new_data,
                current_query()
            );

    ELSIF (TG_OP = 'DELETE') THEN
        old_data := row_to_json(OLD);

        INSERT INTO {{schema}}.event_log
            (table_name, user_name, action, original_data, query)
            VALUES (
                TG_TABLE_NAME::TEXT,
                session_user::TEXT,
                TG_OP,
                old_data,
                current_query()
            );

    ELSIF (TG_OP = 'INSERT') THEN
        new_data = row_to_json(NEW);

        INSERT INTO {{schema}}.event_log
            (table_name, user_name, action, new_data, query)
            VALUES (
                TG_TABLE_NAME::TEXT,
                session_user::TEXT,
                TG_OP,
                new_data,
                current_query()
            );
    END IF;

    RETURN NULL;
END;
$function$
;

-- {{schema}}.experiments definition

-- Drop table

-- DROP TABLE {{schema}}.experiments;

CREATE TABLE {{schema}}.experiments (
	id int8 NOT NULL,
	created_at timestamptz NOT NULL DEFAULT CURRENT_TIMESTAMP,
	created_by text NOT NULL,
	last_modified timestamptz NOT NULL DEFAULT now(),
	"name" text NOT NULL,
	override_keys {{schema}}.not_null_text [] NOT NULL,
	status {{schema}}.experiment_status_type NOT NULL,
	traffic_percentage int4 NOT NULL,
	context json NOT NULL,
	variants json NOT NULL,
	last_modified_by text NOT NULL DEFAULT 'Null'::text,
	chosen_variant text NULL,
	CONSTRAINT experiments_pkey PRIMARY KEY (id),
	CONSTRAINT experiments_traffic_percentage_check CHECK ((traffic_percentage >= 0))
);
CREATE INDEX experiment_created_date_index ON {{schema}}.experiments USING btree (created_at) INCLUDE (id);
CREATE INDEX experiment_last_modified_index ON {{schema}}.experiments USING btree (last_modified) INCLUDE (id, created_at);
CREATE INDEX experiment_status_index ON {{schema}}.experiments USING btree (status) INCLUDE (created_at, last_modified);

-- Table Triggers

create trigger experiments_audit after
insert
    or
delete
    or
update
    on
    {{schema}}.experiments for each row execute function {{schema}}.event_logger();


-- {{schema}}.event_log_y2023m08 definition

CREATE TABLE {{schema}}.event_log_y2023m08 PARTITION OF {{schema}}.event_log  FOR VALUES FROM ('2023-08-01 00:00:00') TO ('2023-09-01 00:00:00');


-- {{schema}}.event_log_y2023m09 definition

CREATE TABLE {{schema}}.event_log_y2023m09 PARTITION OF {{schema}}.event_log  FOR VALUES FROM ('2023-09-01 00:00:00') TO ('2023-10-01 00:00:00');


-- {{schema}}.event_log_y2023m10 definition

CREATE TABLE {{schema}}.event_log_y2023m10 PARTITION OF {{schema}}.event_log  FOR VALUES FROM ('2023-10-01 00:00:00') TO ('2023-11-01 00:00:00');


-- {{schema}}.event_log_y2023m11 definition

CREATE TABLE {{schema}}.event_log_y2023m11 PARTITION OF {{schema}}.event_log  FOR VALUES FROM ('2023-11-01 00:00:00') TO ('2023-12-01 00:00:00');


-- {{schema}}.event_log_y2023m12 definition

CREATE TABLE {{schema}}.event_log_y2023m12 PARTITION OF {{schema}}.event_log  FOR VALUES FROM ('2023-12-01 00:00:00') TO ('2024-01-01 00:00:00');


-- {{schema}}.event_log_y2024m01 definition

CREATE TABLE {{schema}}.event_log_y2024m01 PARTITION OF {{schema}}.event_log  FOR VALUES FROM ('2024-01-01 00:00:00') TO ('2024-02-01 00:00:00');


-- {{schema}}.event_log_y2024m02 definition

CREATE TABLE {{schema}}.event_log_y2024m02 PARTITION OF {{schema}}.event_log  FOR VALUES FROM ('2024-02-01 00:00:00') TO ('2024-03-01 00:00:00');


-- {{schema}}.event_log_y2024m03 definition

CREATE TABLE {{schema}}.event_log_y2024m03 PARTITION OF {{schema}}.event_log  FOR VALUES FROM ('2024-03-01 00:00:00') TO ('2024-04-01 00:00:00');


-- {{schema}}.event_log_y2024m04 definition

CREATE TABLE {{schema}}.event_log_y2024m04 PARTITION OF {{schema}}.event_log  FOR VALUES FROM ('2024-04-01 00:00:00') TO ('2024-05-01 00:00:00');


-- {{schema}}.event_log_y2024m05 definition

CREATE TABLE {{schema}}.event_log_y2024m05 PARTITION OF {{schema}}.event_log  FOR VALUES FROM ('2024-05-01 00:00:00') TO ('2024-06-01 00:00:00');


-- {{schema}}.event_log_y2024m06 definition

CREATE TABLE {{schema}}.event_log_y2024m06 PARTITION OF {{schema}}.event_log  FOR VALUES FROM ('2024-06-01 00:00:00') TO ('2024-07-01 00:00:00');


-- {{schema}}.event_log_y2024m07 definition

CREATE TABLE {{schema}}.event_log_y2024m07 PARTITION OF {{schema}}.event_log  FOR VALUES FROM ('2024-07-01 00:00:00') TO ('2024-08-01 00:00:00');