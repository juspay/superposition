-- Your SQL goes here
-- Name: functions; Type: TABLE; Schema: public; Owner: -
--
CREATE TABLE public.config_versions (
    id bigint PRIMARY KEY,
    config json NOT NULL,
    config_hash TEXT NOT NULL,
    tags TEXT[] check (array_position(tags, null) is null),
    created_at timestamp without time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);

CREATE INDEX IF NOT EXISTS config_verions_tags_index ON public.config_versions USING gin(tags);
CREATE INDEX IF NOT EXISTS config_versions_id_index ON public.config_versions(id);
--