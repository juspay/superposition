-- Your SQL goes here
-- Name: webhooks; Type: TABLE; Schema: public; Owner: -
--

CREATE TYPE public.http_method AS ENUM (
    'GET',
    'PUT',
    'POST',
    'DELETE',
    'PATCH',
    'HEAD'
);

CREATE TABLE public.webhooks (
    name text PRIMARY KEY,
    description text NOT NULL,
    enabled boolean NOT NULL,
    url text NOT NULL,
    method public.http_method NOT NULL DEFAULT 'POST',
    payload_version text NOT NULL,
    custom_headers json NOT NULL DEFAULT '{}'::json,
    events text[] NOT NULL,
    max_retries integer NOT NULL DEFAULT 0,
    last_triggered_at timestamp without time zone,
    change_reason TEXT NOT NULL,
    created_by text NOT NULL,
    created_at timestamp without time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
    last_modified_by text NOT NULL,
    last_modified_at timestamp without time zone NOT NULL DEFAULT CURRENT_TIMESTAMP
);
--
-- Name: webhooks webhooks_audit; Type: TRIGGER; Schema: public; Owner: -
--
CREATE TRIGGER webhooks_audit AFTER INSERT OR DELETE OR UPDATE ON public.webhooks FOR EACH ROW EXECUTE FUNCTION public.event_logger();