-- Your SQL goes here
-- Name: webhooks; Type: TABLE; Schema: public; Owner: -
--
CREATE TABLE public.webhooks (
    name text PRIMARY KEY,
    description text NOT NULL,
    enabled boolean NOT NULL DEFAULT true,
    url text NOT NULL,
    method text NOT NULL DEFAULT 'POST',
    version text NOT NULL,
    custom_headers json,
    events varchar(100)[] NOT NULL CHECK (array_position(events, NULL) IS NULL),
    max_retries integer NOT NULL DEFAULT 0,
    last_triggered_at timestamp,
    created_by text NOT NULL,
    created_at timestamp without time zone NOT NULL DEFAULT CURRENT_TIMESTAMP,
    last_modified_by text NOT NULL,
    last_modified_at timestamp without time zone NOT NULL DEFAULT CURRENT_TIMESTAMP
);
--
-- Name: webhooks webhooks_audit; Type: TRIGGER; Schema: public; Owner: -
--
CREATE TRIGGER webhooks_audit AFTER INSERT OR DELETE OR UPDATE ON public.webhooks FOR EACH ROW EXECUTE FUNCTION public.event_logger();