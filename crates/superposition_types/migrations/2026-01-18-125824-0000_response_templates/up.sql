CREATE TABLE IF NOT EXISTS public.response_templates (
    name VARCHAR(20) PRIMARY KEY,
    description TEXT NOT NULL,
    change_reason TEXT NOT NULL,
    context_id TEXT NOT NULL,
    context JSON NOT NULL,
    content_type VARCHAR(50) NOT NULL,
    template TEXT NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    last_modified_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    created_by TEXT NOT NULL,
    last_modified_by TEXT NOT NULL
);

CREATE INDEX IF NOT EXISTS idx_response_templates_name ON public.response_templates(name);
CREATE INDEX IF NOT EXISTS idx_response_templates_created_at ON public.response_templates(created_at);
CREATE INDEX IF NOT EXISTS idx_response_templates_last_modified_at ON public.response_templates(last_modified_at);
CREATE TRIGGER response_templates_audit AFTER INSERT OR DELETE OR UPDATE ON public.response_templates FOR EACH ROW EXECUTE FUNCTION public.event_logger();
