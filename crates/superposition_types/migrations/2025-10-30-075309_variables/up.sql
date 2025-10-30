CREATE TABLE IF NOT EXISTS public.variables (
    name VARCHAR(50) PRIMARY KEY, 
    value TEXT NOT NULL,
    description TEXT NOT NULL,
    change_reason TEXT NOT NULL,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    last_modified_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    created_by VARCHAR(200) NOT NULL,
    last_modified_by VARCHAR(200) NOT NULL
);

CREATE INDEX IF NOT EXISTS idx_variables_created_at ON public.variables(created_at);
CREATE INDEX IF NOT EXISTS idx_variables_last_modified_at ON public.variables(last_modified_at);
CREATE TRIGGER variables_audit AFTER INSERT OR DELETE OR UPDATE ON public.variables FOR EACH ROW EXECUTE FUNCTION public.event_logger();